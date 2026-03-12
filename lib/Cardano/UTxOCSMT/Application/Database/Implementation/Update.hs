module Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate
    , mkSplitUpdate
    , newSplitState

      -- * Bench mode
    , BenchOps (..)
    , allOps
    , noOps

      -- * Tracing
    , UpdateTrace (..)
    , renderUpdateTrace
    , measureUpdateDurations

      -- * Low level operations, exposed for testing
    , forwardTip
    , updateRollbackPoint
    , sampleRollbackPoints
    , newState
    , newFinality
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , ArmageddonTrace
    , armageddon
    , renderArmageddonTrace
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( Meta
    , RollbackPointKV
    , pattern UTxORollbackPoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps (..)
    , RunTransaction (..)
    , journalEmpty
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , TipOf
    , Update (..)
    )
import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer)
import Data.Function (fix)
import Data.List.SampleFibonacci
    ( sampleAtFibonacciIntervals
    )

import Data.Monoid (Sum (..))

import Data.Tracer.Measure (Timing (..), measureDuration)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , lastEntry
    , nextEntry
    , prevEntry
    )
import Database.KV.Transaction
    ( Transaction
    , delete
    , iterating
    , query
    )
import MTS.Rollbacks.Store qualified as Store
import MTS.Rollbacks.Types qualified as RP
import Ouroboros.Network.Point (WithOrigin (..))

-- | Controls which DB operations run inside 'forwardTip'.
data BenchOps = BenchOps
    { doQueryTip :: Bool
    -- ^ Read tip from rollback points
    , doCsmt :: Bool
    -- ^ Run CSMT insert/delete operations
    , doRollback :: Bool
    -- ^ Store rollback point (includes root hash read)
    }

-- | All operations enabled (normal mode).
allOps :: BenchOps
allOps = BenchOps True True True

-- | No operations (baseline: transaction overhead only).
noOps :: BenchOps
noOps = BenchOps False False False

-- | Query the current tip directly from rollback points.
queryTip
    :: MonadFail m
    => Transaction m cf (Columns slot hash key value) op (WithOrigin slot)
queryTip = do
    mt <- Store.queryTip RollbackPoints
    case mt of
        Nothing -> lift $ fail "No tip in rollback points"
        Just t -> pure t

-- | Trace events for database update operations.
data UpdateTrace slot hash
    = UpdateArmageddon ArmageddonTrace
    | -- | CSMT ops begin (inside transaction)
      UpdateCSMTBegin slot
    | -- | CSMT ops done (before rollback point)
      UpdateCSMTDone slot Int Int
    | -- | CSMT ops with measured duration (seconds)
      UpdateCSMTMeasured slot Int Int Double
    | -- | Rollback point begin
      UpdateRollbackBegin slot
    | -- | Rollback point stored (after CSMT ops)
      UpdateForwardTip slot Int Int (Maybe hash)
    | -- | Rollback point with measured duration (seconds)
      UpdateRollbackMeasured
        slot
        Int
        Int
        (Maybe hash)
        Double
    | UpdateNewState [slot]
    | UpdateJournalReplay
    | -- | Finality begin (before pruning)
      UpdateFinalityBegin slot
    | -- | Finality pruning complete
      UpdateFinalityDone slot Int
    | -- | Finality with measured duration (seconds)
      UpdateFinalityMeasured slot Int Double
    | -- | Per-UTxO timing: inserts then deletes (μs per operation)
      UpdatePerUtxoTiming [Double] [Double]
    | -- | Clock overhead: consecutive clock reads (μs)
      UpdateClockOverhead Double
    | -- | Loop overhead: clock-only iterations (μs per iter)
      UpdateLoopOverhead Double
    | -- | Raw Map.insert timing outside Transaction monad (μs per iter)
      UpdateRawMapInsert Double
    | -- | Sub-operation breakdown: kvInsert μs, serialize μs, journalInsert μs
      UpdateSubOps Double Double Double
    | -- | Full transact begin (before transact call)
      UpdateTransactBegin slot
    | -- | Full transact done (after transact returns)
      UpdateTransactDone slot
    | -- | Full transact with measured duration (seconds)
      UpdateTransactMeasured slot Double
    deriving (Show)

-- | Render an 'UpdateTrace' to a human-readable string.
renderUpdateTrace
    :: Show slot => UpdateTrace slot hash -> String
renderUpdateTrace (UpdateArmageddon t) =
    renderArmageddonTrace t
renderUpdateTrace (UpdateCSMTBegin slot) =
    "CSMT begin at " ++ show slot
renderUpdateTrace (UpdateCSMTDone slot nInsert nDelete) =
    "CSMT done at "
        ++ show slot
        ++ ": "
        ++ show nInsert
        ++ " inserts, "
        ++ show nDelete
        ++ " deletes"
renderUpdateTrace
    (UpdateCSMTMeasured slot nInsert nDelete secs) =
        "CSMT at "
            ++ show slot
            ++ ": "
            ++ show nInsert
            ++ " inserts, "
            ++ show nDelete
            ++ " deletes ("
            ++ show (round (secs * 1e6) :: Int)
            ++ "μs)"
renderUpdateTrace (UpdateRollbackBegin slot) =
    "Rollback begin at " ++ show slot
renderUpdateTrace (UpdateForwardTip slot nInsert nDelete _) =
    "Rollback point at "
        ++ show slot
        ++ ": "
        ++ show nInsert
        ++ " inserts, "
        ++ show nDelete
        ++ " deletes"
renderUpdateTrace
    ( UpdateRollbackMeasured
            slot
            nInsert
            nDelete
            _
            secs
        ) =
        "Rollback point at "
            ++ show slot
            ++ ": "
            ++ show nInsert
            ++ " inserts, "
            ++ show nDelete
            ++ " deletes ("
            ++ show (round (secs * 1e6) :: Int)
            ++ "μs)"
renderUpdateTrace (UpdateNewState slots) =
    "New update state with rollback points at: "
        ++ show slots
renderUpdateTrace UpdateJournalReplay =
    "Replaying journal to build CSMT tree"
renderUpdateTrace (UpdateFinalityBegin slot) =
    "Finality begin at " ++ show slot
renderUpdateTrace (UpdateFinalityDone slot pruned) =
    "Finality done at "
        ++ show slot
        ++ ": pruned "
        ++ show pruned
        ++ " rollback points"
renderUpdateTrace (UpdateFinalityMeasured slot pruned secs) =
    "Finality at "
        ++ show slot
        ++ ": pruned "
        ++ show pruned
        ++ " rollback points ("
        ++ show (round (secs * 1e6) :: Int)
        ++ "μs)"
renderUpdateTrace (UpdateClockOverhead us) =
    "ClockOverhead: "
        ++ show (round us :: Int)
        ++ "μs"
renderUpdateTrace (UpdateLoopOverhead us) =
    "LoopOverhead: "
        ++ show (round us :: Int)
        ++ "μs/iter"
renderUpdateTrace (UpdateRawMapInsert us) =
    "RawMapInsert: "
        ++ show (round us :: Int)
        ++ "μs/iter"
renderUpdateTrace (UpdateSubOps kvUs serUs jrnUs) =
    "SubOps: kv="
        ++ show (round kvUs :: Int)
        ++ "μs ser="
        ++ show (round serUs :: Int)
        ++ "μs jrn="
        ++ show (round jrnUs :: Int)
        ++ "μs"
renderUpdateTrace (UpdatePerUtxoTiming inserts deletes) =
    let showStats label ts = case ts of
            [] -> label ++ ": none"
            _ ->
                label
                    ++ ": n="
                    ++ show (length ts)
                    ++ " min="
                    ++ show (round (minimum ts) :: Int)
                    ++ "μs max="
                    ++ show (round (maximum ts) :: Int)
                    ++ "μs mean="
                    ++ show
                        ( round (sum ts / fromIntegral (length ts))
                            :: Int
                        )
                    ++ "μs"
    in  showStats "Ins" inserts
            ++ " | "
            ++ showStats "Del" deletes
renderUpdateTrace (UpdateTransactBegin slot) =
    "Transact begin at " ++ show slot
renderUpdateTrace (UpdateTransactDone slot) =
    "Transact done at " ++ show slot
renderUpdateTrace (UpdateTransactMeasured slot secs) =
    "Transact at "
        ++ show slot
        ++ " ("
        ++ show (round (secs * 1e6) :: Int)
        ++ "μs)"

newState
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> BenchOps
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward; use to check if at tip and emit Synced
    -> ArmageddonParams hash
    -> RunTransaction cf op slot hash key value m
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> m (Update m slot key value, [slot])
newState
    TraceWith{tracer, trace}
    benchOps
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam = do
        (cps, rollbackCount) <-
            transact $ do
                cps <-
                    iterating
                        RollbackPoints
                        sampleRollbackPoints
                rollbackCount <-
                    Store.countPoints RollbackPoints
                pure (cps, rollbackCount)
        trace $ UpdateNewState cps
        let update =
                mkUpdate
                    tracer
                    benchOps
                    ops
                    slotHash
                    onForward
                    armageddonParams
                    runTransaction
                    securityParam
                    rollbackCount
        pure (update, cps)

{- | Initialize split-mode state.

Start in KVOnly mode unless the journal is empty AND the database
already has rollback points (meaning a previous KVOnly phase
completed and replayed successfully). A fresh empty database
starts in KVOnly mode.
-}
newSplitState
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> BenchOps
    -> Bool
    -- ^ Is this a fresh (genesis) database?
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -- ^ KVOnly ops (phase 1)
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -- ^ Full ops (phase 2)
    -> m ()
    -- ^ Replay callback
    -> (slot -> TipOf slot -> Bool)
    -- ^ Tip detection predicate
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -> ArmageddonParams hash
    -> RunTransaction cf op slot hash key value m
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> m (Update m slot key value, [slot])
newSplitState
    tw@TraceWith{tracer, trace}
    benchOps
    isGenesis
    kvOps
    fullOps
    replay
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam = do
        (cps, rollbackCount, empty) <-
            transact $ do
                cps <-
                    iterating
                        RollbackPoints
                        sampleRollbackPoints
                rollbackCount <-
                    Store.countPoints RollbackPoints
                empty <- journalEmpty
                pure (cps, rollbackCount, empty)
        trace $ UpdateNewState cps
        let update =
                if empty && not isGenesis
                    then
                        mkUpdate
                            tracer
                            benchOps
                            fullOps
                            slotHash
                            onForward
                            armageddonParams
                            runTransaction
                            securityParam
                            rollbackCount
                    else
                        mkSplitUpdate
                            tw
                            benchOps
                            kvOps
                            fullOps
                            replay
                            isAtTip
                            slotHash
                            onForward
                            armageddonParams
                            runTransaction
                            securityParam
                            rollbackCount
        pure (update, cps)

-- | Apply forward tip.
forwardTip
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> BenchOps
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> hash
    -> Int
    -- ^ rollback point count
    -> slot
    -- ^ slot at which operations happen
    -> [Operation key value]
    -- ^ operations to apply
    -> Transaction
        m
        cf
        (Columns slot hash key value)
        op
        Bool
forwardTip
    TraceWith{trace}
    BenchOps{doQueryTip, doCsmt, doRollback}
    CSMTOps{csmtInsert, csmtDelete, csmtRootHash}
    hash
    count
    slot
    ops = do
        let io = lift . lift
        tip <-
            if doQueryTip
                then queryTip
                else pure Origin
        if At slot > tip
            && (not (null ops) || count < 2)
            then do
                io . trace $ UpdateCSMTBegin slot
                result <-
                    if doCsmt
                        then forM (zip [0 :: Int ..] ops)
                            $ \case
                                (_, Insert k v) -> do
                                    csmtInsert k v
                                    pure (Sum 1, Sum 0, [Delete k])
                                (i, Delete k) -> do
                                    mx <- query KVCol k
                                    csmtDelete k
                                    case mx of
                                        Nothing ->
                                            error
                                                $ "forwardTip: cannot"
                                                    <> " invert Delete"
                                                    <> " at slot "
                                                    <> show slot
                                                    <> " op #"
                                                    <> show i
                                        Just x ->
                                            pure
                                                ( Sum 0
                                                , Sum 1
                                                , [Insert k x]
                                                )
                        else pure []
                let (Sum nInserts, Sum nDeletes, invs) =
                        mconcat result
                io . trace
                    $ UpdateCSMTDone slot nInserts nDeletes
                if doRollback
                    then do
                        io . trace $ UpdateRollbackBegin slot
                        merkleRoot <-
                            updateRollbackPoint
                                csmtRootHash
                                hash
                                slot
                                $ reverse invs
                        io . trace
                            $ UpdateForwardTip
                                slot
                                nInserts
                                nDeletes
                                merkleRoot
                    else
                        io . trace
                            $ UpdateForwardTip
                                slot
                                nInserts
                                nDeletes
                                Nothing
                pure True
            else pure False

updateRollbackPoint
    :: (Ord slot, Monad m)
    => Transaction m cf (Columns slot hash key value) op (Maybe hash)
    -- ^ Root hash query
    -> hash
    -> slot
    -> [Operation key value]
    -> Transaction m cf (Columns slot hash key value) op (Maybe hash)
updateRollbackPoint rootHashQuery pointHash pointSlot inverseOps = do
    merkleRoot <- rootHashQuery
    Store.storeRollbackPoint RollbackPoints (At pointSlot)
        $ UTxORollbackPoint pointHash inverseOps merkleRoot
    pure merkleRoot

{- | Prune oldest rollback points when count exceeds
the security parameter k. Returns number pruned.
-}
pruneExcess
    :: (Ord slot, Monad m)
    => Int
    -> Int
    -> RunTransaction cf op slot hash key value m
    -> m Int
pruneExcess securityParam currentCount RunTransaction{transact}
    | excess <= 0 = pure 0
    | otherwise =
        transact
            $ iterating RollbackPoints
            $ do
                me <- firstEntry
                go excess me
  where
    excess = currentCount - securityParam
    go 0 _ = pure 0
    go _ Nothing = pure 0
    go n (Just Entry{entryKey}) = do
        lift $ delete RollbackPoints entryKey
        next <- nextEntry
        (+ 1) <$> go (n - 1) next

sampleRollbackPoints
    :: Monad m
    => Cursor
        (Transaction m cf (Columns slot hash key value) op)
        (RollbackPointKV slot hash key value)
        [slot]
sampleRollbackPoints = do
    me <- lastEntry
    case me of
        Nothing -> pure []
        Just h -> do
            rest <- sampleAtFibonacciIntervals prevEntry
            pure $ keepAts . fmap entryKey $ h : rest

keepAts :: [WithOrigin a] -> [a]
keepAts = flip foldr [] $ \case
    Origin -> id
    At x -> (x :)

-- | Apply inverse operations from a rollback point
rollbackRollbackPoint
    :: Monad m
    => CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> RP.RollbackPoint (Operation key value) (Meta hash)
    -> Transaction m cf (Columns slot hash key value) op ()
rollbackRollbackPoint CSMTOps{csmtInsert, csmtDelete} (UTxORollbackPoint _ ops _) =
    forM_ ops $ \case
        Insert k v -> csmtInsert k v
        Delete k -> csmtDelete k

{- | Create a database update state object that threads a
rollback point counter through each continuation.
-}
mkUpdate
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> BenchOps
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward
    -> ArmageddonParams hash
    -- ^ Armageddon parameters
    -> RunTransaction cf op slot hash key value m
    -- ^ Function to run a transaction
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> Int
    -- ^ Initial rollback point count
    -> Update m slot key value
mkUpdate
    tw@TraceWith{contra, trace}
    benchOps
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam =
        go
      where
        go count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        trace $ UpdateTransactBegin slot
                        stored <-
                            transact
                                $ forwardTip
                                    tw
                                    benchOps
                                    ops
                                    (slotHash slot)
                                    count
                                    slot
                                    operations
                        trace $ UpdateTransactDone slot
                        onForward slot chainTip
                        if stored
                            then do
                                let newCount = count + 1
                                pruned <-
                                    pruneExcess
                                        securityParam
                                        newCount
                                        runTransaction
                                pure $ go (newCount - pruned)
                            else pure $ go count
                , rollbackTipApply = \case
                    At s -> do
                        r <-
                            transact $ do
                                tip <- queryTip
                                if At s > tip
                                    then
                                        pure
                                            $ Store.RollbackSucceeded
                                                0
                                    else
                                        Store.rollbackTo
                                            RollbackPoints
                                            ( rollbackRollbackPoint
                                                ops
                                            )
                                            (At s)
                        case r of
                            Store.RollbackSucceeded deleted ->
                                pure
                                    $ Syncing
                                    $ go (count - deleted)
                            Store.RollbackImpossible -> do
                                armageddonCall
                                pure
                                    $ Truncating
                                    $ go 0
                    Origin -> do
                        armageddonCall
                        pure $ Syncing $ go 0
                , forwardFinalityApply = \s -> do
                    trace $ UpdateFinalityBegin s
                    pruned <-
                        transact
                            $ Store.pruneBelow
                                RollbackPoints
                                (At s)
                    trace $ UpdateFinalityDone s pruned
                    pure $ go (count - pruned)
                }
        armageddonCall =
            armageddon
                (contra UpdateArmageddon)
                runTransaction
                armageddonParams

{- | Two-phase update: starts in KVOnly mode, switches to Full
after replaying the journal on first tip-touch.

The follower is blocked during replay (forwardTipApply runs
synchronously), making the transition atomic w.r.t. chain sync.
-}
mkSplitUpdate
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> BenchOps
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -- ^ KVOnly ops (phase 1)
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -- ^ Full ops (phase 2)
    -> m ()
    -- ^ Replay callback
    -> (slot -> TipOf slot -> Bool)
    -- ^ Tip detection predicate
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward
    -> ArmageddonParams hash
    -> RunTransaction cf op slot hash key value m
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> Int
    -- ^ Initial rollback point count
    -> Update m slot key value
mkSplitUpdate
    tw@TraceWith{contra, trace}
    benchOps
    kvOps
    fullOps
    replay
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam =
        goKVOnly
      where
        goKVOnly count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        trace $ UpdateTransactBegin slot
                        stored <-
                            transact
                                $ forwardTip
                                    tw
                                    benchOps
                                    kvOps
                                    (slotHash slot)
                                    count
                                    slot
                                    operations
                        trace $ UpdateTransactDone slot
                        onForward slot chainTip
                        count' <-
                            if stored
                                then do
                                    let newCount = count + 1
                                    pruned <-
                                        pruneExcess
                                            securityParam
                                            newCount
                                            runTransaction
                                    pure (newCount - pruned)
                                else pure count
                        if isAtTip slot chainTip
                            then do
                                trace UpdateJournalReplay
                                replay
                                pure $ goFull count'
                            else
                                pure
                                    $ goKVOnly count'
                , rollbackTipApply = \case
                    At s -> do
                        r <-
                            transact $ do
                                tip <- queryTip
                                if At s > tip
                                    then
                                        pure
                                            $ Store.RollbackSucceeded
                                                0
                                    else
                                        Store.rollbackTo
                                            RollbackPoints
                                            ( rollbackRollbackPoint
                                                kvOps
                                            )
                                            (At s)
                        case r of
                            Store.RollbackSucceeded deleted ->
                                pure
                                    $ Syncing
                                    $ goKVOnly
                                        (count - deleted)
                            Store.RollbackImpossible -> do
                                armageddonCall
                                pure
                                    $ Truncating
                                    $ goKVOnly 0
                    Origin -> do
                        armageddonCall
                        pure
                            $ Syncing
                            $ goKVOnly 0
                , forwardFinalityApply = \s -> do
                    trace $ UpdateFinalityBegin s
                    pruned <-
                        transact
                            $ Store.pruneBelow
                                RollbackPoints
                                (At s)
                    trace $ UpdateFinalityDone s pruned
                    pure $ goKVOnly (count - pruned)
                }
        goFull =
            mkUpdate
                tw
                benchOps
                fullOps
                slotHash
                onForward
                armageddonParams
                runTransaction
                securityParam
        armageddonCall =
            armageddon
                (contra UpdateArmageddon)
                runTransaction
                armageddonParams

-- | Determines whether a new finality point can be set
newFinality
    :: MonadFail m
    => (WithOrigin slot -> WithOrigin slot -> Bool)
    -> Transaction m cf (Columns slot hash key value) op (Maybe slot)
newFinality isFinal = do
    tip <- queryTip
    iterating RollbackPoints $ do
        me <- firstEntry
        flip ($ me) Origin $ fix $ \go current finality ->
            case current of
                Nothing -> pure Nothing
                Just Entry{entryKey} ->
                    if isFinal tip entryKey
                        then do
                            current' <- nextEntry
                            go current' entryKey
                        else pure $ case finality of
                            Origin -> Nothing
                            At p -> Just p

{- | Wrap a tracer with duration measurement for
CSMT ops, rollback point storage, and finality.

'UpdateCSMTBegin' → 'UpdateCSMTDone' becomes
'UpdateCSMTMeasured'.
'UpdateCSMTDone' → 'UpdateForwardTip' becomes
'UpdateRollbackMeasured'.
'UpdateFinalityBegin' → 'UpdateFinalityDone' becomes
'UpdateFinalityMeasured'.
-}
measureUpdateDurations
    :: Tracer IO (UpdateTrace slot hash)
    -> IO (Tracer IO (UpdateTrace slot hash))
measureUpdateDurations downstream =
    measureDuration
        Monotonic
        selectTransactBegin
        selectTransactDone
        composeTransact
        =<< measureDuration
            Monotonic
            selectCSMTBegin
            selectCSMTDone
            composeCSMT
        =<< measureDuration
            Monotonic
            selectRollbackBegin
            selectRollbackEnd
            composeRollback
        =<< measureDuration
            Monotonic
            selectFinalityBegin
            selectFinalityEnd
            composeFinality
            downstream
  where
    selectTransactBegin = \case
        UpdateTransactBegin s -> Just s
        _ -> Nothing
    selectTransactDone = \case
        UpdateTransactDone s -> Just s
        _ -> Nothing
    composeTransact _ =
        UpdateTransactMeasured
    selectCSMTBegin = \case
        UpdateCSMTBegin s -> Just s
        _ -> Nothing
    selectCSMTDone = \case
        UpdateCSMTDone s n d -> Just (s, n, d)
        _ -> Nothing
    composeCSMT _ (s, n, d) =
        UpdateCSMTMeasured s n d
    selectRollbackBegin = \case
        UpdateRollbackBegin s -> Just s
        _ -> Nothing
    selectRollbackEnd = \case
        UpdateForwardTip s n d r -> Just (s, n, d, r)
        _ -> Nothing
    composeRollback _ (s, n, d, r) =
        UpdateRollbackMeasured s n d r
    selectFinalityBegin = \case
        UpdateFinalityBegin s -> Just s
        _ -> Nothing
    selectFinalityEnd = \case
        UpdateFinalityDone s p -> Just (s, p)
        _ -> Nothing
    composeFinality _ (s, p) =
        UpdateFinalityMeasured s p
