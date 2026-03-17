module Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate
    , mkSplitUpdate
    , newSplitState

      -- * Phase-aware ops selector
    , Phase (..)
    , SplitMode (..)
    , mkSplitMode

      -- * Forward operation control
    , ForwardOps (..)
    , fullForwardOps
    , kvOnlyForwardOps
    , benchBaselineOps

      -- * Tracing
    , UpdateTrace (..)
    , renderUpdateTrace
    , measureUpdateDurations

      -- * Low level operations, exposed for testing
    , forwardTip
    , updateRollbackPoint
    , sampleRollbackPoints
    , newState
    )
where

import CSMT.MTS (CommonOps (..), Ops (..), journalEmptyT)
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
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , TipOf
    , Update (..)
    )
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.SampleFibonacci
    ( sampleAtFibonacciIntervals
    )
import MTS.Interface qualified as MTS (Mode (..))

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

{- | Controls which DB operations run inside 'forwardTip'.

Used to distinguish KVOnly sync (skip rollback storage
for throughput) from Full sync (all operations).
-}
data ForwardOps = ForwardOps
    { doQueryTip :: Bool
    -- ^ Read tip from rollback points
    , doCsmt :: Bool
    -- ^ Run CSMT insert/delete operations
    , doRollback :: Bool
    -- ^ Store rollback point (includes root hash read)
    }

-- | All operations enabled (Full sync mode).
fullForwardOps :: ForwardOps
fullForwardOps = ForwardOps True True True

{- | KVOnly sync: CSMT insert\/delete runs (journal-only
ops) but skip tip query and rollback point storage.
-}
kvOnlyForwardOps :: ForwardOps
kvOnlyForwardOps = ForwardOps False True False

{- | No operations (benchmark baseline: transaction
overhead only).
-}
benchBaselineOps :: ForwardOps
benchBaselineOps = ForwardOps False False False

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
    "Forward tip at "
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
    -> ForwardOps
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
    -> ( slot
         -> Transaction
                m
                cf
                (Columns slot hash key value)
                op
                ()
       )
    -- ^ Save checkpoint (runs inside forwardTip transaction)
    -> m (Update m slot key value, [slot])
newState
    TraceWith{tracer, trace}
    forwardOps
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam
    saveCheckpoint = do
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
                    forwardOps
                    ops
                    slotHash
                    onForward
                    armageddonParams
                    runTransaction
                    securityParam
                    saveCheckpoint
                    rollbackCount
        pure (update, cps)

{- | Initialize split-mode state.

Start in KVOnly mode unless the journal is empty AND the database
already has rollback points (meaning a previous KVOnly phase
completed and replayed successfully). A fresh empty database
starts in KVOnly mode.

Uses the 'Ops' GADT from MTS for automatic journal replay
and mode transitions.
-}
newSplitState
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       , MonadIO m
       )
    => Tracer m (UpdateTrace slot hash)
    -> ForwardOps
    -> Bool
    -- ^ Is this a fresh (genesis) database?
    -> Ops
        'MTS.KVOnly
        m
        cf
        (Columns slot hash key value)
        op
        key
        value
        hash
    -- ^ KVOnly ops with built-in replay and transition
    -> (slot -> TipOf slot -> Bool)
    -- ^ Tip detection predicate
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -> ArmageddonParams hash
    -> RunTransaction cf op slot hash key value m
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> ( slot
         -> Transaction
                m
                cf
                (Columns slot hash key value)
                op
                ()
       )
    -- ^ Save checkpoint (runs inside forwardTip transaction)
    -> m (Update m slot key value, [slot])
newSplitState
    tw@TraceWith{tracer, trace}
    forwardOps
    isGenesis
    ops
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam
    saveCheckpoint = do
        (cps, rollbackCount, empty) <-
            transact $ do
                cps <-
                    iterating
                        RollbackPoints
                        sampleRollbackPoints
                rollbackCount <-
                    Store.countPoints RollbackPoints
                empty <- journalEmptyT JournalCol
                pure (cps, rollbackCount, empty)
        trace $ UpdateNewState cps
        if empty && not isGenesis
            then do
                -- Transition to Full immediately (replay is
                -- a no-op since journal is empty)
                mFull <- liftIO (toFull ops)
                case mFull of
                    Just fullOps -> do
                        let csmtOps = fullOpsToCSMTOps fullOps
                        pure
                            ( mkUpdate
                                tracer
                                forwardOps
                                csmtOps
                                slotHash
                                onForward
                                armageddonParams
                                runTransaction
                                securityParam
                                saveCheckpoint
                                rollbackCount
                            , cps
                            )
                    Nothing ->
                        fail
                            "newSplitState: toFull failed"
            else
                pure
                    ( mkSplitUpdate
                        tw
                        forwardOps
                        ops
                        isAtTip
                        slotHash
                        onForward
                        armageddonParams
                        runTransaction
                        securityParam
                        saveCheckpoint
                        rollbackCount
                    , cps
                    )

-- | Apply forward tip.
forwardTip
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       )
    => Tracer m (UpdateTrace slot hash)
    -> ForwardOps
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
    ForwardOps{doQueryTip, doCsmt, doRollback}
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
    -> ForwardOps
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
    -> ( slot
         -> Transaction
                m
                cf
                (Columns slot hash key value)
                op
                ()
       )
    -- ^ Save checkpoint (runs inside forwardTip transaction)
    -> Int
    -- ^ Initial rollback point count
    -> Update m slot key value
mkUpdate
    tw@TraceWith{contra, trace}
    forwardOps
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam
    saveCheckpoint =
        go
      where
        go count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        trace $ UpdateTransactBegin slot
                        stored <-
                            transact $ do
                                r <-
                                    forwardTip
                                        tw
                                        forwardOps
                                        ops
                                        (slotHash slot)
                                        count
                                        slot
                                        operations
                                saveCheckpoint slot
                                pure r
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
                }
        armageddonCall =
            armageddon
                (contra UpdateArmageddon)
                runTransaction
                armageddonParams

{- | Two-phase update: starts in KVOnly mode, switches to Full
after replaying the journal on first tip-touch.

Uses the 'Ops' GADT's 'toFull' for automatic journal replay
and mode transition. The follower is blocked during replay
(forwardTipApply runs synchronously).
-}
mkSplitUpdate
    :: ( Ord key
       , Ord slot
       , Show slot
       , MonadFail m
       , MonadIO m
       )
    => Tracer m (UpdateTrace slot hash)
    -> ForwardOps
    -> Ops
        'MTS.KVOnly
        m
        cf
        (Columns slot hash key value)
        op
        key
        value
        hash
    -- ^ KVOnly ops with built-in replay and transition
    -> (slot -> TipOf slot -> Bool)
    -- ^ Tip detection predicate
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward
    -> ArmageddonParams hash
    -> RunTransaction cf op slot hash key value m
    -> Int
    -- ^ Security parameter k (max rollback depth)
    -> ( slot
         -> Transaction
                m
                cf
                (Columns slot hash key value)
                op
                ()
       )
    -- ^ Save checkpoint (runs inside forwardTip transaction)
    -> Int
    -- ^ Initial rollback point count
    -> Update m slot key value
mkSplitUpdate
    tw@TraceWith{contra, trace}
    forwardOps
    ops
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact}
    securityParam
    saveCheckpoint =
        goKVOnly
      where
        kvCSMTOps = kvCommonToCSMTOps (kvCommon ops)
        goKVOnly count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        trace $ UpdateTransactBegin slot
                        stored <-
                            transact $ do
                                r <-
                                    forwardTip
                                        tw
                                        kvOnlyForwardOps
                                        kvCSMTOps
                                        (slotHash slot)
                                        count
                                        slot
                                        operations
                                saveCheckpoint slot
                                pure r
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
                                mFull <-
                                    liftIO (toFull ops)
                                case mFull of
                                    Just fullOps ->
                                        pure
                                            $ goFull
                                                ( fullOpsToCSMTOps
                                                    fullOps
                                                )
                                                count'
                                    Nothing ->
                                        fail
                                            "mkSplitUpdate: toFull failed"
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
                                                kvCSMTOps
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
                }
        goFull fullCSMTOps =
            mkUpdate
                tw
                forwardOps
                fullCSMTOps
                slotHash
                onForward
                armageddonParams
                runTransaction
                securityParam
                saveCheckpoint
        armageddonCall =
            armageddon
                (contra UpdateArmageddon)
                runTransaction
                armageddonParams

-- | Sync phase for split-mode bootstrap.
data Phase = KVOnly | Full
    deriving stock (Show, Eq)

{- | Phase-aware operations selector with transition
logic.

Upstream owns the state machine (phase transitions),
downstream owns the transaction. This lets consumers
like CageFollower use the same KVOnly→Full strategy
without going through the 'Update' continuation API.
-}
data SplitMode m slot tip ops = SplitMode
    { currentOps :: m ops
    -- ^ Get current phase's operations
    , afterForward :: slot -> tip -> m ()
    -- ^ Post-forward hook: trigger transition
    , currentPhase :: m Phase
    -- ^ Query current phase
    }

{- | Create a 'SplitMode' backed by the 'Ops' GADT.

The Ops GADT enforces correctness: 'toFull' replays
the journal and returns Full ops atomically. The
caller only sees 'CSMTOps' — the GADT transitions
are internal.

In 'KVOnly', 'afterForward' checks 'isAtTip'; on
first @True@ it calls 'toFull' (which replays the
journal) and switches to 'Full'. Once in 'Full',
'afterForward' is a no-op.
-}
mkSplitMode
    :: (Monad m)
    => Ops
        'MTS.KVOnly
        m
        cf
        (Columns slot hash key value)
        op
        key
        value
        hash
    -> (s -> t -> Bool)
    -- ^ Tip proximity predicate
    -> Phase
    -- ^ Initial phase
    -> IO
        ( SplitMode
            IO
            s
            t
            ( CSMTOps
                ( Transaction
                    m
                    cf
                    (Columns slot hash key value)
                    op
                )
                key
                value
                hash
            )
        )
mkSplitMode ops isAtTip initial = do
    let kvCSMTOps = kvCommonToCSMTOps (kvCommon ops)
    -- Mutable state: Left = KVOnly, Right = Full CSMTOps
    initialState <- case initial of
        KVOnly -> pure (Left kvCSMTOps)
        Full -> do
            mFull <- toFull ops
            case mFull of
                Just fullOps ->
                    pure
                        $ Right
                        $ fullOpsToCSMTOps fullOps
                Nothing ->
                    error
                        "mkSplitMode: toFull failed on init"
    ref <- newIORef initialState
    pure
        SplitMode
            { currentOps = do
                either id id <$> readIORef ref
            , afterForward = \slot tip -> do
                st <- readIORef ref
                case st of
                    Left _ | isAtTip slot tip -> do
                        mFull <- toFull ops
                        case mFull of
                            Just fullOps ->
                                writeIORef ref
                                    $ Right
                                    $ fullOpsToCSMTOps
                                        fullOps
                            Nothing ->
                                error
                                    "mkSplitMode: toFull failed"
                    _ -> pure ()
            , currentPhase = do
                st <- readIORef ref
                pure $ case st of
                    Left _ -> KVOnly
                    Right _ -> Full
            }

-- | Convert KVOnly 'CommonOps' to 'CSMTOps' (root hash always 'Nothing').
kvCommonToCSMTOps
    :: (Monad m)
    => CommonOps m cf d ops k v
    -> CSMTOps (Transaction m cf d ops) k v a
kvCommonToCSMTOps CommonOps{opsInsert, opsDelete} =
    CSMTOps
        { csmtInsert = opsInsert
        , csmtDelete = opsDelete
        , csmtRootHash = pure Nothing
        }

-- | Convert 'Full' 'Ops' to 'CSMTOps'.
fullOpsToCSMTOps
    :: Ops 'MTS.Full m cf d ops k v a
    -> CSMTOps (Transaction m cf d ops) k v a
fullOpsToCSMTOps
    OpsFull
        { fullCommon = CommonOps{opsInsert, opsDelete}
        , opsRootHash
        } =
        CSMTOps
            { csmtInsert = opsInsert
            , csmtDelete = opsDelete
            , csmtRootHash = opsRootHash
            }

{- | Wrap a tracer with duration measurement for
CSMT ops and rollback point storage.

'UpdateCSMTBegin' → 'UpdateCSMTDone' becomes
'UpdateCSMTMeasured'.
'UpdateCSMTDone' → 'UpdateForwardTip' becomes
'UpdateRollbackMeasured'.
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
