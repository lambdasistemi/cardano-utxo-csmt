module Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate
    , mkSplitUpdate
    , newSplitState

      -- * Tracing
    , UpdateTrace (..)
    , renderUpdateTrace

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
    , iterating
    , query
    )
import MTS.Rollbacks.Store qualified as Store
import MTS.Rollbacks.Types qualified as RP
import Ouroboros.Network.Point (WithOrigin (..))

-- | Query the current tip directly from rollback points.
queryTip
    :: MonadFail m
    => Transaction m cf (Columns slot hash key value) op (WithOrigin slot)
queryTip = do
    mt <- Store.queryTip RollbackPoints
    case mt of
        Nothing -> lift $ fail "No tip in rollback points"
        Just t -> pure t

data UpdateTrace slot hash
    = UpdateArmageddon ArmageddonTrace
    | UpdateForwardTip slot Int Int (Maybe hash)
    | UpdateNewState [slot]
    | UpdateJournalReplay
    deriving (Show)

renderUpdateTrace :: Show slot => UpdateTrace slot hash -> String
renderUpdateTrace (UpdateArmageddon t) = renderArmageddonTrace t
renderUpdateTrace (UpdateForwardTip slot nInsert nDelete _merkleRoot) =
    "Forward tip at "
        ++ show slot
        ++ ": "
        ++ show nInsert
        ++ " inserts, "
        ++ show nDelete
        ++ " deletes"
renderUpdateTrace (UpdateNewState slots) =
    "New update state with rollback points at: " ++ show slots
renderUpdateTrace UpdateJournalReplay =
    "Replaying journal to build CSMT tree"

newState
    :: (Ord key, Ord slot, Show slot, MonadFail m)
    => Tracer m (UpdateTrace slot hash)
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
    -> m (Update m slot key value, [slot])
newState
    TraceWith{tracer, trace}
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact} = do
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
        pure
            $ (,cps)
            $ mkUpdate
                tracer
                ops
                slotHash
                onForward
                armageddonParams
                runTransaction
                rollbackCount

{- | Initialize split-mode state.

If the journal is non-empty, start in KVOnly mode (will replay
on first tip touch). If empty, start in Full mode directly.
-}
newSplitState
    :: (Ord key, Ord slot, Show slot, MonadFail m)
    => Tracer m (UpdateTrace slot hash)
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
    -> m (Update m slot key value, [slot])
newSplitState
    tw@TraceWith{trace}
    kvOps
    fullOps
    replay
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact} = do
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
        pure
            $ (,cps)
            $ mkSplitUpdate
                tw
                kvOps
                fullOps
                replay
                isAtTip
                slotHash
                onForward
                armageddonParams
                runTransaction
                rollbackCount

{- | Apply forward tip .
We compose csmt transactions for each operation with an updateRollbackPoint one
-}
forwardTip
    :: (Ord key, Ord slot, Show slot, MonadFail m)
    => Tracer m (UpdateTrace slot hash)
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
    CSMTOps{csmtInsert, csmtDelete, csmtRootHash}
    hash
    count
    slot
    ops = do
        tip <- queryTip
        if At slot > tip
            && (not (null ops) || count < 2)
            then do
                result <-
                    forM (zip [0 :: Int ..] ops) $ \case
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
                let (Sum nInserts, Sum nDeletes, invs) =
                        mconcat result
                merkleRoot <-
                    updateRollbackPoint
                        csmtRootHash
                        hash
                        slot
                        $ reverse invs
                lift . lift . trace
                    $ UpdateForwardTip
                        slot
                        nInserts
                        nDeletes
                        merkleRoot
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
    :: (Ord key, Ord slot, Show slot, MonadFail m)
    => Tracer m (UpdateTrace slot hash)
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
    -- ^ Initial rollback point count
    -> Update m slot key value
mkUpdate
    TraceWith{tracer, contra}
    ops
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact} =
        go
      where
        go count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        stored <-
                            transact
                                $ forwardTip
                                    tracer
                                    ops
                                    (slotHash slot)
                                    count
                                    slot
                                    operations
                        onForward slot chainTip
                        pure
                            $ go
                            $ if stored
                                then count + 1
                                else count
                , rollbackTipApply = \case
                    At s -> do
                        r <-
                            transact $ do
                                tip <- queryTip
                                if At s > tip
                                    then
                                        pure
                                            $ Store.RollbackSucceeded 0
                                    else
                                        Store.rollbackTo
                                            RollbackPoints
                                            (rollbackRollbackPoint ops)
                                            (At s)
                        case r of
                            Store.RollbackSucceeded deleted ->
                                pure
                                    $ Syncing
                                    $ go
                                        (count - deleted)
                            Store.RollbackImpossible -> do
                                armageddonCall
                                pure
                                    $ Truncating
                                    $ go 0
                    Origin -> do
                        armageddonCall
                        pure $ Syncing $ go 0
                , forwardFinalityApply = \s -> do
                    pruned <-
                        transact
                            $ Store.pruneBelow
                                RollbackPoints
                                (At s)
                    pure
                        $ go (count - pruned)
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
    :: (Ord key, Ord slot, Show slot, MonadFail m)
    => Tracer m (UpdateTrace slot hash)
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
    -- ^ Initial rollback point count
    -> Update m slot key value
mkSplitUpdate
    TraceWith{tracer, contra, trace}
    kvOps
    fullOps
    replay
    isAtTip
    slotHash
    onForward
    armageddonParams
    runTransaction@RunTransaction{transact} =
        goKVOnly
      where
        goKVOnly count =
            Update
                { forwardTipApply =
                    \slot chainTip operations -> do
                        stored <-
                            transact
                                $ forwardTip
                                    tracer
                                    kvOps
                                    (slotHash slot)
                                    count
                                    slot
                                    operations
                        onForward slot chainTip
                        let count' =
                                if stored
                                    then count + 1
                                    else count
                        if isAtTip slot chainTip
                            then do
                                trace UpdateJournalReplay
                                replay
                                pure $ goFull count'
                            else
                                pure $ goKVOnly count'
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
                        pure $ Syncing $ goKVOnly 0
                , forwardFinalityApply = \s -> do
                    pruned <-
                        transact
                            $ Store.pruneBelow
                                RollbackPoints
                                (At s)
                    pure
                        $ goKVOnly (count - pruned)
                }
        goFull =
            mkUpdate
                tracer
                fullOps
                slotHash
                onForward
                armageddonParams
                runTransaction
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
