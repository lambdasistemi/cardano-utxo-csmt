module Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate

      -- * Tracing
    , UpdateTrace (..)
    , renderUpdateTrace

      -- * Low level operations, exposed for testing
    , forwardFinality
    , forwardTip
    , updateRollbackPoint
    , rollbackTip
    , sampleRollbackPoints
    , countRollbackPoints
    , newState
    , newFinality
    , RollbackResult (..)
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
    ( RollbackPoint (..)
    , RollbackPointKV
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
    , seekKey
    )
import Database.KV.Transaction
    ( Transaction
    , delete
    , insert
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Query the current tip directly from rollback points.
queryTip
    :: MonadFail m
    => Transaction m cf (Columns slot hash key value) op (WithOrigin slot)
queryTip =
    iterating RollbackPoints $ do
        ml <- lastEntry
        case ml of
            Nothing -> lift . lift $ fail "No tip in rollback points"
            Just e -> pure $ entryKey e

data UpdateTrace slot hash
    = UpdateArmageddon ArmageddonTrace
    | UpdateForwardTip slot Int Int (Maybe hash)
    | UpdateNewState [slot]
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
                    iterating
                        RollbackPoints
                        countRollbackPoints
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
updateRollbackPoint rootHashQuery pointHash pointSlot rbpInverseOperations = do
    rpbMerkleRoot <- rootHashQuery
    insert RollbackPoints (At pointSlot)
        $ RollbackPoint
            { rbpHash = pointHash
            , rbpInverseOperations
            , rpbMerkleRoot
            }
    pure rpbMerkleRoot
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

-- | Count total rollback points in the DB.
countRollbackPoints
    :: Monad m
    => Cursor
        (Transaction m cf (Columns slot hash key value) op)
        (RollbackPointKV slot hash key value)
        Int
countRollbackPoints = do
    me <- firstEntry
    ($ me) $ fix $ \go current -> case current of
        Nothing -> pure 0
        Just _ -> do
            next <- nextEntry
            (+ 1) <$> go next

keepAts :: [WithOrigin a] -> [a]
keepAts = flip foldr [] $ \case
    Origin -> id
    At x -> (x :)

rollbackRollbackPoint
    :: Monad m
    => CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> RollbackPoint slot hash key value
    -> Transaction m cf (Columns slot hash key value) op ()
rollbackRollbackPoint CSMTOps{csmtInsert, csmtDelete} RollbackPoint{rbpInverseOperations} =
    forM_ rbpInverseOperations $ \case
        Insert k v -> csmtInsert k v
        Delete k -> csmtDelete k

-- | Result of a rollback attempt. Just a mirror, without continuation of 'Interface.State'
data RollbackResult
    = RollbackSucceeded
    | RollbackImpossible

{- | Create a transaction that performs a rollback to the given slot
Returns whether the rollback was successful, failed but possible (with a list
of rollback points to intersect against), or impossible (in which case the
database should be truncated)
It DOES NOT encode the truncation as a transaction because that would potentially
be too big to fit in memory
Rollback is performed by seeking the exact rollback point, and then applying all
inverse operations down to that point excluded
If the exact rollback point is not found, we return a list of available rollback points
If the list is empty, rollback is impossible and the database should be truncated
-}
rollbackTip
    :: (Ord slot, MonadFail m)
    => CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
    -> slot
    -- ^ Slot to rollback to
    -> Transaction
        m
        cf
        (Columns slot hash key value)
        op
        (RollbackResult, Int)
rollbackTip ops slot = do
    tip <- queryTip
    if At slot > tip
        then pure (RollbackSucceeded, 0)
        else iterating RollbackPoints $ do
            me <- seekKey $ At slot
            case me of
                Just (Entry (At foundSlot) RollbackPoint{})
                    | foundSlot == slot -> do
                        ml <- lastEntry
                        deleted <-
                            ($ ml) $ fix $ \go current ->
                                case current of
                                    Nothing ->
                                        lift . lift
                                            $ fail
                                                "rollbackTipApply: inconsistent rollback points"
                                    Just Entry{entryKey, entryValue}
                                        | entryKey > At slot -> do
                                            lift $ do
                                                rollbackRollbackPoint
                                                    ops
                                                    entryValue
                                                delete
                                                    RollbackPoints
                                                    entryKey
                                            prev <- prevEntry
                                            (+ 1) <$> go prev
                                        | otherwise -> pure 0
                        pure (RollbackSucceeded, deleted)
                    | otherwise ->
                        pure (RollbackImpossible, 0)
                _ -> pure (RollbackImpossible, 0)

-- | Apply forward finality .
forwardFinality
    :: (Ord slot, Monad m)
    => slot
    -> Transaction m cf (Columns slot hash key value) op Int
forwardFinality slot = do
    iterating RollbackPoints $ do
        me <- firstEntry
        ($ me) $ fix $ \go current ->
            case current of
                Nothing -> pure 0
                Just Entry{entryKey}
                    | entryKey < At slot -> do
                        lift $ delete RollbackPoints entryKey
                        next <- nextEntry
                        (+ 1) <$> go next
                    | otherwise -> pure 0

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
                        (r, deleted) <-
                            transact
                                $ rollbackTip ops s
                        case r of
                            RollbackSucceeded ->
                                pure
                                    $ Syncing
                                    $ go
                                        (count - deleted)
                            RollbackImpossible -> do
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
                            $ forwardFinality s
                    pure
                        $ go (count - pruned)
                }
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
