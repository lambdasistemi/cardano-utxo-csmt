{-# LANGUAGE ConstraintKinds #-}

{-
This module exposes an API to express properties about a database.

It provides a monad transformer stack 'WithExpected' that combines a
'ReaderT' for accessing the database and generator context, and a 'StateT'
for maintaining the expected state of the database.

The expected state is represented by the 'Expected' data type, which tracks
the expected key-value associations, the tip and immutable slots, and the
opposite operations for rollbacks.

-}
module Cardano.UTxOCSMT.Application.Database.Properties.Expected
    ( Expected (..)
    , WithExpected
    , PropertyWithExpected
    , PropertyConstraints
    , Generator (..)
    , Context (..)
    , expectedForward
    , runDb
    , forwardTip
    , expectedNewestSlot
    , asksGenerator
    , getDump
    , expectedKeys
    , expectedOpposite
    , getTip
    , getFinality
    , emptyExpected
    , rollbackTip
    , expectedAllOpposites
    , expectedFinality
    , expectedRollbackDepth
    , runWithExpected
    , asksSecurityParam
    )
where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Dump (..)
    , Operation (..)
    , Query
    , State (..)
    , TipOf
    , Update (..)
    , dumpDatabase
    )
import Cardano.UTxOCSMT.Application.Database.Interface qualified as Interface
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State
    ( MonadState (..)
    , MonadTrans (..)
    , StateT
    , evalStateT
    , gets
    )
import Data.Foldable (Foldable (..))
import GHC.Stack (HasCallStack)
import Ouroboros.Network.Point (WithOrigin (..))
import Test.QuickCheck (Gen)
import Test.QuickCheck.Monadic (PropertyM, run)

-- | Constraints required for properties
type PropertyConstraints m slot key value =
    ( Eq value
    , Show slot
    , Show key
    , Show value
    , Ord slot
    , Ord key
    , Monad m
    )

-- | Generator for slots, keys, and values
data Generator slot key value = Generator
    { genSlot :: Gen slot
    , genKey :: Gen key
    , genValue :: Gen value
    }

-- | Context for properties
data Context m slot key value = Context
    { contextDatabase :: Query m slot key value
    , contextGenerator :: Generator slot key value
    , contextSecurityParam :: Int
    }

-- | Expected state of the database
data Expected slot key value = Expected
    { expectedAssocs :: [(key, value)]
    , expectedTip :: WithOrigin slot
    , expectedOpposites :: [(slot, [Operation key value])]
    }
    deriving (Show, Eq)

emptyExpected :: Expected slot key value
emptyExpected =
    Expected
        { expectedAssocs = []
        , expectedTip = Origin
        , expectedOpposites = []
        }

{- | Derive expected finality from the rollback window.
The DB always starts with an Origin rollback point.
Finality is Origin until we have more than k opposites
(meaning Origin has been pruned away).
-}
expectedFinality
    :: Int -> Expected slot key value -> WithOrigin slot
expectedFinality k Expected{expectedOpposites}
    | length expectedOpposites < k = Origin
    | otherwise =
        case expectedOpposites of
            [] -> Origin
            ops -> At . fst $ last ops

-- | Number of rollback points currently stored
expectedRollbackDepth :: Expected slot key value -> Int
expectedRollbackDepth = length . expectedOpposites

-- | Monad transformer stack for properties with expected state
type WithExpected m slot key value =
    ReaderT
        (Context m slot key value)
        (TrackingExpected m slot key value)

type TrackingExpected m slot key value =
    StateT (Expected slot key value, State m slot key value) m

runWithExpected
    :: PropertyConstraints m slot key value
    => Context m slot key value
    -> Update m slot key value
    -> WithExpected m slot key value a
    -> m a
runWithExpected context box prop =
    flip evalStateT (emptyExpected, Syncing box)
        $ runReaderT prop context

-- | Access the generator from the context
asksGenerator
    :: PropertyConstraints m slot key value
    => PropertyWithExpected
        m
        slot
        key
        value
        (Generator slot key value)
asksGenerator = lift $ asks contextGenerator

-- | Access the security parameter from the context
asksSecurityParam
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value Int
asksSecurityParam = lift $ asks contextSecurityParam

-- | Property monad with expected state
type PropertyWithExpected m slot key value =
    PropertyM (WithExpected m slot key value)

runDb
    :: Monad m
    => (Query m slot key value -> m a)
    -> PropertyWithExpected m slot key value a
runDb f = run $ do
    db <- asks contextDatabase
    lift . lift $ f db

-- | Proxy to database 'dumpDatabase'
getDump
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (Dump slot key value)
getDump = do
    keys <- lift . lift $ gets $ expectedAssocs . fst
    runDb $ dumpDatabase $ fmap fst keys

-- | Provide an expected opposite operation
expectedOpposite
    :: (Eq key, Monad m, HasCallStack)
    => Operation key value
    -> PropertyWithExpected m slot key value (Operation key value)
expectedOpposite op = lift . lift $ gets $ \(expct, _) -> opposite expct op

expectedAllOpposites
    :: Monad m
    => PropertyWithExpected m slot key value [Operation key value]
expectedAllOpposites = do
    opps <- lift . lift $ gets $ \(expct, _) -> expectedOpposites expct
    pure $ concatMap snd opps

-- | Proxy to database 'getTip'
getTip
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (WithOrigin slot)
getTip = runDb $ \db -> Interface.getTip db

-- | Proxy to database 'getFinality'
getFinality
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value (WithOrigin slot)
getFinality = runDb $ \db -> Interface.getFinality db

-- | Get all expected keys
expectedKeys :: Expected slot key value -> [key]
expectedKeys = fmap fst . expectedAssocs

-- | Get the newest expected slot
expectedNewestSlot :: Expected slot key value -> WithOrigin slot
expectedNewestSlot Expected{expectedOpposites} =
    case expectedOpposites of
        [] -> Origin
        (x : _) -> At . fst $ x

applyOperation :: Eq a => [(a, b)] -> Operation a b -> [(a, b)]
applyOperation expct (Insert k v) = (k, v) : expct
applyOperation expct (Delete k) = filter (\(k', _) -> k' /= k) expct

applyOperations :: Eq a => [(a, b)] -> [Operation a b] -> [(a, b)]
applyOperations = foldl' applyOperation

applyOpposite
    :: Eq slot
    => [(slot, [Operation key value])]
    -> slot
    -> Operation key value
    -> [(slot, [Operation key value])]
applyOpposite [] slot op = [(slot, [op])]
applyOpposite ((slt, ops) : rest) slot op
    | slt == slot = (slt, op : ops) : rest
    | otherwise = (slot, [op]) : (slt, ops) : rest

opposite
    :: (Eq key, HasCallStack)
    => Expected slot key value
    -> Operation key value
    -> Operation key value
opposite _ (Insert k _) = Delete k
opposite expct (Delete k) =
    case lookup k (expectedAssocs expct) of
        Just v -> Insert k v
        Nothing -> error "opposite: key not found in expected contents"

expectedForward
    :: (Eq key, Ord slot, HasCallStack)
    => Int
    -> Expected slot key value
    -> slot
    -> [Operation key value]
    -> Expected slot key value
expectedForward securityParam old@Expected{expectedTip} slot ops
    | At slot <= expectedTip = old
    | otherwise =
        pruneByCount securityParam
            $ foldl' apply old{expectedTip = At slot} ops
  where
    apply ex op =
        ex
            { expectedAssocs =
                applyOperation (expectedAssocs ex) op
            , expectedOpposites =
                applyOpposite
                    (expectedOpposites ex)
                    slot
                    (opposite ex op)
            }

-- | Drop oldest rollback points when count exceeds k
pruneByCount
    :: Int
    -> Expected slot key value
    -> Expected slot key value
pruneByCount k ex@Expected{expectedOpposites}
    | length expectedOpposites <= k = ex
    | otherwise =
        ex{expectedOpposites = take k expectedOpposites}

-- | Proxy to database 'forward' that also updates expected state
forwardTip
    :: forall m slot key value
     . ( PropertyConstraints m slot key value
       , HasCallStack
       , TipOf slot ~ slot
       )
    => slot
    -> [Operation key value]
    -> PropertyWithExpected m slot key value ()
forwardTip slot ops = do
    k <- asksSecurityParam
    lift . lift $ do
        (ex, databaseState) <- get
        case databaseState of
            Syncing update -> do
                cont <-
                    lift $ forwardTipApply update slot slot ops
                put
                    ( expectedForward k ex slot ops
                    , Syncing cont
                    )
            Intersecting _ _ ->
                error
                    "forwardTip: cannot forward while intersecting"
            Truncating _ ->
                error
                    "forwardTip: cannot forward while resetting"

expectedRollback
    :: (Eq key, Ord slot)
    => Int
    -> Expected slot key value
    -> WithOrigin slot
    -> Expected slot key value
expectedRollback
    k
    old@Expected
        { expectedAssocs = assocs
        , expectedOpposites = opposites
        , expectedTip
        }
    (At slot)
        | At slot >= expectedTip = old
        | At slot < expectedTip
            && At slot >= expectedFinality k old =
            let
                (ops, newOpposites) =
                    break (\(slt, _) -> slt <= slot) opposites
                newAssocs =
                    applyOperations assocs $ ops >>= snd
            in
                Expected
                    { expectedAssocs = newAssocs
                    , expectedTip = At slot
                    , expectedOpposites = newOpposites
                    }
        | otherwise = emptyExpected
expectedRollback _ _ Origin =
    emptyExpected

-- | Proxy to database 'rollback' that also updates expected state
rollbackTip
    :: forall m slot key value
     . PropertyConstraints m slot key value
    => WithOrigin slot
    -> PropertyWithExpected m slot key value ()
rollbackTip newTip = do
    k <- asksSecurityParam
    lift . lift $ do
        (ex, databaseState) <- get
        case databaseState of
            Syncing update -> do
                cont <- lift $ rollbackTipApply update newTip
                case cont of
                    s@(Syncing{}) -> do
                        put (expectedRollback k ex newTip, s)
                    s -> put (ex, s)
            Intersecting _ _ -> do
                error "rollbackTip: cannot rollback to a point not in reset points"
            Truncating _ -> do
                error "rollbackTip: cannot rollback while resetting"
