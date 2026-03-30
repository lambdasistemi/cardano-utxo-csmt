{- |
Module      : Cardano.UTxOCSMT.Application.Database.Interface
Description : Abstract database interface for UTxO storage

This module defines the core interfaces for database operations:

* 'Query' - Read-only access to database state
* 'Update' - Write operations with tip/finality tracking
* 'State' - FSM states for chain sync protocol handling

The database tracks both a mutable tip (latest applied slot) and an
immutable finality point (confirmed slot beyond which no rollbacks occur).
-}
module Cardano.UTxOCSMT.Application.Database.Interface
    ( -- * Query interface
      Query (..)
    , hoistQuery

      -- * Operation
    , Operation (..)
    , inverseOp

      -- * Sentinel wrapper
    , WithSentinel (..)

      -- * Update interface
    , Update (..)
    , State (..)
    , TipOf

      -- * Database dump, for inspection/testing
    , Dump (..)
    , dumpDatabase
    , emptyDump
    )
where

import Cardano.UTxOCSMT.Ouroboros.Types (TipOf)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Prelude hiding (truncate)

{- | Database sentinel wrapper for rollback column keys.
Unlike ouroboros's 'WithOrigin', this type is explicitly
a storage concept (sentinel marker), not a chain concept.
-}
data WithSentinel a = Sentinel | Value a
    deriving (Eq, Ord, Show)

-- | Represents an operation on the database
data Operation key value
    = Insert key value
    | Delete key
    deriving (Show, Eq)

-- | State of the database update process
data State m slot key value
    = -- | Database is syncing, accepts forward and rollback operations
      Syncing (Update m slot key value)
    | {- | Database is intersecting, has a list of slots to intersect against
      Contrary to what one believes there is nothing to do with the choosen slot
      Only rollbacks can really move the tip
      -}
      Intersecting [slot] (Update m slot key value)
    | -- | Database is truncating, no possible rollbacks, the protocol should reset to Origin
      Truncating (Update m slot key value)

{- | Represents an update to the database. We offer a continuation-based API so that
the database implementation can thread an internal state without messing up with the
monad stack.
Valid consumers should always pick the continuation
-}
data Update m slot key value = Update
    { forwardTipApply
        :: slot
        -> TipOf slot
        -> [Operation key value]
        -> m (Update m slot key value)
    {- ^ Apply operations at the given slot with current chain tip,
    moving the tip forward
    -}
    , rollbackTipApply
        :: WithSentinel slot
        -> m (State m slot key value)
    -- ^ Rollback to the given slot, possibly truncating the database
    }

-- | Read-only database query interface.
data Query m slot key value = Query
    { getValue :: key -> m (Maybe value)
    -- ^ Look up a value by key
    , getTip :: m (WithSentinel slot)
    -- ^ Get the current tip slot (latest applied)
    , getFinality :: m (WithSentinel slot)
    -- ^ Get the finality slot (immutable point)
    , getByAddress :: ByteString -> m [(key, value)]
    -- ^ Get all UTxOs at a given address (raw address bytes)
    , awaitValue :: key -> Maybe Int -> m (Maybe value)
    {- ^ Block until a key appears or timeout (seconds) expires.
    Uses STM retry on a commit notification TVar.
    -}
    }

-- | Let a transaction runner apply to all queries
hoistQuery
    :: (forall a. m a -> n a)
    -> Query m slot key value
    -> Query n slot key value
hoistQuery nat Query{getValue, getTip, getFinality, getByAddress, awaitValue} =
    Query
        { getValue = nat . getValue
        , getTip = nat getTip
        , getFinality = nat getFinality
        , getByAddress = nat . getByAddress
        , awaitValue = \k t -> nat (awaitValue k t)
        }

{- | Get the inverse of an operation, needs access to the database to retrieve
  values for deletions
-}
inverseOp
    :: Monad m
    => (key -> m (Maybe value))
    -> Operation key value
    -> m (Maybe (Operation key value))
inverseOp value op = case op of
    Insert k _v -> pure $ Just (Delete k)
    Delete k -> do
        mv <- value k
        case mv of
            Just v -> pure $ Just (Insert k v)
            Nothing -> pure Nothing

-- | A dump of the database contents for inspection/testing
data Dump slot key value = Dump
    { dumpTip :: WithSentinel slot
    , dumpFinality :: WithSentinel slot
    , dumpAssocs :: [(key, value)]
    }
    deriving (Show, Eq)

-- | An empty database dump
emptyDump :: Dump slot key value
emptyDump =
    Dump
        { dumpTip = Sentinel
        , dumpFinality = Sentinel
        , dumpAssocs = []
        }

{- | Dump the contents of the database for the given keys. It's up to the caller
  to provide the keys of interest.
-}
dumpDatabase
    :: (Monad m, Ord key)
    => [key]
    -> Query m slot key value
    -> m (Dump slot key value)
dumpDatabase keys db = do
    tip <- getTip db
    immutable <- getFinality db
    contents <- traverse (\k -> fmap (k,) (getValue db k)) keys
    let presentContents = sortOn fst $ [(k, v) | (k, Just v) <- contents]
    pure
        $ Dump
            { dumpTip = tip
            , dumpFinality = immutable
            , dumpAssocs = presentContents
            }
