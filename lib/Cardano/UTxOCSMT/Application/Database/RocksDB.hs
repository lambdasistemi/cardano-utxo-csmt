{- |
Module      : Cardano.UTxOCSMT.Application.Database.RocksDB
Description : RocksDB-backed database implementation

This module provides the RocksDB backend for the UTxO CSMT database.
It handles:

* Transaction management with atomic batch operations
* Column family setup for different data types (UTxOs, CSMT, rollback points)
* Integration with the abstract 'Update' and 'Query' interfaces
-}
module Cardano.UTxOCSMT.Application.Database.RocksDB
    ( RocksDBTransaction
    , RocksDBQuery
    , newRunRocksDBTransaction
    , newRocksDBState
    , createUpdateState
    , createSplitUpdateState
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms
    , codecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( UpdateTrace
    , newSplitState
    , newState
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query
    , TipOf
    , Update
    )
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Tracer (Tracer, nullTracer)
import Data.Maybe (isNothing)
import Database.KV.Cursor (firstEntry)
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction (iterating)
import Database.KV.Transaction qualified as L
import Database.RocksDB (BatchOp, ColumnFamily, DB (..))
import UnliftIO (MonadUnliftIO)

type RocksDBTransaction m slot hash key value =
    L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp

type RocksDBQuery m slot hash key value =
    Query m slot key value

-- | Create a 'RunTransaction' for RocksDB
newRunRocksDBTransaction
    :: (MonadUnliftIO m, MonadFail m, MonadMask m)
    => DB
    -> Prisms slot hash key value
    -- ^ Prisms for serializing/deserializing keys and values
    -> m (RunTransaction ColumnFamily BatchOp slot hash key value m)
newRunRocksDBTransaction db prisms = do
    L.RunTransaction rt <- newRunTransaction db prisms
    pure $ RunTransaction rt

newRunTransaction
    :: (MonadIO m, MonadUnliftIO n, MonadMask n, MonadFail n)
    => DB
    -> Prisms slot hash key value
    -> m
        ( L.RunTransaction
            n
            ColumnFamily
            (Columns slot hash key value)
            BatchOp
        )
newRunTransaction db prisms =
    L.newRunTransaction
        $ mkRocksDBDatabase db
        $ mkColumns (columnFamilies db)
        $ codecs prisms

newRocksDBState
    :: ( MonadUnliftIO m
       , MonadFail m
       , Ord key
       , Ord slot
       , Show slot
       , MonadMask m
       )
    => Tracer m (UpdateTrace slot hash)
    -> DB
    -> Prisms slot hash key value
    -> CSMTOps
        (L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp)
        key
        value
        hash
    -- ^ CSMT operations (insert, delete, root hash)
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward; use to check if at tip and emit Synced
    -> ArmageddonParams hash
    -> m
        ( (Update m slot key value, [slot])
        , RunTransaction ColumnFamily BatchOp slot hash key value m
        )
newRocksDBState
    tracer
    db
    prisms
    ops
    slotHash
    onForward
    armageddonParams = do
        runner <- newRunRocksDBTransaction db prisms
        ensureInitialized runner armageddonParams
        (,runner)
            <$> newState tracer ops slotHash onForward armageddonParams runner

-- | Create Update state from an existing runner
createUpdateState
    :: (MonadFail m, Ord key, Ord slot, Show slot)
    => Tracer m (UpdateTrace slot hash)
    -> CSMTOps
        (L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp)
        key
        value
        hash
    -- ^ CSMT operations (insert, delete, root hash)
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -- ^ Called after each forward; use to check if at tip and emit Synced
    -> ArmageddonParams hash
    -> RunTransaction ColumnFamily BatchOp slot hash key value m
    -> m (Update m slot key value, [slot])
createUpdateState tracer ops slotHash onForward armageddonParams runner = do
    ensureInitialized runner armageddonParams
    newState tracer ops slotHash onForward armageddonParams runner

{- | Create split-mode Update state from an existing runner.

Starts in KVOnly mode if journal is non-empty, otherwise Full.
-}
createSplitUpdateState
    :: (MonadFail m, MonadIO m, Ord key, Ord slot, Show slot)
    => Tracer m (UpdateTrace slot hash)
    -> CSMTOps
        (L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp)
        key
        value
        hash
    -- ^ KVOnly ops
    -> CSMTOps
        (L.Transaction m ColumnFamily (Columns slot hash key value) BatchOp)
        key
        value
        hash
    -- ^ Full ops
    -> m ()
    -- ^ Replay callback
    -> (slot -> TipOf slot -> Bool)
    -- ^ Tip detection predicate
    -> (slot -> hash)
    -> (slot -> TipOf slot -> m ())
    -> ArmageddonParams hash
    -> RunTransaction ColumnFamily BatchOp slot hash key value m
    -> m (Update m slot key value, [slot])
createSplitUpdateState
    tracer
    kvOps
    fullOps
    replay
    isAtTip
    slotHash
    onForward
    armageddonParams
    runner = do
        ensureInitialized runner armageddonParams
        newSplitState
            tracer
            kvOps
            fullOps
            replay
            isAtTip
            slotHash
            onForward
            armageddonParams
            runner

{- | Ensure the database has been initialized with an Origin rollback point.
This makes the public API self-initializing so callers can't forget to
call 'setup' before creating state.
-}
ensureInitialized
    :: (Ord slot, Monad m)
    => RunTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
ensureInitialized runner@RunTransaction{transact} armageddonParams = do
    empty <-
        transact $ iterating RollbackPoints $ isNothing <$> firstEntry
    when empty $ setup nullTracer runner armageddonParams
