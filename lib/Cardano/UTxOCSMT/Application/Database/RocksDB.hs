{- |
Module      : Cardano.UTxOCSMT.Application.Database.RocksDB
Description : RocksDB-backed database implementation

This module provides the RocksDB backend for the UTxO CSMT database.
It handles:

* Transaction management with atomic batch operations
* Column family setup for different data types (UTxOs, CSMT, rollback points)
-}
module Cardano.UTxOCSMT.Application.Database.RocksDB
    ( RocksDBTransaction
    , RocksDBQuery
    , newRunRocksDBTransaction
    , newRunRocksDBTransactionUnguarded
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns
    , Prisms
    , codecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query
    )
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
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

-- | Create an unguarded 'RunTransaction' for parallel replay.
newRunRocksDBTransactionUnguarded
    :: (MonadUnliftIO m, MonadFail m)
    => DB
    -> Prisms slot hash key value
    -> RunTransaction ColumnFamily BatchOp slot hash key value m
newRunRocksDBTransactionUnguarded db prisms =
    RunTransaction
        $ L.runTransactionUnguarded
        $ mkRocksDBDatabase db
        $ mkColumns (columnFamilies db)
        $ codecs prisms
