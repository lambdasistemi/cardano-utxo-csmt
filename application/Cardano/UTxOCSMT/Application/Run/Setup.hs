module Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    , checkEmptyRollbacks
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Setup
-- Description : Database initialization
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module handles database setup, including detection of existing
-- data and initialization of new databases from genesis.

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getBaseCheckpoint
    , putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps (..)
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( decodePoint
    , encodePoint
    )
import Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    )
import Cardano.UTxOCSMT.Bootstrap.Genesis
    ( genesisEpochSlots
    , genesisNetworkMagic
    , genesisSecurityParam
    , genesisStabilityWindow
    , genesisUtxoPairs
    , readByronGenesisUtxoPairs
    , readShelleyGenesis
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Monad (forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (isNothing)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , pattern TraceWith
    )
import Data.Word (Word64)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.KV.Transaction qualified as L
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Point (WithOrigin (..))

{- | Result of database setup containing the starting point
for chain sync.
-}
data SetupResult = SetupResult
    { setupStartingPoint :: Point
    -- ^ Starting point for chain sync (Origin or stored checkpoint)
    , setupIsGenesis :: Bool
    -- ^ True if the DB was freshly initialized (no prior data)
    , setupSecurityParam :: Word64
    -- ^ Security parameter k from genesis
    , setupStabilityWindow :: Word64
    -- ^ Stability window in slots: @ceiling(3k\/f)@
    , setupNetworkMagic :: NetworkMagic
    -- ^ Network magic derived from shelley-genesis.json
    , setupEpochSlots :: Word64
    -- ^ Byron epoch slots derived from genesis (10 * k)
    }

{- | Set up the database, potentially bootstrapping from genesis.

This function handles three scenarios:

  1. Existing database: Returns the stored checkpoint
  2. New database with genesis file: Inserts genesis UTxOs,
     starts from Origin
  3. New database without bootstrap: Initializes empty database
-}
setupDB
    :: Tracer IO MainTraces
    -- ^ Tracer for logging setup events
    -> FilePath
    -- ^ Path to shelley-genesis.json (always required)
    -> Maybe FilePath
    {- ^ Optional path to byron-genesis.json for genesis
    bootstrap
    -}
    -> ArmageddonParams hash
    -- ^ Parameters for database initialization
    -> CSMTOps
        ( L.Transaction
            IO
            ColumnFamily
            (Columns Point hash LazyByteString LazyByteString)
            BatchOp
        )
        LazyByteString
        LazyByteString
        hash
    -- ^ CSMT operations (insert, delete, root hash)
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> IO SetupResult
    -- ^ Setup result with starting point
setupDB
    TraceWith{contra, trace}
    genesisFilePath
    mByronGenesisFile
    armageddonParams
    ops
    runner@RunTransaction{transact} = do
        -- Check checkpoint first
        mCheckpoint <- transact $ getBaseCheckpoint decodePoint
        case mCheckpoint of
            Just point -> do
                (k, sw, magic, es, _) <- loadGenesis
                trace $ NotEmpty point
                return
                    SetupResult
                        { setupStartingPoint = point
                        , setupIsGenesis = False
                        , setupSecurityParam = k
                        , setupStabilityWindow = sw
                        , setupNetworkMagic = magic
                        , setupEpochSlots = es
                        }
            -- No legacy checkpoint: either fresh DB
            -- or Runner-based follower (stores rollback
            -- points but not legacy checkpoints).
            -- In both cases, regularSetup seeds genesis
            -- only if the CSMT is empty.
            Nothing -> regularSetup
      where
        -- \| Load genesis and extract parameters + UTxO pairs
        loadGenesis
            :: IO
                ( Word64
                , Word64
                , NetworkMagic
                , Word64
                , [(LazyByteString, LazyByteString)]
                )
        loadGenesis = do
            g <- readShelleyGenesis genesisFilePath
            pure
                ( genesisSecurityParam g
                , genesisStabilityWindow g
                , genesisNetworkMagic g
                , genesisEpochSlots g
                , genesisUtxoPairs g
                )

        originPoint :: Point
        originPoint = Network.Point Origin

        regularSetup = do
            setup (contra New) runner armageddonParams
            (k, sw, magic, es, shelleyPairs) <- loadGenesis
            -- Load Byron genesis UTxOs
            byronPairs <- case mByronGenesisFile of
                Just path -> readByronGenesisUtxoPairs path
                Nothing -> pure []
            let pairs = byronPairs ++ shelleyPairs
                hasGenesis = not (null pairs)
            transact $ do
                forM_ pairs $ uncurry (csmtInsert ops)
                putBaseCheckpoint
                    decodePoint
                    encodePoint
                    originPoint
            when hasGenesis
                $ trace
                $ GenesisBootstrap (length pairs)
            return
                SetupResult
                    { setupStartingPoint = originPoint
                    , setupIsGenesis = True
                    , setupSecurityParam = k
                    , setupStabilityWindow = sw
                    , setupNetworkMagic = magic
                    , setupEpochSlots = es
                    }

{- | Check if the rollbacks column family is empty.

Returns 'True' if the database is new (no rollback points stored),
'False' if it contains existing data.
-}
checkEmptyRollbacks
    :: RunTransaction
        ColumnFamily
        BatchOp
        Point
        hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> IO Bool
    -- ^ 'True' if database is empty
checkEmptyRollbacks (RunTransaction runTx) =
    runTx $ do
        mfe <- iterating RollbackPoints firstEntry
        return $ isNothing mfe
