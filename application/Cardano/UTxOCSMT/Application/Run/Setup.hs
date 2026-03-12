{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    , checkEmptyRollbacks
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Setup
-- Description : Database initialization and Mithril bootstrap
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module handles database setup, including detection of existing data,
-- initialization of new databases, and optional bootstrapping from Mithril
-- snapshots for faster initial sync.

import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams
    , cleanup
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( clearBootstrapInProgress
    , getBaseCheckpoint
    , getSkipSlot
    , isBootstrapInProgress
    , putBaseCheckpoint
    , setBootstrapInProgress
    , setSkipSlot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps (..)
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Options (MithrilOptions (..))
import Cardano.UTxOCSMT.Application.Run.Config
    ( decodePoint
    , encodePoint
    )
import Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    , NodeValidationTrace (..)
    )
import Cardano.UTxOCSMT.Bootstrap.Genesis
    ( genesisSecurityParam
    , genesisStabilityWindow
    , genesisUtxoPairs
    , readByronGenesisUtxoPairs
    , readShelleyGenesis
    )
import Cardano.UTxOCSMT.Mithril.AncillaryVerifier
    ( parseVerificationKey
    )
import Cardano.UTxOCSMT.Mithril.Client (defaultMithrilConfig)
import Cardano.UTxOCSMT.Mithril.Client qualified as MithrilClient
import Cardano.UTxOCSMT.Mithril.Import
    ( ImportResult (..)
    , ImportTrace (..)
    , importFromMithril
    )
import Cardano.UTxOCSMT.Mithril.Options qualified as Mithril
import Cardano.UTxOCSMT.Ouroboros.Connection
    ( NodeConnectionError (..)
    , validateNodeConnection
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Monad (forM_, when)
import Control.Tracer (Contravariant (..), Tracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (isNothing)
import Data.Text (unpack)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Word (Word16, Word64)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction (iterating)
import Database.KV.Transaction qualified as L
import Database.RocksDB (BatchOp, ColumnFamily)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (PortNumber)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO.Temp (withSystemTempDirectory)

{- | Result of database setup containing the starting point for chain sync
and optional Mithril slot information.
-}
data SetupResult = SetupResult
    { setupStartingPoint :: Point
    -- ^ Starting point for chain sync (Origin or Mithril checkpoint)
    , setupMithrilSlot :: Maybe Word64
    -- ^ If bootstrapped from Mithril, the slot to skip headers until
    , setupIsGenesis :: Bool
    -- ^ True if the DB was freshly initialized (no prior data)
    , setupSecurityParam :: Word64
    -- ^ Security parameter k from genesis (max rollback depth in blocks)
    , setupStabilityWindow :: Word64
    -- ^ Stability window in slots: @ceiling(3k\/f)@
    }

{- | Set up the database, potentially bootstrapping from Mithril or genesis.

This function handles four scenarios:

  1. Existing database: Returns the stored checkpoint
  2. New database with genesis file: Inserts genesis UTxOs, starts from Origin
  3. New database with Mithril: Downloads and imports UTxO snapshot
  4. New database without bootstrap: Initializes empty database

When Mithril bootstrap is enabled, validates the node connection first
to fail fast if the node is unreachable (rather than after a long import).
-}
setupDB
    :: Tracer IO MainTraces
    -- ^ Tracer for logging setup events
    -> Point
    -- ^ Default starting point (used if not bootstrapping)
    -> FilePath
    -- ^ Path to shelley-genesis.json (always required)
    -> Maybe FilePath
    -- ^ Optional path to byron-genesis.json for genesis bootstrap
    -> MithrilOptions
    -- ^ Mithril configuration options
    -> NetworkMagic
    -- ^ Network magic for node connection
    -> String
    -- ^ Node hostname
    -> PortNumber
    -- ^ Node port
    -> Bool
    -- ^ Skip node validation
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
    -- ^ Setup result with starting point and optional Mithril slot
setupDB
    TraceWith{tracer, trace, contra}
    startingPoint
    genesisFilePath
    mByronGenesisFile
    mithrilOpts
    networkMagic
    nodeName
    nodePort
    skipValidation
    armageddonParams
    ops
    runner@RunTransaction{transact} = do
        -- Check for incomplete bootstrap and clean up if needed
        incomplete <- transact $ isBootstrapInProgress decodePoint
        when incomplete $ do
            trace IncompleteBootstrapDetected
            cleanup (contra New) runner armageddonParams
            trace IncompleteBootstrapCleaned

        -- Check checkpoint first (works for both KVOnly and Full mode)
        (mCheckpoint, skipSlot) <- transact $ do
            cp <- getBaseCheckpoint decodePoint
            ss <- getSkipSlot decodePoint
            pure (cp, ss)
        case mCheckpoint of
            Just point -> do
                (k, sw, _) <- loadGenesis
                trace $ NotEmpty point
                return
                    SetupResult
                        { setupStartingPoint = point
                        , setupMithrilSlot = skipSlot
                        , setupIsGenesis = False
                        , setupSecurityParam = k
                        , setupStabilityWindow = sw
                        }
            Nothing -> do
                -- No checkpoint: check rollback points for
                -- backwards compatibility with old DBs
                new <- checkEmptyRollbacks runner
                if new
                    then do
                        if mithrilEnabled mithrilOpts
                            || Mithril.mithrilBootstrapOnly
                                mithrilOpts
                            then do
                                validationOk <- validateNode
                                if validationOk
                                    then bootstrapFromMithril
                                    else regularSetup
                            else regularSetup
                    else
                        error
                            "setupDB: Database has rollback \
                            \points but no checkpoint"
      where
        -- \| Load genesis and extract parameters + UTxO pairs
        loadGenesis
            :: IO
                ( Word64
                , Word64
                , [(LazyByteString, LazyByteString)]
                )
        loadGenesis = do
            g <- readShelleyGenesis genesisFilePath
            pure
                ( genesisSecurityParam g
                , genesisStabilityWindow g
                , genesisUtxoPairs g
                )

        -- \| Validate node connection, returning True if OK or skipped
        validateNode :: IO Bool
        validateNode
            | skipValidation = pure True
            | otherwise = do
                let portNum = fromIntegral nodePort :: Word16
                trace
                    $ NodeValidation
                    $ ValidatingNodeConnection nodeName portNum
                -- 30 second timeout (in microseconds)
                result <-
                    validateNodeConnection
                        networkMagic
                        nodeName
                        nodePort
                        30_000_000
                case result of
                    Right () -> do
                        trace $ NodeValidation NodeValidationSuccess
                        pure True
                    Left err -> do
                        trace $ NodeValidation $ NodeValidationFailed err
                        error
                            $ "Node connection validation failed: "
                                ++ renderConnectionError err
                                ++ "\n\nCheck your --node-name and --node-port "
                                ++ "settings, or use --skip-node-validation "
                                ++ "to bypass this check."

        renderConnectionError :: NodeConnectionError -> String
        renderConnectionError (NodeResolutionFailed msg) =
            "Failed to resolve hostname: " ++ msg
        renderConnectionError (NodeConnectionFailed msg) =
            "Connection failed: " ++ msg
        renderConnectionError NodeConnectionTimeout =
            "Connection timed out after 30 seconds"

        originPoint :: Point
        originPoint = Network.Point Origin

        regularSetup = do
            setup (contra New) runner armageddonParams
            (k, sw, shelleyPairs) <- loadGenesis
            -- Load Byron genesis UTxOs
            byronPairs <- case mByronGenesisFile of
                Just path -> readByronGenesisUtxoPairs path
                Nothing -> pure []
            let pairs = byronPairs ++ shelleyPairs
                hasGenesis = not (null pairs)
                start
                    | hasGenesis = originPoint
                    | otherwise = startingPoint
            transact $ do
                forM_ pairs $ uncurry (csmtInsert ops)
                putBaseCheckpoint
                    decodePoint
                    encodePoint
                    start
            when hasGenesis
                $ trace
                $ GenesisBootstrap (length pairs)
            return
                SetupResult
                    { setupStartingPoint = start
                    , setupMithrilSlot = Nothing
                    , setupIsGenesis = True
                    , setupSecurityParam = k
                    , setupStabilityWindow = sw
                    }

        bootstrapFromMithril = do
            -- Create HTTP manager for Mithril API calls
            manager <- newManager tlsManagerSettings

            -- Determine download directory
            let downloadDir = Mithril.mithrilDownloadDir mithrilOpts

            case downloadDir of
                Just dir -> runMithrilBootstrap manager dir
                Nothing ->
                    -- Use temporary directory for downloads
                    withSystemTempDirectory "mithril-snapshot" $ \tempDir ->
                        runMithrilBootstrap manager tempDir

        runMithrilBootstrap manager downloadDir =
            case Mithril.mithrilAggregatorUrl mithrilOpts of
                Nothing -> do
                    -- No aggregator URL provided - fail with clear error
                    trace
                        $ Mithril
                        $ ImportError MithrilClient.MithrilMissingAggregatorUrl
                    regularSetup
                Just aggregatorUrl -> do
                    let baseConfig =
                            defaultMithrilConfig
                                manager
                                (Mithril.mithrilNetwork mithrilOpts)
                                aggregatorUrl
                                downloadDir
                        -- Determine ancillary verification key
                        ancillaryVk
                            | Mithril.mithrilSkipAncillaryVerification
                                mithrilOpts =
                                Nothing
                            | Just keyHex <-
                                Mithril.mithrilAncillaryVk mithrilOpts =
                                either
                                    (const Nothing)
                                    Just
                                    (parseVerificationKey keyHex)
                            | otherwise =
                                Nothing
                        mithrilConfig =
                            baseConfig
                                { MithrilClient.mithrilGenesisVk =
                                    unpack
                                        <$> Mithril.mithrilGenesisVk mithrilOpts
                                , MithrilClient.mithrilClientPath =
                                    Mithril.mithrilClientPath mithrilOpts
                                , MithrilClient.mithrilAncillaryVk = ancillaryVk
                                }

                    let markBootstrapInProgress =
                            transact
                                $ setBootstrapInProgress
                                    decodePoint
                                    encodePoint

                    let insertIO k v = transact $ csmtInsert ops k v
                    result <-
                        importFromMithril
                            (contramap Mithril tracer)
                            mithrilConfig
                            markBootstrapInProgress
                            insertIO

                    (k, sw, _) <- loadGenesis
                    case result of
                        ImportSuccess{importCheckpoint, importSlot} -> do
                            -- Mithril import succeeded, UTxOs already imported
                            -- Don't save checkpoint yet - it will be saved when
                            -- we reach the target slot during chain sync
                            setup (contra New) runner armageddonParams
                            -- Save checkpoint and skip slot (will be cleared
                            -- when we reach the Mithril slot during chain sync)
                            -- Also clear the bootstrap-in-progress marker
                            transact $ do
                                putBaseCheckpoint
                                    decodePoint
                                    encodePoint
                                    importCheckpoint
                                setSkipSlot decodePoint encodePoint importSlot
                                clearBootstrapInProgress decodePoint encodePoint
                            return
                                SetupResult
                                    { setupStartingPoint = importCheckpoint
                                    , setupMithrilSlot = Just importSlot
                                    , setupIsGenesis = True
                                    , setupSecurityParam = k
                                    , setupStabilityWindow = sw
                                    }
                        ImportFailed err -> do
                            trace $ Mithril $ ImportError err
                            error
                                $ "Mithril bootstrap failed: "
                                    <> show err
                                    <> "\nCannot continue with incomplete bootstrap."
                        ImportExtractionFailed err -> do
                            trace $ Mithril $ ImportExtractionError err
                            error
                                $ "Mithril extraction failed: "
                                    <> show err
                                    <> "\nCannot continue with incomplete bootstrap."

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
