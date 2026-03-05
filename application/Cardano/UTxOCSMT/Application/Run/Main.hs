{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.Main
    ( main
    )
where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( clearSkipSlot
    , putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , kvOnlyCSMTOps
    , mkCSMTOps
    , replayJournal
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( createSplitUpdateState
    , newRunRocksDBTransaction
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( BootstrapPhase (..)
    , MetricsEvent (..)
    , MetricsParams (..)
    , metricsTracer
    )
import Cardano.UTxOCSMT.Application.Options
    ( ConnectionMode (..)
    , Options (..)
    , epochSlotsFor
    , networkMagic
    , optionsParser
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( application
    , applicationN2C
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , context
    , decodePoint
    , encodePoint
    , mFinality
    , prisms
    , slotHash
    , withRocksDB
    )
import Cardano.UTxOCSMT.Application.Run.Query
    ( mkReadyResponse
    , queryInclusionProof
    , queryMerkleRoots
    , queryUTxOsByAddress
    )
import Cardano.UTxOCSMT.Application.Run.RenderMetrics
    ( renderMetrics
    )
import Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    )
import Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    , matchHighFrequencyEvents
    , renderThrottledMainTraces
    , stealMetricsEvent
    )
import Cardano.UTxOCSMT.HTTP.Server (runAPIServer, runDocsServer)
import Cardano.UTxOCSMT.Mithril.Options qualified as Mithril
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , newTVarIO
    , readTVarIO
    , writeTVar
    )
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (when, (<=<))
import Control.Tracer (Contravariant (..), nullTracer, traceWith)
import Data.ByteString.Lazy qualified as BL
import Data.Tracer.Intercept (intercept)
import Data.Tracer.LogFile (logTracer)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Throttle (throttleByFrequency)
import Data.Tracer.Timestamp (timestampTracer)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Main.Utf8 (withUtf8)
import OptEnvConf (runParser)
import Ouroboros.Consensus.Ledger.SupportsPeerSelection (PortNumber)
import Ouroboros.Network.Block (SlotNo (..), pointSlot)
import Ouroboros.Network.Point (WithOrigin (..))
import Paths_cardano_utxo_csmt (version)
import System.Exit (exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- | Start an HTTP service in a linked async thread
startHTTPService
    :: (String -> IO ())
    -> IO ()
    -> Maybe PortNumber
    -> (PortNumber -> IO ())
    -> IO ()
startHTTPService _ _ Nothing _ = return ()
startHTTPService logError traceAction (Just port) runServer = do
    traceAction
    link <=< async
        $ runServer port `catch` \(e :: SomeException) -> do
            logError $ "HTTP server crashed: " ++ displayException e

-- | Main entry point
main :: IO ()
main = withUtf8 $ do
    hSetBuffering stdout NoBuffering
    options@Options
        { dbPath
        , logPath
        , apiPort
        , metricsOn
        , startingPoint
        , headersQueueSize
        , mithrilOptions
        } <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in a rocksDB database"
            optionsParser
    logTracer logPath $ \basicTracer -> do
        let appTracer
                | metricsOn, Nothing <- logPath = nullTracer
                | otherwise = basicTracer
        metricsVar <- newTVarIO Nothing
        metricsEvent <-
            metricsTracer
                $ MetricsParams
                    { qlWindow = 100
                    , utxoSpeedWindow = 1000
                    , blockSpeedWindow = 100
                    , metricsOutput = \ !metrics -> do
                        when metricsOn $ renderMetrics metrics
                        atomically $ writeTVar metricsVar (Just metrics)
                    , metricsFrequency = 1_000_000
                    }
        TraceWith{tracer, trace, contra} <-
            fmap (intercept metricsEvent stealMetricsEvent) $ do
                throttled <-
                    throttleByFrequency
                        [matchHighFrequencyEvents]
                        (contramap renderThrottledMainTraces appTracer)
                newThreadSafeTracer $ timestampTracer throttled
        startHTTPService
            (trace . HTTPServiceError)
            (trace ServeDocs)
            (apiDocsPort options)
            $ flip runDocsServer apiPort
        trace Boot
        withRocksDB dbPath $ \db -> do
            -- Create runner first (no logging)
            let CSMTContext{fromKV = fkv, hashing = h} = context
                fullOps = mkCSMTOps fkv h
                kvOps = kvOnlyCSMTOps BL.toStrict
                ops = fullOps
            runner <-
                newRunRocksDBTransaction
                    db
                    prisms

            let getReadyResponse =
                    mkReadyResponse (syncThreshold options)
                        <$> readTVarIO metricsVar

            -- Start API server early so /metrics is available during bootstrap
            startHTTPService
                (trace . HTTPServiceError)
                (trace ServeApi)
                apiPort
                $ \port ->
                    runAPIServer
                        port
                        (readTVarIO metricsVar)
                        (queryMerkleRoots runner)
                        (queryInclusionProof runner)
                        (queryUTxOsByAddress runner)
                        getReadyResponse

            -- Do Mithril bootstrap before creating Update state
            -- N2C mode skips TCP node validation (uses Unix socket)
            let (setupNodeName, setupNodePort, setupSkipValidation) =
                    case connectionMode options of
                        N2N{n2nHost, n2nPort} ->
                            (n2nHost, n2nPort, skipNodeValidation options)
                        N2C{} ->
                            ("localhost", 0, True)
            SetupResult{setupStartingPoint, setupMithrilSlot} <-
                setupDB
                    tracer
                    startingPoint
                    (genesisFile options)
                    (byronGenesisFile options)
                    mithrilOptions
                    (networkMagic options)
                    setupNodeName
                    setupNodePort
                    setupSkipValidation
                    armageddonParams
                    ops
                    runner

            -- Now create the Update state (logs "New update state")
            let onForward blockPoint chainTipSlot =
                    case pointSlot blockPoint of
                        At blockSlot
                            | blockSlot >= chainTipSlot ->
                                traceWith metricsEvent $ BootstrapPhaseEvent Synced
                        _ -> pure ()
                isAtTip blockPoint chainTipSlot =
                    case pointSlot blockPoint of
                        At blockSlot -> blockSlot >= chainTipSlot
                        _ -> False
                replay =
                    replayJournal 1000 BL.fromStrict fkv h runner
            (state, slots) <-
                createSplitUpdateState
                    (contra Update)
                    kvOps
                    fullOps
                    replay
                    isAtTip
                    slotHash
                    onForward
                    armageddonParams
                    runner

            -- Check if we should exit after bootstrap
            when (Mithril.mithrilBootstrapOnly mithrilOptions) $ do
                trace $ BootstrapOnlyExit setupStartingPoint
                exitSuccess

            -- Create checkpoint action and skip configuration
            let setCheckpoint point =
                    transact runner $ do
                        putBaseCheckpoint decodePoint encodePoint point
                        clearSkipSlot decodePoint encodePoint
                mSkipTargetSlot = SlotNo <$> setupMithrilSlot

            -- Log before starting the application
            trace ApplicationStarting

            -- Emit bootstrap phase based on whether we need to sync headers
            case mSkipTargetSlot of
                Just _ ->
                    traceWith metricsEvent $ BootstrapPhaseEvent SyncingHeaders
                Nothing ->
                    traceWith metricsEvent $ BootstrapPhaseEvent Synced

            let epochSlots =
                    EpochSlots $ epochSlotsFor $ network options
            result <-
                ( case connectionMode options of
                    N2N{n2nHost, n2nPort} ->
                        application
                            epochSlots
                            (networkMagic options)
                            n2nHost
                            n2nPort
                            setupStartingPoint
                            headersQueueSize
                            setCheckpoint
                            mSkipTargetSlot
                            metricsEvent
                            (contra Application)
                            state
                            slots
                            (mFinality runner)
                    N2C{n2cSocket} ->
                        applicationN2C
                            epochSlots
                            (networkMagic options)
                            n2cSocket
                            setupStartingPoint
                            setCheckpoint
                            mSkipTargetSlot
                            metricsEvent
                            (contra Application)
                            state
                            slots
                            (mFinality runner)
                )
                    `catch` \(e :: SomeException) -> do
                        trace
                            $ HTTPServiceError
                            $ "Application crashed: "
                                ++ displayException e
                        error
                            $ "main: application crashed: "
                                ++ displayException e
            error $ "main: application exited unexpectedly with: " ++ show result
