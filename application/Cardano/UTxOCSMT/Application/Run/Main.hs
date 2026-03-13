{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.Run.Main
    ( main
    )
where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , kvOnlyCSMTOps
    , mkCSMTOps
    , replayJournal
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( fullForwardOps
    , measureUpdateDurations
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
import Data.Time.Clock (diffUTCTime)
import Data.Tracer.Intercept (intercept)
import Data.Tracer.LogFile (logTracer)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Throttle (throttleByFrequency)
import Data.Tracer.Timestamp (utcTimestampTracer)
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
        , headersQueueSize
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
                        (\t1 t2 -> realToFrac (diffUTCTime t2 t1))
                        [matchHighFrequencyEvents]
                        (contramap renderThrottledMainTraces appTracer)
                newThreadSafeTracer $ utcTimestampTracer throttled
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

            SetupResult
                { setupStartingPoint
                , setupIsGenesis
                , setupSecurityParam
                , setupStabilityWindow
                } <-
                setupDB
                    tracer
                    (genesisFile options)
                    (byronGenesisFile options)
                    armageddonParams
                    ops
                    runner

            -- Now create the Update state (logs "New update state")
            let stabilityWindow =
                    SlotNo setupStabilityWindow
                onForward blockPoint chainTipSlot =
                    case pointSlot blockPoint of
                        At blockSlot
                            | blockSlot >= chainTipSlot ->
                                traceWith metricsEvent $ BootstrapPhaseEvent Synced
                        _ -> pure ()
                isAtTip blockPoint chainTipSlot =
                    case pointSlot blockPoint of
                        At blockSlot ->
                            blockSlot + stabilityWindow
                                >= chainTipSlot
                        _ -> False
                replay =
                    replayJournal 1000 BL.fromStrict fkv h runner
            updateTracer <-
                measureUpdateDurations (contra Update)
            (state, slots) <-
                createSplitUpdateState
                    updateTracer
                    fullForwardOps
                    setupIsGenesis
                    kvOps
                    fullOps
                    replay
                    isAtTip
                    slotHash
                    onForward
                    armageddonParams
                    runner
                    (fromIntegral setupSecurityParam)
                    ( putBaseCheckpoint
                        decodePoint
                        encodePoint
                    )

            -- Create checkpoint action
            let setCheckpoint point =
                    transact runner
                        $ putBaseCheckpoint
                            decodePoint
                            encodePoint
                            point

            -- Log before starting the application
            trace ApplicationStarting
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
                            Nothing
                            metricsEvent
                            (contra Application)
                            state
                            slots
                    N2C{n2cSocket} ->
                        applicationN2C
                            epochSlots
                            (networkMagic options)
                            n2cSocket
                            setupStartingPoint
                            setCheckpoint
                            Nothing
                            metricsEvent
                            (contra Application)
                            state
                            slots
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
