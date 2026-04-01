module Cardano.UTxOCSMT.Application.Run.Main
    ( main
    )
where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.UTxOCSMT.Application.Database.Backend
    ( createBackend
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( putBaseCheckpoint
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , DbState (..)
    , ReadyState (..)
    , RunTransaction (..)
    , mkCSMTOps
    , openCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( WithSentinel (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    , newRunRocksDBTransactionUnguarded
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (..)
    , SyncPhase (..)
    , metricsFold
    )
import Cardano.UTxOCSMT.Application.Options
    ( ConnectionMode (..)
    , Options (..)
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
    , queryAwait
    , queryInclusionProof
    , queryMerkleRoots
    , queryUTxOsByAddress
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
import ChainFollower.Backend qualified as Backend
import ChainFollower.Runner (Phase (..), readCheckpoint)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, catch, displayException)
import Control.Lens (iso)
import Control.Monad ((<=<))
import Control.Tracer (Contravariant (..), Tracer (..), traceWith)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (diffUTCTime)
import Data.Tracer.Fold (foldTracer)
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

-- | Write each traced value to an IORef before passing downstream.
shareTracer :: IORef (Maybe a) -> Tracer IO a -> Tracer IO a
shareTracer ref downstream = Tracer $ \a -> do
    writeIORef ref (Just a)
    traceWith downstream a

-- | Main entry point
main :: IO ()
main = withUtf8 $ do
    hSetBuffering stdout NoBuffering
    options@Options
        { dbPath
        , logPath
        , apiPort
        , headersQueueSize
        } <-
        runParser
            version
            "Tracking cardano UTxOs in a CSMT in a rocksDB database"
            optionsParser
    logTracer logPath $ \basicTracer -> do
        let appTracer = basicTracer

        -- IORef for sharing metrics with HTTP (via shareTracer pattern)
        metricsRef <- newIORef Nothing

        -- Commit notification TVar for awaitValue
        commitNotify <- newTVarIO (0 :: Int)

        -- Break the circular dependency:
        -- metricsEvent needs mainTracer (to inject MetricsReport)
        -- mainTracer needs metricsEvent (for intercept)
        mainTracerRef <- newIORef (Tracer $ const $ pure ())
        let feedbackTracer = Tracer $ \a ->
                readIORef mainTracerRef >>= \t -> traceWith t a

        -- Metrics pipeline: thread-safe → timestamp → fold → share → feedback
        foldedTracer <-
            foldTracer
                metricsFold
                ( shareTracer metricsRef
                    $ contramap MetricsReport feedbackTracer
                )
        metricsEvent <-
            newThreadSafeTracer
                $ utcTimestampTracer foldedTracer

        -- Main tracer pipeline: intercept → thread-safe → timestamp → throttle → render → log
        TraceWith{tracer, trace, contra} <-
            fmap (intercept metricsEvent stealMetricsEvent) $ do
                throttled <-
                    throttleByFrequency
                        (\t1 t2 -> realToFrac (diffUTCTime t2 t1))
                        [matchHighFrequencyEvents]
                        (contramap renderThrottledMainTraces appTracer)
                newThreadSafeTracer $ utcTimestampTracer throttled

        -- Close the loop
        writeIORef mainTracerRef tracer
        startHTTPService
            (trace . HTTPServiceError)
            (trace ServeDocs)
            (apiDocsPort options)
            $ flip runDocsServer apiPort
        trace Boot
        withRocksDB dbPath $ \db -> do
            -- Create runner first (no logging)
            let CSMTContext{fromKV = fkv, hashing = h} = context
                ops = mkCSMTOps fkv h
            runner <-
                newRunRocksDBTransaction
                    db
                    prisms
            let unguardedRunner =
                    newRunRocksDBTransactionUnguarded
                        db
                        prisms
            -- Open ops with crash recovery
            dbState <-
                openCSMTOps
                    4
                    1000
                    (iso BL.toStrict BL.fromStrict)
                    fkv
                    h
                    (transact runner)
                    (transact unguardedRunner)
                    (trace . JournalReplay)
            -- Resolve DbState: recover if crashed, then
            -- extract KVOnly ops
            let resolve (NeedsRecovery recover) = do
                    trace DbNeedsRecovery
                    st <- recover
                    trace DbRecoveryDone
                    resolve st
                resolve (Ready (ChooseKVOnly kvOps)) = do
                    trace DbReadyKVOnly
                    pure kvOps
                resolve (Ready (ChooseFull _)) =
                    fail "openCSMTOps: unexpected ChooseFull"
            kvOnlyOps <- resolve dbState

            let getReadyResponse =
                    mkReadyResponse (syncThreshold options)
                        <$> readIORef metricsRef

            -- Start API server early so /metrics is available during bootstrap
            startHTTPService
                (trace . HTTPServiceError)
                (trace ServeApi)
                apiPort
                $ \port ->
                    runAPIServer
                        port
                        (readIORef metricsRef)
                        (queryMerkleRoots runner)
                        (queryInclusionProof runner)
                        (queryUTxOsByAddress runner)
                        getReadyResponse
                        (queryAwait commitNotify runner)

            SetupResult
                { setupStartingPoint
                , setupSecurityParam
                , setupNetworkMagic
                , setupEpochSlots
                } <-
                setupDB
                    tracer
                    (genesisFile options)
                    (byronGenesisFile options)
                    armageddonParams
                    ops
                    runner

            -- Create Backend.Init
            let backendInit =
                    createBackend kvOnlyOps slotHash

            -- Start in restoration — Runner transitions
            -- to following when atTip=True
            restoring <-
                Backend.start backendInit
            let initialPhase =
                    InRestoration restoring

            -- Read checkpoint from rollback column
            mCheckpoint <-
                transact runner
                    $ readCheckpoint Rollbacks
            let availablePoints =
                    case mCheckpoint of
                        Just (Value cp) -> [cp]
                        _ -> [setupStartingPoint]

            -- Create checkpoint actions
            let setCheckpoint point =
                    transact runner
                        $ putBaseCheckpoint
                            decodePoint
                            encodePoint
                            point
            -- Log before starting the application
            trace ApplicationStarting
            traceWith metricsEvent
                $ SyncPhaseEvent Synced

            let epochSlots =
                    EpochSlots setupEpochSlots
            result <-
                ( case connectionMode options of
                    N2N{n2nHost, n2nPort} ->
                        application
                            (syncThreshold options)
                            epochSlots
                            setupNetworkMagic
                            n2nHost
                            n2nPort
                            setupStartingPoint
                            headersQueueSize
                            setCheckpoint
                            metricsEvent
                            (contra Application)
                            runner
                            backendInit
                            armageddonParams
                            (fromIntegral setupSecurityParam)
                            commitNotify
                            initialPhase
                            availablePoints
                    N2C{n2cSocket} ->
                        applicationN2C
                            (syncThreshold options)
                            epochSlots
                            setupNetworkMagic
                            n2cSocket
                            setupStartingPoint
                            setCheckpoint
                            metricsEvent
                            (contra Application)
                            runner
                            backendInit
                            armageddonParams
                            (fromIntegral setupSecurityParam)
                            commitNotify
                            initialPhase
                            availablePoints
                )
                    `catch` \(e :: SomeException) -> do
                        trace
                            $ HTTPServiceError
                            $ "Application crashed: "
                                ++ displayException e
                        error
                            $ "main: application crashed: "
                                ++ displayException e
            error
                $ "main: application exited"
                    ++ " unexpectedly with: "
                    ++ show result
