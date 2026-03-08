module Cardano.UTxOCSMT.Application.Run.Application
    ( application
    , applicationN2C
    , ApplicationTrace (..)
    , renderApplicationTrace
    )
where

import CSMT ()
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength
    , Fetched (..)
    , HeaderSkipProgress (..)
    , mkBlockFetchApplication
    )
import Cardano.UTxOCSMT.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.ChainSyncN2C
    ( mkN2CChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( BootstrapPhase (..)
    , MetricsEvent (..)
    )
import Cardano.UTxOCSMT.Application.UTxOs
    ( Change (..)
    , uTxOsWithTxCount
    )
import Cardano.UTxOCSMT.Ouroboros.Connection (runNodeApplication)
import Cardano.UTxOCSMT.Ouroboros.ConnectionN2C
    ( runLocalNodeApplication
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import Control.Exception (throwIO)
import Control.Monad (replicateM_, when)
import Control.Tracer (Tracer (..), contramap, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Function (fix)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Void (Void)
import Data.Word (Word64)
import Ouroboros.Consensus.Block (getHeader)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (PortNumber)
import Ouroboros.Network.Point
    ( WithOrigin (..)
    )
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- | Events emitted by the application
data ApplicationTrace
    = ApplicationIntersectionAt Point
    | ApplicationIntersectionFailed
    | -- | Rollback starting to the given point
      ApplicationRollingBack Point
    | -- | Block processed at slot with tx count and UTxO change count
      ApplicationBlockProcessed SlotNo Int Int
    | -- | Header sync progress during Mithril catch-up
      ApplicationHeaderSkipProgress HeaderSkipProgress
    | -- | Finality check result
      ApplicationFinalityResult (Maybe Point)
    | -- | Finality pruned
      ApplicationFinalityPruned Point
    | -- | Finality check duration in microseconds
      ApplicationFinalityUs Int
    deriving (Show)

-- | Render an 'ApplicationTrace'
renderApplicationTrace :: ApplicationTrace -> String
renderApplicationTrace (ApplicationIntersectionAt point) =
    "Intersected at point: " ++ show point
renderApplicationTrace ApplicationIntersectionFailed =
    "Intersection failed, resetting to origin"
renderApplicationTrace (ApplicationRollingBack point) =
    "Rolling back to point: " ++ show point
renderApplicationTrace (ApplicationBlockProcessed slot txCount utxoCount) =
    "Block processed: slot "
        ++ show (unSlotNo slot)
        ++ ", "
        ++ show txCount
        ++ " txs, "
        ++ show utxoCount
        ++ " UTxO changes"
renderApplicationTrace (ApplicationHeaderSkipProgress progress) =
    "Syncing headers: slot "
        ++ show (unSlotNo $ skipCurrentSlot progress)
        ++ " / "
        ++ show (unSlotNo $ skipTargetSlot progress)
renderApplicationTrace (ApplicationFinalityResult Nothing) =
    "Finality: Nothing"
renderApplicationTrace (ApplicationFinalityResult (Just p)) =
    "Finality: Just " ++ show p
renderApplicationTrace (ApplicationFinalityPruned p) =
    "Finality pruned at " ++ show p
renderApplicationTrace (ApplicationFinalityUs us) =
    "Finality check " ++ show us ++ "us"

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

-- | Create a tracer that only emits progress events every N slots
throttleBySlot
    :: Word64
    -- ^ Slot interval between logs
    -> Tracer IO HeaderSkipProgress
    -> IO (Tracer IO HeaderSkipProgress)
throttleBySlot interval baseTracer = do
    lastSlotRef <- newIORef 0
    pure $ Tracer $ \progress -> do
        let SlotNo currentSlot = skipCurrentSlot progress
        lastSlot <- readIORef lastSlotRef
        when (currentSlot >= lastSlot + interval) $ do
            writeIORef lastSlotRef currentSlot
            traceWith baseTracer progress

type DBState = State IO Point ByteString ByteString

intersector
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> DBState
    -> Intersector Fetched
intersector TraceWith{trace, tracer} trUTxO mFinality updater =
    Intersector
        { intersectFound = \point -> do
            trace $ ApplicationIntersectionAt point
            pure $ follower tracer trUTxO mFinality updater
        , intersectNotFound = do
            trace ApplicationIntersectionFailed
            pure
                ( intersector tracer trUTxO mFinality updater
                , [origin]
                )
        }

changeToOperation :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

follower
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> DBState
    -> Follower Fetched
follower
    TraceWith{trace, tracer}
    trUTxO
    newFinalityTarget
    db = ($ db) $ fix $ \go currentDB ->
        Follower
            { rollForward = \Fetched{fetchedPoint, fetchedBlock} tipSlot -> do
                let (txCount, utxoChanges) = uTxOsWithTxCount fetchedBlock
                    ops = changeToOperation <$> utxoChanges
                    opsCount = length ops
                replicateM_ opsCount trUTxO
                case Network.pointSlot fetchedPoint of
                    At slot
                        | txCount > 0 ->
                            trace
                                $ ApplicationBlockProcessed
                                    slot
                                    txCount
                                    opsCount
                    _ -> pure ()
                newDB <- case currentDB of
                    Syncing update -> do
                        newUpdate <-
                            forwardTipApply update fetchedPoint tipSlot ops
                        t0 <- getMonotonicTimeNSec
                        finality <- newFinalityTarget
                        t1 <- getMonotonicTimeNSec
                        let us =
                                fromIntegral (t1 - t0)
                                    `div` 1000
                                    :: Int
                        trace $ ApplicationFinalityResult finality
                        trace $ ApplicationFinalityUs us
                        Syncing <$> case finality of
                            Nothing -> pure newUpdate
                            Just slot -> do
                                trace $ ApplicationFinalityPruned slot
                                forwardFinalityApply newUpdate slot
                    _ -> error "follower: cannot roll forward while intersecting"
                pure $ go newDB
            , rollBackward = \point ->
                rollingBack
                    tracer
                    trUTxO
                    newFinalityTarget
                    point
                    go
                    currentDB
            }

rollingBack
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> IO (Maybe Point)
    -> Point
    -> (DBState -> Follower Fetched)
    -> DBState
    -> IO (ProgressOrRewind Fetched)
rollingBack TraceWith{trace, tracer} trUTxO newFinalityTarget point follower' state = do
    trace $ ApplicationRollingBack point
    case state of
        Syncing update -> do
            newState <- rollbackTipApply update (At point)
            pure $ case newState of
                Syncing newUpdate ->
                    Progress
                        $ follower'
                        $ Syncing newUpdate
                Truncating newUpdate ->
                    Reset
                        $ intersector tracer trUTxO newFinalityTarget
                        $ Syncing newUpdate
                Intersecting ps newUpdate ->
                    Rewind ps
                        $ intersector tracer trUTxO newFinalityTarget
                        $ Syncing newUpdate
        _ -> error "rollingBack: cannot roll back while intersecting"

application
    :: EpochSlots
    -- ^ Epoch slots for Byron codec
    -> NetworkMagic
    -- ^ Network magic
    -> String
    -- ^ Node name
    -> PortNumber
    -- ^ Port number
    -> Point
    -- ^ Starting point
    -> EventQueueLength
    -- ^ Headers queue size
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> Maybe SlotNo
    -- ^ Optional skip until slot for Mithril bootstrap
    -> Tracer IO MetricsEvent
    -- ^ Tracer for metrics events
    -> Tracer IO ApplicationTrace
    -- ^ Tracer for application events
    -> Update IO Point ByteString ByteString
    -- ^ Initial database FSM update
    -> [Point]
    -- ^ Available points to sync from
    -> IO (Maybe Point)
    -- ^ Finality target. TODO redesign the Update object to avoid this
    -> IO Void
application
    epochSlots
    networkMagic
    nodeName
    portNumber
    startingPoint
    headersQueueSize
    setCheckpoint
    mSkipTargetSlot
    TraceWith{trace = metricTrace, contra = metricContra}
    TraceWith{tracer}
    initialDBUpdate
    availablePoints
    mFinality =
        do
            hSetBuffering stdout NoBuffering

            let counting = metricTrace UTxOChangeEvent

                -- Tracer that emits to both app trace and metrics
                metricsSkipTracer = Tracer
                    $ \HeaderSkipProgress{skipCurrentSlot, skipTargetSlot} ->
                        metricTrace
                            $ HeaderSyncProgressEvent
                                skipCurrentSlot
                                skipTargetSlot
                appSkipTracer = contramap ApplicationHeaderSkipProgress tracer

            skipProgressTracer <-
                throttleBySlot 1000 (appSkipTracer <> metricsSkipTracer)

            let onSkipComplete =
                    metricTrace $ BootstrapPhaseEvent Synced

            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (metricContra BlockFetchEvent)
                    skipProgressTracer
                    setCheckpoint
                    onSkipComplete
                    mSkipTargetSlot
                    $ intersector tracer counting mFinality
                    $ Syncing initialDBUpdate
            let chainFollowingApplication =
                    mkChainSyncApplication
                        (metricContra BlockInfoEvent)
                        (metricContra ChainTipEvent)
                        headerIntersector
                        $ if null availablePoints
                            then
                                [startingPoint]
                            else availablePoints
            result <-
                runNodeApplication
                    epochSlots
                    networkMagic
                    nodeName
                    portNumber
                    chainFollowingApplication
                    blockFetchApplication

            case result of
                Left err -> throwIO err
                Right (Left ()) ->
                    error "application: chain following application exited unexpectedly"
                Right _ -> error "application: impossible branch reached"

-- | N2C variant: connects via Unix socket, no BlockFetch needed
applicationN2C
    :: EpochSlots
    -- ^ Epoch slots for Byron codec
    -> NetworkMagic
    -- ^ Network magic
    -> FilePath
    -- ^ Socket path
    -> Point
    -- ^ Starting point
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> Maybe SlotNo
    -- ^ Optional skip until slot for Mithril bootstrap
    -> Tracer IO MetricsEvent
    -- ^ Tracer for metrics events
    -> Tracer IO ApplicationTrace
    -- ^ Tracer for application events
    -> Update IO Point ByteString ByteString
    -- ^ Initial database FSM update
    -> [Point]
    -- ^ Available points to sync from
    -> IO (Maybe Point)
    -- ^ Finality target
    -> IO Void
applicationN2C
    epochSlots
    networkMagic'
    socketPath
    startingPoint
    setCheckpoint
    mSkipTargetSlot
    TraceWith{trace = metricTrace, contra = metricContra}
    TraceWith{tracer}
    initialDBUpdate
    availablePoints
    mFinality =
        do
            hSetBuffering stdout NoBuffering

            let counting = metricTrace UTxOChangeEvent

                metricsSkipTracer = Tracer
                    $ \HeaderSkipProgress{skipCurrentSlot, skipTargetSlot} ->
                        metricTrace
                            $ HeaderSyncProgressEvent
                                skipCurrentSlot
                                skipTargetSlot
                appSkipTracer = contramap ApplicationHeaderSkipProgress tracer

            skipProgressTracer <-
                throttleBySlot 1000 (appSkipTracer <> metricsSkipTracer)

            let onSkipComplete =
                    metricTrace $ BootstrapPhaseEvent Synced

                blockIntersector =
                    intersector tracer counting mFinality
                        $ Syncing initialDBUpdate

                points =
                    if null availablePoints
                        then [startingPoint]
                        else availablePoints

                chainSyncApp =
                    mkN2CChainSyncApplication
                        (metricContra (BlockInfoEvent . getHeader))
                        (metricContra ChainTipEvent)
                        skipProgressTracer
                        setCheckpoint
                        onSkipComplete
                        mSkipTargetSlot
                        blockIntersector
                        points

            result <-
                runLocalNodeApplication
                    epochSlots
                    networkMagic'
                    socketPath
                    chainSyncApp

            case result of
                Left err -> throwIO err
                Right () ->
                    error
                        "applicationN2C: chain sync exited unexpectedly"
