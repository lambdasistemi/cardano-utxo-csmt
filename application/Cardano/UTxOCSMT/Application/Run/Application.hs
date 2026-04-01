module Cardano.UTxOCSMT.Application.Run.Application
    ( application
    , applicationN2C
    , ApplicationTrace (..)
    , renderApplicationTrace
    ) where

import CSMT.Hashes (Hash)
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Node.Client.N2C.ChainSync
    ( Fetched (..)
    , runChainSyncN2C
    )
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength
    , mkBlockFetchApplication
    )
import Cardano.UTxOCSMT.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.ChainSyncN2C
    ( mkN2CChainSyncApplication
    )
import Cardano.UTxOCSMT.Application.Database.Backend
    ( RunnerEvent (..)
    , processBlock
    , rollbackTo
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , armageddon
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , WithSentinel (..)
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (..)
    , SyncPhase (..)
    , SyncThreshold (..)
    )
import Cardano.UTxOCSMT.Application.UTxOs
    ( Change (..)
    , uTxOsWithTxCount
    )
import Cardano.UTxOCSMT.Ouroboros.Connection
    ( runNodeApplication
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Follower (..)
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    )
import ChainFollower.Backend qualified as Backend
import ChainFollower.Rollbacks.Store qualified as Store
import ChainFollower.Runner (Phase (..))
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Control.Tracer (Tracer (..), contramap)
import Data.ByteString.Lazy (ByteString)

import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Data.Void (Void)
import Database.KV.Transaction (Transaction)
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Consensus.Block (getHeader)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.PeerSelection.RelayAccessPoint
    ( PortNumber
    )
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , stdout
    )

-- | Events emitted by the application
data ApplicationTrace
    = ApplicationIntersectionAt Point
    | ApplicationIntersectionFailed
    | ApplicationRollingBack Point
    | ApplicationBlockProcessed SlotNo Int Int
    | ApplicationRunnerEvent (RunnerEvent (WithSentinel Point))
    deriving (Show)

-- | Render an 'ApplicationTrace'
renderApplicationTrace
    :: ApplicationTrace -> String
renderApplicationTrace
    (ApplicationIntersectionAt point) =
        "Intersected at point: " ++ show point
renderApplicationTrace
    ApplicationIntersectionFailed =
        "Intersection failed, resetting to origin"
renderApplicationTrace
    (ApplicationRollingBack point) =
        "Rolling back to point: " ++ show point
renderApplicationTrace
    (ApplicationBlockProcessed slot txCount utxoCount) =
        "Block processed: slot "
            ++ show (unSlotNo slot)
            ++ ", "
            ++ show txCount
            ++ " txs, "
            ++ show utxoCount
            ++ " UTxO changes"
renderApplicationTrace
    (ApplicationRunnerEvent event) =
        "Runner: " ++ case event of
            BlockRestored slot ->
                "block restored at " ++ show slot
            BlockFollowed slot ->
                "block followed at " ++ show slot
            PhaseTransition slot ->
                "PHASE TRANSITION restoration → following at "
                    ++ show slot

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

-- | The transaction type for the application.
type AppTx =
    Transaction
        IO
        ColumnFamily
        (Columns Point Hash ByteString ByteString)
        BatchOp

-- | Concrete phase type for the application.
type AppPhase =
    Phase
        IO
        ColumnFamily
        (Columns Point Hash ByteString ByteString)
        BatchOp
        (Point, [Operation ByteString ByteString])
        [Operation ByteString ByteString]
        (Hash, Maybe Hash)

changeToOperation
    :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

intersector
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> (MetricsEvent -> IO ())
    -> SyncThreshold
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        ByteString
        ByteString
        IO
    -> Backend.Init
        IO
        AppTx
        ( Point
        , [Operation ByteString ByteString]
        )
        [Operation ByteString ByteString]
        (Hash, Maybe Hash)
    -> ArmageddonParams Hash
    -> Int
    -- ^ Security parameter (stability window)
    -> TVar Int
    -- ^ Commit notification counter
    -> AppPhase
    -> Intersector Point SlotNo Fetched
intersector
    TraceWith{trace, tracer}
    trUTxO
    metricTrace
    syncThreshold'
    runner
    backendInit
    armageddonParams
    securityParam
    notifyTVar
    phase =
        Intersector
            { intersectFound = \point -> do
                trace $ ApplicationIntersectionAt point
                pure
                    $ follower
                        tracer
                        trUTxO
                        metricTrace
                        syncThreshold'
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        notifyTVar
                        phase
            , intersectNotFound = do
                trace ApplicationIntersectionFailed
                pure
                    ( intersector
                        tracer
                        trUTxO
                        metricTrace
                        syncThreshold'
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        notifyTVar
                        phase
                    , [origin]
                    )
            }

follower
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> (MetricsEvent -> IO ())
    -> SyncThreshold
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        ByteString
        ByteString
        IO
    -> Backend.Init
        IO
        AppTx
        ( Point
        , [Operation ByteString ByteString]
        )
        [Operation ByteString ByteString]
        (Hash, Maybe Hash)
    -> ArmageddonParams Hash
    -> Int
    -- ^ Security parameter (stability window)
    -> TVar Int
    -- ^ Commit notification counter
    -> AppPhase
    -> Follower Point SlotNo Fetched
follower
    TraceWith{trace, tracer}
    trUTxO
    metricTrace
    syncThreshold'
    runner@RunTransaction{transact}
    backendInit
    armageddonParams
    securityParam
    notifyTVar =
        go
      where
        go phase =
            Follower
                { rollForward =
                    \Fetched{fetchedPoint, fetchedBlock} tipSlot -> do
                        let (txCount, utxoChanges) =
                                uTxOsWithTxCount fetchedBlock
                            ops =
                                changeToOperation <$> utxoChanges
                            opsCount = length ops
                            blockSlot = case Network.pointSlot fetchedPoint of
                                At (SlotNo s) -> s
                                _ -> 0
                            withinStabilityWindow =
                                blockSlot + fromIntegral securityParam
                                    >= unSlotNo tipSlot
                            slotsBehind =
                                if unSlotNo tipSlot >= blockSlot
                                    then unSlotNo tipSlot - blockSlot
                                    else 0
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
                        phase' <-
                            processBlock
                                ( contramap
                                    ApplicationRunnerEvent
                                    tracer
                                )
                                withinStabilityWindow
                                transact
                                Rollbacks
                                securityParam
                                (Value fetchedPoint)
                                (fetchedPoint, ops)
                                phase
                        metricTrace $ SyncPhaseEvent $ case phase' of
                            InRestoration _ -> Restoring
                            InFollowing _ _
                                | slotsBehind
                                    <= unSyncThreshold syncThreshold' ->
                                    Synced
                                | otherwise -> Following
                        atomically $ modifyTVar' notifyTVar (+ 1)
                        pure $ go phase'
                , rollBackward = \point -> do
                    trace $ ApplicationRollingBack point
                    case phase of
                        InFollowing n f -> do
                            (result, n') <-
                                transact
                                    $ rollbackTo
                                        Rollbacks
                                        f
                                        n
                                        (Value point)
                            case result of
                                Store.RollbackSucceeded _ ->
                                    pure
                                        $ Progress
                                        $ go
                                            (InFollowing n' f)
                                Store.RollbackImpossible
                                    | n' == 0 ->
                                        pure
                                            $ Progress
                                            $ go
                                                (InFollowing 0 f)
                                    | otherwise -> do
                                        armageddon
                                            ( Tracer
                                                $ const
                                                $ pure ()
                                            )
                                            runner
                                            armageddonParams
                                        restoring <-
                                            Backend.start
                                                backendInit
                                        pure
                                            $ Reset
                                            $ intersector
                                                tracer
                                                trUTxO
                                                metricTrace
                                                syncThreshold'
                                                runner
                                                backendInit
                                                armageddonParams
                                                securityParam
                                                notifyTVar
                                                ( InRestoration
                                                    restoring
                                                )
                        InRestoration _ ->
                            pure
                                $ Progress
                                $ go phase
                }

application
    :: SyncThreshold
    -> EpochSlots
    -> NetworkMagic
    -> String
    -> PortNumber
    -> Point
    -> EventQueueLength
    -> (Point -> IO ())
    -> Tracer IO MetricsEvent
    -> Tracer IO ApplicationTrace
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        ByteString
        ByteString
        IO
    -> Backend.Init
        IO
        AppTx
        ( Point
        , [Operation ByteString ByteString]
        )
        [Operation ByteString ByteString]
        (Hash, Maybe Hash)
    -> ArmageddonParams Hash
    -> Int
    -- ^ Security parameter (stability window)
    -> TVar Int
    -- ^ Commit notification counter
    -> AppPhase
    -> [Point]
    -> IO Void
application
    syncThreshold'
    epochSlots
    networkMagic
    nodeName
    portNumber
    startingPoint
    headersQueueSize
    setCheckpoint
    TraceWith{trace = metricTrace, contra = metricContra}
    TraceWith{tracer}
    runner
    backendInit
    armageddonParams
    securityParam
    notifyTVar
    initialPhase
    availablePoints =
        do
            hSetBuffering stdout NoBuffering

            let counting = metricTrace UTxOChangeEvent
                onSynced =
                    metricTrace $ SyncPhaseEvent Synced

            (blockFetchApplication, headerIntersector) <-
                mkBlockFetchApplication
                    headersQueueSize
                    (metricContra BlockFetchEvent)
                    (Tracer $ const $ pure ())
                    setCheckpoint
                    onSynced
                    Nothing
                    $ intersector
                        tracer
                        counting
                        metricTrace
                        syncThreshold'
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        notifyTVar
                        initialPhase
            let chainFollowingApplication =
                    mkChainSyncApplication
                        (metricContra BlockInfoEvent)
                        (metricContra ChainTipEvent)
                        headerIntersector
                        $ if null availablePoints
                            then [startingPoint]
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
                    error
                        "application: chain following\
                        \ exited unexpectedly"
                Right _ ->
                    error
                        "application: impossible branch"

applicationN2C
    :: SyncThreshold
    -> EpochSlots
    -> NetworkMagic
    -> FilePath
    -> Point
    -> (Point -> IO ())
    -> Tracer IO MetricsEvent
    -> Tracer IO ApplicationTrace
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        ByteString
        ByteString
        IO
    -> Backend.Init
        IO
        AppTx
        ( Point
        , [Operation ByteString ByteString]
        )
        [Operation ByteString ByteString]
        (Hash, Maybe Hash)
    -> ArmageddonParams Hash
    -> Int
    -- ^ Security parameter (stability window)
    -> TVar Int
    -- ^ Commit notification counter
    -> AppPhase
    -> [Point]
    -> IO Void
applicationN2C
    syncThreshold'
    epochSlots
    networkMagic'
    socketPath
    startingPoint
    setCheckpoint
    TraceWith{trace = metricTrace, contra = metricContra}
    TraceWith{tracer}
    runner
    backendInit
    armageddonParams
    securityParam
    notifyTVar
    initialPhase
    availablePoints =
        do
            hSetBuffering stdout NoBuffering

            let counting = metricTrace UTxOChangeEvent
                onSynced =
                    metricTrace $ SyncPhaseEvent Synced
                blockIntersector =
                    intersector
                        tracer
                        counting
                        metricTrace
                        syncThreshold'
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        notifyTVar
                        initialPhase
                points =
                    if null availablePoints
                        then [startingPoint]
                        else availablePoints
                chainSyncApp =
                    mkN2CChainSyncApplication
                        ( metricContra
                            (BlockInfoEvent . getHeader)
                        )
                        (metricContra ChainTipEvent)
                        (Tracer $ const $ pure ())
                        setCheckpoint
                        onSynced
                        Nothing
                        blockIntersector
                        points

            result <-
                runChainSyncN2C
                    epochSlots
                    networkMagic'
                    socketPath
                    chainSyncApp

            case result of
                Left err -> throwIO err
                Right () ->
                    error
                        "applicationN2C: chain sync\
                        \ exited unexpectedly"
