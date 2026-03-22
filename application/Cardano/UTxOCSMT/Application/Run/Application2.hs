module Cardano.UTxOCSMT.Application.Run.Application2
    ( application2
    , applicationN2C2
    , ApplicationTrace (..)
    , renderApplicationTrace
    ) where

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
    ( processBlock
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
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (..)
    , SyncPhase (Synced)
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
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Control.Tracer (Tracer (..))
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

origin :: Network.Point block
origin = Network.Point{getPoint = Origin}

-- | The transaction type for the application.
type AppTx =
    Transaction
        IO
        ColumnFamily
        (Columns Point ByteString ByteString ByteString)
        BatchOp

-- | Concrete phase type for the application.
type AppPhase =
    Phase
        IO
        ColumnFamily
        (Columns Point ByteString ByteString ByteString)
        BatchOp
        (Point, [Operation ByteString ByteString])
        [Operation ByteString ByteString]
        (ByteString, Maybe ByteString)

changeToOperation
    :: Change -> Operation ByteString ByteString
changeToOperation (Spend k) = Delete k
changeToOperation (Create k v) = Insert k v

intersector
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        ByteString
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
        (ByteString, Maybe ByteString)
    -> ArmageddonParams ByteString
    -> Int
    -- ^ Security parameter (stability window)
    -> AppPhase
    -> Intersector Point SlotNo Fetched
intersector
    TraceWith{trace, tracer}
    trUTxO
    runner
    backendInit
    armageddonParams
    securityParam
    phase =
        Intersector
            { intersectFound = \point -> do
                trace $ ApplicationIntersectionAt point
                pure
                    $ follower
                        tracer
                        trUTxO
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        phase
            , intersectNotFound = do
                trace ApplicationIntersectionFailed
                pure
                    ( intersector
                        tracer
                        trUTxO
                        runner
                        backendInit
                        armageddonParams
                        securityParam
                        phase
                    , [origin]
                    )
            }

follower
    :: Tracer IO ApplicationTrace
    -> IO ()
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        ByteString
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
        (ByteString, Maybe ByteString)
    -> ArmageddonParams ByteString
    -> Int
    -- ^ Security parameter (stability window)
    -> AppPhase
    -> Follower Point SlotNo Fetched
follower
    TraceWith{trace, tracer}
    trUTxO
    runner@RunTransaction{transact}
    backendInit
    armageddonParams
    securityParam =
        go
      where
        go phase =
            Follower
                { rollForward =
                    \Fetched{fetchedPoint, fetchedBlock} _tipSlot -> do
                        let (txCount, utxoChanges) =
                                uTxOsWithTxCount fetchedBlock
                            ops =
                                changeToOperation <$> utxoChanges
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
                        phase' <-
                            transact
                                $ processBlock
                                    Rollbacks
                                    securityParam
                                    (At fetchedPoint)
                                    (fetchedPoint, ops)
                                    phase
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
                                        (At point)
                            case result of
                                Store.RollbackSucceeded _ ->
                                    pure
                                        $ Progress
                                        $ go
                                            (InFollowing n' f)
                                Store.RollbackImpossible -> do
                                    armageddon
                                        (Tracer $ const $ pure ())
                                        runner
                                        armageddonParams
                                    restoring <-
                                        Backend.startRestoring
                                            backendInit
                                    pure
                                        $ Reset
                                        $ intersector
                                            tracer
                                            trUTxO
                                            runner
                                            backendInit
                                            armageddonParams
                                            securityParam
                                            (InRestoration 0 restoring)
                        InRestoration _ _ ->
                            error
                                "follower: cannot roll back\
                                \ in restoration mode"
                }

application2
    :: EpochSlots
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
        ByteString
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
        (ByteString, Maybe ByteString)
    -> ArmageddonParams ByteString
    -> Int
    -- ^ Security parameter (stability window)
    -> AppPhase
    -> [Point]
    -> IO Void
application2
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
                        runner
                        backendInit
                        armageddonParams
                        securityParam
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
                        "application2: chain following\
                        \ exited unexpectedly"
                Right _ ->
                    error
                        "application2: impossible branch"

applicationN2C2
    :: EpochSlots
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
        ByteString
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
        (ByteString, Maybe ByteString)
    -> ArmageddonParams ByteString
    -> Int
    -- ^ Security parameter (stability window)
    -> AppPhase
    -> [Point]
    -> IO Void
applicationN2C2
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
                        runner
                        backendInit
                        armageddonParams
                        securityParam
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
                        "applicationN2C2: chain sync\
                        \ exited unexpectedly"
