{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.BlockFetch
Description : Block fetching client for Cardano node

This module implements the BlockFetch mini-protocol client that retrieves
full block data from the Cardano node. It works in coordination with
the ChainSync client:

1. ChainSync receives headers and queues them
2. BlockFetch retrieves full blocks for queued headers
3. Fetched blocks are passed to the database update layer

The bounded queue prevents memory exhaustion during fast sync periods.
-}
module Cardano.UTxOCSMT.Application.BlockFetch
    ( mkBlockFetchApplication
    , Fetched (..)
    , EventQueueLength (..)
    , BlockFetchApplication
    , HeaderSkipProgress (..)
    )
where

import Cardano.Node.Client.N2C.ChainSync
    ( Fetched (..)
    )
import Cardano.UTxOCSMT.Application.ChainSync
    ( Follower (..)
    , ProgressOrRewind (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , BlockFetchApplication
    , Header
    , Intersector (..)
    , Point
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTVar
    , flushTBQueue
    , isEmptyTBQueue
    , newTBQueueIO
    , newTVarIO
    , readTVarIO
    , writeTBQueue
    , writeTVar
    )
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Lens (makeWrapped)
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (SlotNo (..), blockPoint, blockSlot)
import Ouroboros.Network.BlockFetch.ClientState (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClient (..)
    , BlockFetchReceiver (..)
    , BlockFetchRequest (..)
    , BlockFetchResponse (..)
    )

-- | Length of the queue of headers to fetch blocks for
newtype EventQueueLength = EventQueueLength Int
    deriving (Show)

makeWrapped ''EventQueueLength

-- | Progress during header skip phase (post-Mithril catch-up)
data HeaderSkipProgress = HeaderSkipProgress
    { skipCurrentSlot :: SlotNo
    -- ^ Current slot being processed
    , skipTargetSlot :: SlotNo
    -- ^ Target slot to reach
    }
    deriving (Show)

{- | Create a block fetch application and promote compute an header intersector
for the chain sync application
-}
mkBlockFetchApplication
    :: EventQueueLength
    -- ^ maximum length of the headers queue
    -> Tracer IO EventQueueLength
    -- ^ metrics tracer
    -> Tracer IO HeaderSkipProgress
    -- ^ tracer for header skip progress during Mithril catch-up
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> IO ()
    -- ^ Action to call when skip phase completes
    -> Maybe SlotNo
    -- ^ Optional skip until slot for Mithril bootstrap
    -> Intersector Point SlotNo Fetched
    -- ^ callback to process each fetched block
    -> IO (BlockFetchApplication, Intersector Point SlotNo Header)
mkBlockFetchApplication
    (EventQueueLength maxQueueLen)
    tr
    skipProgressTracer
    setCheckpoint
    onSkipComplete
    mSkipTargetSlot
    blockIntersector = do
        queue <- newQueue maxQueueLen
        blockFollowerVar <- newEmptyMVar
        -- Track whether we've passed the skip threshold
        skipActiveVar <- newTVarIO (isJust mSkipTargetSlot)
        pure
            ( blockFetchClient tr blockFollowerVar queue
            , mkHeaderIntersector
                blockFollowerVar
                queue
                skipProgressTracer
                setCheckpoint
                onSkipComplete
                mSkipTargetSlot
                skipActiveVar
                blockIntersector
            )

headerFollower
    :: MVar (Follower Point SlotNo Fetched)
    -> Queue (Point, SlotNo)
    -> Tracer IO HeaderSkipProgress
    -- ^ Tracer for skip progress
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> IO ()
    -- ^ Action to call when skip phase completes
    -> Maybe SlotNo
    -- ^ Target slot to skip until (Nothing = no skip)
    -> StrictTVar IO Bool
    -- ^ True while skip is active
    -> Follower Point SlotNo Header
headerFollower
    blockFollowerVar
    queue@Queue{pushQueue, waitEmptyQueue}
    skipProgressTracer
    setCheckpoint
    onSkipComplete
    mSkipTargetSlot
    skipActiveVar = fix $ \go ->
        Follower
            { rollForward = \header tipSlot -> do
                let headerSlot = blockSlot header
                    point = blockPoint header

                -- Check if we should skip block fetching
                skipActive <- readTVarIO skipActiveVar
                case (skipActive, mSkipTargetSlot) of
                    (True, Just skipTargetSlot)
                        | headerSlot >= skipTargetSlot -> do
                            -- Reached target slot: save checkpoint, disable skip
                            atomically $ writeTVar skipActiveVar False
                            setCheckpoint point
                            onSkipComplete
                            -- Now start normal operation
                            pushQueue (point, tipSlot)
                        | otherwise ->
                            -- Still skipping: emit progress event
                            traceWith
                                skipProgressTracer
                                HeaderSkipProgress
                                    { skipCurrentSlot = headerSlot
                                    , skipTargetSlot = skipTargetSlot
                                    }
                    _ ->
                        -- Normal operation: push to queue
                        pushQueue (point, tipSlot)

                pure go
            , rollBackward = \point -> do
                waitEmptyQueue
                -- we are safe as the only source of headers is rollForward above
                -- and it's alternative to rollBackward
                Follower{rollBackward} <- takeMVar blockFollowerVar
                progressOrRewind <- rollBackward point
                case progressOrRewind of
                    Progress blockFollower -> do
                        putMVar blockFollowerVar blockFollower
                        pure $ Progress go
                    Rewind points blockIntersector -> do
                        pure
                            $ Rewind points
                            $ mkHeaderIntersector
                                blockFollowerVar
                                queue
                                skipProgressTracer
                                setCheckpoint
                                onSkipComplete
                                mSkipTargetSlot
                                skipActiveVar
                                blockIntersector
                    Reset blockIntersector -> do
                        pure
                            $ Reset
                            $ mkHeaderIntersector
                                blockFollowerVar
                                queue
                                skipProgressTracer
                                setCheckpoint
                                onSkipComplete
                                mSkipTargetSlot
                                skipActiveVar
                                blockIntersector
            }

mkHeaderIntersector
    :: MVar (Follower Point SlotNo Fetched)
    -> Queue (Point, SlotNo)
    -> Tracer IO HeaderSkipProgress
    -- ^ Tracer for skip progress
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> IO ()
    -- ^ Action to call when skip phase completes
    -> Maybe SlotNo
    -- ^ Target slot to skip until (Nothing = no skip)
    -> StrictTVar IO Bool
    -> Intersector Point SlotNo Fetched
    -> Intersector Point SlotNo Header
mkHeaderIntersector
    blockFollowerVar
    q
    skipProgressTracer
    setCheckpoint
    onSkipComplete
    mSkipTargetSlot
    skipActiveVar =
        fix $ \go blockIntersector ->
            Intersector
                { intersectFound = \point -> do
                    blockFollower <- intersectFound blockIntersector point
                    putMVar blockFollowerVar blockFollower
                    pure
                        $ headerFollower
                            blockFollowerVar
                            q
                            skipProgressTracer
                            setCheckpoint
                            onSkipComplete
                            mSkipTargetSlot
                            skipActiveVar
                , intersectNotFound = do
                    (blockIntersector', points) <- intersectNotFound blockIntersector
                    pure (go blockIntersector', points)
                }

blockFetchReceiver
    :: MVar (Follower Point SlotNo Fetched)
    -> NonEmpty (Point, SlotNo)
    -> IO (BlockFetchReceiver Block IO)
blockFetchReceiver blockFollowerVar pointsWithTips = do
    blockFollower <- takeMVar blockFollowerVar
    pure
        $ ($ NE.toList pointsWithTips)
        $ ($ blockFollower)
        $ fix
        $ \fetchOne bf ps ->
            BlockFetchReceiver
                { handleBlock = \block -> do
                    case ps of
                        [] ->
                            error
                                "mkBlockFetchApplication: \
                                \more blocks fetched than requested"
                        (p, tip) : ps' -> do
                            bf' <-
                                rollForward
                                    bf
                                    Fetched
                                        { fetchedPoint = p
                                        , fetchedBlock = block
                                        , fetchedTip = tip
                                        }
                                    tip
                            pure $ fetchOne bf' ps'
                , handleBatchDone = putMVar blockFollowerVar bf
                }

blockFetchClient
    :: Tracer IO EventQueueLength
    -> MVar (Follower Point SlotNo Fetched)
    -> Queue (Point, SlotNo)
    -> BlockFetchClient Block Point IO ()
blockFetchClient tracer blockFollowerVar Queue{flushQueue} = fix $ \go ->
    BlockFetchClient $ do
        pointsWithTips <- flushQueue
        traceWith tracer $ EventQueueLength $ length pointsWithTips
        let points = fmap fst pointsWithTips
        pure
            $ SendMsgRequestRange
                (mkRange points)
                BlockFetchResponse
                    { handleStartBatch =
                        blockFetchReceiver blockFollowerVar pointsWithTips
                    , handleNoBlocks = error "blockFetchClient: no blocks to fetch"
                    }
                go

mkRange :: NonEmpty a -> ChainRange a
mkRange xs@(x :| _) = ChainRange x (NE.last xs)

retryNull
    :: MonadSTM m
    => [a]
    -> STM m (NonEmpty a)
retryNull [] = retry
retryNull (y : ys) = pure (y :| ys)

-- | A simple bounded queue
data Queue a = Queue
    { pushQueue :: a -> IO ()
    , flushQueue :: IO (NonEmpty a)
    , waitEmptyQueue :: IO ()
    }

newQueue
    :: Int
    -> IO (Queue a)
newQueue size = do
    queue <- newTBQueueIO $ fromIntegral size
    let push = writeTBQueue queue
        flush = flushTBQueue queue >>= retryNull
        waitEmpty = do
            isEmpty <- isEmptyTBQueue queue
            unless isEmpty retry
    pure
        Queue
            { pushQueue = atomically <$> push
            , flushQueue = atomically flush
            , waitEmptyQueue = atomically waitEmpty
            }
