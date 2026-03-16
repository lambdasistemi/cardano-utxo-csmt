{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Cardano.UTxOCSMT.Application.Metrics
Description : Real-time metrics collection for chain synchronization

This module provides a metrics collection system that tracks:

* Block fetch queue statistics
* UTxO change processing speed
* Block processing speed
* Current blockchain era
* Current Merkle root
* Chain tip slot (for sync status detection)

Metrics are collected via a 'Tracer' and can be output at configurable intervals.
-}
module Cardano.UTxOCSMT.Application.Metrics
    ( metricsTracer
    , MetricsEvent (..)
    , MetricsParams (..)
    , Metrics (..)
    , SyncPhase (..)
    , renderBlockPoint
    , renderPoint
    , renderPrometheus
    )
where

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.Metrics.Types
    ( Metrics (..)
    , MetricsEvent (..)
    , MetricsParams (..)
    , SyncPhase (..)
    , renderBlockPoint
    , renderPoint
    , renderPrometheus
    , _BaseCheckpointEvent
    , _BlockDecodeDurationEvent
    , _BlockFetchEvent
    , _BlockInfoEvent
    , _CSMTDurationEvent
    , _ChainTipEvent
    , _FinalityDurationEvent
    , _InternalCsmtOpsEvent
    , _InternalQueryTipEvent
    , _InternalRollbackStoreEvent
    , _MerkleRootEvent
    , _RollbackDurationEvent
    , _SyncPhaseEvent
    , _TotalBlockDurationEvent
    , _TransactionDurationEvent
    , _UTxOChangeEvent
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header, Point)
import Control.Comonad (Comonad (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , flushTQueue
    , newTQueueIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTQueue
    , writeTVar
    )
import Control.Foldl (Fold (..), handles)
import Control.Foldl qualified as Fold
import Control.Foldl.Extra (averageOverWindow, speedoMeter)
import Control.Lens
    ( APrism'
    , Lens'
    , aside
    , clonePrism
    , lens
    , to
    , _Wrapped
    )
import Control.Monad (forever, (<=<))
import Control.Tracer (Tracer (..))
import Data.Profunctor (Profunctor (..))
import Data.SOP.Strict (index_NS)
import Data.Time (UTCTime)
import Data.Tracer.Timestamp (Timestamped (..), utcTimestampTracer)
import Ouroboros.Consensus.HardFork.Combinator (OneEraHeader (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as HF
import Ouroboros.Network.Block (SlotNo (..))

-- | Lens for accessing the event in a 'Timestamped' wrapper
timestampedEventL :: Lens' (Timestamped t a) a
timestampedEventL = lens timestampedEvent (\t e -> t{timestampedEvent = e})

-- | Support to use event prisms with 'aside'
timedAsTuple :: Timestamped UTCTime a -> (UTCTime, a)
timedAsTuple (Timestamped t e) = (t, e)

---------- Metrics specific folds ----------

type TimestampedMetrics = Timestamped UTCTime MetricsEvent

-- track the last block point seen
lastBlockPointFold
    :: Fold TimestampedMetrics (Maybe (UTCTime, Header))
lastBlockPointFold =
    lmap timedAsTuple
        $ handles (aside _BlockInfoEvent) Fold.last

-- track average block fetch queue length
averageBlockFetchLength :: Int -> Fold TimestampedMetrics Double
averageBlockFetchLength window =
    handles
        (timestampedEventL . _BlockFetchEvent . _Wrapped . to fromIntegral)
        $ averageOverWindow window

-- track max block fetch queue length
maxBlockFetchQueueLength :: Fold TimestampedMetrics (Maybe Int)
maxBlockFetchQueueLength =
    handles
        (timestampedEventL . _BlockFetchEvent . _Wrapped)
        Fold.maximum

-- track speed of some event type
speedOfSomeEvent
    :: Int -> APrism' a b -> Fold (Timestamped UTCTime a) Double
speedOfSomeEvent window prism =
    lmap timedAsTuple
        $ handles (aside prism)
        $ lmap fst
        $ speedoMeter window

-- speed of utxo changes processed
utxoSpeedFold :: Int -> Fold TimestampedMetrics Double
utxoSpeedFold window = speedOfSomeEvent window _UTxOChangeEvent

-- speed of blocks processed
blockSpeedFold :: Int -> Fold TimestampedMetrics Double
blockSpeedFold window = speedOfSomeEvent window _BlockInfoEvent

-- total number of utxo changes processed
totalUtxoChangesFold :: Fold TimestampedMetrics Int
totalUtxoChangesFold =
    handles
        (timestampedEventL . _UTxOChangeEvent)
        Fold.genericLength

currentEraFold :: Fold TimestampedMetrics (Maybe String)
currentEraFold =
    handles (timestampedEventL . _BlockInfoEvent) $ lmap getEra Fold.last
  where
    getEra :: Header -> String
    getEra (HF.HardForkHeader (OneEraHeader x)) = case index_NS x of
        0 -> "byron"
        1 -> "shelley"
        2 -> "allegra"
        3 -> "mary"
        4 -> "alonzo"
        5 -> "babbage"
        6 -> "conway"
        _ -> "unknown"

getCurrentMerkleRoot :: Fold TimestampedMetrics (Maybe Hash)
getCurrentMerkleRoot = handles (timestampedEventL . _MerkleRootEvent) Fold.last

getBaseCheckpoint :: Fold TimestampedMetrics (Maybe Point)
getBaseCheckpoint = handles (timestampedEventL . _BaseCheckpointEvent) Fold.last

-- track the chain tip slot
chainTipSlotFold :: Fold TimestampedMetrics (Maybe SlotNo)
chainTipSlotFold = handles (timestampedEventL . _ChainTipEvent) Fold.last

-- track sync phase
syncPhaseFold :: Fold TimestampedMetrics (Maybe SyncPhase)
syncPhaseFold = handles (timestampedEventL . _SyncPhaseEvent) Fold.last

-- | Average duration in microseconds over a window.
avgDurationFold
    :: Int -> APrism' MetricsEvent Double -> Fold TimestampedMetrics Double
avgDurationFold window prism =
    handles
        (timestampedEventL . clonePrism prism . to secsToMicros)
        $ averageOverWindow window
  where
    secsToMicros :: Double -> Double
    secsToMicros s = s * 1e6

-- | Cumulative sum of durations in microseconds.
cumulativeDurationFold
    :: APrism' MetricsEvent Double -> Fold TimestampedMetrics Double
cumulativeDurationFold prism =
    handles
        (timestampedEventL . clonePrism prism . to secsToMicros)
        Fold.sum
  where
    secsToMicros :: Double -> Double
    secsToMicros s = s * 1e6

-- | Cumulative sum of durations already in microseconds.
cumulativeUsFold
    :: APrism' MetricsEvent Double
    -> Fold TimestampedMetrics Double
cumulativeUsFold prism =
    handles
        (timestampedEventL . clonePrism prism)
        Fold.sum

-- | Count events matching a prism.
countEventFold
    :: APrism' MetricsEvent a -> Fold TimestampedMetrics Int
countEventFold prism =
    handles (timestampedEventL . clonePrism prism) Fold.genericLength

-- track the whole set of metrics
metricsFold :: MetricsParams -> Fold TimestampedMetrics Metrics
metricsFold MetricsParams{qlWindow, utxoSpeedWindow, blockSpeedWindow} =
    Metrics
        <$> averageBlockFetchLength qlWindow
        <*> maxBlockFetchQueueLength
        <*> totalUtxoChangesFold
        <*> lastBlockPointFold
        <*> utxoSpeedFold utxoSpeedWindow
        <*> blockSpeedFold blockSpeedWindow
        <*> currentEraFold
        <*> getCurrentMerkleRoot
        <*> getBaseCheckpoint
        <*> chainTipSlotFold
        <*> syncPhaseFold
        <*> avgDurationFold blockSpeedWindow _CSMTDurationEvent
        <*> avgDurationFold blockSpeedWindow _RollbackDurationEvent
        <*> avgDurationFold blockSpeedWindow _FinalityDurationEvent
        <*> avgDurationFold blockSpeedWindow _BlockDecodeDurationEvent
        <*> avgDurationFold blockSpeedWindow _TransactionDurationEvent
        <*> avgDurationFold blockSpeedWindow _TotalBlockDurationEvent
        <*> countEventFold _TotalBlockDurationEvent
        <*> cumulativeDurationFold _BlockDecodeDurationEvent
        <*> cumulativeDurationFold _TransactionDurationEvent
        <*> cumulativeDurationFold _CSMTDurationEvent
        <*> cumulativeDurationFold _RollbackDurationEvent
        <*> cumulativeDurationFold _FinalityDurationEvent
        <*> cumulativeDurationFold _TotalBlockDurationEvent
        <*> cumulativeUsFold _InternalQueryTipEvent
        <*> cumulativeUsFold _InternalCsmtOpsEvent
        <*> cumulativeUsFold _InternalRollbackStoreEvent

-- | Create a metrics tracer that collects metrics and outputs them
metricsTracer :: MetricsParams -> IO (Tracer IO MetricsEvent)
metricsTracer params@MetricsParams{metricsFrequency, metricsOutput} = do
    eventsQ <- newTQueueIO -- unbounded or we risk to slow down the application
    -- shared state for metrics accumulation
    metricsV <- newTVarIO $ metricsFold params
    link <=< async $ forever $ do
        -- let events accumulate, no need to load CPU as they come with timestamps
        threadDelay 100_000
        (es, currentFold) <- atomically $ do
            es <- flushTQueue eventsQ
            currentFold <- readTVar metricsV
            pure (es, currentFold)

        let !newFold = Fold.fold (duplicate currentFold) es
        atomically $ writeTVar metricsV newFold

    -- output loop
    link <=< async $ forever $ do
        threadDelay metricsFrequency
        readTVarIO metricsV >>= metricsOutput . extract
    pure
        $ utcTimestampTracer
        $ Tracer
        $ \msg -> atomically $ writeTQueue eventsQ msg
