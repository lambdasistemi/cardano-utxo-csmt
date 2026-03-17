{- |
Module      : Cardano.UTxOCSMT.Application.Metrics
Description : Real-time metrics collection for chain synchronization

This module provides a metrics fold that accumulates trace events into
a 'Metrics' snapshot. The fold is designed for use with 'foldTracer'
from contra-tracer-contrib.

@
pipeline <- foldTracer metricsFold downstream
@
-}
module Cardano.UTxOCSMT.Application.Metrics
    ( metricsFold
    , MetricsEvent (..)
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
import Data.Profunctor (Profunctor (..))
import Data.SOP.Strict (index_NS)
import Data.Time (UTCTime)
import Data.Tracer.Timestamp (Timestamped (..))
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
getCurrentMerkleRoot =
    handles (timestampedEventL . _MerkleRootEvent) Fold.last

getBaseCheckpoint :: Fold TimestampedMetrics (Maybe Point)
getBaseCheckpoint =
    handles (timestampedEventL . _BaseCheckpointEvent) Fold.last

-- track the chain tip slot
chainTipSlotFold :: Fold TimestampedMetrics (Maybe SlotNo)
chainTipSlotFold =
    handles (timestampedEventL . _ChainTipEvent) Fold.last

-- track sync phase
syncPhaseFold :: Fold TimestampedMetrics (Maybe SyncPhase)
syncPhaseFold =
    handles (timestampedEventL . _SyncPhaseEvent) Fold.last

-- | Average duration in microseconds over a window.
avgDurationFold
    :: Int
    -> APrism' MetricsEvent Double
    -> Fold TimestampedMetrics Double
avgDurationFold window prism =
    handles
        (timestampedEventL . clonePrism prism . to secsToMicros)
        $ averageOverWindow window
  where
    secsToMicros :: Double -> Double
    secsToMicros s = s * 1e6

-- | Cumulative sum of durations in microseconds.
cumulativeDurationFold
    :: APrism' MetricsEvent Double
    -> Fold TimestampedMetrics Double
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

{- | Metrics fold configuration.

Controls the sliding window sizes for various metrics.
-}
data MetricsFoldParams = MetricsFoldParams
    { qlWindow :: Int
    , utxoSpeedWindow :: Int
    , blockSpeedWindow :: Int
    }

-- | Default metrics fold parameters.
defaultMetricsFoldParams :: MetricsFoldParams
defaultMetricsFoldParams =
    MetricsFoldParams
        { qlWindow = 100
        , utxoSpeedWindow = 1000
        , blockSpeedWindow = 100
        }

-- | Accumulate timestamped metrics events into a 'Metrics' snapshot.
metricsFold :: Fold TimestampedMetrics Metrics
metricsFold = metricsFoldWith defaultMetricsFoldParams

-- | Accumulate with custom window parameters.
metricsFoldWith
    :: MetricsFoldParams -> Fold TimestampedMetrics Metrics
metricsFoldWith MetricsFoldParams{qlWindow, utxoSpeedWindow, blockSpeedWindow} =
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
