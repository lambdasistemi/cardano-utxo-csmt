{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.Metrics.Types
Description : Data types for metrics collection

This module defines the core data types used for metrics collection:

* 'BootstrapPhase' - phases during application startup
* 'MetricsEvent' - events that update metrics state
* 'ExtractionProgress' - progress of UTxO extraction
* 'HeaderSyncProgress' - progress of header synchronization
* 'Metrics' - the aggregated metrics state
* 'MetricsParams' - configuration for metrics collection
-}
module Cardano.UTxOCSMT.Application.Metrics.Types
    ( -- * Bootstrap phases
      BootstrapPhase (..)

      -- * Metrics events
    , MetricsEvent (..)
    , _BlockFetchEvent
    , _UTxOChangeEvent
    , _BlockInfoEvent
    , _MerkleRootEvent
    , _BaseCheckpointEvent
    , _ChainTipEvent
    , _BootstrapPhaseEvent
    , _ExtractionTotalEvent
    , _ExtractionProgressEvent
    , _HeaderSyncProgressEvent
    , _DownloadProgressEvent
    , _CountingProgressEvent
    , _CSMTDurationEvent
    , _RollbackDurationEvent
    , _FinalityDurationEvent
    , _BlockDecodeDurationEvent
    , _TransactionDurationEvent
    , _TotalBlockDurationEvent
    , _InternalQueryTipEvent
    , _InternalCsmtOpsEvent
    , _InternalRollbackStoreEvent

      -- * Progress types
    , ExtractionProgress (..)
    , HeaderSyncProgress (..)

      -- * Metrics state
    , Metrics (..)
    , MetricsParams (..)

      -- * Render utilities
    , renderBlockPoint
    , renderPoint
    , renderPrometheus
    )
where

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header, Point)
import Control.Lens
    ( (&)
    , (.~)
    , (?~)
    )
import Control.Lens.TH (makePrisms)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Proxy (Proxy (..))
import Data.Swagger
    ( ToSchema (..)
    , declareSchemaRef
    , description
    , properties
    , required
    )
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.IsList (IsList (..))
import Ouroboros.Network.Block (SlotNo (..), blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))

-- | Bootstrap phase during startup
data BootstrapPhase
    = -- | Downloading Mithril snapshot
      Downloading
    | -- | Counting UTxOs in snapshot
      Counting
    | -- | Extracting UTxOs from Mithril snapshot
      Extracting
    | -- | Syncing headers after Mithril import
      SyncingHeaders
    | -- | Rolling back due to chain reorganization
      RollingBack
    | -- | Fully synced with chain tip
      Synced
    deriving (Show, Eq)

instance ToJSON BootstrapPhase where
    toJSON = \case
        Downloading -> "downloading"
        Counting -> "counting"
        Extracting -> "extracting"
        SyncingHeaders -> "syncing_headers"
        RollingBack -> "rolling_back"
        Synced -> "synced"

instance ToSchema BootstrapPhase where
    declareNamedSchema _ =
        return
            $ Swagger.NamedSchema (Just "BootstrapPhase")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerString
            & Swagger.enum_
                ?~ [ "downloading"
                   , "counting"
                   , "extracting"
                   , "syncing_headers"
                   , "rolling_back"
                   , "synced"
                   ]
            & description
                ?~ "Current bootstrap phase: downloading, counting, \
                   \extracting, syncing_headers, rolling_back, or synced"

-- | The signal we receive to update the metrics
data MetricsEvent
    = -- | some blocks are going to be fetched
      BlockFetchEvent EventQueueLength
    | -- | one utxo change has been processed
      UTxOChangeEvent
    | -- | one block has been processed
      BlockInfoEvent Header
    | -- | the current merkle root
      MerkleRootEvent Hash
    | -- | the base checkpoint for the UTxO set
      BaseCheckpointEvent Point
    | -- | the current chain tip slot from ChainSync
      ChainTipEvent SlotNo
    | -- | current bootstrap phase
      BootstrapPhaseEvent BootstrapPhase
    | -- | extraction total (from decoded ledger state)
      ExtractionTotalEvent Word64
    | -- | extraction progress (UTxOs extracted so far)
      ExtractionProgressEvent Word64
    | -- | header sync progress (current slot, target slot)
      HeaderSyncProgressEvent SlotNo SlotNo
    | -- | download progress (bytes downloaded so far)
      DownloadProgressEvent Word64
    | -- | counting progress (UTxOs counted so far)
      CountingProgressEvent Word64
    | -- | CSMT operation duration (seconds)
      CSMTDurationEvent Double
    | -- | Rollback point storage duration (seconds)
      RollbackDurationEvent Double
    | -- | Finality pruning duration (seconds)
      FinalityDurationEvent Double
    | -- | Block decode duration (seconds)
      BlockDecodeDurationEvent Double
    | -- | Transaction duration (seconds)
      TransactionDurationEvent Double
    | -- | Total block processing duration (seconds)
      TotalBlockDurationEvent Double
    | -- | Internal queryTip duration (microseconds)
      InternalQueryTipEvent Double
    | -- | Internal CSMT ops duration (microseconds)
      InternalCsmtOpsEvent Double
    | -- | Internal rollback store duration (microseconds)
      InternalRollbackStoreEvent Double
    deriving (Show)

makePrisms ''MetricsEvent

-- | Progress of UTxO extraction from Mithril snapshot
data ExtractionProgress = ExtractionProgress
    { extractionCurrent :: Word64
    -- ^ Number of UTxOs extracted so far
    , extractionTotal :: Maybe Word64
    -- ^ Total number of UTxOs to extract (if known)
    , extractionPercent :: Maybe Double
    -- ^ Percentage complete (if total is known)
    , extractionRate :: Double
    -- ^ Extraction rate (UTxOs per second)
    , extractionEta :: Maybe Double
    -- ^ Estimated seconds remaining (if total and rate are known)
    }
    deriving (Show, Eq)

instance ToJSON ExtractionProgress where
    toJSON
        ExtractionProgress
            { extractionCurrent
            , extractionTotal
            , extractionPercent
            , extractionRate
            , extractionEta
            } =
            object
                [ "current" .= extractionCurrent
                , "total" .= extractionTotal
                , "percent" .= extractionPercent
                , "rate" .= extractionRate
                , "eta" .= extractionEta
                ]

instance ToSchema ExtractionProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeDoubleSchema <- declareSchemaRef (Proxy @(Maybe Double))
        doubleSchema <- declareSchemaRef (Proxy @Double)
        return
            $ Swagger.NamedSchema (Just "ExtractionProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("current", word64Schema)
                    , ("total", maybeWord64Schema)
                    , ("percent", maybeDoubleSchema)
                    , ("rate", doubleSchema)
                    , ("eta", maybeDoubleSchema)
                    ]
            & required .~ ["current", "rate"]
            & description
                ?~ "Progress of UTxO extraction from Mithril snapshot"

-- | Progress of header synchronization after Mithril import
data HeaderSyncProgress = HeaderSyncProgress
    { headerCurrentSlot :: SlotNo
    -- ^ Current slot being processed
    , headerTargetSlot :: SlotNo
    -- ^ Target slot to reach
    }
    deriving (Show, Eq)

instance ToJSON HeaderSyncProgress where
    toJSON HeaderSyncProgress{headerCurrentSlot, headerTargetSlot} =
        object
            [ "currentSlot" .= unSlotNo headerCurrentSlot
            , "targetSlot" .= unSlotNo headerTargetSlot
            ]

instance ToSchema HeaderSyncProgress where
    declareNamedSchema _ = do
        word64Schema <- declareSchemaRef (Proxy @Word64)
        return
            $ Swagger.NamedSchema (Just "HeaderSyncProgress")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("currentSlot", word64Schema)
                    , ("targetSlot", word64Schema)
                    ]
            & required .~ ["currentSlot", "targetSlot"]
            & description
                ?~ "Progress of header synchronization after Mithril import"

-- | Tracked metrics
data Metrics = Metrics
    { averageQueueLength :: Double
    , maxQueueLength :: Maybe Int
    , utxoChangesCount :: Int
    , lastBlockPoint :: Maybe (UTCTime, Header)
    , utxoSpeed :: Double
    , blockSpeed :: Double
    , currentEra :: Maybe String
    , currentMerkleRoot :: Maybe Hash
    , baseCheckpoint :: Maybe Point
    , chainTipSlot :: Maybe SlotNo
    -- ^ The current chain tip slot from ChainSync protocol
    , bootstrapPhase :: Maybe BootstrapPhase
    -- ^ Current bootstrap phase during startup
    , extractionProgress :: Maybe ExtractionProgress
    -- ^ Progress of UTxO extraction from Mithril snapshot
    , headerSyncProgress :: Maybe HeaderSyncProgress
    -- ^ Progress of header synchronization after Mithril import
    , downloadedBytes :: Maybe Word64
    -- ^ Bytes downloaded during Mithril snapshot download
    , countingProgress :: Maybe Word64
    -- ^ UTxOs counted so far during counting phase
    , avgCSMTDuration :: Double
    -- ^ Average CSMT operation duration in microseconds
    , avgRollbackDuration :: Double
    -- ^ Average rollback point storage duration in microseconds
    , avgFinalityDuration :: Double
    -- ^ Average finality pruning duration in microseconds
    , avgBlockDecodeDuration :: Double
    -- ^ Average block decode duration in microseconds
    , avgTransactionDuration :: Double
    -- ^ Average transaction duration in microseconds
    , avgTotalBlockDuration :: Double
    -- ^ Average total block processing duration in microseconds
    , cumulativeBlocks :: Int
    -- ^ Total number of blocks processed
    , cumulativeBlockDecodeDuration :: Double
    -- ^ Cumulative block decode duration in microseconds
    , cumulativeTransactionDuration :: Double
    -- ^ Cumulative transaction duration in microseconds
    , cumulativeCSMTDuration :: Double
    -- ^ Cumulative CSMT operation duration in microseconds
    , cumulativeRollbackDuration :: Double
    -- ^ Cumulative rollback point storage duration in microseconds
    , cumulativeFinalityDuration :: Double
    -- ^ Cumulative finality pruning duration in microseconds
    , cumulativeTotalBlockDuration :: Double
    -- ^ Cumulative total block processing duration in microseconds
    , cumulativeInternalQueryTip :: Double
    -- ^ Cumulative internal queryTip duration in microseconds
    , cumulativeInternalCsmtOps :: Double
    -- ^ Cumulative internal CSMT ops duration in microseconds
    , cumulativeInternalRollbackStore :: Double
    -- ^ Cumulative internal rollback store duration in microseconds
    }
    deriving (Show)

instance ToJSON Metrics where
    toJSON
        Metrics
            { averageQueueLength
            , maxQueueLength
            , utxoChangesCount
            , lastBlockPoint
            , utxoSpeed
            , blockSpeed
            , currentEra
            , currentMerkleRoot
            , baseCheckpoint
            , chainTipSlot
            , bootstrapPhase
            , extractionProgress
            , headerSyncProgress
            , downloadedBytes
            , countingProgress
            , avgCSMTDuration
            , avgRollbackDuration
            , avgFinalityDuration
            , avgBlockDecodeDuration
            , avgTransactionDuration
            , avgTotalBlockDuration
            , cumulativeBlocks
            , cumulativeBlockDecodeDuration
            , cumulativeTransactionDuration
            , cumulativeCSMTDuration
            , cumulativeRollbackDuration
            , cumulativeFinalityDuration
            , cumulativeTotalBlockDuration
            , cumulativeInternalQueryTip
            , cumulativeInternalCsmtOps
            , cumulativeInternalRollbackStore
            } =
            object
                [ "averageQueueLength" .= averageQueueLength
                , "maxQueueLength" .= maxQueueLength
                , "utxoChangesCount" .= utxoChangesCount
                , "lastBlockPoint"
                    .= fmap (Text.pack . renderBlockPoint) lastBlockPoint
                , "utxoSpeed" .= utxoSpeed
                , "blockSpeed" .= blockSpeed
                , "currentEra" .= currentEra
                , "currentMerkleRoot" .= fmap (Text.pack . show) currentMerkleRoot
                , "baseCheckpoint" .= fmap (Text.pack . renderPoint) baseCheckpoint
                , "chainTipSlot" .= fmap unSlotNo chainTipSlot
                , "bootstrapPhase" .= bootstrapPhase
                , "extractionProgress" .= extractionProgress
                , "headerSyncProgress" .= headerSyncProgress
                , "downloadedBytes" .= downloadedBytes
                , "countingProgress" .= countingProgress
                , "avgCSMTDuration" .= avgCSMTDuration
                , "avgRollbackDuration" .= avgRollbackDuration
                , "avgFinalityDuration" .= avgFinalityDuration
                , "avgBlockDecodeDuration" .= avgBlockDecodeDuration
                , "avgTransactionDuration" .= avgTransactionDuration
                , "avgTotalBlockDuration" .= avgTotalBlockDuration
                , "cumulativeBlocks" .= cumulativeBlocks
                , "cumulativeBlockDecodeDuration"
                    .= cumulativeBlockDecodeDuration
                , "cumulativeTransactionDuration"
                    .= cumulativeTransactionDuration
                , "cumulativeCSMTDuration"
                    .= cumulativeCSMTDuration
                , "cumulativeRollbackDuration"
                    .= cumulativeRollbackDuration
                , "cumulativeFinalityDuration"
                    .= cumulativeFinalityDuration
                , "cumulativeTotalBlockDuration"
                    .= cumulativeTotalBlockDuration
                , "cumulativeInternalQueryTip"
                    .= cumulativeInternalQueryTip
                , "cumulativeInternalCsmtOps"
                    .= cumulativeInternalCsmtOps
                , "cumulativeInternalRollbackStore"
                    .= cumulativeInternalRollbackStore
                ]

-- | Render a block point as a string for display
renderBlockPoint :: (a, Header) -> [Char]
renderBlockPoint (_, header) = renderPoint $ blockPoint header

-- | Render a Point as a string for display
renderPoint
    :: Point -> [Char]
renderPoint (Network.Point Origin) = "Origin"
renderPoint (Network.Point (At block)) =
    show (blockPointHash block)
        ++ "@"
        ++ show (unSlotNo $ blockPointSlot block)

instance ToSchema Metrics where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy @Double)
        maybeIntSchema <- declareSchemaRef (Proxy @(Maybe Int))
        intSchema <- declareSchemaRef (Proxy @Int)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        maybeBootstrapPhaseSchema <-
            declareSchemaRef (Proxy @(Maybe BootstrapPhase))
        maybeExtractionProgressSchema <-
            declareSchemaRef (Proxy @(Maybe ExtractionProgress))
        maybeHeaderSyncProgressSchema <-
            declareSchemaRef (Proxy @(Maybe HeaderSyncProgress))
        return
            $ Swagger.NamedSchema (Just "Metrics")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("averageQueueLength", doubleSchema)
                    , ("maxQueueLength", maybeIntSchema)
                    , ("utxoChangesCount", intSchema)
                    , ("lastBlockPoint", maybeStringSchema)
                    , ("utxoSpeed", doubleSchema)
                    , ("blockSpeed", doubleSchema)
                    , ("currentEra", maybeStringSchema)
                    , ("currentMerkleRoot", maybeStringSchema)
                    , ("baseCheckpoint", maybeStringSchema)
                    , ("chainTipSlot", maybeWord64Schema)
                    , ("bootstrapPhase", maybeBootstrapPhaseSchema)
                    , ("extractionProgress", maybeExtractionProgressSchema)
                    , ("headerSyncProgress", maybeHeaderSyncProgressSchema)
                    , ("downloadedBytes", maybeWord64Schema)
                    , ("countingProgress", maybeWord64Schema)
                    , ("avgCSMTDuration", doubleSchema)
                    , ("avgRollbackDuration", doubleSchema)
                    , ("avgFinalityDuration", doubleSchema)
                    , ("avgBlockDecodeDuration", doubleSchema)
                    , ("avgTransactionDuration", doubleSchema)
                    , ("avgTotalBlockDuration", doubleSchema)
                    , ("cumulativeBlocks", intSchema)
                    ,
                        ( "cumulativeBlockDecodeDuration"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeTransactionDuration"
                        , doubleSchema
                        )
                    , ("cumulativeCSMTDuration", doubleSchema)
                    ,
                        ( "cumulativeRollbackDuration"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeFinalityDuration"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeTotalBlockDuration"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeInternalQueryTip"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeInternalCsmtOps"
                        , doubleSchema
                        )
                    ,
                        ( "cumulativeInternalRollbackStore"
                        , doubleSchema
                        )
                    ]
            & required
                .~ [ "averageQueueLength"
                   , "maxQueueLength"
                   , "utxoChangesCount"
                   , "lastBlockPoint"
                   , "utxoSpeed"
                   , "blockSpeed"
                   , "currentEra"
                   , "currentMerkleRoot"
                   , "baseCheckpoint"
                   , "chainTipSlot"
                   , "bootstrapPhase"
                   , "extractionProgress"
                   , "headerSyncProgress"
                   , "downloadedBytes"
                   , "avgCSMTDuration"
                   , "avgRollbackDuration"
                   , "avgFinalityDuration"
                   , "avgBlockDecodeDuration"
                   , "avgTransactionDuration"
                   , "avgTotalBlockDuration"
                   , "countingProgress"
                   , "cumulativeBlocks"
                   , "cumulativeBlockDecodeDuration"
                   , "cumulativeTransactionDuration"
                   , "cumulativeCSMTDuration"
                   , "cumulativeRollbackDuration"
                   , "cumulativeFinalityDuration"
                   , "cumulativeTotalBlockDuration"
                   , "cumulativeInternalQueryTip"
                   , "cumulativeInternalCsmtOps"
                   , "cumulativeInternalRollbackStore"
                   ]
            & description
                ?~ "Metrics about CSMT operations and blockchain synchronization"

-- | Metrics configuration parameters
data MetricsParams = MetricsParams
    { qlWindow :: Int
    -- ^ how many samples to consider for average queue length
    , utxoSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , blockSpeedWindow :: Int
    -- ^ how many samples to consider for speed calculation
    , metricsOutput :: Metrics -> IO ()
    -- ^ function to output the metrics
    , metricsFrequency :: Int
    -- ^ frequency in microseconds to output the metrics
    }

-- | Render metrics in Prometheus exposition text format
renderPrometheus :: Metrics -> Text
renderPrometheus m =
    Text.unlines
        $ concat
            [ gauge
                "queue_length_avg"
                "Average block fetch queue length"
                (averageQueueLength m)
            , maybe
                []
                ( gauge
                    "queue_length_max"
                    "Maximum block fetch queue length"
                    . fromIntegral @Int @Double
                )
                (maxQueueLength m)
            , counter
                "utxo_changes_total"
                "Total UTxO changes processed"
                (fromIntegral @Int @Double $ utxoChangesCount m)
            , gauge
                "utxo_speed"
                "UTxO changes per second"
                (utxoSpeed m)
            , gauge
                "block_speed"
                "Blocks processed per second"
                (blockSpeed m)
            , maybe
                []
                ( gauge
                    "chain_tip_slot"
                    "Chain tip slot from node"
                    . fromIntegral @Word64 @Double
                    . unSlotNo
                )
                (chainTipSlot m)
            , lastBlockSlotLines m
            , readyLine m
            , counter
                "blocks_total"
                "Total blocks processed"
                (fromIntegral @Int @Double $ cumulativeBlocks m)
            , gauge
                "avg_csmt_duration_us"
                "Average CSMT operation duration in microseconds"
                (avgCSMTDuration m)
            , gauge
                "avg_rollback_duration_us"
                "Average rollback duration in microseconds"
                (avgRollbackDuration m)
            , gauge
                "avg_finality_duration_us"
                "Average finality pruning duration in microseconds"
                (avgFinalityDuration m)
            , gauge
                "avg_block_decode_duration_us"
                "Average block decode duration in microseconds"
                (avgBlockDecodeDuration m)
            , gauge
                "avg_transaction_duration_us"
                "Average transaction duration in microseconds"
                (avgTransactionDuration m)
            , gauge
                "avg_total_block_duration_us"
                "Average total block processing duration in microseconds"
                (avgTotalBlockDuration m)
            , counter
                "cumulative_block_decode_duration_us"
                "Cumulative block decode duration in microseconds"
                (cumulativeBlockDecodeDuration m)
            , counter
                "cumulative_transaction_duration_us"
                "Cumulative transaction duration in microseconds"
                (cumulativeTransactionDuration m)
            , counter
                "cumulative_csmt_duration_us"
                "Cumulative CSMT operation duration in microseconds"
                (cumulativeCSMTDuration m)
            , counter
                "cumulative_rollback_duration_us"
                "Cumulative rollback duration in microseconds"
                (cumulativeRollbackDuration m)
            , counter
                "cumulative_finality_duration_us"
                "Cumulative finality pruning duration in microseconds"
                (cumulativeFinalityDuration m)
            , counter
                "cumulative_total_block_duration_us"
                "Cumulative total block processing duration in microseconds"
                (cumulativeTotalBlockDuration m)
            , counter
                "cumulative_query_tip_us"
                "Cumulative internal queryTip duration in microseconds"
                (cumulativeInternalQueryTip m)
            , counter
                "cumulative_csmt_ops_us"
                "Cumulative internal CSMT ops duration in microseconds"
                (cumulativeInternalCsmtOps m)
            , counter
                "cumulative_rollback_store_us"
                "Cumulative internal rollback store duration in microseconds"
                (cumulativeInternalRollbackStore m)
            ]
  where
    prefix :: Text
    prefix = "cardano_utxo_csmt"

    metric
        :: Text -> Text -> Text -> Double -> [Text]
    metric typ name help val =
        [ "# HELP " <> prefix <> "_" <> name <> " " <> help
        , "# TYPE " <> prefix <> "_" <> name <> " " <> typ
        , prefix <> "_" <> name <> " " <> showDouble val
        ]

    gauge :: Text -> Text -> Double -> [Text]
    gauge = metric "gauge"

    counter :: Text -> Text -> Double -> [Text]
    counter = metric "counter"

    showDouble :: Double -> Text
    showDouble v =
        let s = Text.pack $ show v
        in  -- Prometheus prefers "0" not "0.0" for integers
            if Text.isSuffixOf ".0" s
                then Text.dropEnd 2 s
                else s

    lastBlockSlotLines :: Metrics -> [Text]
    lastBlockSlotLines metrics = case lastBlockPoint metrics of
        Just (_, header) ->
            case blockPoint header of
                Network.Point (At block) ->
                    gauge
                        "processed_slot"
                        "Last processed slot number"
                        ( fromIntegral @Word64 @Double
                            $ unSlotNo
                            $ blockPointSlot block
                        )
                _ -> []
        Nothing -> []

    readyLine :: Metrics -> [Text]
    readyLine metrics =
        gauge
            "ready"
            "Service readiness (1=ready, 0=not ready)"
            ( case bootstrapPhase metrics of
                Just Synced -> 1
                _ -> 0
            )
