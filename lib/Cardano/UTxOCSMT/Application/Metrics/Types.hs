{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Cardano.UTxOCSMT.Application.Metrics.Types
Description : Data types for metrics collection

This module defines the core data types used for metrics collection:

* 'SyncPhase' - current synchronization phase
* 'MetricsEvent' - events that update metrics state
* 'Metrics' - the aggregated metrics state
* 'MetricsParams' - configuration for metrics collection
-}
module Cardano.UTxOCSMT.Application.Metrics.Types
    ( -- * Sync phases
      SyncPhase (..)
    , SyncThreshold (..)

      -- * Metrics events
    , MetricsEvent (..)
    , _BlockFetchEvent
    , _UTxOChangeEvent
    , _BlockInfoEvent
    , _MerkleRootEvent
    , _BaseCheckpointEvent
    , _ChainTipEvent
    , _SyncPhaseEvent
    , _CSMTDurationEvent
    , _RollbackDurationEvent
    , _FinalityDurationEvent
    , _BlockDecodeDurationEvent
    , _TransactionDurationEvent
    , _TotalBlockDurationEvent
    , _InternalQueryTipEvent
    , _InternalCsmtOpsEvent
    , _InternalRollbackStoreEvent

      -- * Metrics state
    , Metrics (..)
    , MetricsParams (..)

      -- * Render utilities
    , renderBlockPoint
    , renderPoint
    , renderPrometheus
    )
where

import CSMT.Hashes (Hash, renderHash)
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
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
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
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Tracer.Prometheus
    ( renderPrometheusLines
    )
import Data.Tracer.Prometheus qualified as Prom
import Data.Word (Word64)
import GHC.IsList (IsList (..))
import Ouroboros.Network.Block (SlotNo (..), blockPoint)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))

-- | Current synchronization phase
data SyncPhase
    = -- | Bulk ingestion, CSMT not built, no queries
      Restoring
    | -- | CSMT built, processing blocks, may be behind
      Following
    | -- | CSMT built, within threshold of tip
      Synced
    deriving (Show, Eq)

instance ToJSON SyncPhase where
    toJSON = \case
        Restoring -> "restoring"
        Following -> "following"
        Synced -> "synced"

instance ToSchema SyncPhase where
    declareNamedSchema _ =
        return
            $ Swagger.NamedSchema (Just "SyncPhase")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerString
            & Swagger.enum_
                ?~ [ "restoring"
                   , "following"
                   , "synced"
                   ]
            & description
                ?~ "Current synchronization phase: restoring, following, or synced"

-- | Maximum slots behind chain tip to be considered synced.
newtype SyncThreshold = SyncThreshold {unSyncThreshold :: Word64}
    deriving stock (Show, Eq)
    deriving newtype (Num, Ord)

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
    | -- | current sync phase
      SyncPhaseEvent SyncPhase
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
    , syncPhase :: Maybe SyncPhase
    -- ^ Current synchronization phase
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
            , syncPhase
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
                , "currentMerkleRoot"
                    .= fmap
                        ( decodeUtf8
                            . convertToBase Base16
                            . renderHash
                        )
                        currentMerkleRoot
                , "baseCheckpoint"
                    .= fmap (Text.pack . renderPoint) baseCheckpoint
                , "chainTipSlot" .= fmap unSlotNo chainTipSlot
                , "syncPhase" .= syncPhase
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
        maybeSyncPhaseSchema <-
            declareSchemaRef (Proxy @(Maybe SyncPhase))
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
                    , ("syncPhase", maybeSyncPhaseSchema)
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
                   , "syncPhase"
                   , "avgCSMTDuration"
                   , "avgRollbackDuration"
                   , "avgFinalityDuration"
                   , "avgBlockDecodeDuration"
                   , "avgTransactionDuration"
                   , "avgTotalBlockDuration"
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
                ?~ "Metrics about CSMT operations and blockchain \
                   \synchronization"

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
    renderPrometheusLines
        $ [ Prom.gauge
                p
                "queue_length_avg"
                "Average block fetch queue length"
                (averageQueueLength m)
          ]
            <> maybe
                []
                ( \v ->
                    [ Prom.gauge
                        p
                        "queue_length_max"
                        "Maximum block fetch queue length"
                        (fromIntegral @Int @Double v)
                    ]
                )
                (maxQueueLength m)
            <> [ Prom.counter
                    p
                    "utxo_changes_total"
                    "Total UTxO changes processed"
                    (fromIntegral @Int @Double $ utxoChangesCount m)
               , Prom.gauge
                    p
                    "utxo_speed"
                    "UTxO changes per second"
                    (utxoSpeed m)
               , Prom.gauge
                    p
                    "block_speed"
                    "Blocks processed per second"
                    (blockSpeed m)
               ]
            <> maybe
                []
                ( \slot ->
                    [ Prom.gauge
                        p
                        "chain_tip_slot"
                        "Chain tip slot from node"
                        ( fromIntegral @Word64 @Double
                            $ unSlotNo slot
                        )
                    ]
                )
                (chainTipSlot m)
            <> lastBlockSlotLines m
            <> [ readyLine m
               , Prom.counter
                    p
                    "blocks_total"
                    "Total blocks processed"
                    (fromIntegral @Int @Double $ cumulativeBlocks m)
               , Prom.gauge
                    p
                    "avg_csmt_duration_us"
                    "Average CSMT operation duration in microseconds"
                    (avgCSMTDuration m)
               , Prom.gauge
                    p
                    "avg_rollback_duration_us"
                    "Average rollback duration in microseconds"
                    (avgRollbackDuration m)
               , Prom.gauge
                    p
                    "avg_finality_duration_us"
                    "Average finality pruning duration in microseconds"
                    (avgFinalityDuration m)
               , Prom.gauge
                    p
                    "avg_block_decode_duration_us"
                    "Average block decode duration in microseconds"
                    (avgBlockDecodeDuration m)
               , Prom.gauge
                    p
                    "avg_transaction_duration_us"
                    "Average transaction duration in microseconds"
                    (avgTransactionDuration m)
               , Prom.gauge
                    p
                    "avg_total_block_duration_us"
                    "Average total block processing duration in microseconds"
                    (avgTotalBlockDuration m)
               , Prom.counter
                    p
                    "cumulative_block_decode_duration_us"
                    "Cumulative block decode duration in microseconds"
                    (cumulativeBlockDecodeDuration m)
               , Prom.counter
                    p
                    "cumulative_transaction_duration_us"
                    "Cumulative transaction duration in microseconds"
                    (cumulativeTransactionDuration m)
               , Prom.counter
                    p
                    "cumulative_csmt_duration_us"
                    "Cumulative CSMT operation duration in microseconds"
                    (cumulativeCSMTDuration m)
               , Prom.counter
                    p
                    "cumulative_rollback_duration_us"
                    "Cumulative rollback duration in microseconds"
                    (cumulativeRollbackDuration m)
               , Prom.counter
                    p
                    "cumulative_finality_duration_us"
                    "Cumulative finality pruning duration in microseconds"
                    (cumulativeFinalityDuration m)
               , Prom.counter
                    p
                    "cumulative_total_block_duration_us"
                    "Cumulative total block processing duration in microseconds"
                    (cumulativeTotalBlockDuration m)
               , Prom.counter
                    p
                    "cumulative_query_tip_us"
                    "Cumulative internal queryTip duration in microseconds"
                    (cumulativeInternalQueryTip m)
               , Prom.counter
                    p
                    "cumulative_csmt_ops_us"
                    "Cumulative internal CSMT ops duration in microseconds"
                    (cumulativeInternalCsmtOps m)
               , Prom.counter
                    p
                    "cumulative_rollback_store_us"
                    "Cumulative internal rollback store duration in microseconds"
                    (cumulativeInternalRollbackStore m)
               ]
  where
    p :: Text
    p = "cardano_utxo_csmt"

    lastBlockSlotLines :: Metrics -> [[Text]]
    lastBlockSlotLines metrics = case lastBlockPoint metrics of
        Just (_, header) ->
            case blockPoint header of
                Network.Point (At block) ->
                    [ Prom.gauge
                        p
                        "processed_slot"
                        "Last processed slot number"
                        ( fromIntegral @Word64 @Double
                            $ unSlotNo
                            $ blockPointSlot block
                        )
                    ]
                _ -> []
        Nothing -> []

    readyLine :: Metrics -> [Text]
    readyLine metrics =
        Prom.gauge
            p
            "ready"
            "Service readiness (1=ready, 0=not ready)"
            ( case syncPhase metrics of
                Just Synced -> 1
                _ -> 0
            )
