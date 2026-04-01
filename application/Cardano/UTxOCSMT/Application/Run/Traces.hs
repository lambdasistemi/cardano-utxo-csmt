module Cardano.UTxOCSMT.Application.Run.Traces
    ( MainTraces (..)
    , NodeValidationTrace (..)
    , renderMainTraces
    , renderThrottledMainTraces
    , stealMetricsEvent
    , matchHighFrequencyEvents
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Traces
-- Description : Trace types and rendering for the main application
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module defines the trace types used for logging throughout the main
-- application lifecycle, including startup, database operations, and HTTP
-- services.

import CSMT.MTS (ReplayEvent (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonTrace
    , renderArmageddonTrace
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( Metrics (..)
    , MetricsEvent (..)
    , SyncPhase (..)
    , renderBlockPoint
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( ApplicationTrace (..)
    , renderApplicationTrace
    )
import Cardano.UTxOCSMT.Ouroboros.Connection
    ( NodeConnectionError (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Tracer.Throttle (Throttled (..))
import Data.Tracer.Timestamp (Timestamped (..))
import Data.Word (Word16)

-- | Trace events for node connection validation.
data NodeValidationTrace
    = -- | Starting validation of node connection
      ValidatingNodeConnection String Word16
    | -- | Node connection validation succeeded
      NodeValidationSuccess
    | -- | Node connection validation failed
      NodeValidationFailed NodeConnectionError
    deriving stock (Show)

{- | Main application trace types for logging various events during
the application lifecycle.
-}
data MainTraces
    = -- | Application is starting up
      Boot
    | -- | Database already contains data at the given checkpoint
      NotEmpty Point
    | -- | New database setup event (armageddon initialization)
      New ArmageddonTrace
    | -- | Chain sync application event
      Application ApplicationTrace
    | -- | API server is starting
      ServeApi
    | -- | API documentation server is starting
      ServeDocs
    | -- | HTTP service encountered an error
      HTTPServiceError String
    | -- | Application is connecting to the node
      ApplicationStarting
    | -- | Node connection validation event
      NodeValidation NodeValidationTrace
    | -- | Genesis bootstrap: inserted N UTxOs from genesis file
      GenesisBootstrap Int
    | -- | Periodic metrics snapshot from fold accumulator
      MetricsReport Metrics
    | -- | Journal replay chunk event
      JournalReplay ReplayEvent
    | -- | Database opened: crash recovery needed
      DbNeedsRecovery
    | -- | Database opened: ready in KVOnly mode
      DbReadyKVOnly
    | -- | Database opened: ready in Full mode
      DbReadyFull
    | -- | Crash recovery completed
      DbRecoveryDone
    deriving (Show)

-- | Render a 'MainTraces' value to a human-readable log string.
renderMainTraces :: MainTraces -> String
renderMainTraces Boot = "Starting up Cardano UTxO CSMT client..."
renderMainTraces (NotEmpty point) =
    "Database is not empty, skipping initial setup. "
        ++ "Current base checkpoint at point: "
        ++ show point
renderMainTraces (New a) =
    "Database is empty, performing initial setup."
        ++ renderArmageddonTrace a
renderMainTraces (Application at) =
    "Application event: " ++ renderApplicationTrace at
renderMainTraces ServeApi =
    "Starting API server..."
renderMainTraces ServeDocs =
    "Starting API documentation server..."
renderMainTraces (HTTPServiceError err) =
    "ERROR: HTTP service failed to start: " ++ err
renderMainTraces ApplicationStarting =
    "Starting Ouroboros node connection and chain sync application..."
renderMainTraces (NodeValidation nvt) =
    "Node validation: " ++ renderNodeValidationTrace nvt
renderMainTraces (GenesisBootstrap n) =
    "Genesis bootstrap: inserted "
        ++ show n
        ++ " UTxOs from genesis file"
renderMainTraces (JournalReplay (ReplayStart cs bs tb ops)) =
    "Journal replay: chunk="
        ++ show cs
        ++ " buckets="
        ++ show bs
        ++ "/"
        ++ show tb
        ++ " ops/bucket="
        ++ show ops
renderMainTraces (JournalReplay ReplayStop) =
    "Journal replay: chunk done"
renderMainTraces DbNeedsRecovery =
    "Database opened: crash recovery needed (sentinel found)"
renderMainTraces DbReadyKVOnly =
    "Database opened: ready in KVOnly mode (journal has entries)"
renderMainTraces DbReadyFull =
    "Database opened: ready in Full mode (journal empty)"
renderMainTraces DbRecoveryDone =
    "Crash recovery completed"
renderMainTraces (MetricsReport m) =
    "blocks="
        ++ show (cumulativeBlocks m)
        ++ " utxos="
        ++ show (utxoChangesCount m)
        ++ " blk/s="
        ++ show (round (blockSpeed m) :: Int)
        ++ " utxo/s="
        ++ show (round (utxoSpeed m) :: Int)
        ++ " tip="
        ++ maybe "N/A" renderBlockPoint (lastBlockPoint m)
        ++ " phase="
        ++ maybe "N/A" show (syncPhase m)

-- | Render a 'NodeValidationTrace' value to a human-readable log string.
renderNodeValidationTrace :: NodeValidationTrace -> String
renderNodeValidationTrace (ValidatingNodeConnection host port) =
    "Validating connection to node at "
        ++ host
        ++ ":"
        ++ show port
        ++ "..."
renderNodeValidationTrace NodeValidationSuccess =
    "Node connection validated successfully"
renderNodeValidationTrace (NodeValidationFailed err) =
    "Node connection validation failed: " ++ renderNodeConnectionError err

-- | Render a 'NodeConnectionError' to a human-readable string.
renderNodeConnectionError :: NodeConnectionError -> String
renderNodeConnectionError (NodeResolutionFailed msg) =
    "Failed to resolve hostname: " ++ msg
renderNodeConnectionError (NodeConnectionFailed msg) =
    "Failed to connect: " ++ msg
renderNodeConnectionError NodeConnectionTimeout =
    "Connection timed out"

{- | Extract metrics events from main traces for interception.

This function is used with the trace interceptor to forward relevant
events to the metrics system without modifying the trace pipeline.
Returns a list of metrics events to emit (empty = no interception).
-}
stealMetricsEvent
    :: MainTraces
    -- ^ The trace to inspect
    -> Maybe MetricsEvent
    -- ^ Corresponding metrics event
stealMetricsEvent (NotEmpty point) =
    Just $ BaseCheckpointEvent point
stealMetricsEvent (Application (ApplicationRollingBack _)) =
    Just $ SyncPhaseEvent Following
stealMetricsEvent _ = Nothing

{- | Match high-frequency events for throttling.

Returns frequency in Hz (events per second) for events that should be
throttled. Download progress, extraction events, and database updates
are throttled to 1 Hz to avoid flooding logs during sync.
-}
matchHighFrequencyEvents :: MainTraces -> Maybe Double
matchHighFrequencyEvents = \case
    MetricsReport{} -> Just 1.0
    JournalReplay{} -> Just 1.0
    _ -> Nothing

{- | Render a throttled 'MainTraces' value to a log string.

Includes timestamp and drop count information when events were throttled.
-}
renderThrottledMainTraces :: Throttled UTCTime MainTraces -> String
renderThrottledMainTraces Throttled{throttledEvent, throttledDropped} =
    let Timestamped{timestampedTime, timestampedEvent} = throttledEvent
        baseMsg = renderMainTraces timestampedEvent
        timestampStr =
            "["
                ++ formatTime
                    defaultTimeLocale
                    "%Y-%m-%d %H:%M:%S%Q"
                    timestampedTime
                ++ "] "
        droppedSuffix
            | throttledDropped > 0 =
                " (+" ++ show throttledDropped ++ " messages dropped)"
            | otherwise = ""
    in  if null baseMsg
            then ""
            else timestampStr ++ baseMsg ++ droppedSuffix
