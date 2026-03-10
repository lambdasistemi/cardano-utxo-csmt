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
-- application lifecycle, including startup, database operations, HTTP services,
-- and Mithril bootstrap events.

import CSMT.Hashes (Hash)
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonTrace
    , renderArmageddonTrace
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( UpdateTrace
        ( UpdateCSMTMeasured
        , UpdateFinalityMeasured
        , UpdateRollbackMeasured
        , UpdateTransactMeasured
        )
    , renderUpdateTrace
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( BootstrapPhase (..)
    , MetricsEvent (..)
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( ApplicationTrace (..)
    , renderApplicationTrace
    )
import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilTrace (MithrilDownloadProgress)
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionTrace
        ( ExtractionCounting
        , ExtractionDecodedState
        , ExtractionProgress
        , ExtractionStreamStarting
        )
    )
import Cardano.UTxOCSMT.Mithril.Import
    ( ImportTrace (..)
    , renderImportTrace
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
    | -- | Database update event (block processing)
      Update (UpdateTrace Point Hash)
    | -- | Chain sync application event
      Application ApplicationTrace
    | -- | API server is starting
      ServeApi
    | -- | API documentation server is starting
      ServeDocs
    | -- | Mithril bootstrap event
      Mithril ImportTrace
    | -- | Exiting after bootstrap-only mode
      BootstrapOnlyExit Point
    | -- | HTTP service encountered an error
      HTTPServiceError String
    | -- | Application is connecting to the node
      ApplicationStarting
    | -- | Node connection validation event
      NodeValidation NodeValidationTrace
    | -- | Incomplete bootstrap detected on startup
      IncompleteBootstrapDetected
    | -- | Incomplete bootstrap cleanup completed
      IncompleteBootstrapCleaned
    | -- | Genesis bootstrap: inserted N UTxOs from genesis file
      GenesisBootstrap Int
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
renderMainTraces (Update ut) =
    "Database update: " ++ renderUpdateTrace ut
renderMainTraces (Application at) =
    "Application event: " ++ renderApplicationTrace at
renderMainTraces ServeApi =
    "Starting API server..."
renderMainTraces ServeDocs =
    "Starting API documentation server..."
renderMainTraces (Mithril mt) =
    "Mithril: " ++ renderImportTrace mt
renderMainTraces (BootstrapOnlyExit point) =
    "Bootstrap complete, exiting (--mithril-bootstrap-only). Checkpoint: "
        ++ show point
renderMainTraces (HTTPServiceError err) =
    "ERROR: HTTP service failed to start: " ++ err
renderMainTraces ApplicationStarting =
    "Starting Ouroboros node connection and chain sync application..."
renderMainTraces (NodeValidation nvt) =
    "Node validation: " ++ renderNodeValidationTrace nvt
renderMainTraces IncompleteBootstrapDetected =
    "Incomplete bootstrap detected, cleaning up partial data..."
renderMainTraces IncompleteBootstrapCleaned =
    "Incomplete bootstrap cleanup completed, retrying bootstrap..."
renderMainTraces (GenesisBootstrap n) =
    "Genesis bootstrap: inserted "
        ++ show n
        ++ " UTxOs from genesis file"

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
stealMetricsEvent (Update (UpdateCSMTMeasured _ _ _ ns)) =
    Just $ CSMTDurationEvent ns
stealMetricsEvent (Update (UpdateRollbackMeasured _ _ _ _ ns)) =
    Just $ RollbackDurationEvent ns
stealMetricsEvent (Update (UpdateFinalityMeasured _ _ ns)) =
    Just $ FinalityDurationEvent ns
stealMetricsEvent (Update (UpdateTransactMeasured _ secs)) =
    Just $ TransactionDurationEvent secs
stealMetricsEvent (NotEmpty point) =
    Just $ BaseCheckpointEvent point
stealMetricsEvent (Application (ApplicationRollingBack _)) =
    Just $ BootstrapPhaseEvent RollingBack
stealMetricsEvent (Mithril ImportStarting) =
    Just $ BootstrapPhaseEvent Downloading
stealMetricsEvent (Mithril (ImportMithril (MithrilDownloadProgress bytes))) =
    Just $ DownloadProgressEvent bytes
stealMetricsEvent (Mithril (ImportExtractingUTxO _)) =
    Just $ BootstrapPhaseEvent Counting
stealMetricsEvent (Mithril (ImportExtraction (ExtractionCounting count))) =
    Just $ CountingProgressEvent count
stealMetricsEvent (Mithril (ImportExtraction (ExtractionDecodedState total))) =
    Just $ ExtractionTotalEvent total
stealMetricsEvent (Mithril (ImportExtraction ExtractionStreamStarting)) =
    Just $ BootstrapPhaseEvent Extracting
stealMetricsEvent (Mithril (ImportExtraction (ExtractionProgress count))) =
    Just $ ExtractionProgressEvent count
stealMetricsEvent _ = Nothing

{- | Match high-frequency events for throttling.

Returns frequency in Hz (events per second) for events that should be
throttled. Download progress, extraction events, and database updates
are throttled to 1 Hz to avoid flooding logs during sync.
-}
matchHighFrequencyEvents :: MainTraces -> Maybe Double
matchHighFrequencyEvents = \case
    Mithril (ImportMithril (MithrilDownloadProgress _)) -> Just 1.0
    Mithril (ImportExtraction (ExtractionCounting _)) -> Just 1.0
    Mithril (ImportExtraction (ExtractionProgress _)) -> Just 1.0
    Update (UpdateCSMTMeasured{}) -> Just 1.0
    Update (UpdateRollbackMeasured{}) -> Just 1.0
    Update (UpdateFinalityMeasured{}) -> Just 1.0
    Update (UpdateTransactMeasured{}) -> Just 1.0
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
