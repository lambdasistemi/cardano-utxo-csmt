{- |
Module      : Cardano.UTxOCSMT.Mithril.Import
Description : Import UTxO data from Mithril snapshots into CSMT database

This module provides the bridge between Mithril snapshots and the CSMT
database. It orchestrates the full bootstrap process:

1. Fetch latest snapshot metadata from Mithril aggregator
2. Download and verify snapshot via mithril-client
3. Extract UTxO set from Cardano ledger state files
4. Bulk import into CSMT database
5. Set checkpoint for chain sync continuation
-}
module Cardano.UTxOCSMT.Mithril.Import
    ( -- * Import operations
      importFromMithril
    , ImportResult (..)

      -- * Tracing
    , ImportTrace (..)
    , renderImportTrace
    )
where

import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilConfig (..)
    , MithrilError
    , MithrilTrace (..)
    , SnapshotMetadata (..)
    , downloadSnapshotHttp
    , fetchLatestSnapshot
    , renderMithrilError
    , renderMithrilTrace
    )
import Cardano.UTxOCSMT.Mithril.Extraction
    ( ExtractionError
    , ExtractionTrace
    , extractUTxOsFromSnapshot
    , renderExtractionError
    , renderExtractionTrace
    )
import Cardano.UTxOCSMT.Mithril.Streaming
    ( StreamTrace
    , defaultStreamConfig
    , renderStreamTrace
    , streamToCSMT
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Tracer (Tracer, contramap, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Tracer.TraceWith (trace, tracer, pattern TraceWith)
import Data.Word (Word64)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))

-- | Result of Mithril import operation
data ImportResult
    = ImportSuccess
        { importCheckpoint :: Point
        -- ^ Checkpoint (Origin, will be updated during chain sync)
        , importCount :: Word64
        -- ^ Number of UTxOs imported
        , importDbPath :: FilePath
        -- ^ Path to the immutable DB
        , importSlot :: Word64
        -- ^ Slot from ledger state for skip-until logic
        }
    | -- | Import failed with Mithril error
      ImportFailed MithrilError
    | -- | Import failed during extraction
      ImportExtractionFailed ExtractionError

-- | Trace events during import
data ImportTrace
    = -- | Starting Mithril import process
      ImportStarting
    | -- | Mithril client operation
      ImportMithril MithrilTrace
    | -- | Extracting UTxO from immutable DB at path
      ImportExtractingUTxO FilePath
    | -- | Extraction trace event
      ImportExtraction ExtractionTrace
    | -- | Streaming trace event
      ImportStreaming StreamTrace
    | -- | Progress: current count, total estimated
      ImportProgress Word64 Word64
    | -- | Import complete: total UTxOs imported, slot, block hash
      ImportComplete Word64
    | -- | Error during import
      ImportError MithrilError
    | -- | Extraction error
      ImportExtractionError ExtractionError
    deriving (Show)

-- | Render trace for logging
renderImportTrace :: ImportTrace -> String
renderImportTrace ImportStarting =
    "Starting Mithril bootstrap import..."
renderImportTrace (ImportMithril mt) =
    renderMithrilTrace mt
renderImportTrace (ImportExtractingUTxO path) =
    "Extracting UTxO set from ledger state: " <> path
renderImportTrace (ImportExtraction et) =
    renderExtractionTrace et
renderImportTrace (ImportStreaming st) =
    renderStreamTrace st
renderImportTrace (ImportProgress current total) =
    "Import progress: "
        <> show current
        <> " / "
        <> show total
        <> " UTxOs"
renderImportTrace (ImportComplete count) =
    "Mithril import complete: "
        <> show count
        <> " UTxOs imported at slot "
renderImportTrace (ImportError err) =
    "Mithril import error: " <> renderMithrilError err
renderImportTrace (ImportExtractionError err) =
    "Extraction error: " <> renderExtractionError err

{- | Import UTxO set from Mithril snapshot into CSMT database

This function orchestrates the full Mithril bootstrap process:

1. Fetches the latest snapshot metadata from the aggregator
2. Downloads and verifies the snapshot using mithril-client
3. Extracts the UTxO set from the downloaded ledger state
4. Streams UTxOs into the CSMT database
5. Returns the checkpoint for chain sync to continue from

The @onBeforeDbWrite@ callback is invoked after download succeeds but
before streaming UTxOs into the database. This is the right moment to
set a bootstrap-in-progress marker: download failures won\'t leave a
stale marker that triggers unnecessary DB wipes on restart.
-}
importFromMithril
    :: Tracer IO ImportTrace
    -- ^ Tracer for progress logging
    -> MithrilConfig
    -- ^ Mithril client configuration
    -> IO ()
    -- ^ Action to run after download succeeds, before DB writes begin
    -> (ByteString -> ByteString -> IO ())
    -- ^ Insert a key-value pair into the CSMT database
    -> IO ImportResult
importFromMithril TraceWith{tracer, trace} config onBeforeDbWrite insertCSMT = do
    trace ImportStarting

    -- Step 1: Fetch latest snapshot metadata
    trace
        $ ImportMithril
        $ MithrilFetchingSnapshot (mithrilAggregatorUrl config)
    snapshotResult <- fetchLatestSnapshot config

    case snapshotResult of
        Left err -> do
            trace $ ImportError err
            pure $ ImportFailed err
        Right snapshot -> do
            let digest = snapshotDigest snapshot
                slot = snapshotBeaconSlot snapshot
                epoch = snapshotBeaconEpoch snapshot

            trace
                $ ImportMithril
                $ MithrilSnapshotFound digest slot epoch

            -- Step 2: Download and verify snapshot
            trace
                $ ImportMithril
                $ MithrilDownloading digest (mithrilDownloadDir config)

            downloadResult <-
                downloadSnapshotHttp
                    (contramap ImportMithril tracer)
                    config
                    snapshot

            case downloadResult of
                Left err -> do
                    traceWith tracer $ ImportError err
                    pure $ ImportFailed err
                Right dbPath -> do
                    traceWith tracer
                        $ ImportMithril
                        $ MithrilDownloadComplete dbPath

                    -- Mark bootstrap-in-progress now that we're about
                    -- to write to the DB. Download failures above won't
                    -- leave a stale marker.
                    onBeforeDbWrite

                    -- Step 3: Extract UTxO and import
                    traceWith tracer $ ImportExtractingUTxO dbPath

                    extractResult <-
                        extractUTxOsFromSnapshot
                            (contramap ImportExtraction tracer)
                            dbPath
                            ( streamToCSMT
                                (contramap ImportStreaming tracer)
                                defaultStreamConfig
                                insertCSMT
                            )

                    case extractResult of
                        Left err -> do
                            traceWith tracer $ ImportExtractionError err
                            pure $ ImportExtractionFailed err
                        Right (count, extractedSlot) -> do
                            let checkpoint = makeCheckpoint snapshot

                            traceWith tracer $ ImportComplete count

                            pure
                                ImportSuccess
                                    { importCheckpoint = checkpoint
                                    , importCount = count
                                    , importDbPath = dbPath
                                    , importSlot = extractedSlot
                                    }

{- | Create a Point from snapshot metadata for chain sync checkpoint

Note: The Mithril API doesn't provide block hash directly, only merkle root.
The chain sync will need to find intersection from the slot number.
For now, we return Origin and let chain sync handle intersection.
-}
makeCheckpoint :: SnapshotMetadata -> Point
makeCheckpoint SnapshotMetadata{} =
    -- TODO: Once we have block hash available, construct proper Point
    -- For now, return Origin - chain sync will find intersection
    Network.Point Origin
