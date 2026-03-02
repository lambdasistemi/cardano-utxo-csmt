{- |
Module      : Cardano.UTxOCSMT.Mithril.Streaming
Description : Stream UTxOs to CSMT bulk import

This module provides functionality to stream UTxOs from Mithril extraction
into the CSMT database. It supports:

* Configurable batch sizes for commit frequency
* Progress reporting at configurable intervals
* Efficient bulk import using the CSMT transaction interface
* Conversion from MemPack (Mithril) to CBOR (chain sync) encoding

The streaming approach follows the pattern from 'Bootstrap.App' for
memory-efficient processing of large UTxO sets.
-}
module Cardano.UTxOCSMT.Mithril.Streaming
    ( -- * Streaming
      streamToCSMT

      -- * Configuration
    , StreamConfig (..)
    , defaultStreamConfig

      -- * Encoding conversion
    , convertToCBOR

      -- * Tracing
    , StreamTrace (..)
    , renderStreamTrace
    )
where

import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Binary (natVersion, serialize)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad (when)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.MemPack (unpack)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

-- | Configuration for streaming to CSMT
data StreamConfig = StreamConfig
    { streamBatchSize :: Word64
    -- ^ Number of entries per transaction batch
    , streamProgressInterval :: Word64
    -- ^ Report progress every N entries
    }
    deriving stock (Show, Eq)

-- | Default streaming configuration
defaultStreamConfig :: StreamConfig
defaultStreamConfig =
    StreamConfig
        { streamBatchSize = 10000
        , streamProgressInterval = 100000
        }

-- | Trace events during streaming
data StreamTrace
    = -- | Starting stream import
      StreamStarting
    | -- | Progress: entries imported, entries per second
      StreamProgress Word64 Double
    | -- | Batch committed
      StreamBatchCommitted Word64
    | -- | Stream complete: total entries
      StreamComplete Word64
    deriving stock (Show, Eq)

-- | Render stream trace for logging
renderStreamTrace :: StreamTrace -> String
renderStreamTrace StreamStarting =
    "Starting UTxO stream import..."
renderStreamTrace (StreamProgress count rate) =
    "Imported "
        <> show count
        <> " UTxOs ("
        <> show (round rate :: Int)
        <> " UTxOs/second)"
renderStreamTrace (StreamBatchCommitted count) =
    "Committed batch at " <> show count <> " entries"
renderStreamTrace (StreamComplete count) =
    "Stream import complete: " <> show count <> " UTxOs"

{- | Stream UTxOs to CSMT database

This function consumes a stream of MemPack-encoded (TxIn, TxOut) pairs,
converts them to CBOR encoding (for chain sync compatibility), and inserts
them into the CSMT database.

The function:
1. Decodes MemPack bytes to Conway-era TxIn/TxOut types
2. Re-encodes to CBOR (matching chain sync format)
3. Reports progress at configurable intervals
4. Returns the total number of entries imported
-}
streamToCSMT
    :: Tracer IO StreamTrace
    -> StreamConfig
    -> (ByteString -> ByteString -> IO ())
    -- ^ Insert a key-value pair into the CSMT database
    -> Stream (Of (ByteString, ByteString)) IO ()
    -> IO Word64
streamToCSMT tracer config insertCSMT stream = do
    traceWith tracer StreamStarting
    startTime <- getCurrentTime
    count <- processStream startTime 0 0 stream
    traceWith tracer $ StreamComplete count
    pure count
  where
    StreamConfig{streamProgressInterval} = config

    processStream
        :: UTCTime
        -> Word64
        -- \^ Successfully imported count
        -> Word64
        -- \^ Skipped (decode failure) count
        -> Stream (Of (ByteString, ByteString)) IO ()
        -> IO Word64
    processStream startTime count skipped stream' = do
        result <- S.next stream'
        case result of
            Left () -> pure count
            Right ((keyBs, valueBs), rest) -> do
                -- Decode MemPack and re-encode to CBOR
                case convertToCBOR keyBs valueBs of
                    Nothing ->
                        -- Skip entries that fail to decode
                        processStream startTime count (skipped + 1) rest
                    Just (cborKey, cborValue) -> do
                        -- Insert CBOR-encoded bytes into CSMT
                        insertCSMT cborKey cborValue

                        let newCount = count + 1

                        -- Report progress at intervals
                        when (newCount `mod` streamProgressInterval == 0) $ do
                            currentTime <- getCurrentTime
                            let elapsed =
                                    realToFrac
                                        $ diffUTCTime currentTime startTime
                                rate =
                                    if elapsed > 0
                                        then fromIntegral newCount / elapsed
                                        else 0
                            traceWith tracer $ StreamProgress newCount rate

                        processStream startTime newCount skipped rest

{- | Convert MemPack-encoded (TxIn, TxOut) to CBOR encoding

Returns Nothing if decoding fails.
-}
convertToCBOR
    :: ByteString
    -- ^ MemPack-encoded TxIn
    -> ByteString
    -- ^ MemPack-encoded TxOut
    -> Maybe (ByteString, ByteString)
convertToCBOR keyBs valueBs = do
    -- Decode MemPack to Haskell types
    txIn <-
        either (const Nothing) Just
            $ unpack @TxIn (LBS.toStrict keyBs)
    txOut <-
        either (const Nothing) Just
            $ unpack @(BabbageTxOut ConwayEra) (LBS.toStrict valueBs)

    -- Re-encode to CBOR (protocol version 11, matching chain sync)
    let cborKey = serialize (natVersion @11) txIn
        cborValue = serialize (natVersion @11) txOut

    pure (cborKey, cborValue)
