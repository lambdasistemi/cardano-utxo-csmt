module Cardano.UTxOCSMT.Application.Run.Query
    ( queryMerkleRoots
    , queryInclusionProof
    , queryUTxOsByAddress
    , mkReadyResponse
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Query
-- Description : HTTP query handlers for the API
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides the query handlers used by the HTTP API server,
-- including merkle root queries, inclusion proof generation, and
-- readiness checks based on sync status.

import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , generateInclusionProof
    , renderHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getAllMerkleRoots
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , queryByAddress
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( BootstrapPhase (..)
    , Metrics (..)
    )
import Cardano.UTxOCSMT.Application.Run.Config (context)
import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import Cardano.UTxOCSMT.HTTP.API
    ( InclusionProofResponse (..)
    , MerkleRootEntry (..)
    , ReadyResponse (..)
    , UTxOByAddressEntry (..)
    )
import Cardano.UTxOCSMT.HTTP.Base16
    ( decodeBase16Text
    , encodeBase16Text
    , unsafeDecodeBase16Text
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Header, Point)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString (toStrict)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Short (toShort)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word16, Word64)
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))

{- | Query all merkle roots from the database.

Returns a list of 'MerkleRootEntry' values containing the slot number,
block hash, and merkle root for each processed block. Results are
filtered to exclude Origin points.
-}
queryMerkleRoots
    :: RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> IO [MerkleRootEntry]
    -- ^ List of merkle root entries
queryMerkleRoots (RunTransaction runTx) =
    runTx $ concatMap toMerkleRootEntry <$> getAllMerkleRoots
  where
    toMerkleRootEntry (slot, blockHash, merkleRoot) =
        case slot of
            Origin -> []
            At (Network.Point Origin) -> []
            At (Network.Point (At (Block slotNo _))) ->
                [MerkleRootEntry{slotNo, blockHash, merkleRoot}]

{- | Retrieve the inclusion proof and UTxO value for a transaction input.

Generates a cryptographic proof that a specific UTxO exists in the
current merkle tree. Returns 'Nothing' if the UTxO is not found.
-}
queryInclusionProof
    :: RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> Text
    -- ^ Transaction ID in base16 encoding
    -> Word16
    -- ^ Transaction output index
    -> IO (Maybe InclusionProofResponse)
    -- ^ Inclusion proof response, if the UTxO exists
queryInclusionProof (RunTransaction runTx) txIdText txIx = do
    runTx $ do
        let CSMTContext{fromKV, hashing} = context
        result <- generateInclusionProof fromKV KVCol CSMTCol txIn
        merkle <- queryMerkleRoot hashing
        pure $ do
            (out, proof') <- result
            let merkleText =
                    fmap
                        (Text.decodeUtf8 . convertToBase Base16 . renderHash)
                        merkle
            pure
                InclusionProofResponse
                    { proofTxId = txIdText
                    , proofTxIx = txIx
                    , proofTxOut = encodeBase16Text $ toStrict out
                    , proofBytes =
                        Text.decodeUtf8 $ convertToBase Base16 proof'
                    , proofMerkleRoot = merkleText
                    }
  where
    txIn = unsafeMkTxIn (toShort $ unsafeDecodeBase16Text txIdText) txIx

-- | Query all UTxOs at a given address.
queryUTxOsByAddress
    :: RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -- ^ Database transaction runner
    -> Text
    -- ^ Address in base16 encoding
    -> IO (Either String [UTxOByAddressEntry])
queryUTxOsByAddress (RunTransaction runTx) addressHex =
    case decodeBase16Text addressHex of
        Left err -> pure $ Left $ "Invalid base16 address: " <> err
        Right addressBytes -> do
            let CSMTContext{fromKV} = context
                addressKey = byteStringToKey addressBytes
            results <- runTx $ queryByAddress fromKV addressKey
            pure $ Right $ fmap toEntry results
  where
    toEntry (txIn, txOut) =
        UTxOByAddressEntry
            { utxoTxIn = encodeBase16Text $ toStrict txIn
            , utxoTxOut = encodeBase16Text $ toStrict txOut
            }

{- | Create a 'ReadyResponse' based on current metrics and sync threshold.

The service is considered ready when:

1. The bootstrap phase is 'Synced'
2. The number of slots behind the chain tip is less than or equal to the
   configured threshold

If no metrics are available yet, the service reports as not ready.
-}
mkReadyResponse
    :: Word64
    -- ^ Sync threshold (maximum slots behind to be considered ready)
    -> Maybe Metrics
    -- ^ Current metrics, if available
    -> ReadyResponse
    -- ^ Readiness response with sync status
mkReadyResponse threshold mMetrics =
    case mMetrics of
        Nothing ->
            ReadyResponse
                { ready = False
                , tipSlot = Nothing
                , processedSlot = Nothing
                , slotsBehind = Nothing
                }
        Just Metrics{chainTipSlot, lastBlockPoint, bootstrapPhase} ->
            let tip = unSlotNo <$> chainTipSlot
                processed = getProcessedSlot lastBlockPoint
                -- Handle case where processed > tip due to protocol timing
                -- (headers can arrive before tip is updated). When this
                -- happens, we're synced so slotsBehind is 0.
                behind = safeSub <$> tip <*> processed
                isSynced = bootstrapPhase == Just Synced
                isReady =
                    isSynced && case behind of
                        Just b -> b <= threshold
                        Nothing -> False
            in  ReadyResponse
                    { ready = isReady
                    , tipSlot = tip
                    , processedSlot = processed
                    , slotsBehind = behind
                    }
  where
    -- \| Safe subtraction that returns 0 instead of underflowing
    safeSub :: Word64 -> Word64 -> Word64
    safeSub a b
        | a >= b = a - b
        | otherwise = 0

    getProcessedSlot
        :: Maybe (a, Header) -> Maybe Word64
    getProcessedSlot Nothing = Nothing
    getProcessedSlot (Just (_, header)) =
        Just $ unSlotNo $ Network.blockSlot header
