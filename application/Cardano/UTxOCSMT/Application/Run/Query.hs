module Cardano.UTxOCSMT.Application.Run.Query
    ( queryMerkleRoots
    , queryInclusionProof
    , queryExclusionProof
    , queryUTxOsByAddress
    , mkReadyResponse
    , queryAwaitValue
    , queryAwait
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
    , generateInclusionProof
    , renderHash
    )
import CSMT.Interface (FromKV (..))
import CSMT.Proof.Exclusion
    ( buildExclusionProof
    , verifyExclusionProof
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
import Cardano.UTxOCSMT.Application.Database.Interface
    ( WithSentinel (..)
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( Metrics (..)
    , SyncPhase (..)
    , SyncThreshold (..)
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( context
    , hashAddressKey
    )
import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import Cardano.UTxOCSMT.HTTP.API
    ( AwaitResponse (..)
    , InclusionProofResponse (..)
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
import Control.Concurrent.STM
    ( TVar
    , atomically
    , readTVar
    , readTVarIO
    , retry
    )
import Control.Lens (view)
import Control.Monad (when)
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
import Database.KV.Transaction (query)
import Database.RocksDB (BatchOp, ColumnFamily)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (Block (..), WithOrigin (..))
import System.Timeout (timeout)

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
            Sentinel -> []
            Value (Network.Point Origin) -> []
            Value (Network.Point (At (Block slotNo _))) ->
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

{- | Query an exclusion proof for a UTxO.

Returns 'True' if the key is NOT in the tree (exclusion
proof found and verified). Returns 'False' if the key
IS in the tree.
-}
queryExclusionProof
    :: RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> Text
    -- ^ Transaction ID in base16 encoding
    -> Word16
    -- ^ Transaction output index
    -> IO Bool
queryExclusionProof (RunTransaction runTx) txIdText txIx =
    runTx $ do
        let CSMTContext{fromKV, hashing} = context
            FromKV{isoK = iso', treePrefix = pfx} = fromKV
            baseKey = view iso' txIn
        -- Try to look up the value to get the full tree key
        -- (with prefix). If no value exists, use the base key.
        mVal <- query KVCol txIn
        let treeKey = case mVal of
                Just v -> pfx v <> baseKey
                Nothing -> baseKey
        mProof <- buildExclusionProof [] CSMTCol hashing treeKey
        pure $ case mProof of
            Nothing -> False
            Just proof -> verifyExclusionProof hashing proof
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
                addressKey = hashAddressKey addressBytes
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
    :: SyncThreshold
    -> Maybe Metrics
    -- ^ Current metrics, if available
    -> ReadyResponse
    -- ^ Readiness response with sync status
mkReadyResponse _threshold mMetrics =
    case mMetrics of
        Nothing ->
            ReadyResponse
                { ready = False
                , tipSlot = Nothing
                , processedSlot = Nothing
                , slotsBehind = Nothing
                }
        Just Metrics{chainTipSlot, lastBlockPoint, syncPhase} ->
            let tip = unSlotNo <$> chainTipSlot
                processed = getProcessedSlot lastBlockPoint
                behind = safeSub <$> tip <*> processed
                isReady = syncPhase == Just Synced
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

{- | Block until a key appears in the store or timeout expires.

Uses a TVar counter that is incremented after each commit.
The STM runtime wakes blocked threads when the counter changes,
at which point we re-check the store for the key.
-}
queryAwaitValue
    :: TVar Int
    -- ^ Commit notification counter
    -> (key -> IO (Maybe value))
    -- ^ getValue function
    -> key
    -- ^ Key to wait for
    -> Maybe Int
    -- ^ Timeout in seconds (Nothing = 30s default)
    -> IO (Maybe value)
queryAwaitValue notifyTVar getValue' key mTimeout = do
    let timeoutMicros = maybe 30_000_000 (* 1_000_000) mTimeout
    timeout timeoutMicros $ do
        let go lastSeen = do
                mval <- getValue' key
                case mval of
                    Just v -> pure v
                    Nothing -> do
                        atomically $ do
                            current <- readTVar notifyTVar
                            when (current == lastSeen) retry
                        go =<< readTVarIO notifyTVar
        go =<< readTVarIO notifyTVar

{- | HTTP await handler: block until a UTxO appears, returning
an AwaitResponse with the base16-encoded TxOut.
-}
queryAwait
    :: TVar Int
    -> RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        LazyByteString
        LazyByteString
        IO
    -> Text
    -> Word16
    -> Maybe Int
    -> IO (Maybe AwaitResponse)
queryAwait notifyTVar (RunTransaction runTx) txIdText txIx mTimeout = do
    let txIn = unsafeMkTxIn (toShort $ unsafeDecodeBase16Text txIdText) txIx
        getValue' k = runTx $ query KVCol k
    mval <- queryAwaitValue notifyTVar getValue' txIn mTimeout
    pure
        $ fmap
            ( \out ->
                AwaitResponse
                    txIdText
                    txIx
                    (encodeBase16Text $ toStrict out)
            )
            mval
