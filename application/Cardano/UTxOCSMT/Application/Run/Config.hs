module Cardano.UTxOCSMT.Application.Run.Config
    ( withRocksDB
    , config
    , armageddonParams
    , context
    , prisms
    , slotHash
    , mFinality
    , distance
    , encodePoint
    , decodePoint
    )
where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Run.Config
-- Description : Database configuration and schema definitions
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides the database configuration for RocksDB, including
-- column family setup, serialization prisms for encoding/decoding values,
-- and the CSMT context for hashing operations.

import CSMT (FromKV (..), Key)
import CSMT.Hashes
    ( Hash (..)
    , byteStringToKey
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.Ledger.Address (unCompactAddr)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Binary (decodeFull, natVersion)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (compactAddrTxOutL)
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( newFinality
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens
    ( Prism'
    , lazy
    , preview
    , prism'
    , review
    , strict
    , view
    , (^.)
    )
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (fromShort, toShort)
import Database.RocksDB
    ( Config (..)
    , DB
    , withDBCF
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..)
    )
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (..))
import Ouroboros.Network.Point qualified as Network

{- | Open a RocksDB database with the required column families.

The database uses five column families:

  * @kv@ - Key-value store for UTxO data
  * @csmt@ - Compact Sparse Merkle Tree nodes
  * @rollbacks@ - Rollback points for chain reorganizations
  * @config@ - Application configuration data
  * @journal@ - Journal entries for KVOnly-mode replay
-}
withRocksDB
    :: FilePath
    -- ^ Path to the database directory
    -> (DB -> IO b)
    -- ^ Action to run with the database handle
    -> IO b
withRocksDB path = do
    withDBCF
        path
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        , ("config", config)
        , ("journal", config)
        ]

{- | Default RocksDB configuration.

Creates the database if it doesn't exist, without paranoid checks
or bloom filters for simpler operation.
-}
config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

{- | Parameters for the armageddon (database reset) operation.

Configures:

  * @noHash@ - Empty hash used as a sentinel value
  * @armageddonBatchSize@ - Number of operations per batch (1000)
-}
armageddonParams :: ArmageddonParams Hash
armageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }

{- | CSMT context for hashing operations.

Configures how keys and values are converted to hashable form
using the standard hash functions from 'CSMT.Hashes'.
The tree key is prefixed with address bytes for by-address queries.
-}
context :: CSMTContext Hash LazyByteString LazyByteString
context =
    CSMTContext
        { fromKV =
            FromKV
                { isoK = strict . isoK fromKVHashes
                , fromV = fromV fromKVHashes . view strict
                , treePrefix =
                    addressPrefix . view strict
                }
        , hashing = hashHashing
        }

-- | Extract address bytes from a CBOR-encoded Conway TxOut, convert to tree key prefix.
addressPrefix :: StrictByteString -> Key
addressPrefix cborTxOut = byteStringToKey addressBytes
  where
    addressBytes =
        case decodeFull
            (natVersion @11)
            (view lazy cborTxOut) of
            Left e ->
                error
                    $ "addressPrefix: decode failed: "
                        <> show e
            Right (txOut :: BabbageTxOut ConwayEra) ->
                fromShort
                    $ unCompactAddr (txOut ^. compactAddrTxOutL)

-- | Encode a CBOR value to strict 'ByteString'.
encodeCBOR :: CBOR.Encoding -> StrictByteString
encodeCBOR = BL.toStrict . CBOR.toLazyByteString

-- | Decode a strict 'ByteString' with a CBOR decoder.
decodeCBOR
    :: (forall s. CBOR.Decoder s a)
    -> StrictByteString
    -> Maybe a
decodeCBOR decoder bs =
    case CBOR.deserialiseFromBytes
        decoder
        (BL.fromStrict bs) of
        Right (_, x) -> Just x
        Left _ -> Nothing

{- | Prisms for encoding and decoding database values.

Provides bidirectional transformations between Haskell types
and their binary representations in RocksDB:

  * @slotP@ - Encodes 'Point' as CBOR
  * @hashP@ - Encodes 'Hash' using the standard iso
  * @keyP@, @valueP@ - Identity transformations for lazy bytestrings
-}
prisms
    :: Prisms Point Hash LazyByteString LazyByteString
prisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString Point
    slotP = prism' encode decode
      where
        encode :: Point -> StrictByteString
        encode (Network.Point Origin) =
            encodeCBOR
                $ CBOR.encodeListLen 1
                    <> CBOR.encodeWord 0
        encode
            ( Network.Point
                    ( At
                            ( Network.Block
                                    (SlotNo slot)
                                    (OneEraHash h)
                                )
                        )
                ) =
                encodeCBOR
                    $ CBOR.encodeListLen 3
                        <> CBOR.encodeWord 1
                        <> CBOR.encodeWord64 slot
                        <> CBOR.encodeBytes
                            (fromShort h)

        decode
            :: StrictByteString -> Maybe Point
        decode = decodeCBOR $ do
            len <- CBOR.decodeListLen
            tag <- CBOR.decodeWord
            case (tag, len) of
                (0, 1) ->
                    pure $ Network.Point Origin
                (1, 3) -> do
                    slot <-
                        SlotNo <$> CBOR.decodeWord64
                    h <-
                        OneEraHash
                            . toShort
                            <$> CBOR.decodeBytes
                    pure
                        $ Network.Point
                            ( At
                                (Network.Block slot h)
                            )
                _ ->
                    fail
                        "slotP: invalid CBOR\
                        \ encoding"

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString LazyByteString
    keyP = lazy

    valueP :: Prism' StrictByteString LazyByteString
    valueP = lazy

{- | Extract the block hash from a 'Point'.

Fails with an error if called on 'Origin' since it has no hash.
-}
slotHash
    :: Point
    -- ^ The point to extract the hash from
    -> Hash
    -- ^ The block hash
slotHash (Network.Point Origin) =
    error "slotHash: Origin has no hash"
slotHash
    ( Network.Point
            (At (Network.Block _ (OneEraHash h)))
        ) =
        Hash $ fromShort h

-- | Encode a Point to ByteString for config storage
encodePoint :: Point -> StrictByteString
encodePoint = review (slotP prisms)

-- | Decode a ByteString to Point for config storage
decodePoint :: StrictByteString -> Maybe Point
decodePoint = preview (slotP prisms)

{- | Compute the finality point based on slot distance.

Returns the most recent point that is considered final (more than
2160 slots behind the tip, approximately 12 hours on Cardano mainnet).
-}
mFinality
    :: MonadFail m
    => RunTransaction cf op Point hash key value m
    -- ^ Database transaction runner
    -> m (Maybe Point)
    -- ^ The finality point, if any
mFinality (RunTransaction runTx) =
    runTx $ newFinality isFinal
  where
    isFinal
        :: WithOrigin Point
        -> WithOrigin Point
        -> Bool
    isFinal tip finality =
        distance tip finality > 2160

{- | Calculate the slot distance between two points.

Returns 0 if the tip is at Origin. Fails with an error for invalid
point combinations (e.g., finality at Origin when tip is not).
-}
distance
    :: WithOrigin Point
    -- ^ The tip point
    -> WithOrigin Point
    -- ^ The finality point
    -> SlotNo
    -- ^ The distance in slots
distance Origin _ = SlotNo 0
distance (At (Network.Point Origin)) _ =
    error "distance: tip at Origin has no slot"
distance
    (At (Network.Point (At (Network.Block slotTip _))))
    Origin = slotTip
distance
    (At (Network.Point (At (Network.Block slotTip _))))
    ( At
            ( Network.Point
                    (At (Network.Block slotFinality _))
                )
        ) =
        SlotNo
            (unSlotNo slotTip - unSlotNo slotFinality)
distance _ _ =
    error "distance: finality at Origin has no slot"
