{- | Rollback point types and serialization.

Re-exports 'RollbackPoint' from @chain-follower@
and provides a pattern synonym for
backward-compatible field access.
-}
module Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( -- * Re-exports from chain-follower
      RP.RollbackPoint (..)

      -- * Pattern synonym
    , pattern UTxORollbackPoint

      -- * Metadata alias
    , Meta

      -- * KV alias
    , RollbackPointKV

      -- * Serialization
    , rollbackPointPrism
    , withOriginPrism
    )
where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
import ChainFollower.Rollbacks.Column (RollbackKV)
import ChainFollower.Rollbacks.Types qualified as RP
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens
    ( Prism'
    , preview
    , prism'
    , review
    )
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Ouroboros.Network.Point (WithOrigin (..))

-- | KV pair for rollback point storage.
type RollbackPointKV slot hash key value =
    RollbackKV
        (WithOrigin slot)
        (Operation key value)
        (hash, Maybe hash)

{- | Metadata type for downstream rollback points.

First component is the block hash (always present),
second is the optional merkle root.
-}
type Meta hash = (hash, Maybe hash)

{- | Pattern synonym mapping the generic 'RollbackPoint'
to downstream field names.

@
UTxORollbackPoint
    { rbpHash :: hash
    , rbpInverseOperations :: [Operation key value]
    , rpbMerkleRoot :: Maybe hash
    }
@
-}
pattern UTxORollbackPoint
    :: hash
    -> [Operation key value]
    -> Maybe hash
    -> RP.RollbackPoint (Operation key value) (Meta hash)
pattern UTxORollbackPoint h ops mr =
    RP.RollbackPoint
        { rpInverses = ops
        , rpMeta = Just (h, mr)
        }

{-# COMPLETE UTxORollbackPoint #-}

-- | Encode a CBOR value to strict 'ByteString'.
encodeCBOR :: CBOR.Encoding -> ByteString
encodeCBOR = BL.toStrict . CBOR.toLazyByteString

-- | Decode a strict 'ByteString' with a CBOR decoder.
decodeCBOR
    :: (forall s. CBOR.Decoder s a)
    -> ByteString
    -> Maybe a
decodeCBOR decoder bs =
    case CBOR.deserialiseFromBytes
        decoder
        (BL.fromStrict bs) of
        Right (_, x) -> Just x
        Left _ -> Nothing

-- | Encode a prism-encoded value as CBOR bytes.
encodeReview
    :: Prism' ByteString a -> a -> CBOR.Encoding
encodeReview p x = CBOR.encodeBytes (review p x)

-- | Decode a prism-encoded value from CBOR bytes.
decodePreview
    :: Prism' ByteString a -> CBOR.Decoder s a
decodePreview p = do
    bs <- CBOR.decodeBytes
    case preview p bs of
        Just x -> pure x
        Nothing ->
            fail "decodePreview: prism decoding failed"

-- | Prism for serializing/deserializing RollbackPoint
rollbackPointPrism
    :: forall hash key value
     . Prism' ByteString hash
    -> Prism' ByteString key
    -> Prism' ByteString value
    -> Prism'
        ByteString
        ( RP.RollbackPoint
            (Operation key value)
            (Meta hash)
        )
rollbackPointPrism hashPrism keyPrism valuePrism =
    prism' encode decode
  where
    encode
        :: RP.RollbackPoint
            (Operation key value)
            (Meta hash)
        -> ByteString
    encode (UTxORollbackPoint h ops mr) =
        encodeCBOR
            $ CBOR.encodeListLen 3
                <> encodeReview hashPrism h
                <> encodeOps ops
                <> encodeMaybe mr

    encodeOps :: [Operation key value] -> CBOR.Encoding
    encodeOps ops =
        CBOR.encodeListLen
            (fromIntegral $ length ops)
            <> foldMap encodeOp ops

    encodeOp :: Operation key value -> CBOR.Encoding
    encodeOp (Delete k) =
        CBOR.encodeListLen 2
            <> CBOR.encodeWord 0
            <> encodeReview keyPrism k
    encodeOp (Insert k v) =
        CBOR.encodeListLen 3
            <> CBOR.encodeWord 1
            <> encodeReview keyPrism k
            <> encodeReview valuePrism v

    encodeMaybe :: Maybe hash -> CBOR.Encoding
    encodeMaybe Nothing = CBOR.encodeListLen 0
    encodeMaybe (Just h) =
        CBOR.encodeListLen 1
            <> encodeReview hashPrism h

    decode
        :: ByteString
        -> Maybe
            ( RP.RollbackPoint
                (Operation key value)
                (Meta hash)
            )
    decode = decodeCBOR $ do
        _ <- CBOR.decodeListLen
        h <- decodePreview hashPrism
        opsLen <- CBOR.decodeListLen
        ops <-
            replicateM opsLen decodeOp
        mbLen <- CBOR.decodeListLen
        mr <- case mbLen of
            0 -> pure Nothing
            1 -> Just <$> decodePreview hashPrism
            _ ->
                fail
                    "rollbackPointPrism: invalid\
                    \ merkle root array length"
        pure $ UTxORollbackPoint h ops mr

    decodeOp :: CBOR.Decoder s (Operation key value)
    decodeOp = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeWord
        case (tag, len) of
            (0, 2) -> Delete <$> decodePreview keyPrism
            (1, 3) ->
                Insert
                    <$> decodePreview keyPrism
                    <*> decodePreview valuePrism
            _ ->
                fail
                    "rollbackPointPrism: invalid\
                    \ operation"

-- | Prism for 'WithOrigin' using CBOR encoding.
withOriginPrism
    :: forall slot
     . Prism' ByteString slot
    -> Prism' ByteString (WithOrigin slot)
withOriginPrism slotP = prism' encode decode
  where
    encode :: WithOrigin slot -> ByteString
    encode Origin =
        encodeCBOR $ CBOR.encodeWord 0
    encode (At slot) =
        encodeCBOR
            $ CBOR.encodeListLen 2
                <> CBOR.encodeWord 1
                <> encodeReview slotP slot

    decode :: ByteString -> Maybe (WithOrigin slot)
    decode = decodeCBOR $ do
        tokenType <- CBOR.peekTokenType
        case tokenType of
            CBOR.TypeUInt -> do
                tag <- CBOR.decodeWord
                case tag of
                    0 -> pure Origin
                    _ ->
                        fail
                            "withOriginPrism:\
                            \ invalid tag"
            CBOR.TypeListLen -> do
                _ <- CBOR.decodeListLen
                tag <- CBOR.decodeWord
                case tag of
                    1 ->
                        At <$> decodePreview slotP
                    _ ->
                        fail
                            "withOriginPrism:\
                            \ invalid tag"
            _ ->
                fail
                    "withOriginPrism:\
                    \ unexpected token"
