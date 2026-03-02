module Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    , RollbackPointKV
    , rollbackPointPrism
    , withOriginPrism
    )
where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
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
import Database.KV.Transaction (KV)
import Ouroboros.Network.Point (WithOrigin (..))

-- | Represents a rollback point in the database
data RollbackPoint slot hash key value = RollbackPoint
    { rbpHash :: hash
    , rbpInverseOperations :: [Operation key value]
    , rpbMerkleRoot :: Maybe hash
    }

-- | Type alias for the KV column storing rollback points
type RollbackPointKV slot hash key value =
    KV
        (WithOrigin slot)
        (RollbackPoint slot hash key value)

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
    :: forall slot hash key value
     . Prism' ByteString hash
    -> Prism' ByteString key
    -> Prism' ByteString value
    -> Prism'
        ByteString
        (RollbackPoint slot hash key value)
rollbackPointPrism hashPrism keyPrism valuePrism =
    prism' encode decode
  where
    encode
        :: RollbackPoint slot hash key value
        -> ByteString
    encode
        RollbackPoint
            { rbpHash
            , rbpInverseOperations
            , rpbMerkleRoot
            } =
            encodeCBOR
                $ CBOR.encodeListLen 3
                    <> encodeReview hashPrism rbpHash
                    <> encodeOps rbpInverseOperations
                    <> encodeMaybe rpbMerkleRoot

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
            (RollbackPoint slot hash key value)
    decode = decodeCBOR $ do
        _ <- CBOR.decodeListLen
        rbpHash <- decodePreview hashPrism
        opsLen <- CBOR.decodeListLen
        rbpInverseOperations <-
            replicateM opsLen decodeOp
        mbLen <- CBOR.decodeListLen
        rpbMerkleRoot <- case mbLen of
            0 -> pure Nothing
            1 -> Just <$> decodePreview hashPrism
            _ ->
                fail
                    "rollbackPointPrism: invalid\
                    \ merkle root array length"
        pure
            RollbackPoint
                { rbpHash
                , rbpInverseOperations
                , rpbMerkleRoot
                }

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
