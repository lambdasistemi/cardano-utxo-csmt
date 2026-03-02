{- |
Module      : Cardano.UTxOCSMT.Application.Database.Implementation.CSMTCodecs
Description : CBOR-based codecs for CSMT column storage
Copyright   : (c) Paolo Veronelli, 2024
License     : Apache-2.0

CBOR serialization prisms for 'Key' and 'Indirect' types,
replacing the cereal-based 'csmtCodecs' from "CSMT.Interface".
-}
module Cardano.UTxOCSMT.Application.Database.Implementation.CSMTCodecs
    ( csmtCBORCodecs
    )
where

import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
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
import Database.KV.Transaction (Codecs (..), KV)

-- | Encode a CBOR value to strict 'ByteString'.
encodeCBOR :: CBOR.Encoding -> ByteString
encodeCBOR = BL.toStrict . CBOR.toLazyByteString

-- | Decode a strict 'ByteString' with a CBOR decoder.
decodeCBOR
    :: (forall s. CBOR.Decoder s a)
    -> ByteString
    -> Maybe a
decodeCBOR decoder bs =
    case CBOR.deserialiseFromBytes decoder (BL.fromStrict bs) of
        Right (_, x) -> Just x
        Left _ -> Nothing

encodeDirection :: Direction -> CBOR.Encoding
encodeDirection L = CBOR.encodeWord 0
encodeDirection R = CBOR.encodeWord 1

decodeDirection :: CBOR.Decoder s Direction
decodeDirection = do
    w <- CBOR.decodeWord
    case w of
        0 -> pure L
        1 -> pure R
        _ -> fail "Invalid direction"

encodeKey :: Key -> CBOR.Encoding
encodeKey dirs =
    CBOR.encodeListLen (fromIntegral $ length dirs)
        <> foldMap encodeDirection dirs

decodeKey :: CBOR.Decoder s Key
decodeKey = do
    len <- CBOR.decodeListLen
    replicateM len decodeDirection

keyCodecCBOR :: Prism' ByteString Key
keyCodecCBOR = prism' encode decode
  where
    encode = encodeCBOR . encodeKey
    decode = decodeCBOR decodeKey

indirectCodecCBOR
    :: Prism' ByteString hash
    -> Prism' ByteString (Indirect hash)
indirectCodecCBOR hashP = prism' encode decode
  where
    encode Indirect{jump, value} =
        encodeCBOR
            $ CBOR.encodeListLen 2
                <> encodeKey jump
                <> CBOR.encodeBytes (review hashP value)
    decode bs = do
        (jump, hashBytes) <-
            decodeCBOR
                ( do
                    _ <- CBOR.decodeListLen
                    j <- decodeKey
                    h <- CBOR.decodeBytes
                    pure (j, h)
                )
                bs
        value <- preview hashP hashBytes
        pure Indirect{jump, value}

{- | CBOR-based codecs for CSMT column storage.

Replaces the cereal-based 'csmtCodecs' from "CSMT.Interface"
with CBOR encoding, decoupling the on-disk format from the
CSMT library internals.
-}
csmtCBORCodecs
    :: Prism' ByteString hash
    -> Codecs (KV Key (Indirect hash))
csmtCBORCodecs hashP =
    Codecs
        { keyCodec = keyCodecCBOR
        , valueCodec = indirectCodecCBOR hashP
        }
