{- |
Module      : Cardano.UTxOCSMT.Signing
Description : Ed25519 signing for merkle root attestations
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Signs @(blockHash, merkleRoot)@ tuples so institutions can
attest the UTxO state at each chain point.

Signing is optional: when no key is configured the service
runs unsigned.
-}
module Cardano.UTxOCSMT.Signing
    ( SigningConfig (..)
    , parseSigningKey
    , signMerkleRoot
    , renderPublicKey
    )
where

import CSMT.Hashes (Hash (..), renderHash)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

-- | Signing configuration: secret key + derived public key.
data SigningConfig = SigningConfig
    { sigSecretKey :: Ed25519.SecretKey
    , sigPublicKey :: Ed25519.PublicKey
    }

-- | Parse a 32-byte Ed25519 secret key from raw bytes.
parseSigningKey :: ByteString -> Maybe SigningConfig
parseSigningKey bs = case Ed25519.secretKey bs of
    CryptoPassed sk ->
        let pk = Ed25519.toPublic sk
        in  Just SigningConfig{sigSecretKey = sk, sigPublicKey = pk}
    CryptoFailed _ -> Nothing

{- | Sign @(blockHash, merkleRoot)@.
The signed payload is the concatenation of the two
32-byte hashes (64 bytes total).
-}
signMerkleRoot
    :: SigningConfig -> Hash -> Hash -> ByteString
signMerkleRoot SigningConfig{sigSecretKey, sigPublicKey} bh mr =
    let payload = renderHash bh <> renderHash mr
    in  convert
            $ Ed25519.sign sigSecretKey sigPublicKey payload

-- | Render the public key as raw bytes.
renderPublicKey :: SigningConfig -> ByteString
renderPublicKey SigningConfig{sigPublicKey} =
    convert sigPublicKey
