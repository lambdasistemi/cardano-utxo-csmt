{- |
Module      : Cardano.UTxOCSMT.HTTP.API
Description : REST API type definitions for UTxO CSMT service

This module defines the Servant API types and data structures for
the HTTP service endpoints:

* @GET /metrics@ - Current synchronization metrics (JSON)
* @GET /metrics/prometheus@ - Metrics in Prometheus exposition format
* @GET /merkle-roots@ - Historical merkle roots by slot
* @GET /proof/:txId/:txIx@ - Inclusion proof for a specific UTxO
* @GET /ready@ - Sync readiness status for orchestration

Response types include JSON serialization and Swagger schema definitions.
-}
module Cardano.UTxOCSMT.HTTP.API
    ( api
    , API
    , DOCS
    , docs
    , MerkleRootEntry (..)
    , InclusionProofResponse (..)
    , UTxOByAddressEntry (..)
    , ReadyResponse (..)
    , AwaitResponse (..)
    )
where

import CSMT.Hashes (Hash, renderHash)
import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.Base16 (decodeBase16Text)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Proxy (Proxy (..))
import Data.Swagger
    ( ToSchema (..)
    , declareSchemaRef
    , description
    , properties
    , required
    )
import Data.Swagger qualified as Swagger
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word16, Word64)
import GHC.IsList (IsList (..))
import Servant
    ( Capture
    , Get
    , JSON
    , PlainText
    , QueryParam
    , type (:<|>)
    , type (:>)
    )
import Servant.Swagger.UI (SwaggerSchemaUI)

-- | Type alias for API documentation endpoint
type DOCS = "api-docs" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

-- | Proxy for API documentation endpoint
docs :: Proxy DOCS
docs = Proxy

-- | Type alias for the API
type API =
    "metrics"
        :> "prometheus"
        :> Get '[PlainText] Text
        :<|> "metrics"
            :> Get '[JSON] Metrics
        :<|> "merkle-roots"
            :> Get '[JSON] [MerkleRootEntry]
        :<|> "proof"
            :> Capture "txId" Text
            :> Capture "txIx" Word16
            :> Get '[JSON] InclusionProofResponse
        :<|> "utxos-by-address"
            :> Capture "address" Text
            :> Get '[JSON] [UTxOByAddressEntry]
        :<|> "ready"
            :> Get '[JSON] ReadyResponse
        :<|> "await"
            :> Capture "txId" Text
            :> Capture "txIx" Word16
            :> QueryParam "timeout" Int
            :> Get '[JSON] AwaitResponse

-- | Proxy for the API
api :: Proxy API
api = Proxy

-- | Entry for a merkle root at a given block
data MerkleRootEntry = MerkleRootEntry
    { blockHash :: Hash
    , merkleRoot :: Maybe Hash
    }
    deriving (Show, Eq)

data InclusionProofResponse = InclusionProofResponse
    { proofTxOut :: Text
    , proofBytes :: Text
    , proofBlockHash :: Text
    }
    deriving (Show, Eq)

-- | A UTxO entry at a given address
data UTxOByAddressEntry = UTxOByAddressEntry
    { utxoTxIn :: Text
    -- ^ CBOR-encoded TxIn in base16
    , utxoTxOut :: Text
    -- ^ CBOR-encoded TxOut in base16
    }
    deriving (Show, Eq)

instance ToJSON UTxOByAddressEntry where
    toJSON UTxOByAddressEntry{utxoTxIn, utxoTxOut} =
        object
            [ "txIn" .= utxoTxIn
            , "txOut" .= utxoTxOut
            ]

instance FromJSON UTxOByAddressEntry where
    parseJSON = withObject "UTxOByAddressEntry" $ \v ->
        UTxOByAddressEntry
            <$> v .: "txIn"
            <*> v .: "txOut"

instance ToSchema UTxOByAddressEntry where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        return
            $ Swagger.NamedSchema (Just "UTxOByAddressEntry")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("txIn", stringSchema)
                    , ("txOut", stringSchema)
                    ]
            & required .~ ["txIn", "txOut"]
            & description
                ?~ "A UTxO entry with CBOR-encoded TxIn and TxOut in base16"

renderHashBase16 :: Hash -> Text
renderHashBase16 = Text.decodeUtf8 . convertToBase Base16 . renderHash

instance ToJSON MerkleRootEntry where
    toJSON MerkleRootEntry{blockHash, merkleRoot} =
        object
            [ "blockHash" .= renderHashBase16 blockHash
            , "merkleRoot" .= fmap renderHashBase16 merkleRoot
            ]

instance FromJSON MerkleRootEntry where
    parseJSON = withObject "MerkleRootEntry" $ \v ->
        MerkleRootEntry
            <$> (v .: "blockHash" >>= parseHashBase16)
            <*> (v .: "merkleRoot" >>= mapM parseHashBase16)
      where
        parseHashBase16 :: Text -> Parser Hash
        parseHashBase16 txt =
            case decodeBase16Text txt of
                Left err -> fail $ "Invalid base16 hash: " ++ err
                Right h -> return h

instance ToJSON InclusionProofResponse where
    toJSON
        InclusionProofResponse
            { proofTxOut
            , proofBytes
            , proofBlockHash
            } =
            object
                [ "txOut" .= proofTxOut
                , "proof" .= proofBytes
                , "blockHash" .= proofBlockHash
                ]

instance FromJSON InclusionProofResponse where
    parseJSON = withObject "InclusionProofResponse" $ \v ->
        InclusionProofResponse
            <$> v .: "txOut"
            <*> v .: "proof"
            <*> v .: "blockHash"
instance ToSchema MerkleRootEntry where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        return
            $ Swagger.NamedSchema (Just "MerkleRootEntry")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("blockHash", stringSchema)
                    , ("merkleRoot", maybeStringSchema)
                    ]
            & required .~ ["blockHash", "merkleRoot"]
            & description ?~ "A merkle root at a given block"

instance ToSchema InclusionProofResponse where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        return
            $ Swagger.NamedSchema (Just "InclusionProofResponse")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("txOut", stringSchema)
                    , ("proof", stringSchema)
                    , ("blockHash", stringSchema)
                    ]
            & required .~ ["txOut", "proof", "blockHash"]
            & description
                ?~ "Inclusion proof, output, and block hash for the given transaction input."

-- | Response for the /ready endpoint indicating sync status
data ReadyResponse = ReadyResponse
    { ready :: Bool
    -- ^ Whether the node is synced and ready to serve requests
    , tipSlot :: Maybe Word64
    -- ^ The current chain tip slot from ChainSync protocol
    , processedSlot :: Maybe Word64
    -- ^ The last processed block slot
    , slotsBehind :: Maybe Word64
    -- ^ Number of slots behind the chain tip
    }
    deriving (Show, Eq)

instance ToJSON ReadyResponse where
    toJSON ReadyResponse{ready, tipSlot, processedSlot, slotsBehind} =
        object
            [ "ready" .= ready
            , "tipSlot" .= tipSlot
            , "processedSlot" .= processedSlot
            , "slotsBehind" .= slotsBehind
            ]

instance FromJSON ReadyResponse where
    parseJSON = withObject "ReadyResponse" $ \v ->
        ReadyResponse
            <$> v .: "ready"
            <*> v .: "tipSlot"
            <*> v .: "processedSlot"
            <*> v .: "slotsBehind"

instance ToSchema ReadyResponse where
    declareNamedSchema _ = do
        boolSchema <- declareSchemaRef (Proxy @Bool)
        maybeWord64Schema <- declareSchemaRef (Proxy @(Maybe Word64))
        return
            $ Swagger.NamedSchema (Just "ReadyResponse")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("ready", boolSchema)
                    , ("tipSlot", maybeWord64Schema)
                    , ("processedSlot", maybeWord64Schema)
                    , ("slotsBehind", maybeWord64Schema)
                    ]
            & required .~ ["ready", "tipSlot", "processedSlot", "slotsBehind"]
            & description
                ?~ "Sync readiness status for orchestration"

-- | Response for the /await endpoint
data AwaitResponse = AwaitResponse
    { awaitTxId :: Text
    -- ^ Transaction ID that was awaited
    , awaitTxIx :: Word16
    -- ^ Transaction index
    , awaitTxOut :: Text
    -- ^ CBOR-encoded TxOut in base16
    }
    deriving (Show, Eq)

instance ToJSON AwaitResponse where
    toJSON AwaitResponse{awaitTxId, awaitTxIx, awaitTxOut} =
        object
            [ "txId" .= awaitTxId
            , "txIx" .= awaitTxIx
            , "txOut" .= awaitTxOut
            ]

instance FromJSON AwaitResponse where
    parseJSON = withObject "AwaitResponse" $ \v ->
        AwaitResponse
            <$> v .: "txId"
            <*> v .: "txIx"
            <*> v .: "txOut"

instance ToSchema AwaitResponse where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        word16Schema <- declareSchemaRef (Proxy @Word16)
        return
            $ Swagger.NamedSchema (Just "AwaitResponse")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("txId", stringSchema)
                    , ("txIx", word16Schema)
                    , ("txOut", stringSchema)
                    ]
            & required .~ ["txId", "txIx", "txOut"]
            & description
                ?~ "Response after awaiting a UTxO to appear in the CSMT"
