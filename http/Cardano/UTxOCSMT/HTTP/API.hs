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
    , ChainPoint (..)
    , MerkleRootEntry (..)
    , InclusionProofResponse (..)
    , UTxOByAddressEntry (..)
    , ReadyResponse (..)
    , AwaitResponse (..)
    )
where

import CSMT.Hashes (Hash, parseHash, renderHash)
import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.Base16 (decodeBase16Text)
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
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
import Ouroboros.Network.Block (SlotNo (..))
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

-- | A Cardano chain point represented by slot number and block hash.
data ChainPoint = ChainPoint
    { chainPointSlotNo :: SlotNo
    , chainPointBlockHash :: Hash
    }
    deriving (Show, Eq)

-- | Entry for a merkle root at a given block
data MerkleRootEntry = MerkleRootEntry
    { slotNo :: SlotNo
    , blockHash :: Hash
    , merkleRoot :: Maybe Hash
    }
    deriving (Show, Eq)

data InclusionProofResponse = InclusionProofResponse
    { proofTxOut :: Text
    , proofBytes :: Text
    , proofSlotNo :: SlotNo
    , proofBlockHash :: Hash
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

parseHashBase16 :: Text -> Parser Hash
parseHashBase16 txt =
    case decodeBase16Text txt of
        Left err -> fail $ "Invalid base16 hash: " ++ err
        Right bs -> case parseHash bs of
            Just h -> return h
            Nothing ->
                fail "Invalid hash length (expected 32 bytes)"

chainPointFields :: ChainPoint -> [Pair]
chainPointFields
    ChainPoint
        { chainPointSlotNo
        , chainPointBlockHash
        } =
        [ "slotNo" .= unSlotNo chainPointSlotNo
        , "blockHash" .= renderHashBase16 chainPointBlockHash
        ]

parseChainPoint :: Object -> Parser ChainPoint
parseChainPoint v =
    ChainPoint . SlotNo
        <$> v .: "slotNo"
        <*> (v .: "blockHash" >>= parseHashBase16)

instance ToJSON ChainPoint where
    toJSON = object . chainPointFields

instance FromJSON ChainPoint where
    parseJSON = withObject "ChainPoint" parseChainPoint

instance ToJSON MerkleRootEntry where
    toJSON MerkleRootEntry{slotNo, blockHash, merkleRoot} =
        object
            $ chainPointFields
                ChainPoint{chainPointSlotNo = slotNo, chainPointBlockHash = blockHash}
                <> [ "merkleRoot" .= fmap renderHashBase16 merkleRoot
                   ]

instance FromJSON MerkleRootEntry where
    parseJSON = withObject "MerkleRootEntry" $ \v -> do
        ChainPoint{chainPointSlotNo, chainPointBlockHash} <-
            parseChainPoint v
        MerkleRootEntry
            chainPointSlotNo
            chainPointBlockHash
            <$> (v .: "merkleRoot" >>= mapM parseHashBase16)

instance ToJSON InclusionProofResponse where
    toJSON
        InclusionProofResponse
            { proofTxOut
            , proofBytes
            , proofSlotNo
            , proofBlockHash
            } =
            object
                $ [ "txOut" .= proofTxOut
                  , "proof" .= proofBytes
                  ]
                    <> chainPointFields
                        ChainPoint
                            { chainPointSlotNo = proofSlotNo
                            , chainPointBlockHash = proofBlockHash
                            }

instance FromJSON InclusionProofResponse where
    parseJSON = withObject "InclusionProofResponse" $ \v -> do
        ChainPoint{chainPointSlotNo, chainPointBlockHash} <-
            parseChainPoint v
        InclusionProofResponse
            <$> v .: "txOut"
            <*> v .: "proof"
            <*> pure chainPointSlotNo
            <*> pure chainPointBlockHash

instance ToSchema ChainPoint where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        word64Schema <- declareSchemaRef (Proxy @Word64)
        return
            $ Swagger.NamedSchema (Just "ChainPoint")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("slotNo", word64Schema)
                    , ("blockHash", stringSchema)
                    ]
            & required .~ ["slotNo", "blockHash"]
            & description ?~ "A chain point with slot number and block hash."
instance ToSchema MerkleRootEntry where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        word64Schema <- declareSchemaRef (Proxy @Word64)
        maybeStringSchema <- declareSchemaRef (Proxy @(Maybe String))
        return
            $ Swagger.NamedSchema (Just "MerkleRootEntry")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("slotNo", word64Schema)
                    , ("blockHash", stringSchema)
                    , ("merkleRoot", maybeStringSchema)
                    ]
            & required .~ ["slotNo", "blockHash", "merkleRoot"]
            & description ?~ "A merkle root at a given block"

instance ToSchema InclusionProofResponse where
    declareNamedSchema _ = do
        stringSchema <- declareSchemaRef (Proxy @String)
        word64Schema <- declareSchemaRef (Proxy @Word64)
        return
            $ Swagger.NamedSchema (Just "InclusionProofResponse")
            $ mempty
            & Swagger.type_ ?~ Swagger.SwaggerObject
            & properties
                .~ fromList
                    [ ("txOut", stringSchema)
                    , ("proof", stringSchema)
                    , ("slotNo", word64Schema)
                    , ("blockHash", stringSchema)
                    ]
            & required .~ ["txOut", "proof", "slotNo", "blockHash"]
            & description
                ?~ "Inclusion proof, output, and chain point for the given transaction input."

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
