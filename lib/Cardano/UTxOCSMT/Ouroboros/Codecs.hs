module Cardano.UTxOCSMT.Ouroboros.Codecs
    ( codecChainSync
    , codecBlockFetch
    , codecKeepAlive
    , codecChainSyncN2C
    ) where

import Cardano.Chain.Slotting (EpochSlots)
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , BlockFetch
    , ChainSync
    , Header
    , N2CChainSync
    , Point
    )
import Codec.Serialise (DeserialiseFailure, Serialise (..))
import Codec.Serialise.Decoding (Decoder)
import Codec.Serialise.Encoding (Encoding)
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Block.Abstract
    ( decodeRawHash
    , encodeRawHash
    )
import Ouroboros.Consensus.Byron.Ledger (ByronBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block
    ( CodecConfig (CardanoCodecConfig)
    )
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node
    ( pattern CardanoNodeToClientVersion16
    , pattern CardanoNodeToNodeVersion2
    )
import Ouroboros.Consensus.HardFork.Combinator.NetworkVersion
    ( HardForkNodeToClientVersion
    , HardForkNodeToNodeVersion
    )
import Ouroboros.Consensus.Node.Serialisation
    ( decodeNodeToClient
    , decodeNodeToNode
    , encodeNodeToClient
    , encodeNodeToNode
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger
    ( CodecConfig (ShelleyCodecConfig)
    )
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
    ( decodeTip
    , encodeTip
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Protocol.BlockFetch.Codec qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Codec qualified as ChainSync
import Ouroboros.Network.Protocol.KeepAlive.Codec qualified as KeepAlive
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)

-- The ChainSync codec for our Block, Point, and Tip types
codecChainSync
    :: EpochSlots
    -> Codec
        ChainSync
        DeserialiseFailure
        IO
        LBS.ByteString
codecChainSync es =
    ChainSync.codecChainSync
        (encHeader es)
        (decHeader es)
        encPoint
        decPoint
        encTip
        decTip

----- Encoding and Decoding Headers -----
encHeader :: EpochSlots -> Header -> Encoding
encHeader es = encodeNodeToNode @Block (ccfg es) version

decHeader :: EpochSlots -> Decoder s Header
decHeader es = decodeNodeToNode @Block (ccfg es) version

version
    :: HardForkNodeToNodeVersion
        (ByronBlock : Consensus.CardanoShelleyEras c)
version = CardanoNodeToNodeVersion2

ccfg :: EpochSlots -> Consensus.CardanoCodecConfig c
ccfg es =
    CardanoCodecConfig
        (ByronCodecConfig es)
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig

--- Encoding and Decoding Points -----
encPoint :: Point -> Encoding
encPoint = encode
decPoint :: Decoder s Point
decPoint = decode

--- Encoding and Decoding Tips -----
encTip :: Network.Tip Block -> Encoding
encTip = encodeTip (encodeRawHash (Proxy @Block))
decTip :: Decoder s (Network.Tip Block)
decTip = decodeTip (decodeRawHash (Proxy @Block))

-- | Real Cardano BlockFetch codec
codecBlockFetch
    :: EpochSlots
    -> Codec BlockFetch DeserialiseFailure IO LBS.ByteString
codecBlockFetch es =
    BlockFetch.codecBlockFetch
        (encBlock es)
        (decBlock es)
        encPoint
        decPoint

-- ----- Encoding and Decoding Blocks -----
encBlock :: EpochSlots -> Block -> Encoding
encBlock es = encodeNodeToNode @Block (ccfg es) version

decBlock :: EpochSlots -> Decoder s Block
decBlock es = decodeNodeToNode @Block (ccfg es) version

codecKeepAlive
    :: Codec
        KeepAlive
        DeserialiseFailure
        IO
        LBS.ByteString
codecKeepAlive = KeepAlive.codecKeepAlive_v2

----- N2C (node-to-client) codecs -----

n2cVersion
    :: HardForkNodeToClientVersion
        (ByronBlock : Consensus.CardanoShelleyEras c)
n2cVersion = CardanoNodeToClientVersion16

-- | N2C ChainSync codec — encodes/decodes full blocks (not headers)
codecChainSyncN2C
    :: EpochSlots
    -> Codec
        N2CChainSync
        DeserialiseFailure
        IO
        LBS.ByteString
codecChainSyncN2C es =
    ChainSync.codecChainSync
        (encBlockN2C es)
        (decBlockN2C es)
        encPointBlock
        decPointBlock
        encTip
        decTip

encBlockN2C :: EpochSlots -> Block -> Encoding
encBlockN2C es = encodeNodeToClient @Block (ccfg es) n2cVersion

decBlockN2C :: EpochSlots -> Decoder s Block
decBlockN2C es = decodeNodeToClient @Block (ccfg es) n2cVersion

encPointBlock :: Network.Point Block -> Encoding
encPointBlock = encode

decPointBlock :: Decoder s (Network.Point Block)
decPointBlock = decode
