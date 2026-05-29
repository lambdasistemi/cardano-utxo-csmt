{- |
Module      : Cardano.UTxOCSMT.Ouroboros.Connection
Description : Node-to-node connection establishment

This module handles connecting to a Cardano node using the node-to-node
protocol. It performs the version handshake and sets up the multiplexed
connection for running ChainSync, BlockFetch, and KeepAlive protocols.
-}
module Cardano.UTxOCSMT.Ouroboros.Connection
    ( runNodeApplication
    , validateNodeConnection
    , NodeConnectionError (..)
    , ChainSyncApplication
    , BlockFetchApplication
    )
where

import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Network.NodeToNode
    ( DiffusionMode (InitiatorOnlyDiffusionMode)
    , NodeToNodeVersion (NodeToNodeV_14)
    , NodeToNodeVersionData (..)
    , nodeToNodeCodecCBORTerm
    , nodeToNodeHandshakeCodec
    )
import Cardano.UTxOCSMT.Application.BlockFetch
    ( BlockFetchApplication
    )
import Cardano.UTxOCSMT.Application.KeepAlive (keepAliveApplication)
import Cardano.UTxOCSMT.Ouroboros.Application
    ( mkOuroborosApplication
    )
import Cardano.UTxOCSMT.Ouroboros.Types (ChainSyncApplication)
import Control.Exception (SomeException, catch, displayException)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)
import Network.Socket
    ( AddrInfo (..)
    , AddrInfoFlag (AI_PASSIVE)
    , PortNumber
    , SocketType (Stream)
    , close
    , connect
    , defaultHints
    , getAddrInfo
    , socket
    )
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Diffusion.Configuration
    ( PeerSharing (PeerSharingDisabled)
    )
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Protocol.Handshake.Codec
    ( cborTermVersionDataCodec
    , noTimeLimitsHandshake
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( Acceptable (acceptableVersion)
    , Queryable (queryVersion)
    , simpleSingletonVersions
    )
import Ouroboros.Network.Snocket
    ( makeSocketBearer
    , socketSnocket
    )
import Ouroboros.Network.Socket
    ( ConnectToArgs (..)
    , HandshakeCallbacks (..)
    , connectToNode
    , nullNetworkConnectTracers
    )
import System.Timeout (timeout)

-- | Connect to a node-to-node chain sync server and run the given application
runNodeApplication
    :: EpochSlots
    -- ^ Byron epoch slots for codec configuration
    -> NetworkMagic
    -- ^ The network magic
    -> String
    -- ^ host
    -> PortNumber
    -- ^ port
    -> ChainSyncApplication
    -- ^ application
    -> BlockFetchApplication
    -- ^ application
    -> IO (Either SomeException (Either () Void))
runNodeApplication
    epochSlots
    magic
    peerName
    peerPort
    chainSyncApplication
    blockFetchApplication = withIOManager $ \iocp -> do
        AddrInfo{addrAddress} <- resolve peerName peerPort
        connectToNode -- withNode
            (socketSnocket iocp) -- TCP
            makeSocketBearer
            ConnectToArgs
                { ctaHandshakeCodec = nodeToNodeHandshakeCodec
                , ctaHandshakeTimeLimits = noTimeLimitsHandshake
                , ctaVersionDataCodec =
                    cborTermVersionDataCodec
                        nodeToNodeCodecCBORTerm
                , ctaConnectTracers = nullNetworkConnectTracers
                , ctaHandshakeCallbacks =
                    HandshakeCallbacks
                        { acceptCb = acceptableVersion
                        , queryCb = queryVersion
                        }
                }
            mempty -- socket options
            ( simpleSingletonVersions
                NodeToNodeV_14
                ( NodeToNodeVersionData
                    { networkMagic = magic
                    , diffusionMode = InitiatorOnlyDiffusionMode
                    , peerSharing = PeerSharingDisabled
                    , query = False
                    }
                )
                ( const
                    $ mkOuroborosApplication
                        epochSlots
                        chainSyncApplication
                        blockFetchApplication
                        keepAliveApplication
                )
            )
            Nothing
            addrAddress

resolve :: String -> PortNumber -> IO AddrInfo
resolve peerName peerPort = do
    let hints =
            defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
    NE.head
        <$> getAddrInfo (Just hints) (Just peerName) (Just $ show peerPort)

-- | Errors that can occur when validating a node connection.
data NodeConnectionError
    = -- | Failed to resolve the hostname
      NodeResolutionFailed String
    | -- | Failed to connect to the node (connection refused, etc.)
      NodeConnectionFailed String
    | -- | Connection attempt timed out
      NodeConnectionTimeout
    deriving stock (Show, Eq)

{- | Validate that a node is reachable before starting expensive operations.

Attempts a TCP connection to the node with a timeout. This is a lightweight
check that verifies network reachability without performing a full protocol
handshake.
-}
validateNodeConnection
    :: NetworkMagic
    -- ^ Network magic (unused, for future handshake validation)
    -> String
    -- ^ Hostname
    -> PortNumber
    -- ^ Port
    -> Int
    -- ^ Timeout in microseconds
    -> IO (Either NodeConnectionError ())
validateNodeConnection _magic hostName portNum timeoutUs = do
    result <- timeout timeoutUs tryConnect
    case result of
        Nothing -> pure $ Left NodeConnectionTimeout
        Just (Left err) -> pure $ Left err
        Just (Right ()) -> pure $ Right ()
  where
    tryConnect :: IO (Either NodeConnectionError ())
    tryConnect = do
        addrResult <-
            (Right <$> resolve hostName portNum)
                `catch` \(e :: SomeException) ->
                    pure $ Left $ NodeResolutionFailed $ displayException e
        case addrResult of
            Left err -> pure $ Left err
            Right AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress} ->
                ( do
                    sock <- socket addrFamily addrSocketType addrProtocol
                    connect sock addrAddress
                        `catch` \(e :: SomeException) -> do
                            close sock
                            ioError $ userError $ displayException e
                    close sock
                    pure $ Right ()
                )
                    `catch` \(e :: SomeException) ->
                        pure $ Left $ NodeConnectionFailed $ displayException e
