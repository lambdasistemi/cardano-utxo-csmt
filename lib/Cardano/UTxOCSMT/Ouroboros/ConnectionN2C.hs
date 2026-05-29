{- |
Module      : Cardano.UTxOCSMT.Ouroboros.ConnectionN2C
Description : Node-to-client connection via Unix socket

This module handles connecting to a Cardano node using the
node-to-client protocol over a Unix domain socket. Only ChainSync
is active; LocalTxSubmission, LocalStateQuery, and LocalTxMonitor
are idle.
-}
module Cardano.UTxOCSMT.Ouroboros.ConnectionN2C
    ( runLocalNodeApplication
    , N2CChainSyncApplication
    ) where

import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Network.NodeToClient
    ( NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nullNetworkConnectTracers
    )
import Cardano.UTxOCSMT.Ouroboros.Codecs (codecChainSyncN2C)
import Cardano.UTxOCSMT.Ouroboros.Types (N2CChainSyncApplication)
import Control.Exception (SomeException)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Void (Void)
import Network.Mux qualified as Mx
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Version
    ( simpleSingletonVersions
    )
import Ouroboros.Network.Snocket (LocalAddress)

-- | Connect to a node via Unix socket and run the N2C ChainSync client
runLocalNodeApplication
    :: EpochSlots
    -- ^ Byron epoch slots for codec configuration
    -> NetworkMagic
    -- ^ The network magic
    -> FilePath
    -- ^ Path to the node's Unix socket
    -> N2CChainSyncApplication
    -- ^ N2C ChainSync client
    -> IO (Either SomeException ())
runLocalNodeApplication epochSlots magic socketPath chainSyncApp =
    withIOManager $ \ioManager ->
        connectTo
            (localSnocket ioManager)
            nullNetworkConnectTracers
            ( simpleSingletonVersions
                NodeToClientV_20
                NodeToClientVersionData
                    { networkMagic = magic
                    , query = False
                    }
                $ const
                $ mkN2CApplication epochSlots chainSyncApp
            )
            socketPath

-- | Maximum protocol limits for mini-protocols
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits
        { maximumIngressQueue = maxBound
        }

-- | Build the N2C application with only ChainSync active
mkN2CApplication
    :: EpochSlots
    -> N2CChainSyncApplication
    -> OuroborosApplicationWithMinimalCtx
        Mx.InitiatorMode
        LocalAddress
        LazyByteString
        IO
        ()
        Void
mkN2CApplication epochSlots chainSyncApp =
    OuroborosApplication
        { getOuroborosApplication =
            [ MiniProtocol
                { miniProtocolNum = MiniProtocolNum 5
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = maximumMiniProtocolLimits
                , miniProtocolRun = runChainSync
                }
            ]
        }
  where
    runChainSync =
        InitiatorProtocolOnly
            $ mkMiniProtocolCbFromPeer
            $ const
                ( nullTracer
                , codecChainSyncN2C epochSlots
                , ChainSync.chainSyncClientPeer chainSyncApp
                )
