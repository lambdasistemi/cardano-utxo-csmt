{- |
Module      : Cardano.UTxOCSMT.Ouroboros.Types
Description : Type aliases for Ouroboros network protocols

This module defines type aliases for working with Cardano's Ouroboros
network protocols. It provides concrete types for:

* Blocks, headers, and chain points
* ChainSync and BlockFetch protocol clients
* Follower/Intersector callbacks for chain following

These aliases simplify working with the complex generic types from
ouroboros-network.
-}
module Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , Header
    , HeaderHash
    , Tip
    , Point
    , BlockFetchApplication
    , ChainSyncApplication
    , KeepAliveApplication
    , ChainSync
    , BlockFetch

      -- * N2C (node-to-client) types
    , N2CChainSync
    , N2CChainSyncApplication

      -- * Type family
    , TipOf

      -- * Follower/Intersector
    , Follower (..)
    , ProgressOrRewind (..)
    , Intersector (..)
    ) where

import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient)
import Ouroboros.Network.Protocol.BlockFetch.Type qualified as BlockFetch
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient)
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.KeepAlive.Client (KeepAliveClient)

-- | Real Cardano Block type
type Block = Consensus.CardanoBlock Consensus.StandardCrypto

-- | Real Cardano Header type
type Header = Consensus.CardanoHeader Consensus.StandardCrypto

-- | Real Cardano Tip type
type Tip = Network.Tip Block

-- | Real Cardano Point type
type Point = Network.Point Header

-- | Type family mapping a slot type to its chain tip type
type family TipOf slot

-- | The chain tip for a 'Point' is a 'SlotNo'
type instance TipOf Point = Network.SlotNo

-- | Trivial tip for unit-slot benchmarks
type instance TipOf () = ()

-- | The header hash type used in the chain sync connection
type HeaderHash = Network.HeaderHash Block

-- | Type alias for BlockFetch application
type BlockFetchApplication = BlockFetchClient Block Point IO ()

-- | Type alias for ChainSync application
type ChainSyncApplication = ChainSyncClient Header Point Tip IO ()

-- | Type alias for KeepAlive protocol
type KeepAliveApplication = KeepAliveClient IO ()

-- | Type alias for ChainSync protocol
type ChainSync = ChainSync.ChainSync Header Point Tip

-- | Type alias for BlockFetch protocol
type BlockFetch = BlockFetch.BlockFetch Block Point

-- | N2C ChainSync uses Block (not Header) and Block-based Tip
type N2CChainSync =
    ChainSync.ChainSync Block (Network.Point Block) (Network.Tip Block)

-- | N2C ChainSync client application
type N2CChainSyncApplication =
    ChainSyncClient Block (Network.Point Block) (Network.Tip Block) IO ()

data ProgressOrRewind h
    = Progress (Follower h)
    | Rewind [Point] (Intersector h)
    | Reset (Intersector h)

-- | An event representing a roll forward or roll backward in the chain
data Follower h = Follower
    { rollForward :: h -> Network.SlotNo -> IO (Follower h)
    -- ^ Roll forward with header/block and current chain tip slot
    , rollBackward :: Point -> IO (ProgressOrRewind h)
    }

data Intersector h = Intersector
    { intersectFound :: Point -> IO (Follower h)
    , intersectNotFound :: IO (Intersector h, [Point])
    }
