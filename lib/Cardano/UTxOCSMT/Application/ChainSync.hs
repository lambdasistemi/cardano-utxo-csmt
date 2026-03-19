{- |
Module      : Cardano.UTxOCSMT.Application.ChainSync
Description : Chain synchronization client for Cardano node

This module implements the ChainSync mini-protocol client that connects
to a Cardano node and follows the chain. It handles:

* Finding intersection points with the local state
* Processing roll-forward events (new blocks)
* Processing roll-backward events (chain reorganizations)

The client uses a 'Follower' abstraction to decouple protocol handling
from actual database updates.
-}
module Cardano.UTxOCSMT.Application.ChainSync
    ( mkChainSyncApplication
    , ChainSyncApplication
    , Follower (..)
    , ProgressOrRewind (..)
    )
where

import Cardano.UTxOCSMT.Ouroboros.Types
    ( ChainSyncApplication
    , Follower (..)
    , Header
    , Intersector (..)
    , Point
    , ProgressOrRewind (..)
    , Tip
    )
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (Origin))
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )

-- The idle state of the chain sync client
type ChainSyncIdle = ClientStIdle Header Point Tip IO ()

{- | Create a ChainSync client application.

The client will attempt to find an intersection with the node's chain
starting from the provided points, then follow the chain forward.
-}
mkChainSyncApplication
    :: Tracer IO Header
    -- ^ Tracer for logging received headers
    -> Tracer IO SlotNo
    -- ^ Tracer for chain tip slot updates
    -> Intersector Point SlotNo Header
    -- ^ Callback handlers for intersection results
    -> [Point]
    -- ^ Starting points to find intersection (usually tip + finality)
    -> ChainSyncApplication
mkChainSyncApplication tracer tipTracer intersector startingPoints =
    ChainSyncClient
        $ pure
        $ intersect tracer tipTracer startingPoints intersector

intersect
    :: Tracer IO Header
    -> Tracer IO SlotNo
    -> [Point]
    -> Intersector Point SlotNo Header
    -> ClientStIdle Header Point Tip IO ()
intersect tracer tipTracer points Intersector{intersectFound, intersectNotFound} =
    SendMsgFindIntersect points
        $ ClientStIntersect
            { recvMsgIntersectFound = \point _ ->
                ChainSyncClient $ do
                    nextFollower <- intersectFound point
                    pure $ next tracer tipTracer nextFollower
            , recvMsgIntersectNotFound = \_ ->
                ChainSyncClient $ do
                    (intersector', points') <- intersectNotFound
                    pure $ intersect tracer tipTracer points' intersector'
            }

next
    :: Tracer IO Header
    -> Tracer IO SlotNo
    -> Follower Point SlotNo Header
    -> ChainSyncIdle
next tracer tipTracer follower = ($ follower)
    $ fix
    $ \go (Follower{rollForward, rollBackward}) ->
        let
            checkResult :: IO (ProgressOrRewind Point SlotNo Header) -> ChainSyncApplication
            checkResult getProgressOrRewind = ChainSyncClient $ do
                progressOrRewind <- getProgressOrRewind
                case progressOrRewind of
                    Progress follower' -> pure $ go follower'
                    Rewind points follower' ->
                        pure $ intersect tracer tipTracer points follower'
                    Reset intersector' ->
                        pure
                            $ intersect
                                tracer
                                tipTracer
                                [Network.Point Origin]
                                intersector'
        in
            SendMsgRequestNext
                (pure ()) -- spare time for other work
                ClientStNext
                    { recvMsgRollForward = \header tip -> do
                        checkResult $ do
                            traceWith tracer header
                            let tipSlot = getTipSlot tip
                            traceWith tipTracer tipSlot
                            Progress <$> rollForward header tipSlot
                    , recvMsgRollBackward = \point _ ->
                        checkResult $ rollBackward point
                    }

-- | Extract the slot from the chain tip (defaults to 0 for genesis)
getTipSlot :: Tip -> SlotNo
getTipSlot Network.TipGenesis = SlotNo 0
getTipSlot (Network.Tip slot _ _) = slot
