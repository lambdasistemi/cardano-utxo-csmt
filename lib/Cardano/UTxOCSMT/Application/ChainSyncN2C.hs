{- |
Module      : Cardano.UTxOCSMT.Application.ChainSyncN2C
Description : N2C ChainSync client receiving full blocks

Node-to-client ChainSync delivers full blocks directly (not headers),
so there is no need for a separate BlockFetch protocol. This module
provides a ChainSync client that feeds blocks directly to the
existing 'Follower'/'Intersector' abstractions.
-}
module Cardano.UTxOCSMT.Application.ChainSyncN2C
    ( mkN2CChainSyncApplication
    ) where

import Cardano.UTxOCSMT.Application.BlockFetch
    ( Fetched (..)
    , HeaderSkipProgress (..)
    )
import Cardano.UTxOCSMT.Ouroboros.Types
    ( Block
    , Follower (..)
    , Intersector (..)
    , N2CChainSyncApplication
    , Point
    , ProgressOrRewind (..)
    )
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block
    ( SlotNo
    , blockPoint
    , blockSlot
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Point (WithOrigin (At, Origin))
import Ouroboros.Network.Point qualified as Network.Point
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )

-- | N2C ChainSync idle state (block-level, not header-level)
type N2CIdle =
    ClientStIdle
        Block
        (Network.Point Block)
        (Network.Tip Block)
        IO
        ()

{- | Create an N2C ChainSync client that receives full blocks.

This is simpler than the N2N path: blocks go directly from
ChainSync to the follower, with no queue or BlockFetch protocol.
-}
mkN2CChainSyncApplication
    :: Tracer IO Block
    -- ^ Tracer for received blocks
    -> Tracer IO SlotNo
    -- ^ Tracer for chain tip slot updates
    -> Tracer IO HeaderSkipProgress
    -- ^ Tracer for skip progress during Mithril catch-up
    -> (Point -> IO ())
    -- ^ Action to set the base checkpoint
    -> IO ()
    -- ^ Action to call when skip phase completes
    -> Maybe SlotNo
    -- ^ Optional skip until slot for Mithril bootstrap
    -> Intersector Point SlotNo Fetched
    -- ^ Callback for intersection/following
    -> [Point]
    -- ^ Starting points to find intersection
    -> N2CChainSyncApplication
mkN2CChainSyncApplication
    blockTracer
    tipTracer
    skipProgressTracer
    setCheckpoint
    onSkipComplete
    mSkipTargetSlot
    blockIntersector
    startingPoints =
        ChainSyncClient
            $ pure
            $ n2cIntersect
                (coercePoints startingPoints)
                blockIntersector
      where
        n2cIntersect
            :: [Network.Point Block]
            -> Intersector Point SlotNo Fetched
            -> N2CIdle
        n2cIntersect points Intersector{intersectFound, intersectNotFound} =
            SendMsgFindIntersect points
                $ ClientStIntersect
                    { recvMsgIntersectFound = \point _ ->
                        ChainSyncClient $ do
                            nextFollower <- intersectFound (uncoercePoint point)
                            pure $ n2cFollow nextFollower mSkipTargetSlot
                    , recvMsgIntersectNotFound = \_ ->
                        ChainSyncClient $ do
                            (intersector', points') <- intersectNotFound
                            pure
                                $ n2cIntersect
                                    (coercePoints points')
                                    intersector'
                    }

        n2cFollow
            :: Follower Point SlotNo Fetched
            -> Maybe SlotNo
            -- \^ Skip target (Nothing = normal operation)
            -> N2CIdle
        n2cFollow initFollower initSkip = ($ (initFollower, initSkip))
            $ fix
            $ \go (Follower{rollForward, rollBackward}, mTarget) ->
                let
                    checkResult
                        :: Maybe SlotNo
                        -> IO (ProgressOrRewind Point SlotNo Fetched)
                        -> N2CChainSyncApplication
                    checkResult nextTarget getProgressOrRewind =
                        ChainSyncClient $ do
                            progressOrRewind <- getProgressOrRewind
                            case progressOrRewind of
                                Progress follower' ->
                                    pure $ go (follower', nextTarget)
                                Rewind points intersector' ->
                                    pure
                                        $ n2cIntersect
                                            (coercePoints points)
                                            intersector'
                                Reset intersector' ->
                                    pure
                                        $ n2cIntersect
                                            [Network.Point Origin]
                                            intersector'
                in
                    SendMsgRequestNext
                        (pure ())
                        ClientStNext
                            { recvMsgRollForward = \block tip ->
                                let slot = blockSlot block
                                    point =
                                        uncoercePoint (blockPoint block)
                                    tipSlot = tipToSlot tip
                                    processBlock target = checkResult target
                                        $ do
                                            traceWith blockTracer block
                                            Progress
                                                <$> rollForward
                                                    Fetched
                                                        { fetchedPoint = point
                                                        , fetchedBlock = block
                                                        , fetchedTip = tipSlot
                                                        }
                                                    tipSlot
                                in  ChainSyncClient $ do
                                        traceTipSlot tip
                                        case mTarget of
                                            Just targetSlot
                                                | slot < targetSlot -> do
                                                    traceWith
                                                        skipProgressTracer
                                                        HeaderSkipProgress
                                                            { skipCurrentSlot =
                                                                slot
                                                            , skipTargetSlot =
                                                                targetSlot
                                                            }
                                                    pure
                                                        $ go
                                                            ( Follower
                                                                { rollForward
                                                                , rollBackward
                                                                }
                                                            , mTarget
                                                            )
                                                | otherwise -> do
                                                    setCheckpoint point
                                                    onSkipComplete
                                                    let ChainSyncClient r =
                                                            processBlock
                                                                Nothing
                                                    r
                                            _ -> do
                                                let ChainSyncClient r =
                                                        processBlock mTarget
                                                r
                            , recvMsgRollBackward = \point _ ->
                                checkResult mTarget
                                    $ rollBackward (uncoercePoint point)
                            }

        traceTipSlot :: Network.Tip Block -> IO ()
        traceTipSlot tip = case tip of
            Network.TipGenesis -> pure ()
            Network.Tip slot _ _ -> traceWith tipTracer slot

        tipToSlot :: Network.Tip Block -> SlotNo
        tipToSlot Network.TipGenesis = 0
        tipToSlot (Network.Tip slot _ _) = slot

-- | Coerce Point Header to Point Block (same HeaderHash)
coercePoints :: [Point] -> [Network.Point Block]
coercePoints = fmap coercePoint

coercePoint :: Point -> Network.Point Block
coercePoint (Network.Point Origin) = Network.Point Origin
coercePoint (Network.Point (At (Network.Point.Block s h))) =
    Network.Point (At (Network.Point.Block s h))

-- | Coerce Point Block back to Point Header
uncoercePoint :: Network.Point Block -> Point
uncoercePoint (Network.Point Origin) = Network.Point Origin
uncoercePoint (Network.Point (At (Network.Point.Block s h))) =
    Network.Point (At (Network.Point.Block s h))
