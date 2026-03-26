module Cardano.UTxOCSMT.Application.Database.Backend
    ( -- * Backend Init
      createBackend

      -- * Re-exports for Application.hs
    , Backend.Init (..)
    , Runner.Phase (..)
    , Runner.processBlock
    , Runner.rollbackTo
    , Runner.rollbackCount
    , Store.RollbackResult (..)
    , Store.queryTip
    , Store.armageddonCleanup
    , Store.armageddonSetup
    , Store.queryHistory
    ) where

-- \|
-- Module      : Cardano.UTxOCSMT.Application.Database.Backend
-- Description : Chain follower backend for UTxO CSMT
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Builds a 'Backend.Init' for the UTxO CSMT application
-- using the chain-follower Runner API. This replaces the
-- old Update\/State\/SplitMode machinery.
--
-- The backend provides:
--
-- \* __Restoring__: KVOnly CSMT operations (no inverses)
-- \* __Following__: Full CSMT operations with inverse
--   computation and merkle root metadata
--
-- The Runner handles all rollback infrastructure:
-- storing rollback points, rollbackTo, pruning.

import CSMT.MTS (Ops, toFull)
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( fullOpsToCSMTOps
    , kvCommon
    , kvCommonToCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
import ChainFollower.Backend qualified as Backend
import ChainFollower.Rollbacks.Store qualified as Store
import ChainFollower.Runner qualified as Runner
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.KV.Transaction
    ( Transaction
    , query
    )
import MTS.Interface qualified as MTS (Mode (..))

-- | Create the backend Init for UTxO CSMT.
createBackend
    :: ( Ord key
       , Show slot
       , MonadFail m
       , MonadIO m
       )
    => Ops
        'MTS.KVOnly
        m
        cf
        (Columns slot hash key value)
        op
        key
        value
        hash
    -> (slot -> hash)
    -> Backend.Init
        m
        ( Transaction
            m
            cf
            (Columns slot hash key value)
            op
        )
        (slot, [Operation key value])
        [Operation key value]
        (hash, Maybe hash)
createBackend ops slotHash =
    Backend.Init
        { Backend.startRestoring =
            pure $ mkRestoring kvCSMTOps
        , Backend.resumeFollowing = do
            mFull <- liftIO (toFull ops)
            case mFull of
                Just fullOps ->
                    pure
                        $ mkFollowing
                            (fullOpsToCSMTOps fullOps)
                Nothing ->
                    fail
                        "createBackend: toFull failed"
        }
  where
    kvCSMTOps = kvCommonToCSMTOps (kvCommon ops)

    mkRestoring csmtOps =
        Backend.Restoring
            { Backend.restore =
                \(_slot, operations) -> do
                    forM_ operations $ \case
                        Insert k v ->
                            csmtInsert csmtOps k v
                        Delete k ->
                            csmtDelete csmtOps k
                    pure $ mkRestoring csmtOps
            , Backend.toFollowing = do
                mFull <- liftIO (toFull ops)
                case mFull of
                    Just fullOps ->
                        pure
                            $ mkFollowing
                                (fullOpsToCSMTOps fullOps)
                    Nothing ->
                        fail
                            "mkRestoring: toFull failed"
            }

    mkFollowing csmtOps =
        Backend.Following
            { Backend.follow =
                \(slot, operations) -> do
                    invs <-
                        forM operations $ \case
                            Insert k v -> do
                                csmtInsert csmtOps k v
                                pure [Delete k]
                            Delete k -> do
                                mx <- query KVCol k
                                csmtDelete csmtOps k
                                case mx of
                                    Nothing ->
                                        error
                                            $ "Backend.follow:"
                                                <> " cannot invert"
                                                <> " Delete at slot "
                                                <> show slot
                                    Just x ->
                                        pure [Insert k x]
                    let inverseOps =
                            reverse (concat invs)
                    merkleRoot <-
                        csmtRootHash csmtOps
                    pure
                        ( inverseOps
                        , Just
                            ( slotHash slot
                            , merkleRoot
                            )
                        , mkFollowing csmtOps
                        )
            , Backend.toRestoring =
                pure $ mkRestoring kvCSMTOps
            , Backend.applyInverse =
                \inverseOps ->
                    forM_ inverseOps $ \case
                        Insert k v ->
                            csmtInsert csmtOps k v
                        Delete k ->
                            csmtDelete csmtOps k
            }
