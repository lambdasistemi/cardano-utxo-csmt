{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Cardano.UTxOCSMT.E2E.GenesisChainSyncSpec
Description : E2E test for genesis bootstrap + chain sync
License     : Apache-2.0
-}
module Cardano.UTxOCSMT.E2E.GenesisChainSyncSpec
    ( spec
    ) where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Node.Client.E2E.Devnet
    ( withCardanoNode
    )
import Cardano.UTxOCSMT.Application.Database.Backend
    ( createBackend
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , DbState (..)
    , ReadyState (..)
    , RunTransaction (..)
    , mkCSMTOps
    , openCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    , newRunRocksDBTransactionUnguarded
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( SyncThreshold (..)
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( applicationN2C
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , context
    , prisms
    , slotHash
    , withRocksDB
    )
import Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    )
import ChainFollower.Backend qualified as Backend
import ChainFollower.Runner (Phase (..))
import Control.Concurrent.STM (newTVarIO)
import Control.Lens (iso)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as BL
import System.FilePath ((</>))
import System.IO.Temp
    ( withSystemTempDirectory
    )
import System.Timeout (timeout)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

genesisDir :: FilePath
genesisDir = "e2e-test/genesis"

shelleyGenesisPath :: FilePath
shelleyGenesisPath =
    genesisDir </> "shelley-genesis.json"

spec :: Spec
spec = describe "Genesis chain sync" $ do
    it "processes blocks from genesis without crashing"
        $ do
            let CSMTContext
                    { fromKV = fkv
                    , hashing = h
                    } = context
                ops = mkCSMTOps fkv h
            withCardanoNode genesisDir
                $ \socketPath _startMs -> do
                    withSystemTempDirectory
                        "e2e-db"
                        $ \dbPath -> do
                            withRocksDB dbPath
                                $ \db -> do
                                    runner <-
                                        newRunRocksDBTransaction
                                            db
                                            prisms
                                    let unguardedRunner =
                                            newRunRocksDBTransactionUnguarded
                                                db
                                                prisms
                                    SetupResult
                                        { setupStartingPoint
                                        , setupNetworkMagic
                                        , setupEpochSlots
                                        } <-
                                        setupDB
                                            nullTracer
                                            shelleyGenesisPath
                                            Nothing
                                            armageddonParams
                                            ops
                                            runner
                                    -- Open ops with crash recovery
                                    dbState <-
                                        openCSMTOps
                                            4
                                            1000
                                            ( iso
                                                BL.toStrict
                                                BL.fromStrict
                                            )
                                            fkv
                                            h
                                            (transact runner)
                                            ( transact
                                                unguardedRunner
                                            )
                                            (const $ pure ())
                                    let resolve
                                            (NeedsRecovery r) =
                                                r >>= resolve
                                        resolve
                                            ( Ready
                                                    ( ChooseKVOnly
                                                            kvOps
                                                        )
                                                ) =
                                                pure kvOps
                                        resolve
                                            (Ready (ChooseFull _)) =
                                                fail
                                                    "unexpected\
                                                    \ ChooseFull"
                                    kvOnlyOps <-
                                        resolve dbState
                                    let backendInit =
                                            createBackend
                                                nullTracer
                                                kvOnlyOps
                                                slotHash
                                    restoring <-
                                        Backend.start
                                            backendInit
                                    let initialPhase =
                                            InRestoration
                                                restoring
                                    notifyTVar <- newTVarIO (0 :: Int)
                                    result <-
                                        timeout
                                            15_000_000
                                            $ applicationN2C
                                                (SyncThreshold 100)
                                                ( EpochSlots
                                                    setupEpochSlots
                                                )
                                                setupNetworkMagic
                                                socketPath
                                                setupStartingPoint
                                                (\_ -> pure ())
                                                nullTracer
                                                nullTracer
                                                runner
                                                backendInit
                                                armageddonParams
                                                maxBound
                                                notifyTVar
                                                initialPhase
                                                [setupStartingPoint]
                                    result
                                        `shouldBe` Nothing
