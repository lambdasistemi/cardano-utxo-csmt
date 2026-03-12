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
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( allOps
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( createUpdateState
    , newRunRocksDBTransaction
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
import Cardano.UTxOCSMT.Mithril.Client
    ( MithrilNetwork (..)
    )
import Cardano.UTxOCSMT.Mithril.Options
    ( MithrilOptions (..)
    )
import Control.Tracer (nullTracer)
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import Ouroboros.Network.Point (WithOrigin (..))
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

devnetMagic :: NetworkMagic
devnetMagic = NetworkMagic 42

originPoint :: Network.Point block
originPoint = Network.Point Origin

disabledMithril :: MithrilOptions
disabledMithril =
    MithrilOptions
        { mithrilEnabled = False
        , mithrilBootstrapOnly = False
        , mithrilNetwork = MithrilPreprod
        , mithrilAggregatorUrl = Nothing
        , mithrilGenesisVk = Nothing
        , mithrilClientPath = "mithril-client"
        , mithrilDownloadDir = Nothing
        , mithrilAncillaryVk = Nothing
        , mithrilSkipAncillaryVerification =
            False
        }

spec :: Spec
spec = describe "Genesis chain sync" $ do
    it "processes blocks from genesis without crashing"
        $ do
            let CSMTContext{fromKV = fkv, hashing = h} =
                    context
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
                                    SetupResult
                                        { setupStartingPoint
                                        } <-
                                        setupDB
                                            nullTracer
                                            originPoint
                                            shelleyGenesisPath
                                            Nothing
                                            disabledMithril
                                            devnetMagic
                                            "localhost"
                                            0
                                            True
                                            armageddonParams
                                            ops
                                            runner
                                    (state, slots) <-
                                        createUpdateState
                                            nullTracer
                                            allOps
                                            ops
                                            slotHash
                                            (\_ _ -> pure ())
                                            armageddonParams
                                            runner
                                            maxBound
                                    result <-
                                        timeout
                                            15_000_000
                                            $ applicationN2C
                                                (EpochSlots 4320)
                                                devnetMagic
                                                socketPath
                                                setupStartingPoint
                                                (\_ -> pure ())
                                                Nothing
                                                nullTracer
                                                nullTracer
                                                state
                                                slots
                                    result
                                        `shouldBe` Nothing
