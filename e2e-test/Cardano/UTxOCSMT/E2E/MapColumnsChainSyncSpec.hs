{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Cardano.UTxOCSMT.E2E.MapColumnsChainSyncSpec
Description : E2E chain sync through mapColumns
License     : Apache-2.0

Same chain sync test as 'GenesisChainSyncSpec' but
routes all CSMT operations through 'mapColumns',
reproducing the pattern used by downstream consumers
that embed CSMT columns inside a wider GADT.
-}
module Cardano.UTxOCSMT.E2E.MapColumnsChainSyncSpec
    ( spec
    ) where

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Node.Client.E2E.Devnet
    ( withCardanoNode
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , codecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( createUpdateState
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( applicationN2C
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , context
    , mFinality
    , prisms
    , slotHash
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
import Control.Lens (prism', type (:~:) (Refl))
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import Data.Dependent.Map qualified as DMap
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction
    ( Codecs (..)
    , DSum ((:=>))
    , GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , mapColumns
    , newRunTransaction
    )
import Database.KV.Transaction qualified as L
import Database.RocksDB
    ( Config (..)
    , DB (..)
    , withDBCF
    )
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

-- | Wider column GADT wrapping CSMT 'Columns'
-- plus one extra dummy column.
data WrappedColumns slot hash key value x where
    InCsmt
        :: Columns slot hash key value x
        -> WrappedColumns slot hash key value x
    ExtraCol
        :: WrappedColumns
            slot
            hash
            key
            value
            (KV ByteString ByteString)

instance GEq (WrappedColumns slot hash key value) where
    geq (InCsmt a) (InCsmt b) = case geq a b of
        Just Refl -> Just Refl
        Nothing -> Nothing
    geq ExtraCol ExtraCol = Just Refl
    geq _ _ = Nothing

instance
    GCompare (WrappedColumns slot hash key value)
    where
    gcompare (InCsmt a) (InCsmt b) =
        case gcompare a b of
            GLT -> GLT
            GEQ -> GEQ
            GGT -> GGT
    gcompare (InCsmt _) ExtraCol = GLT
    gcompare ExtraCol (InCsmt _) = GGT
    gcompare ExtraCol ExtraCol = GEQ

genesisDir :: FilePath
genesisDir = "e2e-test/genesis"

shelleyGenesisPath :: FilePath
shelleyGenesisPath =
    genesisDir </> "shelley-genesis.json"

devnetMagic :: NetworkMagic
devnetMagic = NetworkMagic 42

originPoint :: Network.Point block
originPoint = Network.Point Origin

dbConfig :: Config
dbConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

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
spec = describe "mapColumns chain sync" $ do
    it "processes blocks through mapColumns without crashing"
        $ do
            let CSMTContext{fromKV = fkv, hashing = h} =
                    context
                ops = mkCSMTOps fkv h
            withCardanoNode genesisDir
                $ \socketPath _startMs -> do
                    withSystemTempDirectory
                        "e2e-db-wrapped"
                        $ \dbPath -> do
                            -- Open DB with 5 CFs:
                            -- 4 CSMT + 1 extra
                            withDBCF
                                dbPath
                                dbConfig
                                [ ("kv", dbConfig)
                                , ("csmt", dbConfig)
                                , ( "rollbacks"
                                  , dbConfig
                                  )
                                , ("config", dbConfig)
                                , ("extra", dbConfig)
                                ]
                                $ \db -> do
                                    -- Build unified codecs
                                    let csmtCods =
                                            codecs prisms
                                        extraCod =
                                            Codecs
                                                { keyCodec =
                                                    prism' id Just
                                                , valueCodec =
                                                    prism' id Just
                                                }
                                        wCodecs =
                                            DMap.fromList
                                                $ ( ExtraCol
                                                        :=> extraCod
                                                  )
                                                    : [ InCsmt col
                                                            :=> cod
                                                      | col :=> cod <-
                                                            DMap.toAscList
                                                                csmtCods
                                                      ]

                                    -- Build unified DB
                                    let wCols =
                                            mkColumns
                                                ( columnFamilies
                                                    db
                                                )
                                                wCodecs
                                        wDb =
                                            mkRocksDBDatabase
                                                db
                                                wCols
                                    L.RunTransaction
                                        run <-
                                        newRunTransaction
                                            wDb

                                    -- Project into
                                    -- CSMT columns via
                                    -- mapColumns
                                    let csmtRunner =
                                            RunTransaction
                                                ( run
                                                    . mapColumns
                                                        InCsmt
                                                )

                                    -- Setup DB
                                    SetupResult
                                        { setupStartingPoint
                                        } <-
                                        setupDB
                                            nullTracer
                                            originPoint
                                            ( Just
                                                shelleyGenesisPath
                                            )
                                            Nothing
                                            disabledMithril
                                            devnetMagic
                                            "localhost"
                                            0
                                            True
                                            armageddonParams
                                            ops
                                            csmtRunner

                                    -- Create update
                                    -- state through
                                    -- mapColumns
                                    (state, slots) <-
                                        createUpdateState
                                            nullTracer
                                            ops
                                            slotHash
                                            ( \_ _ ->
                                                pure ()
                                            )
                                            armageddonParams
                                            csmtRunner

                                    result <-
                                        timeout
                                            15_000_000
                                            $ applicationN2C
                                                ( EpochSlots
                                                    4320
                                                )
                                                devnetMagic
                                                socketPath
                                                setupStartingPoint
                                                ( \_ ->
                                                    pure ()
                                                )
                                                Nothing
                                                nullTracer
                                                nullTracer
                                                state
                                                slots
                                                ( mFinality
                                                    csmtRunner
                                                )
                                    result
                                        `shouldBe` Nothing
