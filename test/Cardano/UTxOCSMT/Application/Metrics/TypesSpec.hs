module Cardano.UTxOCSMT.Application.Metrics.TypesSpec (spec) where

import Cardano.UTxOCSMT.Application.Metrics.Types
    ( Metrics (..)
    , SyncPhase (..)
    , renderPrometheus
    )
import Data.Text qualified as T
import Ouroboros.Network.Block (SlotNo (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Gen, choose, elements, forAll)

genNonNegInt :: Gen Int
genNonNegInt = abs <$> choose (0, 1000000)

defaultMetrics :: Metrics
defaultMetrics =
    Metrics
        { averageQueueLength = 0
        , maxQueueLength = Nothing
        , utxoChangesCount = 0
        , lastBlockPoint = Nothing
        , utxoSpeed = 0
        , blockSpeed = 0
        , currentEra = Nothing
        , currentMerkleRoot = Nothing
        , baseCheckpoint = Nothing
        , chainTipSlot = Nothing
        , syncPhase = Nothing
        , avgCSMTDuration = 0
        , avgRollbackDuration = 0
        , avgFinalityDuration = 0
        , avgBlockDecodeDuration = 0
        , avgTransactionDuration = 0
        , avgTotalBlockDuration = 0
        , cumulativeBlocks = 0
        , cumulativeBlockDecodeDuration = 0
        , cumulativeTransactionDuration = 0
        , cumulativeCSMTDuration = 0
        , cumulativeRollbackDuration = 0
        , cumulativeFinalityDuration = 0
        , cumulativeTotalBlockDuration = 0
        , cumulativeInternalQueryTip = 0
        , cumulativeInternalCsmtOps = 0
        , cumulativeInternalRollbackStore = 0
        }

spec :: Spec
spec = describe "Metrics.Types" $ do
    describe "SyncPhase" $ do
        it "show produces valid strings" $ do
            forAll (elements [Restoring, Following, Synced]) $ \phase ->
                show phase `shouldSatisfy` (not . null)

        it "Following /= Synced" $ do
            Following `shouldSatisfy` (/= Synced)

    describe "renderPrometheus" $ do
        it "contains prefix cardano_utxo_csmt" $ do
            let rendered = renderPrometheus defaultMetrics
            T.isInfixOf "cardano_utxo_csmt" rendered `shouldBe` True

        it "contains ready metric" $ do
            let rendered = renderPrometheus defaultMetrics
            T.isInfixOf "ready" rendered `shouldBe` True

        it "ready=1 when synced" $ do
            let m = defaultMetrics{syncPhase = Just Synced}
                rendered = renderPrometheus m
            T.isInfixOf "cardano_utxo_csmt_ready 1" rendered `shouldBe` True

        it "ready=0 when following" $ do
            let m = defaultMetrics{syncPhase = Just Following}
                rendered = renderPrometheus m
            T.isInfixOf "cardano_utxo_csmt_ready 0" rendered `shouldBe` True

        it "includes utxo_changes_total" $ do
            forAll genNonNegInt $ \n ->
                let m = defaultMetrics{utxoChangesCount = n}
                    rendered = renderPrometheus m
                in  T.isInfixOf "utxo_changes_total" rendered `shouldBe` True

        it "includes chain_tip_slot when present" $ do
            forAll (choose (0, maxBound)) $ \slot ->
                let m = defaultMetrics{chainTipSlot = Just (SlotNo slot)}
                    rendered = renderPrometheus m
                in  T.isInfixOf "chain_tip_slot" rendered `shouldBe` True

        it "omits chain_tip_slot when absent" $ do
            let rendered = renderPrometheus defaultMetrics
            T.isInfixOf "chain_tip_slot" rendered `shouldBe` False

        it "includes blocks_total" $ do
            forAll genNonNegInt $ \n ->
                let m = defaultMetrics{cumulativeBlocks = n}
                    rendered = renderPrometheus m
                in  T.isInfixOf "blocks_total" rendered `shouldBe` True

        it "cumulative counters appear in output" $ do
            let m =
                    defaultMetrics
                        { cumulativeBlockDecodeDuration = 100
                        , cumulativeTransactionDuration = 200
                        , cumulativeCSMTDuration = 300
                        }
                rendered = renderPrometheus m
            T.isInfixOf "cumulative_block_decode_duration_us" rendered
                `shouldBe` True
            T.isInfixOf "cumulative_transaction_duration_us" rendered
                `shouldBe` True
            T.isInfixOf "cumulative_csmt_duration_us" rendered
                `shouldBe` True
