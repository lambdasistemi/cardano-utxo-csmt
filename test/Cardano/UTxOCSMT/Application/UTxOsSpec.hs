module Cardano.UTxOCSMT.Application.UTxOsSpec (spec) where

import Cardano.Crypto.Hash.Class (Hash (..))
import Cardano.Ledger.Api.Tx.In (mkTxIxPartial)
import Cardano.Ledger.Api.Tx.In qualified as Shelley
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.UTxOCSMT.Application.UTxOs
    ( Change (..)
    , cborEncode
    , mkShelleyTxIn
    , unsafeMkTxIn
    )
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Word (Word16)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Gen, arbitrary, choose, forAll, vectorOf)

genShortByteString32 :: Gen SBS.ShortByteString
genShortByteString32 = SBS.pack <$> vectorOf 32 arbitrary

genIndex :: Gen Word16
genIndex = choose (0, 1000)

spec :: Spec
spec = describe "UTxOs" $ do
    describe "unsafeMkTxIn" $ do
        it "produces non-empty CBOR" $ do
            forAll ((,) <$> genShortByteString32 <*> genIndex) $ \(txId, ix) ->
                BL.length (unsafeMkTxIn txId ix) `shouldSatisfy` (> 0)

        it "same inputs produce same output (deterministic)" $ do
            forAll ((,) <$> genShortByteString32 <*> genIndex) $ \(txId, ix) ->
                unsafeMkTxIn txId ix `shouldBe` unsafeMkTxIn txId ix

    describe "mkShelleyTxIn" $ do
        it "roundtrips through cborEncode consistently" $ do
            forAll ((,) <$> genShortByteString32 <*> genIndex) $ \(sbs, ix) ->
                let txId = Shelley.TxId $ unsafeMakeSafeHash $ UnsafeHash sbs
                    encoded = mkShelleyTxIn ix txId
                    direct = cborEncode $ Shelley.TxIn txId (mkTxIxPartial $ fromIntegral ix)
                in  encoded `shouldBe` direct

    describe "Change" $ do
        it "Spend show starts with 'd '" $ do
            forAll (BL.pack <$> arbitrary) $ \bs ->
                take 2 (show (Spend bs)) `shouldBe` "d "

        it "Create show starts with 'i '" $ do
            forAll ((,) . BL.pack <$> arbitrary <*> (BL.pack <$> arbitrary))
                $ \(k, v) ->
                    take 2 (show (Create k v)) `shouldBe` "i "
