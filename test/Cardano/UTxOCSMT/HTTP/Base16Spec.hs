module Cardano.UTxOCSMT.HTTP.Base16Spec (spec) where

import Cardano.UTxOCSMT.HTTP.Base16
    ( decodeBase16Text
    , encodeBase16Text
    , unsafeDecodeBase16Text
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , elements
    , forAll
    , listOf
    , vectorOf
    )

genByteString :: Gen ByteString
genByteString = BS.pack <$> arbitrary

genHexText :: Gen T.Text
genHexText = do
    n <- abs <$> arbitrary
    chars <- vectorOf (2 * (n `mod` 128)) (elements "0123456789abcdef")
    pure $ T.pack chars

spec :: Spec
spec = describe "Base16" $ do
    it "decode . encode = Right id (roundtrip)" $ do
        forAll genByteString $ \bs ->
            decodeBase16Text (encodeBase16Text bs) `shouldBe` Right @String bs

    it "encode . unsafeDecode = id (for valid hex)" $ do
        forAll genHexText $ \hex ->
            encodeBase16Text (unsafeDecodeBase16Text hex :: ByteString)
                `shouldBe` hex

    it "encode produces only hex chars" $ do
        forAll genByteString $ \bs ->
            T.all isHexDigit (encodeBase16Text bs) `shouldBe` True

    it "decode rejects non-hex input" $ do
        forAll (listOf $ elements "ghijklmnopqrstuvwxyz!@#$%") $ \badChars ->
            let txt = T.pack ('g' : badChars)
            in  (decodeBase16Text txt :: Either String ByteString)
                    `shouldSatisfy` isLeft
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False
