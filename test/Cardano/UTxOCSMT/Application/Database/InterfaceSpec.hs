module Cardano.UTxOCSMT.Application.Database.InterfaceSpec (spec) where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , Query (..)
    , hoistQuery
    , inverseOp
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Identity (Identity (..))
import Ouroboros.Network.Point (WithOrigin (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, forAll)

genBS :: Gen ByteString
genBS = BS.pack <$> arbitrary

genKey :: Gen Int
genKey = arbitrary

genValue :: Gen String
genValue = arbitrary

spec :: Spec
spec = describe "Database.Interface" $ do
    describe "inverseOp" $ do
        it "Insert k v -> Just (Delete k)" $ do
            forAll ((,) <$> genKey <*> genValue) $ \(k, v) ->
                let result = runIdentity $ inverseOp (const $ pure Nothing) (Insert k v)
                in  result `shouldBe` Just (Delete k)

        it "Delete k with value present -> Just (Insert k v)" $ do
            forAll ((,) <$> genKey <*> genValue) $ \(k, v) ->
                let lookup' k' = pure $ if k' == k then Just v else Nothing
                    result = runIdentity $ inverseOp lookup' (Delete k)
                in  result `shouldBe` Just (Insert k v)

        it "Delete k with no value -> Nothing" $ do
            forAll genKey $ \k ->
                let result =
                        runIdentity $ inverseOp (const $ pure Nothing) (Delete @Int @String k)
                in  result `shouldBe` Nothing

    describe "hoistQuery" $ do
        it "hoistQuery id preserves getValue" $ do
            forAll genBS $ \bs ->
                let q =
                        Query
                            { getValue = \_ -> Identity (Just bs)
                            , getTip = Identity Origin
                            , getFinality = Identity Origin
                            , getByAddress = \_ -> Identity []
                            }
                    q' = hoistQuery id q
                in  runIdentity (getValue q' ()) `shouldBe` Just bs

        it "hoistQuery id preserves getTip" $ do
            let q =
                    Query
                        { getValue = \_ -> Identity (Nothing :: Maybe ByteString)
                        , getTip = Identity (At (42 :: Int))
                        , getFinality = Identity Origin
                        , getByAddress = \_ -> Identity []
                        }
                q' = hoistQuery id q
            runIdentity (getTip q') `shouldBe` At 42
