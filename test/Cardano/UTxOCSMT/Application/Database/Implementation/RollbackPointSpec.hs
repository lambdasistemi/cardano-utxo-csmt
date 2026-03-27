module Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPointSpec
    ( spec
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( rollbackListPrism
    , rollbackPointPrism
    , withOriginPrism
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
import ChainFollower.Rollbacks.Types qualified as RP
import Control.Lens
    ( Prism'
    , preview
    , prism'
    , review
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Ouroboros.Network.Point (WithOrigin (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , arbitrary
    , forAll
    , listOf
    , oneof
    )

-- | Test that a prism roundtrips: preview p (review p x) === Just x
roundtrip :: (Eq a, Show a) => Prism' s a -> a -> IO ()
roundtrip p x = preview p (review p x) `shouldBe` Just x

idPrism :: Prism' ByteString ByteString
idPrism = prism' id Just

genBS :: Gen ByteString
genBS = BS.pack <$> arbitrary

genOperation :: Gen (Operation ByteString ByteString)
genOperation =
    oneof
        [ Delete <$> genBS
        , Insert <$> genBS <*> genBS
        ]

genMaybeMerkle :: Gen (Maybe ByteString)
genMaybeMerkle = oneof [pure Nothing, Just <$> genBS]

genRollbackPoint
    :: Gen
        ( RP.RollbackPoint
            (Operation ByteString ByteString)
            (ByteString, Maybe ByteString)
        )
genRollbackPoint = do
    h <- genBS
    ops <- listOf genOperation
    mr <- genMaybeMerkle
    pure
        RP.RollbackPoint
            { rpInverses = ops
            , rpMeta = Just (h, mr)
            }

genRollbackListPoint
    :: Gen
        ( RP.RollbackPoint
            [Operation ByteString ByteString]
            (ByteString, Maybe ByteString)
        )
genRollbackListPoint = do
    h <- genBS
    ops <- listOf genOperation
    mr <- genMaybeMerkle
    pure
        RP.RollbackPoint
            { rpInverses = [ops]
            , rpMeta = Just (h, mr)
            }

genWithOrigin :: Gen (WithOrigin ByteString)
genWithOrigin =
    oneof
        [ pure Origin
        , At <$> genBS
        ]

spec :: Spec
spec = describe "RollbackPoint" $ do
    describe "withOriginPrism" $ do
        it "roundtrips Origin" $ do
            roundtrip (withOriginPrism idPrism) Origin

        it "roundtrips At slot" $ do
            forAll genBS $ \slot ->
                roundtrip (withOriginPrism idPrism) (At slot)

        it "roundtrips arbitrary WithOrigin" $ do
            forAll genWithOrigin $ \wo ->
                roundtrip (withOriginPrism idPrism) wo

    describe "rollbackPointPrism" $ do
        it "roundtrips a rollback point" $ do
            forAll genRollbackPoint $ \rp ->
                roundtrip (rollbackPointPrism idPrism idPrism idPrism) rp

        it "roundtrips with empty operations" $ do
            forAll ((,) <$> genBS <*> genMaybeMerkle) $ \(h, mr) ->
                let rp =
                        RP.RollbackPoint
                            { rpInverses = []
                            , rpMeta = Just (h, mr)
                            }
                in  roundtrip (rollbackPointPrism idPrism idPrism idPrism) rp

        it "roundtrips with only Delete operations" $ do
            forAll genBS $ \h ->
                forAll (listOf (Delete <$> genBS)) $ \ops ->
                    let rp =
                            RP.RollbackPoint
                                { rpInverses = ops
                                , rpMeta = Just (h, Nothing)
                                }
                    in  roundtrip (rollbackPointPrism idPrism idPrism idPrism) rp

        it "roundtrips with only Insert operations" $ do
            forAll genBS $ \h ->
                forAll (listOf (Insert <$> genBS <*> genBS)) $ \ops ->
                    let rp =
                            RP.RollbackPoint
                                { rpInverses = ops
                                , rpMeta = Just (h, Just h)
                                }
                    in  roundtrip (rollbackPointPrism idPrism idPrism idPrism) rp

    describe "rollbackListPrism" $ do
        it "roundtrips a rollback list point" $ do
            forAll genRollbackListPoint $ \rp ->
                roundtrip (rollbackListPrism idPrism idPrism idPrism) rp

        it "roundtrips with empty operations" $ do
            forAll ((,) <$> genBS <*> genMaybeMerkle) $ \(h, mr) ->
                let rp =
                        RP.RollbackPoint
                            { rpInverses = [[]]
                            , rpMeta = Just (h, mr)
                            }
                in  roundtrip (rollbackListPrism idPrism idPrism idPrism) rp

        it "roundtrips with mixed operations" $ do
            forAll genBS $ \h ->
                forAll (listOf genOperation) $ \ops ->
                    forAll genMaybeMerkle $ \mr ->
                        let rp =
                                RP.RollbackPoint
                                    { rpInverses = [ops]
                                    , rpMeta = Just (h, mr)
                                    }
                        in  roundtrip (rollbackListPrism idPrism idPrism idPrism) rp
