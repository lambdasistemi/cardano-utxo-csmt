{- | End-to-end tests for the Runner-based chain follower.

These tests exercise the new path: 'createBackend' → 'Backend.Init'
→ 'processBlock' / 'rollbackTo' from 'ChainFollower.Runner'.
-}
module Cardano.UTxOCSMT.Application.Database.E2ERunnerSpec
    ( spec
    )
where

import CSMT (FromKV (..), Hashing)
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTOps (..)
    , RunTransaction (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , TipOf
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import ChainFollower.Backend qualified as Backend
import ChainFollower.Rollbacks.Store qualified as Store
import ChainFollower.Runner
    ( Phase (..)
    , processBlock
    , rollbackTo
    )
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens (lazy, prism', strict, view)
import Control.Monad (foldM, forM, forM_)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (toShort)
import Database.KV.Database ()
import Database.KV.RocksDB ()
import Database.KV.Transaction qualified as KV
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , withDBCF
    )
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (choose, forAll, ioProperty, property)

type instance TipOf SlotNo = SlotNo

testPrisms
    :: Prisms SlotNo Hash BL.ByteString BL.ByteString
testPrisms =
    Prisms
        { slotP =
            prism'
                ( BL.toStrict
                    . CBOR.toLazyByteString
                    . CBOR.encodeWord64
                    . fromIntegral
                    . unSlotNo
                )
                ( \bs ->
                    case CBOR.deserialiseFromBytes
                        CBOR.decodeWord64
                        (BL.fromStrict bs) of
                        Right (_, w) ->
                            Just (fromIntegral w)
                        Left _ -> Nothing
                )
        , hashP = isoHash
        , keyP = lazy
        , valueP = lazy
        }

testFromKV :: FromKV BL.ByteString BL.ByteString Hash
testFromKV =
    FromKV
        { isoK = strict . isoK fromKVHashes
        , fromV = fromV fromKVHashes . view strict
        , treePrefix = const []
        }

testHashing :: Hashing Hash
testHashing = hashHashing

testSlotHash :: SlotNo -> Hash
testSlotHash n =
    mkHash $ BC.pack $ "blockhash" ++ show n

testConfig :: Config
testConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }

mkTestKey :: String -> BL.ByteString
mkTestKey s =
    BL.fromStrict $ padTo32 $ BC.pack s
  where
    padTo32 bs =
        bs
            `BC.append` BC.replicate
                (32 - BC.length bs)
                '_'

mkTestValue :: String -> BL.ByteString
mkTestValue = BL.fromStrict . BC.pack

-- | Concrete column type.
type Cols =
    Columns SlotNo Hash BL.ByteString BL.ByteString

-- | Concrete transaction type.
type Tx a =
    KV.Transaction IO ColumnFamily Cols BatchOp a

-- | Concrete phase type.
type TestPhase =
    Phase
        IO
        ColumnFamily
        Cols
        BatchOp
        (SlotNo, [Operation BL.ByteString BL.ByteString])
        [Operation BL.ByteString BL.ByteString]
        (Hash, Maybe Hash)

-- | Build a Restoring continuation from CSMTOps.
mkRestoring
    :: CSMTOps
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        BL.ByteString
        BL.ByteString
        Hash
    -> Backend.Restoring
        IO
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        (SlotNo, [Operation BL.ByteString BL.ByteString])
        [Operation BL.ByteString BL.ByteString]
        (Hash, Maybe Hash)
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
        , Backend.toFollowing =
            pure $ mkFollowing csmtOps
        }

-- | Build a Following continuation from CSMTOps.
mkFollowing
    :: CSMTOps
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        BL.ByteString
        BL.ByteString
        Hash
    -> Backend.Following
        IO
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        (SlotNo, [Operation BL.ByteString BL.ByteString])
        [Operation BL.ByteString BL.ByteString]
        (Hash, Maybe Hash)
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
                            mx <- KV.query KVCol k
                            csmtDelete csmtOps k
                            case mx of
                                Nothing ->
                                    error
                                        $ "mkFollowing: cannot"
                                            <> " invert Delete"
                                            <> " at slot "
                                            <> show slot
                                Just x -> pure [Insert k x]
                let inverseOps = reverse (concat invs)
                merkleRoot <- csmtRootHash csmtOps
                pure
                    ( inverseOps
                    , Just (testSlotHash slot, merkleRoot)
                    , mkFollowing csmtOps
                    )
        , Backend.toRestoring =
            pure
                Backend.Restoring
                    { Backend.restore =
                        const
                            $ error
                                "test: restore not expected"
                    , Backend.toFollowing =
                        pure $ mkFollowing csmtOps
                    }
        , Backend.applyInverse =
            \inverseOps ->
                forM_ inverseOps $ \case
                    Insert k v -> csmtInsert csmtOps k v
                    Delete k -> csmtDelete csmtOps k
        }

-- | Set up a fresh DB with the Runner API.
withFreshDB
    :: ( TestPhase
         -> (forall a. Tx a -> IO a)
         -> IO r
       )
    -> IO r
withFreshDB action =
    withSystemTempDirectory "e2e-runner" $ \dir ->
        withDBCF
            dir
            testConfig
            [ ("kv", testConfig)
            , ("csmt", testConfig)
            , ("rollbacks", testConfig)
            , ("config", testConfig)
            , ("journal", testConfig)
            , ("rollbacks2", testConfig)
            ]
            $ \db -> do
                RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        testPrisms
                let csmtOps =
                        mkCSMTOps testFromKV testHashing
                -- Set up sentinel
                transact
                    $ Store.armageddonSetup
                        Rollbacks
                        Origin
                        Nothing
                -- Start in following mode
                let phase =
                        InFollowing 1 (mkFollowing csmtOps)
                action phase transact

-- | Set up a fresh DB starting in Restoration mode.
withFreshDBRestoration
    :: ( TestPhase
         -> (forall a. Tx a -> IO a)
         -> IO r
       )
    -> IO r
withFreshDBRestoration action =
    withSystemTempDirectory "e2e-runner-restore" $ \dir ->
        withDBCF
            dir
            testConfig
            [ ("kv", testConfig)
            , ("csmt", testConfig)
            , ("rollbacks", testConfig)
            , ("config", testConfig)
            , ("journal", testConfig)
            , ("rollbacks2", testConfig)
            ]
            $ \db -> do
                RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        testPrisms
                let csmtOps =
                        mkCSMTOps testFromKV testHashing
                -- No sentinel — fresh DB, restoration mode
                let phase =
                        InRestoration 0 (mkRestoring csmtOps)
                action phase transact

spec :: Spec
spec = describe "E2E Runner" $ do
    it "initializes and processes a block" $ do
        withFreshDB $ \phase transact -> do
            _ <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 1))
                        (SlotNo 1, [])
                        phase
            pure ()

    it "forward/rollback cycle" $ do
        withFreshDB $ \phase transact -> do
            let key1 = mkTestKey "utxo1"
                val1 = mkTestValue "output1"
            -- Forward slot 1
            phase1 <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 1))
                        ( SlotNo 1
                        , [Insert key1 val1]
                        )
                        phase
            -- Forward slot 2
            let key2 = mkTestKey "utxo2"
                val2 = mkTestValue "output2"
            phase2 <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 2))
                        ( SlotNo 2
                        , [Insert key2 val2]
                        )
                        phase1
            -- Rollback to slot 1
            case phase2 of
                InFollowing n f -> do
                    (result, _) <-
                        transact
                            $ rollbackTo
                                Rollbacks
                                f
                                n
                                (At (SlotNo 1))
                    case result of
                        Store.RollbackSucceeded deleted ->
                            deleted `shouldBe` 1
                        Store.RollbackImpossible ->
                            fail "unexpected RollbackImpossible"
                InRestoration _ _ ->
                    fail "expected Following"

    it "multiple forwards with inserts and deletes"
        $ do
            withFreshDB $ \phase transact -> do
                let key1 = mkTestKey "utxo1"
                    val1 = mkTestValue "output1"
                    key2 = mkTestKey "utxo2"
                    val2 = mkTestValue "output2"
                -- Slot 1: insert two
                phase1 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            ( SlotNo 1
                            , [Insert key1 val1, Insert key2 val2]
                            )
                            phase
                -- Slot 2: delete one, insert another
                let key3 = mkTestKey "utxo3"
                    val3 = mkTestValue "output3"
                phase2 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 2))
                            ( SlotNo 2
                            , [Delete key1, Insert key3 val3]
                            )
                            phase1
                -- Slot 3: empty block
                _ <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 3))
                            (SlotNo 3, [])
                            phase2
                pure ()

    it "rollback to Origin" $ do
        withFreshDB $ \phase transact -> do
            phase1 <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 1))
                        (SlotNo 1, [])
                        phase
            case phase1 of
                InFollowing n f -> do
                    (result, _) <-
                        transact
                            $ rollbackTo
                                Rollbacks
                                f
                                n
                                Origin
                    case result of
                        Store.RollbackSucceeded _ ->
                            pure ()
                        Store.RollbackImpossible ->
                            fail "unexpected RollbackImpossible"
                InRestoration _ _ ->
                    fail "expected Following"

    it "queryHistory returns metadata" $ do
        withFreshDB $ \phase transact -> do
            let key1 = mkTestKey "utxo1"
                val1 = mkTestValue "output1"
            -- Follow one block
            _ <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 1))
                        ( SlotNo 1
                        , [Insert key1 val1]
                        )
                        phase
            -- Query history
            history <-
                transact
                    $ Store.queryHistory Rollbacks
            -- Should have sentinel + 1 block = 2 entries
            length history `shouldBe` 2

    it "forward + finality pruning" $ do
        withFreshDB $ \phase transact -> do
            -- Follow 3 blocks with k=1 (keep 2 points)
            -- Auto-pruning kicks in when count > k+1
            phase1 <-
                transact
                    $ processBlock
                        Rollbacks
                        1
                        (At (SlotNo 1))
                        (SlotNo 1, [])
                        phase
            phase2 <-
                transact
                    $ processBlock
                        Rollbacks
                        1
                        (At (SlotNo 2))
                        (SlotNo 2, [])
                        phase1
            _ <-
                transact
                    $ processBlock
                        Rollbacks
                        1
                        (At (SlotNo 3))
                        (SlotNo 3, [])
                        phase2
            -- With k=1, only slot 2 and 3 remain
            history <-
                transact
                    $ Store.queryHistory Rollbacks
            length history `shouldBe` 2

    it "preprod block crash scenario" $ do
        let hex s = case B16.decode (BC.pack s) of
                Right bs -> bs
                Left e -> error e
            txIn txid =
                unsafeMkTxIn (toShort $ hex txid)
            key366b_0 =
                txIn
                    "366b3fa797964f629662812c\
                    \15c1989a2b5aead30344f2d7\
                    \ccf40bf4611c3c79"
                    0
            keyd4be_0 =
                txIn
                    "d4bebf0c9b57c3e7ae745b39\
                    \337920b9d7dc2a2b61eb78e3\
                    \9d88bcc59ae7693a"
                    0
            keyd4be_1 =
                txIn
                    "d4bebf0c9b57c3e7ae745b39\
                    \337920b9d7dc2a2b61eb78e3\
                    \9d88bcc59ae7693a"
                    1
            keyb768_0 =
                txIn
                    "b76811c33424b8cf7b61a285\
                    \152715f5a7f40e89f68a7782\
                    \c5df515146e2413f"
                    0
            keyb768_1 =
                txIn
                    "b76811c33424b8cf7b61a285\
                    \152715f5a7f40e89f68a7782\
                    \c5df515146e2413f"
                    1
        withFreshDB $ \phase transact -> do
            -- Bootstrap: insert what block 282639 needs
            phase1 <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 1))
                        ( SlotNo 1
                        , [Insert key366b_0 "out-366b-0"]
                        )
                        phase
            -- Block 282639: 1 delete + 2 inserts
            phase2 <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 12903843))
                        ( SlotNo 12903843
                        ,
                            [ Delete key366b_0
                            , Insert keyd4be_0 "out-d4be-0"
                            , Insert keyd4be_1 "out-d4be-1"
                            ]
                        )
                        phase1
            -- Block 283086: delete d4be#1
            _ <-
                transact
                    $ processBlock
                        Rollbacks
                        maxBound
                        (At (SlotNo 12912634))
                        ( SlotNo 12912634
                        ,
                            [ Delete keyd4be_1
                            , Insert keyb768_0 "out-b768-0"
                            , Insert keyb768_1 "out-b768-1"
                            ]
                        )
                        phase2
            pure ()

    it "rollback points queryTip on fresh DB" $ do
        withFreshDB $ \_phase transact -> do
            mTip <-
                transact
                    $ Store.queryTip Rollbacks
            -- Fresh DB has sentinel at Origin
            mTip `shouldBe` Just Origin

    describe "Restoration mode" $ do
        it "processes blocks in restoration" $ do
            withFreshDBRestoration $ \phase transact -> do
                let key1 = mkTestKey "utxo1"
                    val1 = mkTestValue "output1"
                phase1 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            ( SlotNo 1
                            , [Insert key1 val1]
                            )
                            phase
                -- Should stay in restoration
                case phase1 of
                    InRestoration _ _ -> pure ()
                    InFollowing _ _ ->
                        fail "expected Restoration"

        it "restoration stores no rollback points" $ do
            withFreshDBRestoration $ \phase transact -> do
                _ <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            (SlotNo 1, [Insert (mkTestKey "k") (mkTestValue "v")])
                            phase
                -- Rollback column should be empty
                count <-
                    transact
                        $ Store.countPoints Rollbacks
                count `shouldBe` 0

        it "restoration then transition to following" $ do
            withFreshDBRestoration $ \phase transact -> do
                let key1 = mkTestKey "utxo1"
                    val1 = mkTestValue "output1"
                -- Restore a block
                phase1 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            ( SlotNo 1
                            , [Insert key1 val1]
                            )
                            phase
                -- Transition to Following
                case phase1 of
                    InRestoration _ restoring -> do
                        following <-
                            Backend.toFollowing restoring
                        let phase2 =
                                InFollowing 0 following
                        -- Process another block in Following
                        phase3 <-
                            transact
                                $ processBlock
                                    Rollbacks
                                    maxBound
                                    (At (SlotNo 2))
                                    ( SlotNo 2
                                    , [Insert (mkTestKey "utxo2") (mkTestValue "output2")]
                                    )
                                    phase2
                        -- Should now have a rollback point
                        case phase3 of
                            InFollowing n _ ->
                                n `shouldBe` 1
                            InRestoration _ _ ->
                                fail "expected Following"
                    InFollowing _ _ ->
                        fail "expected Restoration"

        it "multiple blocks in restoration then follow + rollback" $ do
            withFreshDBRestoration $ \phase transact -> do
                -- Restore 3 blocks
                phase1 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            ( SlotNo 1
                            , [Insert (mkTestKey "utxo1") (mkTestValue "out1")]
                            )
                            phase
                phase2 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 2))
                            ( SlotNo 2
                            , [Insert (mkTestKey "utxo2") (mkTestValue "out2")]
                            )
                            phase1
                phase3 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 3))
                            ( SlotNo 3
                            , [Insert (mkTestKey "utxo3") (mkTestValue "out3")]
                            )
                            phase2
                -- Transition to Following
                case phase3 of
                    InRestoration _ restoring -> do
                        following <-
                            Backend.toFollowing restoring
                        -- Set up sentinel for rollback
                        transact
                            $ Store.armageddonSetup
                                Rollbacks
                                (At (SlotNo 3))
                                Nothing
                        let phase4 =
                                InFollowing 1 following
                        -- Follow 2 more blocks
                        phase5 <-
                            transact
                                $ processBlock
                                    Rollbacks
                                    maxBound
                                    (At (SlotNo 4))
                                    ( SlotNo 4
                                    , [Insert (mkTestKey "utxo4") (mkTestValue "out4")]
                                    )
                                    phase4
                        phase6 <-
                            transact
                                $ processBlock
                                    Rollbacks
                                    maxBound
                                    (At (SlotNo 5))
                                    ( SlotNo 5
                                    , [Insert (mkTestKey "utxo5") (mkTestValue "out5")]
                                    )
                                    phase5
                        -- Rollback to slot 4
                        case phase6 of
                            InFollowing n f -> do
                                (result, _) <-
                                    transact
                                        $ rollbackTo
                                            Rollbacks
                                            f
                                            n
                                            (At (SlotNo 4))
                                case result of
                                    Store.RollbackSucceeded deleted ->
                                        deleted `shouldBe` 1
                                    Store.RollbackImpossible ->
                                        fail "unexpected RollbackImpossible"
                            InRestoration _ _ ->
                                fail "expected Following"
                    InFollowing _ _ ->
                        fail "expected Restoration after restore"

    describe "Property tests" $ do
        it "blocks deeper than k are pruned"
            $ property
            $ forAll (choose (2, 5 :: Int))
            $ \k ->
                forAll (choose (k + 2, 15)) $ \n ->
                    ioProperty $ withFreshDB $ \phase transact -> do
                        let slots =
                                map (SlotNo . fromIntegral) [1 .. n]
                        _ <-
                            foldM
                                ( \p slot ->
                                    transact
                                        $ processBlock
                                            Rollbacks
                                            (fromIntegral k)
                                            (At slot)
                                            (slot, [])
                                            p
                                )
                                phase
                                slots
                        history <-
                            transact
                                $ Store.queryHistory Rollbacks
                        length history
                            `shouldSatisfy` (<= fromIntegral k + 1)

        it "blocks within k are rollbackable"
            $ property
            $ forAll (choose (2, 5 :: Int))
            $ \k ->
                forAll (choose (k + 1, 15)) $ \n ->
                    ioProperty $ withFreshDB $ \phase transact -> do
                        let slots =
                                map (SlotNo . fromIntegral) [1 .. n]
                        finalPhase <-
                            foldM
                                ( \p slot ->
                                    transact
                                        $ processBlock
                                            Rollbacks
                                            (fromIntegral k)
                                            (At slot)
                                            (slot, [])
                                            p
                                )
                                phase
                                slots
                        -- Try rolling back to each of the last k slots
                        let rollbackTargets =
                                map (SlotNo . fromIntegral)
                                    $ take k [n, n - 1 ..]
                        case finalPhase of
                            InFollowing count following -> do
                                forM_ rollbackTargets $ \target -> do
                                    (result, _) <-
                                        transact
                                            $ rollbackTo
                                                Rollbacks
                                                following
                                                count
                                                (At target)
                                    case result of
                                        Store.RollbackSucceeded _ ->
                                            pure ()
                                        Store.RollbackImpossible ->
                                            fail
                                                $ "expected rollback to "
                                                    <> show target
                                                    <> " to succeed"
                            InRestoration _ _ ->
                                fail "expected Following"

        it "rollback undoes changes"
            $ property
            $ ioProperty
            $ withFreshDB
            $ \phase transact -> do
                let key1 = mkTestKey "prop-undo"
                    val1 = mkTestValue "val-undo"
                -- Slot 1: empty block
                phase1 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 1))
                            (SlotNo 1, [])
                            phase
                -- Slot 2: insert key
                phase2 <-
                    transact
                        $ processBlock
                            Rollbacks
                            maxBound
                            (At (SlotNo 2))
                            ( SlotNo 2
                            , [Insert key1 val1]
                            )
                            phase1
                -- Verify key exists before rollback
                valBefore <-
                    transact
                        $ KV.query KVCol key1
                valBefore `shouldSatisfy` \case
                    Just _ -> True
                    Nothing -> False
                -- Rollback to slot 1 (undoes slot 2 insert)
                case phase2 of
                    InFollowing n f -> do
                        _ <-
                            transact
                                $ rollbackTo
                                    Rollbacks
                                    f
                                    n
                                    (At (SlotNo 1))
                        -- Key from slot 2 should be gone
                        valAfter <-
                            transact
                                $ KV.query KVCol key1
                        valAfter `shouldBe` Nothing
                    InRestoration _ _ ->
                        fail "expected Following"
