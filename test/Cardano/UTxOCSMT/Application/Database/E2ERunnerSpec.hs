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
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    , codecs
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
import Control.Monad (forM, forM_)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
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
import Test.Hspec (Spec, describe, it, shouldBe)

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
                        InFollowing (mkFollowing csmtOps)
                action phase transact

spec :: Spec
spec = describe "E2E Runner" $ do
    it "initializes and processes a block" $ do
        withFreshDB $ \phase transact -> do
            _ <-
                transact
                    $ processBlock
                        Rollbacks
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
                        (At (SlotNo 2))
                        ( SlotNo 2
                        , [Insert key2 val2]
                        )
                        phase1
            -- Rollback to slot 1
            case phase2 of
                InFollowing f -> do
                    result <-
                        transact
                            $ rollbackTo
                                Rollbacks
                                f
                                (At (SlotNo 1))
                    case result of
                        Store.RollbackSucceeded n ->
                            n `shouldBe` 1
                        Store.RollbackImpossible ->
                            fail "unexpected RollbackImpossible"
                InRestoration _ ->
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
                        (At (SlotNo 1))
                        (SlotNo 1, [])
                        phase
            case phase1 of
                InFollowing f -> do
                    result <-
                        transact
                            $ rollbackTo
                                Rollbacks
                                f
                                Origin
                    case result of
                        Store.RollbackSucceeded _ ->
                            pure ()
                        Store.RollbackImpossible ->
                            fail "unexpected RollbackImpossible"
                InRestoration _ ->
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
