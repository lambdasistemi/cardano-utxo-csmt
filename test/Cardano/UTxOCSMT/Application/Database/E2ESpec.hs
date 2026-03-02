{- | End-to-end tests for the public RocksDB API ('newRocksDBState').

These tests exercise the full public entry point — 'newRocksDBState' —
on a fresh database without a prior 'setup' call, covering the bug
scenario from issue #101.
-}
module Cardano.UTxOCSMT.Application.Database.E2ESpec
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
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , TipOf
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRocksDBState
    )
import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import Control.Lens (lazy, prism', strict, view)
import Control.Tracer (nullTracer)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (toShort)
import Data.Serialize (getWord64be, putWord64be)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Database.RocksDB (Config (..), withDBCF)
import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

type instance TipOf SlotNo = SlotNo

testPrisms :: Prisms SlotNo Hash BL.ByteString BL.ByteString
testPrisms =
    Prisms
        { slotP =
            prism'
                (evalPutM . putWord64be . fromIntegral . unSlotNo)
                (fmap fromIntegral . evalGetM getWord64be)
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

testArmageddonParams :: ArmageddonParams Hash
testArmageddonParams =
    ArmageddonParams{armageddonBatchSize = 1000, noHash = mkHash ""}

testSlotHash :: SlotNo -> Hash
testSlotHash n = mkHash $ BC.pack $ "blockhash" ++ show n

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
mkTestKey s = BL.fromStrict $ padTo32 $ BC.pack s
  where
    padTo32 bs = bs `BC.append` BC.replicate (32 - BC.length bs) '_'

mkTestValue :: String -> BL.ByteString
mkTestValue = BL.fromStrict . BC.pack

withFreshDB
    :: (Update IO SlotNo BL.ByteString BL.ByteString -> IO a) -> IO a
withFreshDB action =
    withSystemTempDirectory "e2e-test" $ \dir ->
        withDBCF
            dir
            testConfig
            [ ("kv", testConfig)
            , ("csmt", testConfig)
            , ("rollbacks", testConfig)
            , ("config", testConfig)
            ]
            $ \db -> do
                ((update, _rollbackPoints), _runner) <-
                    newRocksDBState
                        nullTracer
                        db
                        testPrisms
                        (mkCSMTOps testFromKV testHashing)
                        testSlotHash
                        (\_ _ -> pure ())
                        testArmageddonParams
                action update

spec :: Spec
spec = describe "E2E newRocksDBState" $ do
    it "initializes a fresh DB without prior setup (#101)" $ do
        withFreshDB $ \update -> do
            -- The fact that we got here without crashing is the test.
            -- forwardTipApply should succeed on the auto-initialized DB.
            _ <- forwardTipApply update 1 1 []
            pure ()

    it "forward/rollback cycle on fresh DB" $ do
        withFreshDB $ \update -> do
            let key1 = mkTestKey "utxo1"
                val1 = mkTestValue "output1"
            -- Forward to slot 1
            update1 <-
                forwardTipApply
                    update
                    (SlotNo 1)
                    (SlotNo 1)
                    [Insert key1 val1]
            -- Forward to slot 2
            let key2 = mkTestKey "utxo2"
                val2 = mkTestValue "output2"
            update2 <-
                forwardTipApply
                    update1
                    (SlotNo 2)
                    (SlotNo 2)
                    [Insert key2 val2]
            -- Rollback to slot 1
            state <- rollbackTipApply update2 (At (SlotNo 1))
            case state of
                Syncing _ -> pure ()
                Intersecting _ _ -> pure ()
                Truncating _ -> fail "unexpected truncation"

    it "forward + finality on fresh DB" $ do
        withFreshDB $ \update -> do
            let key1 = mkTestKey "utxo1"
                val1 = mkTestValue "output1"
            update1 <-
                forwardTipApply
                    update
                    (SlotNo 1)
                    (SlotNo 1)
                    [Insert key1 val1]
            update2 <-
                forwardTipApply
                    update1
                    (SlotNo 2)
                    (SlotNo 2)
                    []
            -- Advance finality past slot 1
            _ <- forwardFinalityApply update2 (SlotNo 1)
            pure ()

    it "multiple forwards with inserts and deletes" $ do
        withFreshDB $ \update -> do
            let key1 = mkTestKey "utxo1"
                val1 = mkTestValue "output1"
                key2 = mkTestKey "utxo2"
                val2 = mkTestValue "output2"
            -- Slot 1: insert two UTxOs
            update1 <-
                forwardTipApply
                    update
                    (SlotNo 1)
                    (SlotNo 1)
                    [Insert key1 val1, Insert key2 val2]
            -- Slot 2: delete one, insert another
            let key3 = mkTestKey "utxo3"
                val3 = mkTestValue "output3"
            update2 <-
                forwardTipApply
                    update1
                    (SlotNo 2)
                    (SlotNo 2)
                    [Delete key1, Insert key3 val3]
            -- Slot 3: empty block
            _ <-
                forwardTipApply
                    update2
                    (SlotNo 3)
                    (SlotNo 3)
                    []
            pure ()

    it "rollback to Origin on fresh DB succeeds" $ do
        withFreshDB $ \update -> do
            update1 <-
                forwardTipApply
                    update
                    (SlotNo 1)
                    (SlotNo 1)
                    []
            -- Origin is within rollback window (finality hasn't moved)
            state <- rollbackTipApply update1 Origin
            case state of
                Syncing _ -> pure ()
                Intersecting _ _ -> pure ()
                Truncating _ -> pure ()

    it "preprod block 282639 + 283086 crash scenario" $ do
        -- Reproduces the crash: d4bebf0c...#1 should be deletable
        -- after being inserted in a prior block.
        let hex s = case B16.decode (BC.pack s) of
                Right bs -> bs
                Left e -> error e
            txIn txid =
                unsafeMkTxIn (toShort $ hex txid)
            -- Preprod tx hashes from Koios
            key366b_0 =
                txIn
                    "366b3fa797964f629662812c15c1989a2b5aead30344f2d7ccf40bf4611c3c79"
                    0
            keyd4be_0 =
                txIn
                    "d4bebf0c9b57c3e7ae745b39337920b9d7dc2a2b61eb78e39d88bcc59ae7693a"
                    0
            keyd4be_1 =
                txIn
                    "d4bebf0c9b57c3e7ae745b39337920b9d7dc2a2b61eb78e39d88bcc59ae7693a"
                    1
            keyb768_0 =
                txIn
                    "b76811c33424b8cf7b61a285152715f5a7f40e89f68a7782c5df515146e2413f"
                    0
            keyb768_1 =
                txIn
                    "b76811c33424b8cf7b61a285152715f5a7f40e89f68a7782c5df515146e2413f"
                    1
        withFreshDB $ \update -> do
            -- Bootstrap: insert what block 282639 needs
            update1 <-
                forwardTipApply
                    update
                    (SlotNo 1)
                    (SlotNo 1)
                    [Insert key366b_0 "out-366b-0"]
            -- Block 282639: 1 delete + 2 inserts
            update2 <-
                forwardTipApply
                    update1
                    (SlotNo 12903843)
                    (SlotNo 12903843)
                    [ Delete key366b_0
                    , Insert keyd4be_0 "out-d4be-0"
                    , Insert keyd4be_1 "out-d4be-1"
                    ]
            -- Block 283086: delete d4be#1 (should NOT crash)
            _ <-
                forwardTipApply
                    update2
                    (SlotNo 12912634)
                    (SlotNo 12912634)
                    [ Delete keyd4be_1
                    , Insert keyb768_0 "out-b768-0"
                    , Insert keyb768_1 "out-b768-1"
                    ]
            pure ()

    it "rollback points are returned on fresh DB" $ do
        withSystemTempDirectory "e2e-rollback-points" $ \dir ->
            withDBCF
                dir
                testConfig
                [ ("kv", testConfig)
                , ("csmt", testConfig)
                , ("rollbacks", testConfig)
                , ("config", testConfig)
                ]
                $ \db -> do
                    ((_, rollbackPoints), _) <-
                        newRocksDBState
                            nullTracer
                            db
                            testPrisms
                            (mkCSMTOps testFromKV testHashing)
                            testSlotHash
                            (\_ _ -> pure ())
                            testArmageddonParams
                    -- Fresh DB should have no rollback slots
                    -- (Origin is in RollbackPoints but it's not a slot)
                    rollbackPoints `shouldBe` []
