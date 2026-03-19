{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DbRecoverySpec
Description : Database crash recovery integration tests
License     : Apache-2.0

Tests openCSMTOps, toFull, and the Update state machine
(with rollbacks) against RocksDB.
-}
module DbRecoverySpec (spec) where

import CSMT (FromKV (..))
import CSMT.Hashes (Hash, fromKVHashes, hashHashing)
import CSMT.MTS (CommonOps (..), Ops (..), ReplayEvent (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , DbState (..)
    , ReadyState (..)
    , RunTransaction (..)
    , openCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( fullForwardOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , State (..)
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( createSplitUpdateState
    , newRunRocksDBTransaction
    , newRunRocksDBTransactionUnguarded
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , prisms
    , slotHash
    , withRocksDB
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Lens (iso, strict, view)
import Control.Monad (foldM, void)
import Control.Tracer (nullTracer)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (toShort)
import DbCompare (dbDiff)
import Ouroboros.Consensus.Cardano ()
import Ouroboros.Consensus.HardFork.Combinator (OneEraHash (..))
import Ouroboros.Network.Block (SlotNo (..), pattern BlockPoint)
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , chooseInt
    , forAll
    , property
    )
import Test.QuickCheck qualified as QC

-- ------------------------------------------------------------------
-- Test context (no Cardano address decoding)
-- ------------------------------------------------------------------

testContext :: CSMTContext Hash BL.ByteString BL.ByteString
testContext =
    CSMTContext
        { fromKV =
            FromKV
                { isoK = strict . isoK fromKVHashes
                , fromV = fromV fromKVHashes . view strict
                , treePrefix = const []
                }
        , hashing = hashHashing
        }

-- ------------------------------------------------------------------
-- Helpers
-- ------------------------------------------------------------------

mkPoint :: Int -> Point
mkPoint n =
    BlockPoint
        (SlotNo $ fromIntegral n)
        (OneEraHash $ toShort $ BS.pack $ replicate 32 $ fromIntegral n)

data ReplayCrash = ReplayCrash deriving (Show)
instance Exception ReplayCrash

noReplay :: ReplayEvent -> IO ()
noReplay _ = pure ()

crashOnReplay :: ReplayEvent -> IO ()
crashOnReplay ReplayStart{} = throwIO ReplayCrash
crashOnReplay ReplayStop = pure ()

-- | Check DbState without resolving.
checkState :: FilePath -> IO String
checkState dbPath = do
    let CSMTContext{fromKV = fkv, hashing = h} = testContext
    withRocksDB dbPath $ \db -> do
        runner <- newRunRocksDBTransaction db prisms
        let ung = newRunRocksDBTransactionUnguarded db prisms
        st <-
            openCSMTOps
                4
                100
                (iso BL.toStrict BL.fromStrict)
                fkv
                h
                (transact runner)
                (transact ung)
                noReplay
        pure $ case st of
            NeedsRecovery _ -> "NeedsRecovery"
            Ready (ChooseKVOnly _) -> "ChooseKVOnly"
            Ready (ChooseFull _) -> "ChooseFull"

-- | KVOnly actions (no Update/rollback).
data KVActions = KVActions
    { kvIns :: BL.ByteString -> BL.ByteString -> IO ()
    , kvDel :: BL.ByteString -> IO ()
    , kvReplay :: IO ()
    }

withKVOnly
    :: FilePath
    -> (ReplayEvent -> IO ())
    -> (KVActions -> IO a)
    -> IO a
withKVOnly dbPath onReplay action = do
    let CSMTContext{fromKV = fkv, hashing = h} = testContext
    withRocksDB dbPath $ \db -> do
        runner <- newRunRocksDBTransaction db prisms
        let ung = newRunRocksDBTransactionUnguarded db prisms
        st <-
            openCSMTOps
                4
                100
                (iso BL.toStrict BL.fromStrict)
                fkv
                h
                (transact runner)
                (transact ung)
                onReplay
        let resolve (NeedsRecovery recover) = recover >>= resolve
            resolve (Ready (ChooseKVOnly ops)) = pure ops
            resolve (Ready (ChooseFull _)) = fail "unexpected ChooseFull"
        ops <- resolve st
        action
            KVActions
                { kvIns = \k v -> transact runner $ opsInsert (kvCommon ops) k v
                , kvDel = transact runner . opsDelete (kvCommon ops)
                , kvReplay = void $ toFull ops
                }

-- | Full Update state machine (forward + rollback).
withUpdate
    :: FilePath
    -> (ReplayEvent -> IO ())
    -> Bool
    -> (Update IO Point BL.ByteString BL.ByteString -> IO a)
    -> IO a
withUpdate dbPath onReplay forceAtTip action = do
    let CSMTContext{fromKV = fkv, hashing = h} = testContext
    withRocksDB dbPath $ \db -> do
        runner <- newRunRocksDBTransaction db prisms
        let ung = newRunRocksDBTransactionUnguarded db prisms
        st <-
            openCSMTOps
                4
                100
                (iso BL.toStrict BL.fromStrict)
                fkv
                h
                (transact runner)
                (transact ung)
                onReplay
        let resolve (NeedsRecovery recover) = recover >>= resolve
            resolve (Ready (ChooseKVOnly ops)) = pure ops
            resolve (Ready (ChooseFull _)) = fail "unexpected ChooseFull"
        kvOps <- resolve st
        (state, _slots) <-
            createSplitUpdateState
                nullTracer
                fullForwardOps
                True
                kvOps
                (\_ _ -> forceAtTip)
                slotHash
                (\_ _ -> pure ())
                armageddonParams
                runner
                10
                (\_ -> pure ())
        action state

feedBlock
    :: Update IO Point BL.ByteString BL.ByteString
    -> Int
    -> SlotNo
    -> [Operation BL.ByteString BL.ByteString]
    -> IO (Update IO Point BL.ByteString BL.ByteString)
feedBlock u slot = forwardTipApply u (mkPoint slot)

mkBlock :: Int -> [Operation BL.ByteString BL.ByteString]
mkBlock n =
    [ Insert
        (BL.pack [fromIntegral n, fromIntegral i, 0, 0])
        (BL.pack [0, 0, fromIntegral i, fromIntegral n])
    | i <- [0 .. 2 :: Int]
    ]

insertN :: (BL.ByteString -> BL.ByteString -> IO ()) -> Int -> IO ()
insertN f n = mapM_ (\i -> f (k i) (v i)) [1 .. n]
  where
    k i = BL.pack [fromIntegral i, 0, 0, 0]
    v i = BL.pack [0, 0, 0, fromIntegral i]

{- | Generate a sequence of blocks. Each block has 1-3
inserts. Occasional blocks also delete a key from an
earlier block.
-}
genBlocks
    :: Int -> Gen [(Int, [Operation BL.ByteString BL.ByteString])]
genBlocks n = fst <$> go [] [] 1
  where
    go acc _live slot
        | slot > n = pure (reverse acc, _live)
    go acc live slot = do
        nOps <- chooseInt (1, 3)
        let inserts =
                [ Insert (k slot i) (v slot i)
                | i <- [0 .. nOps - 1]
                ]
            newKeys = [k slot i | i <- [0 .. nOps - 1]]
        -- Occasionally delete a live key
        (delOps, live') <-
            if null live
                then pure ([], live ++ newKeys)
                else do
                    doDel <- chooseInt (0, 2)
                    if doDel == 0
                        then do
                            idx <- chooseInt (0, length live - 1)
                            let dk = live !! idx
                                live'' = take idx live ++ drop (idx + 1) live
                            pure ([Delete dk], live'' ++ newKeys)
                        else pure ([], live ++ newKeys)
        go ((slot, inserts ++ delOps) : acc) live' (slot + 1)
    k slot i = BL.pack [fromIntegral slot, fromIntegral i, 0, 0]
    v slot i = BL.pack [0, 0, fromIntegral i, fromIntegral slot]

-- ------------------------------------------------------------------
-- Spec
-- ------------------------------------------------------------------

spec :: Spec
spec = describe "Database recovery" $ do
    -- KVOnly-level tests
    it "fresh DB → ChooseKVOnly"
        $ withSystemTempDirectory "db-fresh"
        $ \p -> do
            s <- checkState p
            s `shouldBe` "ChooseKVOnly"

    it "inserts without replay → ChooseKVOnly"
        $ withSystemTempDirectory "db-ins"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns} ->
                insertN kvIns 10
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    it "inserts + clean replay → ChooseKVOnly"
        $ withSystemTempDirectory "db-replay"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns, kvReplay} -> do
                insertN kvIns 10
                kvReplay
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    it "crash during replay → NeedsRecovery"
        $ withSystemTempDirectory "db-crash"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns} ->
                insertN kvIns 10
            _ <- try @SomeException
                $ withKVOnly p crashOnReplay
                $ \KVActions{kvReplay} ->
                    kvReplay
            checkState p >>= (`shouldBe` "NeedsRecovery")

    it "crash + recovery → ChooseKVOnly"
        $ withSystemTempDirectory "db-recover"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns} ->
                insertN kvIns 10
            _ <- try @SomeException
                $ withKVOnly p crashOnReplay
                $ \KVActions{kvReplay} ->
                    kvReplay
            checkState p >>= (`shouldBe` "NeedsRecovery")
            withKVOnly p noReplay $ \_ -> pure ()
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    it "insert + delete + replay → ChooseKVOnly"
        $ withSystemTempDirectory "db-del"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns, kvDel, kvReplay} -> do
                insertN kvIns 10
                mapM_
                    (\i -> kvDel $ BL.pack [fromIntegral i, 0, 0, 0])
                    [1 .. 5 :: Int]
                kvReplay
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    it "two cycles: insert → replay → insert → crash → recover"
        $ withSystemTempDirectory "db-2cycle"
        $ \p -> do
            withKVOnly p noReplay $ \KVActions{kvIns, kvReplay} -> do
                insertN kvIns 5
                kvReplay
            withKVOnly p noReplay $ \KVActions{kvIns} ->
                insertN kvIns 5
            _ <- try @SomeException
                $ withKVOnly p crashOnReplay
                $ \KVActions{kvReplay} ->
                    kvReplay
            checkState p >>= (`shouldBe` "NeedsRecovery")
            withKVOnly p noReplay $ \_ -> pure ()
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    -- Update-level tests (with rollbacks)
    it "feed blocks + rollback → ChooseKVOnly"
        $ withSystemTempDirectory "db-rb"
        $ \p -> do
            withUpdate p noReplay False $ \u -> do
                u1 <- feedBlock u 1 (SlotNo 100) $ mkBlock 1
                u2 <- feedBlock u1 2 (SlotNo 100) $ mkBlock 2
                u3 <- feedBlock u2 3 (SlotNo 100) $ mkBlock 3
                st <- rollbackTipApply u3 $ At $ mkPoint 1
                case st of
                    Syncing u4 ->
                        void $ feedBlock u4 2 (SlotNo 100) $ mkBlock 12
                    _ -> fail "expected Syncing after rollback"
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    it "feed + rollback + replay crash → NeedsRecovery → recover"
        $ withSystemTempDirectory "db-rb-crash"
        $ \p -> do
            withUpdate p noReplay False $ \u -> do
                u1 <- feedBlock u 1 (SlotNo 100) $ mkBlock 1
                u2 <- feedBlock u1 2 (SlotNo 100) $ mkBlock 2
                u3 <- feedBlock u2 3 (SlotNo 100) $ mkBlock 3
                st <- rollbackTipApply u3 $ At $ mkPoint 1
                case st of
                    Syncing u4 ->
                        void $ feedBlock u4 2 (SlotNo 100) $ mkBlock 12
                    _ -> fail "expected Syncing"
            -- Force replay with crash
            _ <- try @SomeException
                $ withUpdate p crashOnReplay True
                $ \u ->
                    void $ feedBlock u 3 (SlotNo 4) $ mkBlock 13
            checkState p >>= (`shouldBe` "NeedsRecovery")
            withUpdate p noReplay False $ \_ -> pure ()
            checkState p >>= (`shouldBe` "ChooseKVOnly")

    -- Property: crash + recover → identical database.
    -- Same blocks fed to both, full column comparison.
    it "crash + recover produces identical DB to clean run"
        $ property
        $ forAll (genBlocks 8)
        $ \blocks -> QC.ioProperty $ do
            let tip = SlotNo 1000
                -- Feed all generated blocks
                feedAll u =
                    foldM
                        (\u' (slot, ops) -> feedBlock u' slot tip ops)
                        u
                        blocks
                -- One extra block to trigger isAtTip → toFull
                triggerSlot = length blocks + 1
            withSystemTempDirectory "db-clean" $ \cleanPath -> do
                withSystemTempDirectory "db-crash-prop" $ \crashPath -> do
                    -- Clean: feed blocks, then trigger replay
                    withUpdate cleanPath noReplay False $ \u ->
                        void $ feedAll u
                    withUpdate cleanPath noReplay True $ \u ->
                        void $ feedBlock u triggerSlot tip []
                    -- Crash: same blocks, crash on replay trigger
                    withUpdate crashPath noReplay False $ \u ->
                        void $ feedAll u
                    _ <- try @SomeException
                        $ withUpdate crashPath crashOnReplay True
                        $ \u ->
                            void $ feedBlock u triggerSlot tip []
                    -- Recover: resolves NeedsRecovery, completes
                    -- replay. Feed same trigger block so rollback
                    -- points match.
                    withUpdate crashPath noReplay True $ \u ->
                        void $ feedBlock u triggerSlot tip []
                    -- Every column must match
                    diffs <- dbDiff cleanPath crashPath
                    diffs `shouldBe` []
