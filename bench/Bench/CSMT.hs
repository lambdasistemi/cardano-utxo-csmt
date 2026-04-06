{-# LANGUAGE BangPatterns #-}

{- |
Module      : Bench.CSMT
Description : CSMT insertion benchmarks

Benchmarks for CSMT insertion performance.
Note: Includes DB setup time as RocksDB uses bracket pattern.
-}
module Bench.CSMT
    ( loadGoldenUtxos
    , runInsertBench
    , runKVOnlyInsertBench
    , runKVOnlySmallBatchBench
    , runPureSerializationBench
    , runKVOnlyInternalTimingBench
    , runKVOnlyPrePopulatedBench
    , runRepeatedTransactionsBench
    , runForwardTipStyleBench
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , CSMTOps (..)
    , RunTransaction (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Codec.Serialise (deserialise)
import Control.Lens (Prism', lazy, prism', strict, view)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Tracer (nullTracer)
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl')
import Data.Monoid (Sum (..))
import Data.Word (Word64)
import Database.KV.Transaction
    ( Transaction
    , delete
    , insert
    , query
    )
import Database.RocksDB
    ( Config (..)
    , withDBCF
    )
import GHC.Clock (getMonotonicTimeNSec)
import System.IO.Temp (withSystemTempDirectory)

-- | Load golden UTxOs from file
loadGoldenUtxos :: IO [(ByteString, ByteString)]
loadGoldenUtxos = do
    content <- LBS.readFile "test/assets/golden-utxos.cbor"
    pure $ deserialise content

-- | Run insertion benchmark
runInsertBench
    :: [(ByteString, ByteString)]
    -> IO ()
runInsertBench utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTContext{fromKV = fkv, hashing = h} = benchContext
                    CSMTOps{csmtInsert} = mkCSMTOps fkv h
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                forM_ utxos $ \(k, v) ->
                    transact $ csmtInsert k v

-- | RocksDB configuration
rocksConfig :: Config
rocksConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

-- | Column families for bench databases.
columnFamilies :: [(String, Config)]
columnFamilies =
    [ ("kv", rocksConfig)
    , ("csmt", rocksConfig)
    , ("config", rocksConfig)
    , ("journal", rocksConfig)
    , ("metrics", rocksConfig)
    , ("rollbacks", rocksConfig)
    ]

-- | Prisms for ByteString keys/values
benchPrisms :: Prisms () Hash ByteString ByteString
benchPrisms = Prisms{..}
  where
    slotP :: Prism' StrictByteString ()
    slotP = prism' (const "") (const $ Just ())

    hashP :: Prism' StrictByteString Hash
    hashP = isoHash

    keyP :: Prism' StrictByteString ByteString
    keyP = lazy

    valueP :: Prism' StrictByteString ByteString
    valueP = lazy

-- | CSMT context for hashing
benchContext :: CSMTContext Hash ByteString ByteString
benchContext =
    CSMTContext
        { fromKV =
            FromKV
                { isoK = strict . isoK fromKVHashes
                , fromV = fromV fromKVHashes . view strict
                , treePrefix = const []
                }
        , hashing = hashHashing
        }

-- | KVOnly insertion benchmark — all inserts in one transaction
runKVOnlyInsertBench
    :: [(ByteString, ByteString)]
    -> IO ()
runKVOnlyInsertBench utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert} =
                        kvOnlyCSMTOps (view strict)
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                transact
                    $ forM_ utxos
                    $ uncurry csmtInsert

-- | KVOnly insertion benchmark — small batches like production
runKVOnlySmallBatchBench
    :: Int
    -> [(ByteString, ByteString)]
    -> IO ()
runKVOnlySmallBatchBench batchSize utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert} =
                        kvOnlyCSMTOps (view strict)
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                forM_ (chunksOf batchSize utxos)
                    $ \chunk ->
                        transact
                            $ forM_ chunk
                            $ uncurry csmtInsert

-- | Pure serialization benchmark — no DB, just BL.toStrict + tag
runPureSerializationBench
    :: [(ByteString, ByteString)]
    -> IO Int
runPureSerializationBench utxos =
    pure
        $! foldl'
            ( \acc (k, v) ->
                let !sk = LBS.toStrict k
                    !sv = B.singleton 0x01 <> LBS.toStrict v
                in  acc + B.length sk + B.length sv
            )
            0
            utxos

-- | Benchmark with internal timing — measures only in-txn time
runKVOnlyInternalTimingBench
    :: Int
    -> [(ByteString, ByteString)]
    -> IO Double
runKVOnlyInternalTimingBench batchSize utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert} =
                        kvOnlyCSMTOps (view strict)
                    clock
                        :: Transaction
                            IO
                            cf
                            cols
                            op
                            Word64
                    clock = lift (lift (liftIO getMonotonicTimeNSec))
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                totalNs <- fmap sum
                    $ forM (chunksOf batchSize utxos)
                    $ \chunk -> transact $ do
                        t1 <- clock
                        result <-
                            forM
                                (zip [0 :: Int ..] chunk)
                                $ \(_, (k, v)) -> do
                                    csmtInsert k v
                                    pure (Sum (1 :: Int), Sum (0 :: Int), [k])
                        let (Sum _nI, Sum _nD, _invs) =
                                mconcat result
                        t2 <- clock
                        pure (t2 - t1)
                pure
                    $ fromIntegral totalNs
                        / (1000 * fromIntegral (length utxos))

-- | Benchmark with pre-populated DB — reproduces production conditions
runKVOnlyPrePopulatedBench
    :: Int
    -- ^ Number of rounds to pre-populate (each round = 1000 UTxOs)
    -> Int
    -- ^ Batch size for measurement
    -> [(ByteString, ByteString)]
    -> IO Double
runKVOnlyPrePopulatedBench rounds batchSize utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert} =
                        kvOnlyCSMTOps (view strict)
                    clock
                        :: Transaction
                            IO
                            cf
                            cols
                            op
                            Word64
                    clock = lift (lift (liftIO getMonotonicTimeNSec))
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                -- Pre-populate: insert rounds × 1000 entries
                forM_ [1 .. rounds] $ \r -> do
                    let prefix =
                            LBS.fromStrict
                                $ B.pack
                                    [ fromIntegral (r `div` 256)
                                    , fromIntegral (r `mod` 256)
                                    ]
                    transact
                        $ forM_ utxos
                        $ \(k, v) ->
                            csmtInsert (prefix <> k) v
                    when (r `mod` 100 == 0)
                        $ putStrLn
                        $ "  pre-populated "
                            ++ show (r * length utxos)
                            ++ " entries"
                -- Now measure on top of populated DB
                let measureUtxos =
                        [ (LBS.fromStrict (B.pack [0xFF, 0xFF]) <> k, v)
                        | (k, v) <- utxos
                        ]
                totalNs <- fmap sum
                    $ forM (chunksOf batchSize measureUtxos)
                    $ \chunk -> transact $ do
                        t1 <- clock
                        result <-
                            forM
                                (zip [0 :: Int ..] chunk)
                                $ \(_, (k, v)) -> do
                                    csmtInsert k v
                                    pure (Sum (1 :: Int), Sum (0 :: Int), [k])
                        let (Sum _nI, Sum _nD, _invs) =
                                mconcat result
                        t2 <- clock
                        pure (t2 - t1)
                pure
                    $ fromIntegral totalNs
                        / (1000 * fromIntegral (length measureUtxos))

{- | Run many separate transactions, measuring per-UTxO cost over time.
Each transaction inserts one batch, like production forwardTip.
-}
runRepeatedTransactionsBench
    :: Int
    -- ^ Number of rounds (each round = all UTxOs)
    -> [(ByteString, ByteString)]
    -> IO ()
runRepeatedTransactionsBench rounds utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert} =
                        kvOnlyCSMTOps (view strict)
                    clock
                        :: Transaction
                            IO
                            cf
                            cols
                            op
                            Word64
                    clock = lift (lift (liftIO getMonotonicTimeNSec))
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                let nUtxos = length utxos
                forM_ [1 .. rounds] $ \r -> do
                    -- Each round: one transaction per UTxO batch
                    -- (simulates one block = one transaction)
                    let prefix =
                            LBS.fromStrict
                                $ B.pack
                                    [ fromIntegral (r `div` 256)
                                    , fromIntegral (r `mod` 256)
                                    ]
                    ns <- transact $ do
                        t1 <- clock
                        forM_ utxos $ \(k, v) ->
                            csmtInsert (prefix <> k) v
                        t2 <- clock
                        pure (t2 - t1)
                    let usPerUtxo =
                            fromIntegral ns
                                / (1000 * fromIntegral nUtxos)
                                :: Double
                    when (r `mod` 50 == 0)
                        $ putStrLn
                        $ "  round "
                            ++ show r
                            ++ " ("
                            ++ show (r * nUtxos)
                            ++ " total entries): "
                            ++ show (round usPerUtxo :: Int)
                            ++ " μs/UTxO"

-- | Split list into chunks of given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (h, t) = splitAt n xs
    in  h : chunksOf n t

{- | Benchmark that mirrors the production forwardTip pattern exactly:
all UTxOs in one transaction, using Operation wrapper, with
clock/forM/clock structure matching the production do-block.
-}
runForwardTipStyleBench
    :: [(ByteString, ByteString)]
    -> IO ()
runForwardTipStyleBench utxos =
    withSystemTempDirectory "csmt-bench" $ \tmpDir ->
        withDBCF
            tmpDir
            rocksConfig
            columnFamilies
            $ \db -> do
                let CSMTOps{csmtInsert, csmtDelete} =
                        kvOnlyCSMTOps (view strict)
                    clock
                        :: Transaction
                            IO
                            cf
                            cols
                            op
                            Word64
                    clock = lift (lift (liftIO getMonotonicTimeNSec))
                    -- Convert to Operations like production does
                    -- Duplicate to get ~2040 like production big blocks
                    doubled =
                        utxos
                            ++ [(LBS.fromStrict (B.singleton 0xFF) <> k, v) | (k, v) <- utxos]
                    ops :: [Operation ByteString ByteString]
                    ops = [Insert k v | (k, v) <- doubled]
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        benchPrisms
                setup
                    nullTracer
                    runner
                    benchArmageddonParams
                -- Run multiple rounds to see if it's stable
                forM_ [1 :: Int .. 10] $ \rnd -> do
                    t0 <- getMonotonicTimeNSec
                    -- This mirrors production goKVOnly exactly:
                    _stored <- transact $ do
                        tCsmt0 <- clock
                        let nTotal = length ops
                        forM_ (zip [0 :: Int ..] ops) $ \(i, op) -> do
                            t <-
                                if i < 10 || i `mod` 100 == 0 || i >= nTotal - 10
                                    then Just <$> clock
                                    else pure Nothing
                            case op of
                                Insert k v ->
                                    csmtInsert k v
                                Delete k ->
                                    void $ csmtDelete k
                            case t of
                                Just t' -> do
                                    t'' <- clock
                                    lift
                                        ( lift
                                            ( liftIO
                                                ( putStrLn
                                                    $ "    op["
                                                        ++ show i
                                                        ++ "]: "
                                                        ++ show (fromIntegral (t'' - t') / 1000 :: Double)
                                                        ++ " μs"
                                                )
                                            )
                                        )
                                Nothing -> pure ()
                        tCsmt1 <- clock
                        let nOps = length ops
                            usPerOp =
                                fromIntegral (tCsmt1 - tCsmt0)
                                    / (1000 * fromIntegral nOps)
                                    :: Double
                        _ <- clock
                        pure (nOps, usPerOp)
                    t1 <- getMonotonicTimeNSec
                    let (nOps, usPerOp) = _stored
                        totalMs =
                            fromIntegral (t1 - t0) / 1e6 :: Double
                    putStrLn
                        $ "  round "
                            ++ show rnd
                            ++ ": "
                            ++ show nOps
                            ++ " ops, "
                            ++ show (Prelude.round usPerOp :: Int)
                            ++ " μs/UTxO internal, "
                            ++ show (Prelude.round totalMs :: Int)
                            ++ " ms total"

-- | Armageddon params for setup
benchArmageddonParams :: ArmageddonParams Hash
benchArmageddonParams =
    ArmageddonParams
        { noHash = mkHash ""
        , armageddonBatchSize = 1000
        }

{- | Construct KVOnly 'CSMTOps' for benchmarking.
Writes to KV + journal but does not update the CSMT tree.
-}
kvOnlyCSMTOps
    :: (Monad m, Ord key)
    => (value -> StrictByteString)
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
kvOnlyCSMTOps serializeValue =
    CSMTOps
        { csmtInsert = \k v -> do
            insert KVCol k v
            insert JournalCol k
                $ B.singleton 0x01 <> serializeValue v
        , csmtDelete = \k -> do
            mv <- query KVCol k
            case mv of
                Nothing -> pure ()
                Just v -> do
                    delete KVCol k
                    insert JournalCol k
                        $ B.singleton 0x00
                            <> serializeValue v
        , csmtRootHash = pure Nothing
        }
