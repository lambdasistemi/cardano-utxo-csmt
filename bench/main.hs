module Main (main) where

import Bench.CSMT
    ( loadGoldenUtxos
    , runForwardTipStyleBench
    , runInsertBench
    , runKVOnlyInsertBench
    , runKVOnlyInternalTimingBench
    , runKVOnlyPrePopulatedBench
    , runKVOnlySmallBatchBench
    , runPureSerializationBench
    , runRepeatedTransactionsBench
    , runStressBench
    )
import Criterion.Main
    ( bench
    , bgroup
    , defaultMain
    , env
    , nfIO
    )
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--internal-only"] -> internalOnly
        ["--batch-scaling"] -> batchScaling
        ["--repeated-txns"] -> repeatedTxns
        ["--forward-tip"] -> forwardTipStyle
        ["--stress"] -> stressBench 2000
        ["--stress", n] -> stressBench (read n)
        _ -> criterionMain

internalOnly :: IO ()
internalOnly = do
    putStrLn "Loading golden UTxOs..."
    utxos <- loadGoldenUtxos
    putStrLn $ "Loaded " ++ show (length utxos) ++ " UTxOs"
    putStrLn ""
    putStrLn "=== Pre-populated DB scaling ==="
    mapM_
        ( \(rounds, label) -> do
            putStrLn $ "--- " ++ label ++ " ---"
            putStrLn $ "  Pre-populating " ++ label ++ "..."
            us <- runKVOnlyPrePopulatedBench rounds 8 utxos
            putStrLn
                $ "  "
                    ++ label
                    ++ " 8/txn: "
                    ++ show (round us :: Int)
                    ++ " μs/UTxO"
            putStrLn ""
        )
        [ (0, "empty DB")
        , (100, "100k entries")
        , (500, "500k entries")
        , (1000, "1M entries")
        , (2000, "2M entries")
        , (3000, "3M entries")
        ]

{- | Run many separate transactions (like production) to see
if per-UTxO cost increases over time
-}
repeatedTxns :: IO ()
repeatedTxns = do
    putStrLn "Loading golden UTxOs..."
    utxos <- loadGoldenUtxos
    putStrLn $ "Loaded " ++ show (length utxos) ++ " UTxOs"
    putStrLn ""
    putStrLn "=== Repeated separate transactions ==="
    runRepeatedTransactionsBench 500 utxos

-- | Test with the exact same code structure as production forwardTip
forwardTipStyle :: IO ()
forwardTipStyle = do
    putStrLn "Loading golden UTxOs..."
    utxos <- loadGoldenUtxos
    putStrLn $ "Loaded " ++ show (length utxos) ++ " UTxOs"
    putStrLn ""
    putStrLn
        "=== Forward-tip style (all ops in one txn, Operation wrapper) ==="
    runForwardTipStyleBench utxos

batchScaling :: IO ()
batchScaling = do
    putStrLn "Loading golden UTxOs..."
    utxos <- loadGoldenUtxos
    let n = length utxos
    putStrLn $ "Loaded " ++ show n ++ " UTxOs"
    putStrLn ""
    putStrLn "=== Batch size scaling (internal timing) ==="
    mapM_
        ( \batchSize -> do
            us <- runKVOnlyInternalTimingBench batchSize utxos
            putStrLn
                $ "  batch="
                    ++ show batchSize
                    ++ ": "
                    ++ show (round us :: Int)
                    ++ " μs/UTxO"
        )
        [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, n]

-- | Stress benchmark: full Update state machine, thousands of blocks
stressBench :: Int -> IO ()
stressBench nBlocks = do
    putStrLn "Loading golden UTxOs..."
    utxos <- loadGoldenUtxos
    putStrLn $ "Loaded " ++ show (length utxos) ++ " UTxOs"
    putStrLn ""
    putStrLn "=== Stress benchmark (full Update state machine) ==="
    runStressBench nBlocks utxos

criterionMain :: IO ()
criterionMain =
    defaultMain
        [ env loadGoldenUtxos $ \utxos ->
            bgroup
                "CSMT"
                [ bench ("full insert " ++ show (length utxos) ++ " UTxOs")
                    $ nfIO
                    $ runInsertBench utxos
                , bench ("kvonly 1-txn " ++ show (length utxos) ++ " UTxOs")
                    $ nfIO
                    $ runKVOnlyInsertBench utxos
                , bench ("kvonly 8/txn " ++ show (length utxos) ++ " UTxOs")
                    $ nfIO
                    $ runKVOnlySmallBatchBench 8 utxos
                , bench ("kvonly 1/txn " ++ show (length utxos) ++ " UTxOs")
                    $ nfIO
                    $ runKVOnlySmallBatchBench 1 utxos
                , bench ("pure serialization " ++ show (length utxos) ++ " UTxOs")
                    $ nfIO
                    $ runPureSerializationBench utxos
                ]
        ]
