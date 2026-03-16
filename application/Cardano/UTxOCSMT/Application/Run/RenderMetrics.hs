module Cardano.UTxOCSMT.Application.Run.RenderMetrics
    ( renderMetrics
    )
where

import CSMT ()
import Cardano.UTxOCSMT.Application.Metrics
    ( Metrics (..)
    , renderBlockPoint
    )
import Data.Maybe (fromMaybe)
import Ouroboros.Network.Block (blockNo)
import System.Console.ANSI (hClearScreen, hSetCursorPosition)
import System.IO (stdout)
import Text.Printf (printf)

renderMetrics :: Metrics -> IO ()
renderMetrics
    Metrics
        { averageQueueLength
        , maxQueueLength
        , utxoChangesCount
        , lastBlockPoint
        , utxoSpeed
        , blockSpeed
        , currentEra
        , currentMerkleRoot
        , syncPhase
        , avgCSMTDuration
        , avgRollbackDuration
        , avgFinalityDuration
        , avgBlockDecodeDuration
        , avgTransactionDuration
        , avgTotalBlockDuration
        } = do
        hClearScreen stdout
        hSetCursorPosition stdout 0 0
        putStrLn
            $ "Sync Phase: "
                ++ maybe "N/A" show syncPhase
                ++ "\nAverage Queue Length: "
                ++ show averageQueueLength
                ++ "\nMax Queue Length: "
                ++ show maxQueueLength
                ++ "\nTotal utxo changes processed: "
                ++ show utxoChangesCount
                ++ "\nUTXO Change Speed (utxo changes/sec): "
                ++ show utxoSpeed
                ++ "\nBlock Processing Speed (blocks/sec): "
                ++ show blockSpeed
                ++ "\nLast Block Point: "
                ++ maybe "N/A" renderBlockPoint lastBlockPoint
                ++ "\nLast Block Number: "
                ++ maybe "N/A" (show . blockNo . snd) lastBlockPoint
                ++ "\nLast Received Block Time: "
                ++ maybe "N/A" (show . fst) lastBlockPoint
                ++ "\nCurrent Merkle Root: "
                ++ maybe "N/A" show currentMerkleRoot
                ++ "\nCurrent Era: "
                ++ fromMaybe "N/A" currentEra
                ++ "\nAvg CSMT Duration: "
                ++ printf "%.0f" avgCSMTDuration
                ++ " μs"
                ++ "\nAvg Rollback Duration: "
                ++ printf "%.0f" avgRollbackDuration
                ++ " μs"
                ++ "\nAvg Finality Duration: "
                ++ printf "%.0f" avgFinalityDuration
                ++ " μs"
                ++ "\nAvg Block Decode: "
                ++ printf "%.0f" avgBlockDecodeDuration
                ++ " μs"
                ++ "\nAvg Transaction: "
                ++ printf "%.0f" avgTransactionDuration
                ++ " μs"
                ++ "\nAvg Total Block: "
                ++ printf "%.0f" avgTotalBlockDuration
                ++ " μs"
