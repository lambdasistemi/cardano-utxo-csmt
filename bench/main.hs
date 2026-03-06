module Main (main) where

import Bench.CSMT (loadGoldenUtxos, runInsertBench)
import Bench.Deserialization
    ( benchDeserialization
    )
import Criterion.Main
    ( bench
    , bgroup
    , defaultMain
    , env
    , nfIO
    )

main :: IO ()
main =
    defaultMain
        [ env loadGoldenUtxos $ \utxos ->
            bgroup
                "CSMT"
                [ bench
                    ( "insert "
                        ++ show (length utxos)
                        ++ " UTxOs"
                    )
                    $ nfIO
                    $ runInsertBench utxos
                ]
        , env
            loadGoldenUtxos
            benchDeserialization
        ]
