{-# LANGUAGE OverloadedStrings #-}

{- |
A debug tool for querying the UTxO CSMT RocksDB database.

Usage:
  db-query --db-path PATH --txid TXID [--index N]

Examples:
  db-query --db-path /data/db --txid d4bebf0c...3a --index 1
  db-query --db-path /data/db --txid d4bebf0c...3a
-}
module Main (main) where

import Cardano.UTxOCSMT.Application.UTxOs (unsafeMkTxIn)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (toShort)
import Database.RocksDB
    ( ColumnFamily
    , Config (..)
    , DB (..)
    , getCF
    , withDBCF
    )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

config :: Config
config =
    Config
        { createIfMissing = False
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> do
            hPutStrLn stderr
                $ unlines
                    [ "Usage: db-query --db-path PATH --txid TXID"
                        ++ " [--index N]"
                    , ""
                    , "Query the UTxO CSMT RocksDB for a TxIn key."
                    , "If --index is omitted, queries indices 0-3."
                    , ""
                    , "Examples:"
                    , "  db-query --db-path /data/db \\"
                    , "    --txid d4bebf0c9b57c3e7...693a"
                    , "    --index 1"
                    ]
            exitFailure
        Just (dbPath, txIdHex, mIndex) -> do
            case B16.decode (B8.pack txIdHex) of
                Left err -> do
                    hPutStrLn stderr $ "Invalid hex: " ++ err
                    exitFailure
                Right txIdBytes
                    | BS.length txIdBytes /= 32 -> do
                        hPutStrLn stderr
                            $ "TxId must be 32 bytes, got "
                                ++ show (BS.length txIdBytes)
                        exitFailure
                    | otherwise ->
                        queryDB dbPath txIdBytes mIndex

queryDB
    :: FilePath -> BS.ByteString -> Maybe Int -> IO ()
queryDB dbPath txIdBytes mIndex = do
    let indices = case mIndex of
            Just i -> [i]
            Nothing -> [0 .. 3]
    withDBCF
        dbPath
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        , ("config", config)
        ]
        $ \db -> do
            kvCF <- case columnFamilies db of
                (cf : _) -> pure cf
                [] -> fail "no column families"
            mapM_ (queryIndex db kvCF txIdBytes) indices

queryIndex
    :: DB -> ColumnFamily -> BS.ByteString -> Int -> IO ()
queryIndex db kvCF txIdBytes idx = do
    let key =
            BL.toStrict
                $ unsafeMkTxIn
                    (toShort txIdBytes)
                    (fromIntegral idx)
    result <- getCF db kvCF key
    case result of
        Nothing ->
            B8.putStrLn
                $ B16.encode (BS.take 8 txIdBytes)
                    <> "...#"
                    <> B8.pack (show idx)
                    <> ": NOT FOUND"
        Just v ->
            B8.putStrLn
                $ B16.encode (BS.take 8 txIdBytes)
                    <> "...#"
                    <> B8.pack (show idx)
                    <> ": FOUND ("
                    <> B8.pack (show (BS.length v))
                    <> " bytes, value hex: "
                    <> B16.encode (BS.take 40 v)
                    <> if BS.length v > 40
                        then "..."
                        else
                            ""
                                <> ")"

parseArgs :: [String] -> Maybe (FilePath, String, Maybe Int)
parseArgs = go Nothing Nothing Nothing
  where
    go _mPath mTxId mIdx ("--db-path" : p : rest) =
        go (Just p) mTxId mIdx rest
    go mPath _mTxId mIdx ("--txid" : t : rest) =
        go mPath (Just t) mIdx rest
    go mPath mTxId _mIdx ("--index" : i : rest) =
        go mPath mTxId (Just (read i)) rest
    go (Just p) (Just t) mIdx [] = Just (p, t, mIdx)
    go _ _ _ _ = Nothing
