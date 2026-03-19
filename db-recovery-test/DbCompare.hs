{- |
Module      : DbCompare
Description : Raw RocksDB database comparison
License     : Apache-2.0

Compares two RocksDB databases column family by column
family using raw iterators. Returns whether they're
identical and reports any differences.
-}
module DbCompare
    ( dbsEqual
    , dbDiff
    , ColumnDiff (..)
    ) where

import Cardano.UTxOCSMT.Application.Run.Config (withRocksDB)
import Data.ByteString (ByteString)
import Database.RocksDB
    ( DB (..)
    , Iterator
    , iterEntry
    , iterFirst
    , iterNext
    , withIterCF
    )

-- | Diff for one column family.
data ColumnDiff = ColumnDiff
    { cdName :: String
    , cdOnlyA :: [(ByteString, ByteString)]
    , cdOnlyB :: [(ByteString, ByteString)]
    , cdValueDiff :: [(ByteString, ByteString, ByteString)]
    }
    deriving (Show, Eq)

cdEmpty :: ColumnDiff -> Bool
cdEmpty (ColumnDiff _ a b v) = null a && null b && null v

-- | Column family names (must match withRocksDB order).
cfNames :: [String]
cfNames = ["default", "kv", "csmt", "rollbacks", "config", "journal"]

-- | Collect all entries from a column family.
collectCF :: DB -> Int -> IO [(ByteString, ByteString)]
collectCF db cfIdx
    | cfIdx >= length (columnFamilies db) = pure []
    | otherwise =
        withIterCF db (columnFamilies db !! cfIdx) $ \iter -> do
            iterFirst iter
            go iter []
  where
    go
        :: Iterator
        -> [(ByteString, ByteString)]
        -> IO [(ByteString, ByteString)]
    go iter acc = do
        me <- iterEntry iter
        case me of
            Nothing -> pure $ reverse acc
            Just kv -> do
                iterNext iter
                go iter (kv : acc)

-- | Diff two sorted lists of (key, value) pairs.
diffEntries
    :: [(ByteString, ByteString)]
    -> [(ByteString, ByteString)]
    -> ( [(ByteString, ByteString)] -- only in A
       , [(ByteString, ByteString)] -- only in B
       , [(ByteString, ByteString, ByteString)] -- value diff
       )
diffEntries [] bs = ([], bs, [])
diffEntries as [] = (as, [], [])
diffEntries aas@((ka, va) : as) bbs@((kb, vb) : bs)
    | ka < kb =
        let (oa, ob, vd) = diffEntries as bbs
        in  ((ka, va) : oa, ob, vd)
    | ka > kb =
        let (oa, ob, vd) = diffEntries aas bs
        in  (oa, (kb, vb) : ob, vd)
    | va == vb = diffEntries as bs
    | otherwise =
        let (oa, ob, vd) = diffEntries as bs
        in  (oa, ob, (ka, va, vb) : vd)

{- | Compare two databases. Returns list of column diffs
(empty list = identical).
-}
dbDiff :: FilePath -> FilePath -> IO [ColumnDiff]
dbDiff pathA pathB = do
    entriesA <- dumpAll pathA
    entriesB <- dumpAll pathB
    let n = max (length entriesA) (length entriesB)
        padA = entriesA ++ repeat []
        padB = entriesB ++ repeat []
        names = cfNames ++ map (\i -> "cf" ++ show i) [length cfNames ..]
        diffs =
            [ let (oa, ob, vd) = diffEntries a b
              in  ColumnDiff name oa ob vd
            | (name, a, b) <- take n $ zip3 names padA padB
            ]
    pure $ filter (not . cdEmpty) diffs
  where
    dumpAll path =
        withRocksDB path $ \db ->
            mapM (collectCF db) [0 .. length (columnFamilies db) - 1]

-- | Check if two databases are identical.
dbsEqual :: FilePath -> FilePath -> IO Bool
dbsEqual a b = null <$> dbDiff a b
