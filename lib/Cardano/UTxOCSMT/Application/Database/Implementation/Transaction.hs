module Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    , CSMTContext (..)
    , CSMTOps (..)
    , mkCSMTOps
    , kvOnlyCSMTOps
    , replayJournal
    , journalEmpty
    , queryMerkleRoot
    , queryByAddress
    )
where

import CSMT
    ( FromKV (..)
    , Hashing
    , inserting
    , insertingTreeOnly
    )
import CSMT.Deletion (deleting, deletingTreeOnly)
import CSMT.Interface (Indirect (..), Key, root)
import CSMT.Proof.Completeness (collectValues)
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Control.Lens (review)
import Control.Monad (unless)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Maybe (catMaybes, isNothing)
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Transaction
    ( KV
    , Transaction
    , delete
    , insert
    , iterating
    , query
    )

newtype RunTransaction cf op slot hash key value m = RunTransaction
    { transact
        :: forall a
         . Transaction m cf (Columns slot hash key value) op a
        -> m a
    }

-- | CSMT context bundling the key/value codec and hashing operations.
data CSMTContext hash key value = CSMTContext
    { fromKV :: FromKV key value hash
    , hashing :: Hashing hash
    }

{- | Transaction-level CSMT operations, closing over 'FromKV' and 'Hashing'.
Serves the same role as 'MerkleTreeStore' from MTS but operates within
the generic 'Transaction' monad with 'Columns'.
-}
data CSMTOps m key value hash = CSMTOps
    { csmtInsert :: key -> value -> m ()
    , csmtDelete :: key -> m ()
    , csmtRootHash :: m (Maybe hash)
    }

-- | Construct 'CSMTOps' from 'FromKV' and 'Hashing', closing over them.
mkCSMTOps
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Hashing hash
    -> CSMTOps
        (Transaction m cf (Columns slot hash key value) op)
        key
        value
        hash
mkCSMTOps fkv h =
    CSMTOps
        { csmtInsert = inserting [] fkv h KVCol CSMTCol
        , csmtDelete = deleting [] fkv h KVCol CSMTCol
        , csmtRootHash = root h CSMTCol []
        }

{- | Construct KVOnly 'CSMTOps' that writes to the KV store and
journal but does not update the CSMT tree. Root hash always
returns 'Nothing'.

The journal records each mutation so that 'replayJournal' can
later rebuild the tree in one pass.
-}
kvOnlyCSMTOps
    :: (Monad m, Ord key)
    => (value -> ByteString)
    -- ^ Serialize value for journal storage
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
                $ journalInsertTag <> serializeValue v
        , csmtDelete = \k -> do
            mv <- query KVCol k
            case mv of
                Nothing -> pure ()
                Just v -> do
                    delete KVCol k
                    insert JournalCol k
                        $ journalDeleteTag
                            <> serializeValue v
        , csmtRootHash = pure Nothing
        }

-- | Journal tag for insert entries.
journalInsertTag :: ByteString
journalInsertTag = B.singleton 0x01

-- | Journal tag for delete entries.
journalDeleteTag :: ByteString
journalDeleteTag = B.singleton 0x00

{- | Replay all journal entries against the CSMT tree, then
clear the journal. Processes in chunks of @chunkSize@ entries.
Traces the number of entries replayed after each chunk.
-}
replayJournal
    :: (Monad m, Ord key)
    => Tracer m Int
    -- ^ Tracer receiving the count of entries replayed per chunk
    -> Int
    -- ^ Chunk size
    -> (ByteString -> value)
    -- ^ Deserialize value from journal storage
    -> FromKV key value hash
    -> Hashing hash
    -> RunTransaction cf op slot hash key value m
    -> m ()
replayJournal tr chunkSize deserializeValue fkv h RunTransaction{transact} =
    loop
  where
    loop = do
        n <- transact $ do
            entries <- iterating JournalCol $ do
                me <- firstEntry
                case me of
                    Nothing -> pure []
                    Just e -> collectN (chunkSize - 1) [e]
            if null entries
                then pure 0
                else do
                    replayEntries fkv h deserializeValue entries
                    pure (length entries)
        unless (n == 0) $ do
            traceWith tr n
            loop

-- | Collect up to @n@ more cursor entries after the first.
collectN
    :: Monad m
    => Int
    -> [Entry c]
    -> Cursor
        (Transaction m cf (Columns slot hash key value) op)
        c
        [Entry c]
collectN 0 acc = pure (reverse acc)
collectN n acc = do
    me <- nextEntry
    case me of
        Nothing -> pure (reverse acc)
        Just e -> collectN (n - 1) (e : acc)

-- | Apply journal entries to the tree and delete them.
replayEntries
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Hashing hash
    -> (ByteString -> value)
    -> [Entry (KV key ByteString)]
    -> Transaction
        m
        cf
        (Columns slot hash key value)
        op
        ()
replayEntries fkv h deserializeValue entries = do
    mapM_ applyEntry entries
    mapM_ (delete JournalCol . entryKey) entries
  where
    applyEntry e =
        let (tag, vBytes) = parseJournalEntry (entryValue e)
            k = entryKey e
            v = deserializeValue vBytes
        in  case tag of
                JInsert ->
                    insertingTreeOnly [] fkv h CSMTCol k v
                JDelete ->
                    deletingTreeOnly [] fkv h CSMTCol k v

-- | Tag for journal entries.
data JournalTag = JInsert | JDelete

-- | Parse a journal entry into tag and value payload.
parseJournalEntry :: ByteString -> (JournalTag, ByteString)
parseJournalEntry bs = case B.uncons bs of
    Just (0x01, rest) -> (JInsert, rest)
    Just (0x00, rest) -> (JDelete, rest)
    _ -> error "parseJournalEntry: invalid tag byte"

-- | Check if the journal column is empty.
journalEmpty
    :: Monad m
    => Transaction m cf (Columns slot hash key value) op Bool
journalEmpty =
    iterating JournalCol $ isNothing <$> firstEntry

queryMerkleRoot
    :: Monad m
    => Hashing hash
    -> Transaction m cf (Columns slot hash key value) op (Maybe hash)
queryMerkleRoot h =
    root h CSMTCol []

{- | Query all UTxOs under a given address prefix.
Uses 'collectValues' to navigate the address-prefixed CSMT,
then reconstructs KV keys from leaf paths via the 'isoK' iso
and looks up values.
-}
queryByAddress
    :: (Monad m, Ord key)
    => FromKV key value hash
    -> Key
    -- ^ Address prefix as CSMT Key
    -> Transaction m cf (Columns slot hash key value) op [(key, value)]
queryByAddress FromKV{isoK} addressKey = do
    indirects <- collectValues CSMTCol [] addressKey
    catMaybes <$> traverse (lookupKV isoK) indirects
  where
    lookupKV isoK' Indirect{jump} = do
        let k = review isoK' jump
        mv <- query KVCol k
        pure $ fmap (k,) mv
