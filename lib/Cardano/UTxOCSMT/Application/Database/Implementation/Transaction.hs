module Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    , CSMTContext (..)
    , CSMTOps (..)
    , mkCSMTOps
    , mkKVOnlyOps
    , replayJournal
    , journalEmpty
    , queryMerkleRoot
    , queryByAddress
    )
where

import CSMT (FromKV (..), Hashing, inserting)
import CSMT.Deletion (deleting)
import CSMT.Hashes (Hash)
import CSMT.Interface (Indirect (..), Key, root)
import CSMT.MTS
    ( CsmtImpl
    , csmtKVOnlyStoreT
    , replayJournalChunkT
    )
import CSMT.Proof.Completeness (collectValues)
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , injectStandalone
    )
import Control.Lens (review)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Database.KV.Cursor (firstEntry)
import Database.KV.Transaction
    ( Transaction
    , iterating
    , mapColumns
    , query
    )
import MTS.Interface
    ( MerkleTreeStore
    , Mode (..)
    , hoistMTS
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

-- ------------------------------------------------------------------
-- KVOnly mode
-- ------------------------------------------------------------------

{- | Build a KVOnly 'MerkleTreeStore' over 'Columns'.

Delegates to 'csmtKVOnlyStoreT' from haskell-mts and lifts
'Standalone' columns into 'Columns' via 'mapColumns'.
-}
mkKVOnlyOps
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> MerkleTreeStore
        'KVOnly
        CsmtImpl
        ( Transaction
            m
            cf
            (Columns slot Hash ByteString ByteString)
            op
        )
mkKVOnlyOps fkv =
    hoistMTS
        (mapColumns injectStandalone)
        (csmtKVOnlyStoreT fkv)

-- ------------------------------------------------------------------
-- Journal replay
-- ------------------------------------------------------------------

-- | Check if the journal column is empty.
journalEmpty
    :: (Monad m)
    => Transaction
        m
        cf
        (Columns slot hash key value)
        op
        Bool
journalEmpty = iterating JournalCol $ do
    me <- firstEntry
    pure $ case me of
        Nothing -> True
        Just _ -> False

{- | Replay journal entries against the CSMT, then clear them.

Delegates to 'replayJournalChunkT' from haskell-mts, lifting
'Standalone' columns into 'Columns' via 'mapColumns'.
-}
replayJournal
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> RunTransaction cf op slot Hash ByteString ByteString m
    -> m ()
replayJournal prefix chunkSize fkv h RunTransaction{transact} =
    loop
  where
    loop = do
        done <-
            transact
                $ mapColumns injectStandalone
                $ replayJournalChunkT prefix chunkSize fkv h
        unless done loop
