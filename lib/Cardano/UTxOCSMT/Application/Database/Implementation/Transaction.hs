module Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    , CSMTContext (..)
    , CSMTOps (..)
    , mkCSMTOps
    , kvCommonToCSMTOps
    , fullOpsToCSMTOps
    , openCSMTOps
    , DbState (..)
    , ReadyState (..)
    , ReplayEvent (..)
    , queryMerkleRoot
    , queryByAddress
    )
where

import CSMT
    ( FromKV (..)
    , Hashing
    , inserting
    )
import CSMT.Deletion (deleting)
import CSMT.Interface (Indirect (..), Key, root)
import CSMT.MTS
    ( CommonOps (..)
    , DbState (..)
    , Ops (..)
    , ReadyState (..)
    , ReplayEvent (..)
    , openOps
    )
import CSMT.Proof.Completeness (collectValues)
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Control.Lens (Iso', review)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Database.KV.Transaction
    ( Transaction
    , query
    )
import MTS.Interface qualified as MTS (Mode (..))

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

{- | Convert KVOnly 'CommonOps' to 'CSMTOps'.
Root hash is always 'Nothing' (KVOnly mode
does not maintain the CSMT tree).
-}
kvCommonToCSMTOps
    :: (Monad m)
    => CommonOps m cf d ops k v
    -> CSMTOps (Transaction m cf d ops) k v a
kvCommonToCSMTOps CommonOps{opsInsert, opsDelete} =
    CSMTOps
        { csmtInsert = opsInsert
        , csmtDelete = opsDelete
        , csmtRootHash = pure Nothing
        }

-- | Convert Full 'Ops' to 'CSMTOps'.
fullOpsToCSMTOps
    :: Ops 'MTS.Full m cf d ops k v a
    -> CSMTOps (Transaction m cf d ops) k v a
fullOpsToCSMTOps
    OpsFull
        { fullCommon =
            CommonOps{opsInsert, opsDelete}
        , opsRootHash
        } =
        CSMTOps
            { csmtInsert = opsInsert
            , csmtDelete = opsDelete
            , csmtRootHash = opsRootHash
            }

{- | Open CSMT ops with crash recovery, wiring 'KVCol',
'CSMTCol', and 'JournalCol' into the column-parametric
'openOps' from MTS.

Returns 'DbState' which must be resolved before use:
- 'NeedsRecovery': run recovery action first
- 'Ready': choose KVOnly or Full mode
-}
openCSMTOps
    :: (Monad m, Ord key, Monoid key)
    => Int
    -- ^ Bucket bits for parallel replay
    -> Int
    -- ^ Chunk size for journal batches
    -> Iso' value ByteString
    -- ^ Journal value serialization
    -> FromKV key value hash
    -> Hashing hash
    -> ( forall b
          . Transaction
                m
                cf
                (Columns slot hash key value)
                op
                b
         -> IO b
       )
    -- ^ Guarded runner for normal ops
    -> ( forall b
          . Transaction
                m
                cf
                (Columns slot hash key value)
                op
                b
         -> IO b
       )
    -- ^ Unguarded runner for parallel replay
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (called per replay chunk)
    -> IO
        ( DbState
            m
            cf
            (Columns slot hash key value)
            op
            key
            value
            hash
        )
openCSMTOps bucketBits chunkSize =
    openOps
        []
        bucketBits
        chunkSize
        KVCol
        CSMTCol
        JournalCol

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
