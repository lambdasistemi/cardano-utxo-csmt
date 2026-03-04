module Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( mkQuery
    , mkTransactionedQuery
    , getAllMerkleRoots
    , getAppConfig
    , putAppConfig
    , modifyAppConfig
    , putBaseCheckpoint
    , getBaseCheckpoint
    , isBootstrapInProgress
    , setBootstrapInProgress
    , clearBootstrapInProgress
    , getSkipSlot
    , setSkipSlot
    , clearSkipSlot
    )
where

import CSMT.Hashes (byteStringToKey)
import CSMT.Interface (Indirect (..), Key)
import CSMT.Proof.Completeness (collectValues)
import Cardano.UTxOCSMT.Application.Database.Implementation.AppConfig
    ( AppConfig (..)
    , decodeAppConfig
    , defaultAppConfig
    , encodeAppConfig
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , ConfigKey (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Query (..)
    , hoistQuery
    )
import Control.Lens (Iso', review)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Word (Word64)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    , prevEntry
    )
import Database.KV.Transaction
    ( Transaction
    , insert
    , iterating
    , query
    )
import Ouroboros.Network.Point (WithOrigin (..))

-- | Create a query interface
mkQuery
    :: (Ord key, MonadFail m)
    => Iso' key Key
    -- ^ Bidirectional conversion between external keys and tree paths
    -> Query
        (Transaction m cf (Columns slot hash key value) op)
        slot
        key
        value
mkQuery isoK =
    Query
        { getValue = query KVCol
        , getTip =
            iterating RollbackPoints $ do
                ml <- lastEntry
                case ml of
                    Nothing -> lift . lift $ fail "No tip in rollback points"
                    Just e -> pure $ entryKey e
        , getFinality =
            iterating RollbackPoints $ do
                mf <- firstEntry
                case mf of
                    Nothing -> lift . lift $ fail "No finality point in rollback points"
                    Just e -> pure $ entryKey e
        , getByAddress = \addressBytes -> do
            let addressKey = byteStringToKey addressBytes
            indirects <- collectValues CSMTCol addressKey
            catMaybes <$> traverse lookupKV indirects
        }
  where
    lookupKV indirect = do
        let k = review isoK (jump indirect)
        mv <- query KVCol k
        pure $ fmap (k,) mv

{- | Create a 'Query' interface for RocksDB where all queries are run in separate transactions
Useful for property testing
-}
mkTransactionedQuery
    :: (Ord key, MonadFail m)
    => Iso' key Key
    -- ^ Bidirectional conversion between external keys and tree paths
    -> RunTransaction cf op slot hash key value m
    -> Query m slot key value
mkTransactionedQuery isoK (RunTransaction runTx) =
    hoistQuery runTx (mkQuery isoK)

{- | Get all merkle roots by iterating in reverse over the RollbackPoints table
Returns a list of (slot, blockHash, merkleRoot) tuples in reverse order (newest first)
-}
getAllMerkleRoots
    :: Monad m
    => Transaction
        m
        cf
        (Columns slot hash key value)
        op
        [(WithOrigin slot, hash, Maybe hash)]
getAllMerkleRoots =
    iterating RollbackPoints $ do
        ml <- lastEntry
        ($ ml) $ fix $ \go current -> case current of
            Nothing -> pure []
            Just
                Entry{entryKey, entryValue = RollbackPoint{rbpHash, rpbMerkleRoot}} -> do
                    rest <- prevEntry >>= go
                    pure $ (entryKey, rbpHash, rpbMerkleRoot) : rest

-- | Get the application configuration
getAppConfig
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> Transaction m cf (Columns slot hash key value) op (AppConfig slot)
getAppConfig decodeSlot = do
    mBs <- query ConfigCol AppConfigKey
    pure $ case mBs of
        Nothing -> defaultAppConfig
        Just bs ->
            fromMaybe
                defaultAppConfig
                (decodeAppConfig decodeSlot bs)

-- | Store the application configuration
putAppConfig
    :: (slot -> ByteString)
    -- ^ Slot encoder
    -> AppConfig slot
    -> Transaction m cf (Columns slot hash key value) op ()
putAppConfig encodeSlot cfg =
    insert ConfigCol AppConfigKey (encodeAppConfig encodeSlot cfg)

-- | Modify the application configuration
modifyAppConfig
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> (AppConfig slot -> AppConfig slot)
    -- ^ Modification function
    -> Transaction m cf (Columns slot hash key value) op ()
modifyAppConfig decodeSlot encodeSlot f = do
    cfg <- getAppConfig decodeSlot
    putAppConfig encodeSlot (f cfg)

-- | Get the base checkpoint from configuration
getBaseCheckpoint
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> Transaction m cf (Columns slot hash key value) op (Maybe slot)
getBaseCheckpoint decodeSlot =
    configBaseCheckpoint <$> getAppConfig decodeSlot

-- | Set the base checkpoint in configuration
putBaseCheckpoint
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> slot
    -> Transaction m cf (Columns slot hash key value) op ()
putBaseCheckpoint decodeSlot encodeSlot checkpoint =
    modifyAppConfig decodeSlot encodeSlot $ \cfg ->
        cfg{configBaseCheckpoint = Just checkpoint}

-- | Check if bootstrap is in progress (incomplete)
isBootstrapInProgress
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> Transaction m cf (Columns slot hash key value) op Bool
isBootstrapInProgress decodeSlot =
    configBootstrapInProgress <$> getAppConfig decodeSlot

-- | Mark bootstrap as in progress
setBootstrapInProgress
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> Transaction m cf (Columns slot hash key value) op ()
setBootstrapInProgress decodeSlot encodeSlot =
    modifyAppConfig decodeSlot encodeSlot $ \cfg ->
        cfg{configBootstrapInProgress = True}

-- | Clear bootstrap in progress marker
clearBootstrapInProgress
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> Transaction m cf (Columns slot hash key value) op ()
clearBootstrapInProgress decodeSlot encodeSlot =
    modifyAppConfig decodeSlot encodeSlot $ \cfg ->
        cfg{configBootstrapInProgress = False}

-- | Get the skip slot (target slot for skip mode after Mithril bootstrap)
getSkipSlot
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> Transaction m cf (Columns slot hash key value) op (Maybe Word64)
getSkipSlot decodeSlot =
    configSkipSlot <$> getAppConfig decodeSlot

-- | Set the skip slot (target for skip mode after Mithril bootstrap)
setSkipSlot
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> Word64
    -- ^ Target slot number
    -> Transaction m cf (Columns slot hash key value) op ()
setSkipSlot decodeSlot encodeSlot slot =
    modifyAppConfig decodeSlot encodeSlot $ \cfg ->
        cfg{configSkipSlot = Just slot}

-- | Clear the skip slot (called when skip mode completes)
clearSkipSlot
    :: Monad m
    => (ByteString -> Maybe slot)
    -- ^ Slot decoder
    -> (slot -> ByteString)
    -- ^ Slot encoder
    -> Transaction m cf (Columns slot hash key value) op ()
clearSkipSlot decodeSlot encodeSlot =
    modifyAppConfig decodeSlot encodeSlot $ \cfg ->
        cfg{configSkipSlot = Nothing}
