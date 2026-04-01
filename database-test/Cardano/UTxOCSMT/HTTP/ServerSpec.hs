module Cardano.UTxOCSMT.HTTP.ServerSpec
    ( spec
    )
where

import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , fromKVHashes
    , generateInclusionProof
    , hashHashing
    , isoHash
    , mkHash
    , renderHash
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
    , queryByAddress
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , TipOf
    , WithSentinel (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Cardano.UTxOCSMT.HTTP.API
    ( InclusionProofResponse (..)
    , MerkleRootEntry (..)
    , ReadyResponse (..)
    , UTxOByAddressEntry (..)
    )
import Cardano.UTxOCSMT.HTTP.Base16
    ( encodeBase16Text
    )
import Cardano.UTxOCSMT.HTTP.Server (apiApp)
import ChainFollower.Backend qualified as Backend
import ChainFollower.Rollbacks.Store qualified as Store
import ChainFollower.Rollbacks.Types (RollbackPoint (..))
import ChainFollower.Runner
    ( Phase (..)
    , processBlock
    )
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens (lazy, prism', strict, view)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (nullTracer)
import Data.Aeson (eitherDecode)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Database.KV.Database ()
import Database.KV.RocksDB ()
import Database.KV.Transaction qualified as KV
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , withDBCF
    )
import Network.HTTP.Types (methodGet, status200, status404)
import Network.Wai (requestMethod)
import Network.Wai.Test
    ( SResponse (..)
    , Session
    , defaultRequest
    , request
    , runSession
    , setPath
    )
import Ouroboros.Network.Block (SlotNo (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | For testing, TipOf SlotNo = SlotNo
type instance TipOf SlotNo = SlotNo

testPrisms
    :: Prisms SlotNo Hash BL.ByteString BL.ByteString
testPrisms =
    Prisms
        { slotP =
            prism'
                ( BL.toStrict
                    . CBOR.toLazyByteString
                    . CBOR.encodeWord64
                    . fromIntegral
                    . unSlotNo
                )
                ( \bs ->
                    case CBOR.deserialiseFromBytes
                        CBOR.decodeWord64
                        (BL.fromStrict bs) of
                        Right (_, w) ->
                            Just (fromIntegral w)
                        Left _ -> Nothing
                )
        , hashP = isoHash
        , keyP = lazy
        , valueP = lazy
        }

testFromKV :: FromKV BL.ByteString BL.ByteString Hash
testFromKV =
    FromKV
        { isoK = strict . isoK fromKVHashes
        , fromV = fromV fromKVHashes . view strict
        , treePrefix = const []
        }

prefixedFromKV :: FromKV BL.ByteString BL.ByteString Hash
prefixedFromKV =
    FromKV
        { isoK = strict . isoK fromKVHashes
        , fromV = fromV fromKVHashes . view strict
        , treePrefix = byteStringToKey . BC.take 4 . BL.toStrict
        }

testCSMTContext :: CSMTContext Hash BL.ByteString BL.ByteString
testCSMTContext =
    CSMTContext
        { fromKV = testFromKV
        , hashing = hashHashing
        }

testConfig :: Config
testConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }

testSlotHash :: SlotNo -> Hash
testSlotHash n =
    mkHash $ BC.pack $ "blockhash" ++ show n

mkTestKey :: String -> BL.ByteString
mkTestKey s =
    BL.fromStrict $ padTo32 $ BC.pack s
  where
    padTo32 bs =
        bs
            `BC.append` BC.replicate
                (32 - BC.length bs)
                '_'

mkTestValue :: String -> BL.ByteString
mkTestValue = BL.fromStrict . BC.pack

encodeBase16 :: ByteString -> Text
encodeBase16 = TE.decodeUtf8 . convertToBase Base16

-- | Concrete column type.
type Cols =
    Columns SlotNo Hash BL.ByteString BL.ByteString

-- | Concrete transaction type.
type Tx a =
    KV.Transaction IO ColumnFamily Cols BatchOp a

-- | Concrete phase type.
type TestPhase =
    Phase
        IO
        ColumnFamily
        Cols
        BatchOp
        (SlotNo, [Operation BL.ByteString BL.ByteString])
        [Operation BL.ByteString BL.ByteString]
        (Hash, Maybe Hash)

columnFamilies :: [(String, Config)]
columnFamilies =
    [ ("kv", testConfig)
    , ("csmt", testConfig)
    , ("rollbacks", testConfig)
    , ("config", testConfig)
    , ("journal", testConfig)
    , ("rollbacks2", testConfig)
    ]

-- | Build a Following continuation from CSMTOps.
mkFollowing
    :: CSMTOps
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        BL.ByteString
        BL.ByteString
        Hash
    -> Backend.Following
        IO
        ( KV.Transaction
            IO
            ColumnFamily
            Cols
            BatchOp
        )
        (SlotNo, [Operation BL.ByteString BL.ByteString])
        [Operation BL.ByteString BL.ByteString]
        (Hash, Maybe Hash)
mkFollowing csmtOps =
    Backend.Following
        { Backend.follow =
            \(slot, operations) -> do
                invs <-
                    mapM
                        ( \case
                            Insert k v -> do
                                csmtInsert csmtOps k v
                                pure [Delete k]
                            Delete k -> do
                                mx <- KV.query KVCol k
                                csmtDelete csmtOps k
                                case mx of
                                    Nothing ->
                                        error
                                            $ "mkFollowing: cannot"
                                                <> " invert Delete"
                                                <> " at slot "
                                                <> show slot
                                    Just x -> pure [Insert k x]
                        )
                        operations
                let inverseOps = reverse (concat invs)
                merkleRoot <- csmtRootHash csmtOps
                pure
                    ( inverseOps
                    , Just (testSlotHash slot, merkleRoot)
                    , mkFollowing csmtOps
                    )
        , Backend.toRestoring =
            pure
                Backend.Restoring
                    { Backend.restore =
                        const
                            $ error
                                "test: restore not expected"
                    , Backend.toFollowing =
                        pure $ mkFollowing csmtOps
                    }
        , Backend.applyInverse =
            mapM_
                ( \case
                    Insert k v -> csmtInsert csmtOps k v
                    Delete k -> csmtDelete csmtOps k
                )
        }

-- | Set up a fresh DB with the Runner API.
withFreshDB
    :: ( RunTransaction
            ColumnFamily
            BatchOp
            SlotNo
            Hash
            BL.ByteString
            BL.ByteString
            IO
         -> TestPhase
         -> (forall a. Tx a -> IO a)
         -> IO r
       )
    -> IO r
withFreshDB action =
    withSystemTempDirectory "http-test" $ \dir ->
        withDBCF
            dir
            testConfig
            columnFamilies
            $ \db -> do
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        testPrisms
                let csmtOps =
                        mkCSMTOps testFromKV hashHashing
                -- Set up sentinel
                transact
                    $ Store.armageddonSetup
                        Rollbacks
                        Sentinel
                        Nothing
                let phase =
                        InFollowing 1 (mkFollowing csmtOps)
                action runner phase transact

-- | Set up a fresh DB with address-prefixed CSMT.
withFreshDBPrefixed
    :: ( RunTransaction
            ColumnFamily
            BatchOp
            SlotNo
            Hash
            BL.ByteString
            BL.ByteString
            IO
         -> TestPhase
         -> (forall a. Tx a -> IO a)
         -> IO r
       )
    -> IO r
withFreshDBPrefixed action =
    withSystemTempDirectory "http-test-prefixed" $ \dir ->
        withDBCF
            dir
            testConfig
            columnFamilies
            $ \db -> do
                runner@RunTransaction{transact} <-
                    newRunRocksDBTransaction
                        db
                        testPrisms
                let csmtOps =
                        mkCSMTOps prefixedFromKV hashHashing
                transact
                    $ Store.armageddonSetup
                        Rollbacks
                        Sentinel
                        Nothing
                let phase =
                        InFollowing 1 (mkFollowing csmtOps)
                action runner phase transact

-- | Query merkle roots from the Rollbacks column (Runner API store).
queryTestMerkleRoots
    :: RunTransaction
        ColumnFamily
        BatchOp
        SlotNo
        Hash
        BL.ByteString
        BL.ByteString
        IO
    -> IO [MerkleRootEntry]
queryTestMerkleRoots (RunTransaction runTx) =
    runTx $ do
        history <- Store.queryHistory Rollbacks
        pure $ concatMap toEntry (reverse history)
  where
    toEntry (slot, RollbackPoint{rpMeta}) = case (slot, rpMeta) of
        (Value slotNo, Just (blockHash, merkleRoot)) ->
            [ MerkleRootEntry
                { slotNo
                , blockHash
                , merkleRoot
                }
            ]
        _ -> []

-- | Query inclusion proof for testing.
queryTestInclusionProof
    :: RunTransaction
        ColumnFamily
        BatchOp
        SlotNo
        Hash
        BL.ByteString
        BL.ByteString
        IO
    -> BL.ByteString
    -> Text
    -> Word16
    -> IO (Maybe InclusionProofResponse)
queryTestInclusionProof (RunTransaction runTx) actualKey txIdText txIx =
    runTx $ do
        let CSMTContext{fromKV = fkv, hashing = h} = testCSMTContext
        result <-
            generateInclusionProof
                fkv
                KVCol
                CSMTCol
                actualKey
        merkle <- queryMerkleRoot h
        pure $ do
            (out, proof') <- result
            let merkleText = fmap (encodeBase16Text . renderHash) merkle
            pure
                InclusionProofResponse
                    { proofTxId = txIdText
                    , proofTxIx = txIx
                    , proofTxOut = encodeBase16 $ BL.toStrict out
                    , proofBytes = encodeBase16Text proof'
                    , proofMerkleRoot = merkleText
                    }

prefixedCSMTContext :: CSMTContext Hash BL.ByteString BL.ByteString
prefixedCSMTContext =
    CSMTContext
        { fromKV = prefixedFromKV
        , hashing = hashHashing
        }

-- | Query UTxOs by address for testing (using prefixed context).
queryTestByAddress
    :: RunTransaction
        ColumnFamily
        BatchOp
        SlotNo
        Hash
        BL.ByteString
        BL.ByteString
        IO
    -> Text
    -> IO (Either String [UTxOByAddressEntry])
queryTestByAddress (RunTransaction runTx) addressHex =
    case decodeBase16Hex addressHex of
        Nothing -> pure $ Left "Invalid base16 address"
        Just addressBytes -> do
            let CSMTContext{fromKV = fkv} = prefixedCSMTContext
                addressKey = byteStringToKey addressBytes
            results <- runTx $ queryByAddress fkv addressKey
            pure $ Right $ fmap toEntry results
  where
    decodeBase16Hex :: Text -> Maybe ByteString
    decodeBase16Hex t =
        case convertFromBase Base16 (TE.encodeUtf8 t) of
            Left (_ :: String) -> Nothing
            Right bs -> Just bs
    toEntry (txIn, txOut) =
        UTxOByAddressEntry
            { utxoTxIn = encodeBase16 $ BL.toStrict txIn
            , utxoTxOut = encodeBase16 $ BL.toStrict txOut
            }

-- | Default by-address handler that returns empty results.
noByAddress :: Text -> IO (Either String [UTxOByAddressEntry])
noByAddress = const $ pure $ Right []

-- | Sample ReadyResponse for synced state.
syncedResponse :: ReadyResponse
syncedResponse =
    ReadyResponse
        { ready = True
        , tipSlot = Just 1000
        , processedSlot = Just 1000
        , slotsBehind = Just 0
        }

-- | Sample ReadyResponse for not synced state.
notSyncedResponse :: ReadyResponse
notSyncedResponse =
    ReadyResponse
        { ready = False
        , tipSlot = Just 1000
        , processedSlot = Just 500
        , slotsBehind = Just 500
        }

-- | Run a WAI session against the apiApp.
session
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> (Text -> IO (Either String [UTxOByAddressEntry]))
    -> IO ReadyResponse
    -> Session b
    -> IO b
session a b c d e = flip runSession $ apiApp a b c d e (\_ _ _ -> pure Nothing)

spec :: Spec
spec = do
    describe "HTTP API" $ do
        describe "GET /ready" $ do
            it "returns synced status" $ do
                withFreshDB $ \runner _phase _transact ->
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/ready"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded = eitherDecode $ simpleBody resp
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right response -> do
                                    ready response `shouldBe` True
                                    tipSlot response `shouldBe` Just 1000
                                    processedSlot response `shouldBe` Just 1000
                                    slotsBehind response `shouldBe` Just 0

            it "returns not synced status" $ do
                withFreshDB $ \runner _phase _transact ->
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        noByAddress
                        (pure notSyncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/ready"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded = eitherDecode $ simpleBody resp
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right response -> do
                                    ready response `shouldBe` False
                                    slotsBehind response `shouldBe` Just 500

        describe "GET /merkle-roots" $ do
            it "returns empty list for fresh database" $ do
                withFreshDB $ \runner _phase _transact ->
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/merkle-roots"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded =
                                    eitherDecode $ simpleBody resp
                                        :: Either String [MerkleRootEntry]
                            liftIO $ decoded `shouldBe` Right []

            it "returns entries after processing blocks" $ do
                withFreshDB $ \runner phase transact -> do
                    let key1 = mkTestKey "utxo1"
                        val1 = mkTestValue "output1"
                    phase1 <-
                        processBlock
                            nullTracer
                            True
                            transact
                            Rollbacks
                            maxBound
                            (Value (SlotNo 100))
                            ( SlotNo 100
                            , [Insert key1 val1]
                            )
                            phase
                    let key2 = mkTestKey "utxo2"
                        val2 = mkTestValue "output2"
                    _ <-
                        processBlock
                            nullTracer
                            True
                            transact
                            Rollbacks
                            maxBound
                            (Value (SlotNo 200))
                            ( SlotNo 200
                            , [Insert key2 val2]
                            )
                            phase1
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/merkle-roots"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded = eitherDecode $ simpleBody resp
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right entries -> do
                                    length entries `shouldBe` 2
                                    slotNo <$> entries
                                        `shouldBe` [ SlotNo 200
                                                   , SlotNo 100
                                                   ]

        describe "GET /proof/{txId}/{txIx}" $ do
            it "returns 404 for non-existent UTxO" $ do
                withFreshDB $ \runner _phase _transact ->
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/proof/deadbeef/0"
                            liftIO $ simpleStatus resp `shouldBe` status404

            it "returns valid proof for existing UTxO" $ do
                withFreshDB $ \runner phase transact -> do
                    let testKey = mkTestKey "testutxo"
                        testValue = mkTestValue "testoutput"
                        testTxId = "7465737475747870"
                        testTxIx = 0 :: Word16
                    _ <-
                        processBlock
                            nullTracer
                            True
                            transact
                            Rollbacks
                            maxBound
                            (Value (SlotNo 100))
                            ( SlotNo 100
                            , [Insert testKey testValue]
                            )
                            phase
                    let proofQuery txId txIx =
                            if txId == testTxId && txIx == testTxIx
                                then
                                    queryTestInclusionProof
                                        runner
                                        testKey
                                        txId
                                        txIx
                                else pure Nothing
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        proofQuery
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/proof/7465737475747870/0"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded =
                                    eitherDecode (simpleBody resp)
                                        :: Either String InclusionProofResponse
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right proof -> do
                                    proofTxId proof `shouldBe` testTxId
                                    proofTxIx proof `shouldBe` testTxIx
                                    proofBytes proof `shouldSatisfy` (not . T.null)
                                    proofTxOut proof `shouldSatisfy` (not . T.null)

        describe "GET /utxos-by-address/:address" $ do
            it "returns empty list for unknown address" $ do
                withFreshDBPrefixed $ \runner _phase _transact -> do
                    let byAddress = queryTestByAddress runner
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        byAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/utxos-by-address/deadbeef"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded =
                                    eitherDecode $ simpleBody resp
                                        :: Either String [UTxOByAddressEntry]
                            liftIO $ decoded `shouldBe` Right []

            it "returns UTxOs matching the address prefix" $ do
                withFreshDBPrefixed $ \runner phase transact -> do
                    let key1 = mkTestKey "utxo1"
                        value1 = BL.fromStrict $ "AAAA" <> "output1-data"
                        key2 = mkTestKey "utxo2"
                        value2 = BL.fromStrict $ "AAAA" <> "output2-data"
                        key3 = mkTestKey "utxo3"
                        value3 = BL.fromStrict $ "BBBB" <> "output3-data"
                    _ <-
                        processBlock
                            nullTracer
                            True
                            transact
                            Rollbacks
                            maxBound
                            (Value (SlotNo 100))
                            ( SlotNo 100
                            ,
                                [ Insert key1 value1
                                , Insert key2 value2
                                , Insert key3 value3
                                ]
                            )
                            phase
                    let byAddress = queryTestByAddress runner
                        addr1Hex = "41414141"
                        addr2Hex = "42424242"
                    -- Query addr1: should return 2 entries
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        byAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        ("/utxos-by-address/" <> addr1Hex)
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded =
                                    eitherDecode $ simpleBody resp
                                        :: Either String [UTxOByAddressEntry]
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right entries -> length entries `shouldBe` 2
                    -- Query addr2: should return 1 entry
                    session
                        (pure Nothing)
                        (queryTestMerkleRoots runner)
                        (\_ _ -> pure Nothing)
                        byAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        ("/utxos-by-address/" <> addr2Hex)
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let decoded =
                                    eitherDecode $ simpleBody resp
                                        :: Either String [UTxOByAddressEntry]
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right entries -> length entries `shouldBe` 1
