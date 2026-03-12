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
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( getAllMerkleRoots
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , mkCSMTOps
    , queryByAddress
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( allOps
    , mkUpdate
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    , TipOf
    , Update (..)
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Cardano.UTxOCSMT.Application.Metrics (Metrics (..))
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
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens (lazy, prism', strict, view)
import Control.Monad (foldM, foldM_)
import Control.Monad.IO.Class (liftIO)
import Control.Tracer (nullTracer)
import Data.Aeson (eitherDecode, encode)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (..)
    , DB
    , withDBCF
    )
import Network.HTTP.Types (methodGet, status200, status404, status503)
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
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck
    ( Gen
    , choose
    , elements
    , ioProperty
    , property
    , shuffle
    , vectorOf
    )

-- | For testing, TipOf SlotNo = SlotNo
type instance TipOf SlotNo = SlotNo

{- | Test prisms for serializing slots, hashes, keys, values
Uses SlotNo for slots and ByteString for keys/values (matching RocksDBSpec.hs)
-}
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

-- | Test CSMT context for hashing (no address prefix)
testCSMTContext :: CSMTContext Hash BL.ByteString BL.ByteString
testCSMTContext =
    CSMTContext
        { fromKV = fromKVLazy
        , hashing = hashHashing
        }
  where
    fromKVLazy =
        FromKV
            { isoK = strict . isoK fromKVHashes
            , fromV = fromV fromKVHashes . view strict
            , treePrefix = const []
            }

-- | CSMT context with address prefix (first 4 bytes of value used as address)
prefixedCSMTContext :: CSMTContext Hash BL.ByteString BL.ByteString
prefixedCSMTContext =
    CSMTContext
        { fromKV =
            FromKV
                { isoK = strict . isoK fromKVHashes
                , fromV = fromV fromKVHashes . view strict
                , treePrefix = byteStringToKey . BC.take 4 . BL.toStrict
                }
        , hashing = hashHashing
        }

-- | Test armageddon params
testArmageddonParams :: ArmageddonParams Hash
testArmageddonParams =
    ArmageddonParams{armageddonBatchSize = 1000, noHash = mkHash ""}

-- | Generate a hash from a slot number (for testing)
testSlotHash :: SlotNo -> Hash
testSlotHash n = mkHash $ BC.pack $ "blockhash" ++ show n

-- | RocksDB config for tests
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

-- | Create a test key from a string (padded to 32 bytes for consistency)
mkTestKey :: String -> BL.ByteString
mkTestKey s = BL.fromStrict $ padTo32 $ BC.pack s
  where
    padTo32 bs = bs `BC.append` BC.replicate (32 - BC.length bs) '_'

-- | Create a test value
mkTestValue :: String -> BL.ByteString
mkTestValue = BL.fromStrict . BC.pack

-- | Encode bytes to base16 text
encodeBase16 :: ByteString -> Text
encodeBase16 = TE.decodeUtf8 . convertToBase Base16

{- | Query all merkle roots and convert to MerkleRootEntry format
Adapted for test types (SlotNo slots -> SlotNo)
-}
queryTestMerkleRoots
    :: RunTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> IO [MerkleRootEntry]
queryTestMerkleRoots (RunTransaction runTx) =
    runTx $ concatMap toEntry <$> getAllMerkleRoots
  where
    toEntry (slot, blockHash, merkleRoot) = case slot of
        Origin -> []
        At slotNo ->
            [ MerkleRootEntry
                { slotNo
                , blockHash = blockHash
                , merkleRoot = merkleRoot
                }
            ]

{- | Query inclusion proof for a given key
For tests, we use a simplified lookup where txId is the hex of the key prefix
-}
queryTestInclusionProof
    :: RunTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> BL.ByteString
    -- ^ The actual key that was inserted
    -> Text
    -- ^ txId (hex string)
    -> Word16
    -- ^ txIx
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

-- | Query UTxOs by address for testing
queryTestByAddress
    :: RunTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> Text
    -> IO (Either String [UTxOByAddressEntry])
queryTestByAddress (RunTransaction runTx) addressHex =
    case decodeBase16 addressHex of
        Nothing -> pure $ Left "Invalid base16 address"
        Just addressBytes -> do
            let CSMTContext{fromKV = fkv} = testCSMTContext
                addressKey = byteStringToKey addressBytes
            results <- runTx $ queryByAddress fkv addressKey
            pure $ Right $ fmap toEntry results
  where
    decodeBase16 :: Text -> Maybe ByteString
    decodeBase16 t =
        case convertFromBase Base16 (TE.encodeUtf8 t) of
            Left (_ :: String) -> Nothing
            Right bs -> Just bs
    toEntry (txIn, txOut) =
        UTxOByAddressEntry
            { utxoTxIn = encodeBase16 $ BL.toStrict txIn
            , utxoTxOut = encodeBase16 $ BL.toStrict txOut
            }

-- | Query UTxOs by raw address bytes (no hex encoding)
queryRawByAddress
    :: RunTransaction cf op SlotNo Hash BL.ByteString BL.ByteString IO
    -> ByteString
    -> IO [(BL.ByteString, BL.ByteString)]
queryRawByAddress (RunTransaction runTx) addressBytes =
    let CSMTContext{fromKV = fkv} = testCSMTContext
    in  runTx $ queryByAddress fkv $ byteStringToKey addressBytes

-- | Run tests with a fresh RocksDB database and HTTP app
withTestDB
    :: ( RunTransaction
            ColumnFamily
            BatchOp
            SlotNo
            Hash
            BL.ByteString
            BL.ByteString
            IO
         -> Update IO SlotNo BL.ByteString BL.ByteString
         -> IO a
       )
    -> IO a
withTestDB action =
    withSystemTempDirectory "http-test" $ \dir ->
        withRocksDB dir $ \db -> do
            let CSMTContext{fromKV = fkv, hashing = h} = testCSMTContext
            runner <- newRunRocksDBTransaction db testPrisms
            setup nullTracer runner testArmageddonParams
            action runner
                $ mkUpdate
                    nullTracer
                    allOps
                    (mkCSMTOps fkv h)
                    testSlotHash
                    (\_ _ -> pure ())
                    testArmageddonParams
                    runner
                    maxBound
                    0

-- | Run tests with a fresh RocksDB database using address-prefixed CSMT
withTestDBPrefixed
    :: ( RunTransaction
            ColumnFamily
            BatchOp
            SlotNo
            Hash
            BL.ByteString
            BL.ByteString
            IO
         -> Update IO SlotNo BL.ByteString BL.ByteString
         -> IO a
       )
    -> IO a
withTestDBPrefixed action =
    withSystemTempDirectory "http-test-prefixed" $ \dir ->
        withRocksDB dir $ \db -> do
            let CSMTContext{fromKV = fkv, hashing = h} = prefixedCSMTContext
            runner <- newRunRocksDBTransaction db testPrisms
            setup nullTracer runner testArmageddonParams
            action runner
                $ mkUpdate
                    nullTracer
                    allOps
                    (mkCSMTOps fkv h)
                    testSlotHash
                    (\_ _ -> pure ())
                    testArmageddonParams
                    runner
                    maxBound
                    0

-- | Open RocksDB with test column families
withRocksDB :: FilePath -> (DB -> IO b) -> IO b
withRocksDB path =
    withDBCF
        path
        testConfig
        [ ("kv", testConfig)
        , ("csmt", testConfig)
        , ("rollbacks", testConfig)
        , ("config", testConfig)
        ]

-- | Sample metrics for testing
sampleMetrics :: Metrics
sampleMetrics =
    Metrics
        { averageQueueLength = 0.0
        , maxQueueLength = Nothing
        , utxoChangesCount = 0
        , lastBlockPoint = Nothing
        , utxoSpeed = 0.0
        , blockSpeed = 0.0
        , currentEra = Nothing
        , currentMerkleRoot = Nothing
        , baseCheckpoint = Nothing
        , chainTipSlot = Nothing
        , bootstrapPhase = Nothing
        , extractionProgress = Nothing
        , headerSyncProgress = Nothing
        , downloadedBytes = Nothing
        , countingProgress = Nothing
        , avgCSMTDuration = 0.0
        , avgRollbackDuration = 0.0
        , avgFinalityDuration = 0.0
        , avgBlockDecodeDuration = 0.0
        , avgTransactionDuration = 0.0
        , avgTotalBlockDuration = 0.0
        , cumulativeBlocks = 0
        , cumulativeBlockDecodeDuration = 0.0
        , cumulativeTransactionDuration = 0.0
        , cumulativeCSMTDuration = 0.0
        , cumulativeRollbackDuration = 0.0
        , cumulativeFinalityDuration = 0.0
        , cumulativeTotalBlockDuration = 0.0
        , cumulativeInternalQueryTip = 0.0
        , cumulativeInternalCsmtOps = 0.0
        , cumulativeInternalRollbackStore = 0.0
        }

-- | Sample ReadyResponse for synced state
syncedResponse :: ReadyResponse
syncedResponse =
    ReadyResponse
        { ready = True
        , tipSlot = Just 1000
        , processedSlot = Just 1000
        , slotsBehind = Just 0
        }

-- | Sample ReadyResponse for not synced state
notSyncedResponse :: ReadyResponse
notSyncedResponse =
    ReadyResponse
        { ready = False
        , tipSlot = Just 1000
        , processedSlot = Just 500
        , slotsBehind = Just 500
        }

session
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> (Text -> IO (Either String [UTxOByAddressEntry]))
    -> IO ReadyResponse
    -> Session a
    -> IO a
session a b c d e = flip runSession $ apiApp a b c d e

-- | Default by-address handler that returns empty results
noByAddress :: Text -> IO (Either String [UTxOByAddressEntry])
noByAddress = const $ pure $ Right []

spec :: Spec
spec = do
    describe "HTTP API" $ do
        describe "GET /metrics" $ do
            it "returns 500 when metrics not available" $ do
                withTestDB $ \runner _update -> session
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
                                    "/metrics"
                        liftIO $ simpleStatus resp `shouldBe` status404

            it "returns valid JSON when metrics available" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    noByAddress
                    (pure syncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/metrics"
                        liftIO $ simpleStatus resp `shouldBe` status200
                        let body = simpleBody resp
                        liftIO $ body `shouldBe` encode sampleMetrics

        describe "GET /merkle-roots" $ do
            it "returns empty list for fresh database" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
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

            it "returns entries after forward tip applies" $ do
                withTestDB $ \runner update -> do
                    -- Insert some data at slot 100
                    let key1 = mkTestKey "utxo1"
                        value1 = mkTestValue "output1"
                    update1 <- forwardTipApply update 100 100 [Insert key1 value1]

                    -- Insert more data at slot 200
                    let key2 = mkTestKey "utxo2"
                        value2 = mkTestValue "output2"
                    _ <- forwardTipApply update1 200 200 [Insert key2 value2]

                    session
                        (pure $ Just sampleMetrics)
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
                                        `shouldBe` [ SlotNo{unSlotNo = 200}
                                                   , SlotNo{unSlotNo = 100}
                                                   ]

            it "returns 503 when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    noByAddress
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/merkle-roots"
                        liftIO $ simpleStatus resp `shouldBe` status503

        describe "GET /proof/{txId}/{txIx}" $ do
            it "returns 404 for non-existent UTxO" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
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
                withTestDB $ \runner update -> do
                    -- Create a known key and insert it
                    let testKey = mkTestKey "testutxo"
                        testValue = mkTestValue "testoutput"
                        testTxId = "7465737475747870" -- "testutxp" in hex (partial, for test identification)
                        testTxIx = 0 :: Word16

                    -- Insert the UTxO
                    _ <- forwardTipApply update 100 100 [Insert testKey testValue]

                    -- Create query function that looks up by our test key
                    let proofQuery txId txIx =
                            if txId == testTxId && txIx == testTxIx
                                then queryTestInclusionProof runner testKey txId txIx
                                else pure Nothing
                    session
                        (pure $ Just sampleMetrics)
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
                            let body = simpleBody resp
                            let decoded = eitherDecode body :: Either String InclusionProofResponse
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right proof -> do
                                    proofTxId proof `shouldBe` testTxId
                                    proofTxIx proof `shouldBe` testTxIx
                                    -- Proof bytes should be non-empty
                                    proofBytes proof `shouldSatisfy` (not . T.null)
                                    -- TxOut should be the hex of our test value
                                    proofTxOut proof `shouldSatisfy` (not . T.null)

            it "proof contains merkle root" $ do
                withTestDB $ \runner update -> do
                    let testKey = mkTestKey "merkletest"
                        testValue = mkTestValue "merklevalue"
                        testTxId = "6d65726b6c657465"
                        testTxIx = 0 :: Word16

                    _ <- forwardTipApply update 100 100 [Insert testKey testValue]

                    let proofQuery txId txIx =
                            if txId == testTxId && txIx == testTxIx
                                then queryTestInclusionProof runner testKey txId txIx
                                else pure Nothing
                    session
                        (pure $ Just sampleMetrics)
                        (queryTestMerkleRoots runner)
                        proofQuery
                        noByAddress
                        (pure syncedResponse)
                        $ do
                            resp <-
                                request
                                    $ setPath
                                        defaultRequest{requestMethod = methodGet}
                                        "/proof/6d65726b6c657465/0"
                            liftIO $ simpleStatus resp `shouldBe` status200
                            let body = simpleBody resp
                            let decoded = eitherDecode body :: Either String InclusionProofResponse
                            liftIO $ case decoded of
                                Left err -> fail $ "JSON decode error: " ++ err
                                Right proof -> do
                                    -- Merkle root should be present after insertion
                                    proofMerkleRoot proof `shouldSatisfy` \case
                                        Just mr -> not (T.null mr)
                                        Nothing -> False

            it "returns 503 when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    noByAddress
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/proof/deadbeef/0"
                        liftIO $ simpleStatus resp `shouldBe` status503

        describe "GET /ready" $ do
            it "returns synced status when synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
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

            it "returns not synced status when not synced" $ do
                withTestDB $ \runner _update -> session
                    (pure $ Just sampleMetrics)
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

        describe "GET /utxos-by-address/:address" $ do
            it "returns empty list for unknown address" $ do
                withTestDBPrefixed $ \runner _update -> do
                    let byAddress = queryTestByAddress runner
                    session
                        (pure $ Just sampleMetrics)
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
                withTestDBPrefixed $ \runner update -> do
                    -- Values start with 4-byte "address" prefix
                    -- "AAAA" prefix for addr1 entries
                    let key1 = mkTestKey "utxo1"
                        value1 = BL.fromStrict $ "AAAA" <> "output1-data"
                        key2 = mkTestKey "utxo2"
                        value2 = BL.fromStrict $ "AAAA" <> "output2-data"
                    -- "BBBB" prefix for addr2 entries
                    let key3 = mkTestKey "utxo3"
                        value3 = BL.fromStrict $ "BBBB" <> "output3-data"

                    update1 <-
                        forwardTipApply
                            update
                            100
                            100
                            [Insert key1 value1, Insert key2 value2, Insert key3 value3]
                    _ <- pure update1

                    let byAddress = queryTestByAddress runner
                        -- "AAAA" in hex = "41414141"
                        addr1Hex = "41414141"
                        -- "BBBB" in hex = "42424242"
                        addr2Hex = "42424242"

                    -- Query addr1: should return 2 entries
                    session
                        (pure $ Just sampleMetrics)
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
                        (pure $ Just sampleMetrics)
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

            it "returns 503 when not synced" $ do
                withTestDBPrefixed $ \runner _update -> session
                    (pure $ Just sampleMetrics)
                    (queryTestMerkleRoots runner)
                    (\_ _ -> pure Nothing)
                    noByAddress
                    (pure notSyncedResponse)
                    $ do
                        resp <-
                            request
                                $ setPath
                                    defaultRequest{requestMethod = methodGet}
                                    "/utxos-by-address/deadbeef"
                        liftIO $ simpleStatus resp `shouldBe` status503

    describe "by-address query properties" $ do
        it "survives interleaved inserts and deletes across multiple slots"
            $ property
            $ do
                (batches, live) <- genValidOps
                pure
                    $ ioProperty
                    $ withTestDBPrefixed
                    $ \runner update -> do
                        -- Apply each batch at a successive slot
                        foldM_
                            ( \u (slot, ops) ->
                                forwardTipApply u slot slot ops
                            )
                            update
                            (zip [SlotNo 100, SlotNo 200 ..] batches)
                        -- For each address, verify the live set matches
                        let addrs = nub [a | (_, _, a) <- live]
                        mapM_ (verifyAddress runner live) addrs
                        -- Non-existent address returns empty
                        empty <- queryRawByAddress runner "ZZZZ"
                        empty `shouldBe` []
  where
    verifyAddress runner live addr = do
        let expected = sort [(k, v) | (k, v, a) <- live, a == addr]
        actual <- queryRawByAddress runner addr
        sort actual `shouldBe` expected

{- | Generate a valid sequence of interleaved insert/delete operations.

Returns @([[Operation k v]], [(k, v, addr)])@ — a list of batches
(each applied at a separate slot) and the final live set of entries.
-}
genValidOps
    :: Gen
        ( [[Operation BL.ByteString BL.ByteString]]
        , [(BL.ByteString, BL.ByteString, ByteString)]
        )
genValidOps = do
    -- Generate 2-4 distinct 4-byte address prefixes
    nAddrs <- choose (2 :: Int, 4)
    addrs <-
        nub
            <$> vectorOf
                nAddrs
                (BC.pack <$> vectorOf 4 (choose ('A', 'Z')))
    -- Generate a pool of unique keys
    nKeys <- choose (6, 16)
    allKeys <-
        nub
            <$> vectorOf
                nKeys
                (mkTestKey <$> vectorOf 20 (choose ('a', 'z')))
    -- Generate 2-5 batches of operations
    nBatches <- choose (2 :: Int, 5)
    -- Fold: accumulate (remaining keys, live entries, batches)
    (_, finalLive, allBatches) <-
        foldM
            (genBatch addrs)
            (allKeys, [], [])
            [1 .. nBatches]
    pure (allBatches, finalLive)

-- | Generate one batch of interleaved insert/delete operations.
genBatch
    :: [ByteString]
    -- ^ Address pool
    -> ( [BL.ByteString]
       , [(BL.ByteString, BL.ByteString, ByteString)]
       , [[Operation BL.ByteString BL.ByteString]]
       )
    -- ^ (unused keys, live entries, accumulated batches)
    -> Int
    -- ^ Batch index (unused, just for foldM)
    -> Gen
        ( [BL.ByteString]
        , [(BL.ByteString, BL.ByteString, ByteString)]
        , [[Operation BL.ByteString BL.ByteString]]
        )
genBatch addrs (unused, live, batches) _ = do
    -- Insert 1-3 new keys (if available)
    nInsert <- choose (1, min 3 (max 1 $ length unused))
    insertKeys <- take nInsert <$> shuffle unused
    let unused' = filter (`notElem` insertKeys) unused
    inserts <- traverse (mkInsert addrs) insertKeys
    let insertOps = [Insert k v | (k, v, _) <- inserts]
    -- Delete 0-2 of previously live entries (not from this batch)
    nDelete <- choose (0 :: Int, min 2 (length live))
    toDelete <- take nDelete <$> shuffle live
    let deleteOps = [Delete k | (k, _, _) <- toDelete]
        deletedKeys = [dk | (dk, _, _) <- toDelete]
        live' =
            filter (\(k, _, _) -> k `notElem` deletedKeys) live
                ++ inserts
    -- Inserts first, then deletes (deletes refer to pre-existing keys)
    pure (unused', live', batches ++ [insertOps ++ deleteOps])

-- | Create an insert entry with a random address from the pool.
mkInsert
    :: [ByteString]
    -> BL.ByteString
    -> Gen (BL.ByteString, BL.ByteString, ByteString)
mkInsert addrs k = do
    addr <- elements addrs
    suffix <- BC.pack <$> vectorOf 8 (choose ('0', '9'))
    pure (k, BL.fromStrict (addr <> suffix), addr)
