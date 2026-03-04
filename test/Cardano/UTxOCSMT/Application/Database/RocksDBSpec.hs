module Cardano.UTxOCSMT.Application.Database.RocksDBSpec
    ( spec
    )
where

import CSMT (FromKV (..))
import CSMT.Backend.RocksDB (RunRocksDB (..))
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.AppConfig
    ( AppConfig (..)
    , decodeAppConfig
    , encodeAppConfig
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (ArmageddonParams)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( clearBootstrapInProgress
    , clearSkipSlot
    , getBaseCheckpoint
    , getSkipSlot
    , isBootstrapInProgress
    , mkTransactionedQuery
    , putBaseCheckpoint
    , setBootstrapInProgress
    , setSkipSlot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunTransaction (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate
    )
import Cardano.UTxOCSMT.Application.Database.Interface (TipOf)
import Cardano.UTxOCSMT.Application.Database.Properties
    ( findValue
    , logOnFailure
    , populateWithSomeContent
    , propertyForwardAfterTipAppliesChanges
    , propertyForwardBeforeTipIsNoOp
    , propertyForwardFinalityAfterFinalityReduceTheRollbackWindow
    , propertyRollbackAfterBeforeTipUndoesChanges
    , propertyRollbackAfterTipDoesNothing
    , propertyRollbackBeforeFinalityTruncatesTheDatabase
    , propertyTipIsAfterFinalityOrMissing
    )
import Cardano.UTxOCSMT.Application.Database.Properties.Expected
    ( Context (..)
    , Generator (..)
    , PropertyWithExpected
    , WithExpected
    , getDump
    , runWithExpected
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens (preview, prism', review)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Tracer (nullTracer)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Char (isAlpha, isAscii)
import Data.List.NonEmpty (NonEmpty (..))
import Database.RocksDB (Config (..), withDBCF)
import Ouroboros.Network.Point (WithOrigin (..))
import System.Directory (removePathForcibly)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (NonNegative)
    , Property
    , Testable (..)
    , forAll
    , ioProperty
    , listOf
    , scale
    , suchThat
    )
import Test.QuickCheck.Monadic (monadic')

-- | For testing, TipOf Int = Int (slot equals tip type)
type instance TipOf Int = Int

newtype GenSlot = GenSlot Int
    deriving (Show, Eq, Ord)

instance Arbitrary GenSlot where
    arbitrary = do
        NonNegative x <- arbitrary
        pure $ GenSlot x

newtype GenKey = GenKey ByteString
    deriving (Show, Eq, Ord)

padto32 :: ByteString -> ByteString
padto32 bs = bs `BC.append` BC.replicate (32 - BC.length bs) '_'

instance Arbitrary GenKey where
    arbitrary =
        GenKey . padto32 . BC.pack
            <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

newtype GenValue = GenValue ByteString
    deriving (Show, Eq, Ord)

instance Arbitrary GenValue where
    arbitrary = GenValue . BC.pack <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

newtype GenHash = GenHash Hash
    deriving (Show, Eq, Ord)

instance Arbitrary GenHash where
    arbitrary =
        GenHash . mkHash . BC.pack
            <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

-- | Generator for AppConfig Int
genAppConfig :: Gen (AppConfig Int)
genAppConfig =
    AppConfig
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

csmtContext :: CSMTContext Hash ByteString ByteString
csmtContext = CSMTContext{fromKV = fromKVHashes, hashing = hashHashing}

prisms :: Prisms Int Hash ByteString ByteString
prisms =
    Prisms
        { slotP =
            prism'
                ( BL.toStrict
                    . CBOR.toLazyByteString
                    . CBOR.encodeWord64
                    . fromIntegral
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
        , keyP = id
        , valueP = id
        }

-- | Encode slot for config storage (uses the prism)
encodeSlot :: Int -> ByteString
encodeSlot = review (slotP prisms)

-- | Decode slot from config storage (uses the prism)
decodeSlot :: ByteString -> Maybe Int
decodeSlot = preview (slotP prisms)

runRocksDBProperties
    :: WithExpected IO Int ByteString ByteString a
    -> IO a
runRocksDBProperties prop =
    withSystemTempDirectory "rocksdb-test" $ \dir ->
        withRocksDB dir $ \(RunRocksDB r) -> do
            db <- r ask
            runner <- newRunRocksDBTransaction db prisms
            contextDatabase <- query db
            setup nullTracer runner armageddonParams
            let ctx =
                    Context
                        { contextDatabase
                        , contextGenerator =
                            Generator
                                { genSlot = do
                                    GenSlot x <- arbitrary
                                    pure x
                                , genKey = do
                                    GenKey x <- arbitrary
                                    pure x
                                , genValue = do
                                    GenValue v <- arbitrary
                                    pure v
                                }
                        }
            runWithExpected ctx (update runner) prop
  where
    CSMTContext{fromKV = fkv, hashing = h} = csmtContext
    armageddonParams = ArmageddonParams 1000 (mkHash "")
    query db =
        mkTransactionedQuery (isoK fkv)
            <$> newRunRocksDBTransaction db prisms
    update runner =
        mkUpdate
            nullTracer
            (mkCSMTOps fkv h)
            (const $ mkHash "")
            (\_ _ -> pure ())
            armageddonParams
            runner
            0

test
    :: PropertyWithExpected
        IO
        Int
        ByteString
        ByteString
        ()
    -> Property
test f =
    property
        $ fmap (ioProperty . runRocksDBProperties)
        $ scale (`div` 3)
        $ monadic' f

spec :: Spec
spec = do
    describe "Slot" $ do
        it "At 0 > Origin" $ do
            (At (0 :: Int) > Origin) `shouldBe` True
    describe "AppConfig CBOR" $ do
        it "roundtrips through encode/decode"
            $ forAll genAppConfig
            $ \cfg ->
                decodeAppConfig decodeSlot (encodeAppConfig encodeSlot cfg)
                    == Just cfg
    describe "findValue" $ do
        it "finds existing value" $ do
            let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
            let result = findValue 2 Nothing lst
            result `shouldBe` Just "two"
        it
            "returns the latest value if the requested key is beyond the largest key"
            $ do
                let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
                let result = findValue 4 Nothing lst
                result `shouldBe` Just "three"
        it "returns Nothing if the requested key is before the smallest key" $ do
            let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
            let result = findValue 0 Nothing lst
            result `shouldBe` Nothing
        it
            "finds a previous value if the requested key is missing but less than the next key"
            $ do
                let lst = [(1 :: Int, "one" :: String), (3, "three")]
                let result = findValue 2 Nothing lst
                result `shouldBe` Just "one"
    describe "RocksDB Database" $ do
        it "can be opened" $ test $ pure ()
        it "can be queried when empty" $ test $ pure ()
        it "can be populated" $ test $ do
            _ <- populateWithSomeContent
            pure ()
        it "keeps tip after finality" $ test $ do
            _ <- populateWithSomeContent
            propertyTipIsAfterFinalityOrMissing
        it "will not forward before tip" $ test $ do
            _ <- populateWithSomeContent
            propertyForwardBeforeTipIsNoOp
        it "will forward after tip and apply changes" $ test $ do
            logOnFailure "Starting forward after tip test"
            getDump >>= logOnFailure . ("Initial dump: " ++) . show
            _ <- populateWithSomeContent
            getDump >>= logOnFailure . ("After load dump: " ++) . show
            propertyForwardAfterTipAppliesChanges
        it "will not rollback to after tip" $ test $ do
            _ <- populateWithSomeContent
            propertyRollbackAfterTipDoesNothing
        it "will rollback before tip and after finality" $ test $ do
            d0 <- getDump
            ds <- populateWithSomeContent
            logOnFailure $ "Generated dumps: " ++ show ds
            let past = (Origin, d0) :| ds
            propertyRollbackAfterBeforeTipUndoesChanges past
        it "will truncate when rolling back before finality" $ test $ do
            _ <- populateWithSomeContent
            propertyRollbackBeforeFinalityTruncatesTheDatabase
        it "will forward finality" $ test $ do
            d0 <- getDump
            ds <- populateWithSomeContent
            logOnFailure $ "Generated dumps: " ++ show ds
            let past = (Origin, d0) :| ds
            propertyForwardFinalityAfterFinalityReduceTheRollbackWindow past
    describe "ConfigCol operations" $ do
        it "stores and retrieves base checkpoint" $ do
            withSystemTempDirectory "rocksdb-config-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunTransaction{transact} <-
                        newRunRocksDBTransaction db prisms
                    -- Initially no checkpoint
                    result1 <- transact $ getBaseCheckpoint decodeSlot
                    result1 `shouldBe` Nothing
                    -- Store checkpoint
                    transact
                        $ putBaseCheckpoint decodeSlot encodeSlot (42 :: Int)
                    -- Retrieve checkpoint
                    result2 <- transact $ getBaseCheckpoint decodeSlot
                    result2 `shouldBe` Just 42
        it "tracks bootstrap in progress marker" $ do
            withSystemTempDirectory "rocksdb-bootstrap-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunTransaction{transact} <-
                        newRunRocksDBTransaction db prisms
                    -- Initially not in progress
                    inProgress1 <-
                        transact
                            $ isBootstrapInProgress decodeSlot
                    inProgress1 `shouldBe` False
                    -- Set marker
                    transact
                        $ setBootstrapInProgress decodeSlot encodeSlot
                    -- Now in progress
                    inProgress2 <-
                        transact
                            $ isBootstrapInProgress decodeSlot
                    inProgress2 `shouldBe` True
                    -- Clear marker
                    transact
                        $ clearBootstrapInProgress decodeSlot encodeSlot
                    -- No longer in progress
                    inProgress3 <-
                        transact
                            $ isBootstrapInProgress decodeSlot
                    inProgress3 `shouldBe` False
        it "stores and retrieves skip slot" $ do
            withSystemTempDirectory "rocksdb-skip-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunTransaction{transact} <-
                        newRunRocksDBTransaction db prisms
                    -- Initially no skip slot
                    slot1 <- transact $ getSkipSlot decodeSlot
                    slot1 `shouldBe` Nothing
                    -- Set skip slot
                    transact
                        $ setSkipSlot decodeSlot encodeSlot 12345678
                    -- Retrieve skip slot
                    slot2 <- transact $ getSkipSlot decodeSlot
                    slot2 `shouldBe` Just 12345678
                    -- Clear skip slot
                    transact
                        $ clearSkipSlot decodeSlot encodeSlot
                    -- No longer set
                    slot3 <- transact $ getSkipSlot decodeSlot
                    slot3 `shouldBe` Nothing
        it "survives database close and reopen (crash recovery)" $ do
            let testDir = "/tmp/smoke-test-db"
            removePathForcibly testDir
            -- First session: set skip slot then "crash" (close DB)
            withRocksDB testDir $ \(RunRocksDB r) -> do
                db <- r ask
                RunTransaction{transact} <-
                    newRunRocksDBTransaction db prisms
                transact
                    $ setSkipSlot decodeSlot encodeSlot 98765432
            -- Second session: "restart" and verify skip slot persisted
            withRocksDB testDir $ \(RunRocksDB r) -> do
                db <- r ask
                RunTransaction{transact} <-
                    newRunRocksDBTransaction db prisms
                slot <- transact $ getSkipSlot decodeSlot
                slot `shouldBe` Just 98765432

withRocksDB
    :: FilePath
    -> (RunRocksDB -> IO b)
    -> IO b
withRocksDB path action = do
    withDBCF
        path
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        , ("config", config)
        , ("journal", config)
        ]
        $ \db -> do
            action $ RunRocksDB $ flip runReaderT db

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }
