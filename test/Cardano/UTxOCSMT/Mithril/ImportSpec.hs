{- t |
Module      : Cardano.UTxOCSMT.Mithril.ImportSpec
Description : Tests for Mithril import to CSMT database

Tests that verify UTxOs can be imported from Mithril snapshots
into the CSMT database correctly.
-}
module Cardano.UTxOCSMT.Mithril.ImportSpec
    ( spec
    )
where

import CSMT.Backend.RocksDB (RunRocksDB (..))
import CSMT.Hashes (Hash, fromKVHashes, hashHashing, isoHash, mkHash)
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (ArmageddonParams)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , CSMTOps (..)
    , RunTransaction (..)
    , mkCSMTOps
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    )
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Codec.Serialise (deserialise)
import Control.Lens (prism')
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Tracer (nullTracer)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy qualified as LBS
import Database.RocksDB (Config (..), withDBCF)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec = describe "Mithril Import" $ do
    describe "CSMT insertion" $ do
        it "imports UTxOs from golden file to database" $ do
            -- Load golden file with CBOR-encoded UTxOs
            goldenBs <- LBS.readFile "test/fixtures/golden-utxos.cbor"
            let utxosLazy :: [(LBS.ByteString, LBS.ByteString)]
                utxosLazy = deserialise goldenBs
                -- Convert to strict ByteString for CSMT
                utxos = map (bimap LBS.toStrict LBS.toStrict) utxosLazy

            -- Verify we loaded the expected count
            length utxos `shouldBe` 1000

            withSystemTempDirectory "import-test" $ \tmpDir ->
                withRocksDB tmpDir $ \(RunRocksDB r) -> do
                    db <- r ask

                    -- Set up database with armageddon
                    let CSMTContext{fromKV = fkv, hashing = h} = csmtContext
                        CSMTOps{csmtInsert} = mkCSMTOps fkv h
                    runner <- newRunRocksDBTransaction db prisms
                    setup nullTracer runner armageddonParams

                    let RunTransaction{transact} = runner

                    -- Insert all UTxOs from golden file
                    forM_ utxos $ \(k, v) ->
                        transact $ csmtInsert k v

        it "handles empty import gracefully" $ do
            withSystemTempDirectory "import-test-empty" $ \tmpDir ->
                withRocksDB tmpDir $ \(RunRocksDB r) -> do
                    db <- r ask
                    let CSMTContext{fromKV = fkv, hashing = h} = csmtContext
                        CSMTOps{csmtInsert} = mkCSMTOps fkv h
                    runner <- newRunRocksDBTransaction db prisms
                    setup nullTracer runner armageddonParams

                    let RunTransaction{transact} = runner
                        emptyUtxos :: [(ByteString, ByteString)]
                        emptyUtxos = []

                    forM_ emptyUtxos $ \(k, v) ->
                        transact $ csmtInsert k v

-- | CSMT context for hashing
csmtContext :: CSMTContext Hash ByteString ByteString
csmtContext = CSMTContext{fromKV = fromKVHashes, hashing = hashHashing}

-- | Prisms for slot/hash encoding
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

-- | Armageddon parameters for database setup
armageddonParams :: ArmageddonParams Hash
armageddonParams = ArmageddonParams 1000 (mkHash "")

-- | Open RocksDB with required column families
withRocksDB
    :: FilePath
    -> (RunRocksDB -> IO b)
    -> IO b
withRocksDB path action =
    withDBCF
        path
        dbConfig
        [ ("kv", dbConfig)
        , ("csmt", dbConfig)
        , ("journal", dbConfig)
        , ("rollbacks", dbConfig)
        ]
        $ \db -> do
            action $ RunRocksDB $ flip runReaderT db

dbConfig :: Config
dbConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }
