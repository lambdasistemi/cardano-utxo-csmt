{- |
Module      : Cardano.UTxOCSMT.Application.OptionsSpec
Description : Unit tests for CLI options parser

Tests for the OptEnvConf-based CLI options parser, verifying:
- Default values are correctly applied
- Option parsing works with various argument combinations
- Network selection parses correctly and derives Mithril network
- Ancillary verification options work as expected
- Config file support works correctly
-}
module Cardano.UTxOCSMT.Application.OptionsSpec
    ( spec
    )
where

import Cardano.UTxOCSMT.Application.Options
    ( CardanoNetwork (..)
    , ConnectionMode (..)
    , MithrilNetwork (..)
    , MithrilOptions (..)
    , Options (..)
    , optionsParserCore
    )
import Data.Aeson (Object, Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import OptEnvConf.Args (parseArgs)
import OptEnvConf.Capability (Capabilities (..))
import OptEnvConf.EnvMap (EnvMap (..))
import OptEnvConf.Error (ParseError)
import OptEnvConf.Run (runParserOn)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

spec :: Spec
spec = describe "Application.Options" $ do
    describe "default values" $ do
        it "has network = Mainnet by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            network opts `shouldBe` Mainnet

        it "has mithrilEnabled = False by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            mithrilEnabled (mithrilOptions opts) `shouldBe` False

        it "has mithrilBootstrapOnly = False by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            mithrilBootstrapOnly (mithrilOptions opts) `shouldBe` False

        it "has mithrilClientPath = 'mithril-client' by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            mithrilClientPath (mithrilOptions opts) `shouldBe` "mithril-client"

        it "has mithrilSkipAncillaryVerification = False by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            mithrilSkipAncillaryVerification (mithrilOptions opts)
                `shouldBe` False

        it "has syncThreshold = 100 by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            syncThreshold opts `shouldBe` 100

    describe "--network option" $ do
        it "parses mainnet and sets MithrilMainnet" $ do
            opts <-
                expectSuccess
                    $ runParser ["-d", "/tmp/db", "--network", "mainnet"]
            network opts `shouldBe` Mainnet
            mithrilNetwork (mithrilOptions opts) `shouldBe` MithrilMainnet

        it "parses preprod and sets MithrilPreprod" $ do
            opts <-
                expectSuccess
                    $ runParser ["-d", "/tmp/db", "--network", "preprod"]
            network opts `shouldBe` Preprod
            mithrilNetwork (mithrilOptions opts) `shouldBe` MithrilPreprod

        it "parses preview and sets MithrilPreview" $ do
            opts <-
                expectSuccess
                    $ runParser ["-d", "/tmp/db", "--network", "preview"]
            network opts `shouldBe` Preview
            mithrilNetwork (mithrilOptions opts) `shouldBe` MithrilPreview

        it "fails on invalid network" $ do
            result <- runParser ["-d", "/tmp/db", "--network", "invalid"]
            isLeft result `shouldBe` True

    describe "--mithril-bootstrap flag" $ do
        it "enables mithril bootstrap when present" $ do
            opts <-
                expectSuccess
                    $ runParser ["-d", "/tmp/db", "--mithril-bootstrap"]
            mithrilEnabled (mithrilOptions opts) `shouldBe` True

    describe "--mithril-bootstrap-only flag" $ do
        it "enables bootstrap-only mode when present" $ do
            opts <-
                expectSuccess
                    $ runParser ["-d", "/tmp/db", "--mithril-bootstrap-only"]
            mithrilBootstrapOnly (mithrilOptions opts) `shouldBe` True

    describe "--mithril-client-path option" $ do
        it "accepts custom path" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--mithril-client-path", "/usr/bin/mithril"]
            mithrilClientPath (mithrilOptions opts)
                `shouldBe` "/usr/bin/mithril"

    describe "--mithril-aggregator-endpoint option" $ do
        it "accepts aggregator URL" $ do
            let url = "https://aggregator.example.com"
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--mithril-aggregator-endpoint", url]
            mithrilAggregatorUrl (mithrilOptions opts) `shouldBe` Just url

    describe "--mithril-genesis-verification-key option" $ do
        it "accepts genesis verification key" $ do
            let key = "5b313233343536373839305d"
            opts <-
                expectSuccess
                    $ runParser
                        [ "-d"
                        , "/tmp/db"
                        , "--mithril-genesis-verification-key"
                        , key
                        ]
            mithrilGenesisVk (mithrilOptions opts) `shouldBe` Just (T.pack key)

    describe "--mithril-download-dir option" $ do
        it "accepts download directory" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--mithril-download-dir", "/tmp/mithril"]
            mithrilDownloadDir (mithrilOptions opts)
                `shouldBe` Just "/tmp/mithril"

    describe "--mithril-ancillary-verification-key option" $ do
        it "accepts ancillary verification key" $ do
            let key = "5b393837363534333231305d"
            opts <-
                expectSuccess
                    $ runParser
                        [ "-d"
                        , "/tmp/db"
                        , "--mithril-ancillary-verification-key"
                        , key
                        ]
            mithrilAncillaryVk (mithrilOptions opts) `shouldBe` Just (T.pack key)

    describe "--mithril-skip-ancillary-verification flag" $ do
        it "enables skip verification when present" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--mithril-skip-ancillary-verification"]
            mithrilSkipAncillaryVerification (mithrilOptions opts)
                `shouldBe` True

    describe "environment variables" $ do
        it "reads MITHRIL_AGGREGATOR_ENDPOINT from environment" $ do
            let env =
                    Map.singleton
                        "MITHRIL_AGGREGATOR_ENDPOINT"
                        "https://env.example.com"
            opts <- expectSuccess $ runParserWithEnv ["-d", "/tmp/db"] env
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://env.example.com"

        it "reads MITHRIL_GENESIS_VERIFICATION_KEY from environment" $ do
            let env =
                    Map.singleton "MITHRIL_GENESIS_VERIFICATION_KEY" "envkey123"
            opts <- expectSuccess $ runParserWithEnv ["-d", "/tmp/db"] env
            mithrilGenesisVk (mithrilOptions opts) `shouldBe` Just "envkey123"

        it "reads MITHRIL_ANCILLARY_VERIFICATION_KEY from environment" $ do
            let env =
                    Map.singleton
                        "MITHRIL_ANCILLARY_VERIFICATION_KEY"
                        "ancillarykey"
            opts <- expectSuccess $ runParserWithEnv ["-d", "/tmp/db"] env
            mithrilAncillaryVk (mithrilOptions opts)
                `shouldBe` Just "ancillarykey"

        it "CLI option overrides environment variable" $ do
            let env =
                    Map.singleton
                        "MITHRIL_AGGREGATOR_ENDPOINT"
                        "https://env.example.com"
            opts <-
                expectSuccess
                    $ runParserWithEnv
                        [ "-d"
                        , "/tmp/db"
                        , "--mithril-aggregator-endpoint"
                        , "https://cli.example.com"
                        ]
                        env
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://cli.example.com"

    describe "--network devnet option" $ do
        it "parses devnet network" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--network", "devnet"]
            network opts `shouldBe` Devnet

    describe "combined options" $ do
        it "parses multiple options together" $ do
            opts <-
                expectSuccess
                    $ runParser
                        [ "-d"
                        , "/tmp/db"
                        , "--network"
                        , "preprod"
                        , "--mithril-bootstrap"
                        , "--mithril-client-path"
                        , "/custom/path"
                        , "--mithril-aggregator-endpoint"
                        , "https://example.com"
                        ]
            network opts `shouldBe` Preprod
            mithrilNetwork (mithrilOptions opts) `shouldBe` MithrilPreprod
            mithrilEnabled (mithrilOptions opts) `shouldBe` True
            mithrilClientPath (mithrilOptions opts) `shouldBe` "/custom/path"
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://example.com"

    describe "config file support" $ do
        it "reads network from config file" $ do
            configObj <-
                parseYamlObject
                    "network: Preview\n\
                    \node-name: test.example.com\n\
                    \node-port: 3001\n\
                    \genesis-file: /tmp/genesis.json\n\
                    \mithril: {}\n"
            opts <-
                expectSuccess
                    $ runParserWithConfig
                        ["-d", "/tmp/db"]
                        Map.empty
                        configObj
            network opts `shouldBe` Preview

        it "reads node-name and node-port from config file" $ do
            configObj <-
                parseYamlObject
                    "node-name: custom-node.example.com\n\
                    \node-port: 9999\n\
                    \genesis-file: /tmp/genesis.json\n\
                    \mithril: {}\n"
            opts <-
                expectSuccess
                    $ runParserWithConfig
                        ["-d", "/tmp/db"]
                        Map.empty
                        configObj
            case connectionMode opts of
                N2N{n2nHost, n2nPort} -> do
                    n2nHost `shouldBe` "custom-node.example.com"
                    n2nPort `shouldBe` 9999
                N2C{} -> fail "expected N2N connection mode"

        it "reads mithril options nested under mithril key" $ do
            configObj <-
                parseYamlObject
                    "network: Preprod\n\
                    \node-name: test.example.com\n\
                    \node-port: 3001\n\
                    \genesis-file: /tmp/genesis.json\n\
                    \mithril:\n\
                    \  mithril-aggregator-endpoint: https://config.example.com\n"
            opts <-
                expectSuccess
                    $ runParserWithConfig
                        ["-d", "/tmp/db"]
                        Map.empty
                        configObj
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://config.example.com"

        it "CLI overrides config file" $ do
            configObj <-
                parseYamlObject
                    "network: Preview\n\
                    \node-name: test.example.com\n\
                    \node-port: 3001\n\
                    \genesis-file: /tmp/genesis.json\n\
                    \mithril:\n\
                    \  mithril-aggregator-endpoint: https://config.example.com\n"
            opts <-
                expectSuccess
                    $ runParserWithConfig
                        [ "-d"
                        , "/tmp/db"
                        , "--mithril-aggregator-endpoint"
                        , "https://cli.example.com"
                        ]
                        Map.empty
                        configObj
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://cli.example.com"

        it "environment variable overrides config file" $ do
            configObj <-
                parseYamlObject
                    "network: Preview\n\
                    \node-name: test.example.com\n\
                    \node-port: 3001\n\
                    \genesis-file: /tmp/genesis.json\n\
                    \mithril:\n\
                    \  mithril-aggregator-endpoint: https://config.example.com\n"
            let env =
                    Map.singleton
                        "MITHRIL_AGGREGATOR_ENDPOINT"
                        "https://env.example.com"
            opts <-
                expectSuccess
                    $ runParserWithConfig ["-d", "/tmp/db"] env configObj
            mithrilAggregatorUrl (mithrilOptions opts)
                `shouldBe` Just "https://env.example.com"

-- | Run the parser with given arguments and empty environment
runParser
    :: [String]
    -> IO (Either (NonEmpty ParseError) Options)
runParser args = runParserWithEnv args Map.empty

-- | Run the parser with given arguments and environment
runParserWithEnv
    :: [String]
    -> Map.Map String String
    -> IO (Either (NonEmpty ParseError) Options)
runParserWithEnv args env =
    runParserOn
        (Capabilities Set.empty) -- No special capabilities needed
        Nothing -- No terminal capabilities
        optionsParserCore
        (parseArgs args)
        (EnvMap env)
        (Just defaultTestConfig)

{- | Default config object for tests
Includes required node-name, node-port, and empty mithril section
-}
defaultTestConfig :: Object
defaultTestConfig =
    KeyMap.fromList
        [ ("node-name", String "test-node.example.com")
        , ("node-port", Number 3001)
        , ("genesis-file", String "/tmp/genesis.json")
        , ("mithril", Object mempty)
        ]

-- | Run the parser with config object
runParserWithConfig
    :: [String]
    -> Map.Map String String
    -> Object
    -> IO (Either (NonEmpty ParseError) Options)
runParserWithConfig args env configObj =
    runParserOn
        (Capabilities Set.empty)
        Nothing
        optionsParserCore
        (parseArgs args)
        (EnvMap env)
        (Just configObj)

-- | Parse a YAML string into an Object
parseYamlObject :: String -> IO Object
parseYamlObject yamlStr =
    case Yaml.decodeEither' (encodeUtf8 yamlStr) of
        Left err -> fail $ "Failed to parse YAML: " <> show err
        Right obj -> pure obj
  where
    encodeUtf8 = T.encodeUtf8 . T.pack

-- | Expect parser to succeed, fail test otherwise
expectSuccess
    :: IO (Either (NonEmpty ParseError) Options)
    -> IO Options
expectSuccess action = do
    result <- action
    case result of
        Right opts -> pure opts
        Left errs ->
            fail
                $ "Expected successful parse, got errors: "
                    <> show errs

-- | Check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
