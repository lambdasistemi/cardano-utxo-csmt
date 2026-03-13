{- |
Module      : Cardano.UTxOCSMT.Application.OptionsSpec
Description : Unit tests for CLI options parser

Tests for the OptEnvConf-based CLI options parser, verifying:
- Default values are correctly applied
- Option parsing works with various argument combinations
- Network selection parses correctly
- Config file support works correctly
-}
module Cardano.UTxOCSMT.Application.OptionsSpec
    ( spec
    )
where

import Cardano.UTxOCSMT.Application.Options
    ( CardanoNetwork (..)
    , ConnectionMode (..)
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

        it "has syncThreshold = 100 by default" $ do
            opts <- expectSuccess $ runParser ["-d", "/tmp/db"]
            syncThreshold opts `shouldBe` 100

    describe "--network option" $ do
        it "parses mainnet" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--network", "mainnet"]
            network opts `shouldBe` Mainnet

        it "parses preprod" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--network", "preprod"]
            network opts `shouldBe` Preprod

        it "parses preview" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--network", "preview"]
            network opts `shouldBe` Preview

        it "parses devnet" $ do
            opts <-
                expectSuccess
                    $ runParser
                        ["-d", "/tmp/db", "--network", "devnet"]
            network opts `shouldBe` Devnet

        it "fails on invalid network" $ do
            result <-
                runParser
                    ["-d", "/tmp/db", "--network", "invalid"]
            isLeft result `shouldBe` True

    describe "config file support" $ do
        it "reads network from config file" $ do
            configObj <-
                parseYamlObject
                    "network: Preview\n\
                    \node-name: test.example.com\n\
                    \node-port: 3001\n\
                    \genesis-file: /tmp/genesis.json\n"
            opts <-
                expectSuccess
                    $ runParserWithConfig
                        ["-d", "/tmp/db"]
                        Map.empty
                        configObj
            network opts `shouldBe` Preview

        it "reads node-name and node-port from config file"
            $ do
                configObj <-
                    parseYamlObject
                        "node-name: custom-node.example.com\n\
                        \node-port: 9999\n\
                        \genesis-file: /tmp/genesis.json\n"
                opts <-
                    expectSuccess
                        $ runParserWithConfig
                            ["-d", "/tmp/db"]
                            Map.empty
                            configObj
                case connectionMode opts of
                    N2N{n2nHost, n2nPort} -> do
                        n2nHost
                            `shouldBe` "custom-node.example.com"
                        n2nPort `shouldBe` 9999
                    N2C{} ->
                        fail "expected N2N connection mode"

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
Includes required node-name, node-port
-}
defaultTestConfig :: Object
defaultTestConfig =
    KeyMap.fromList
        [ ("node-name", String "test-node.example.com")
        , ("node-port", Number 3001)
        , ("genesis-file", String "/tmp/genesis.json")
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
        Left err ->
            fail $ "Failed to parse YAML: " <> show err
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
