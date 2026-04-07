{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.UTxOCSMT.Application.Options
    ( Options (..)
    , ConnectionMode (..)
    , CardanoNetwork (..)
    , optionsParser
    , optionsParserCore
    )
where

import Autodocodec
    ( HasCodec (..)
    , dimapCodec
    , shownBoundedEnumCodec
    )

import Control.Applicative ((<|>))

import Cardano.UTxOCSMT.Application.BlockFetch
    ( EventQueueLength (..)
    )
import Cardano.UTxOCSMT.Application.Metrics.Types
    ( SyncThreshold (..)
    )
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word16)
import Network.Socket (PortNumber)
import OptEnvConf
    ( Parser
    , auto
    , conf
    , filePathSetting
    , help
    , long
    , maybeReader
    , metavar
    , option
    , optional
    , reader
    , setting
    , short
    , str
    , switch
    , value
    , withYamlConfig
    )
import Path (Abs, File, Path)

-- | Cardano network selection
data CardanoNetwork
    = Mainnet
    | Preprod
    | Preview
    | Devnet
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

instance HasCodec CardanoNetwork where
    codec = shownBoundedEnumCodec

-- | Orphan instance for PortNumber (Word16 underneath)
instance HasCodec PortNumber where
    codec =
        dimapCodec fromIntegral (fromIntegral :: PortNumber -> Word16) codec

-- | How to connect to a Cardano node
data ConnectionMode
    = -- | Node-to-node over TCP (ChainSync headers + BlockFetch)
      N2N
        { n2nHost :: String
        , n2nPort :: PortNumber
        }
    | -- | Node-to-client over Unix socket (ChainSync full blocks)
      N2C
        { n2cSocket :: FilePath
        }
    deriving stock (Show)

data Options = Options
    { network :: CardanoNetwork
    , connectionMode :: ConnectionMode
    , headersQueueSize :: EventQueueLength
    , dbPath :: FilePath
    , logPath :: Maybe FilePath
    , apiPort :: Maybe PortNumber
    , apiDocsPort :: Maybe PortNumber
    , metricsOn :: Bool
    , syncThreshold :: SyncThreshold
    -- ^ Maximum slots behind chain tip to be considered synced (default: 100)
    , genesisFile :: FilePath
    -- ^ Path to shelley-genesis.json (required for security parameter)
    , byronGenesisFile :: Maybe FilePath
    -- ^ Path to byron-genesis.json for bootstrap from genesis
    , signingKey :: Maybe ByteString
    -- ^ Ed25519 secret key for signing merkle roots (base16)
    }

-- | Option to specify a YAML configuration file
configFileOption :: Parser (Maybe (Path Abs File))
configFileOption =
    optional
        $ filePathSetting
            [ long "config-file"
            , short 'c'
            , help "Path to YAML configuration file"
            , metavar "FILE"
            , option
            ]

dbPathOption :: Parser FilePath
dbPathOption =
    setting
        [ long "db-path"
        , short 'd'
        , help "Path to the CSMT RocksDB database"
        , metavar "DIR"
        , reader str
        , option
        ]

logPathOption :: Parser (Maybe FilePath)
logPathOption =
    optional
        $ setting
            [ long "log-path"
            , short 'l'
            , help
                "Path to the log file (logs to stdout if not specified)"
            , metavar "FILE"
            , reader str
            , option
            ]

-- | Parse Cardano network from string
readCardanoNetwork :: String -> Maybe CardanoNetwork
readCardanoNetwork "mainnet" = Just Mainnet
readCardanoNetwork "preprod" = Just Preprod
readCardanoNetwork "preview" = Just Preview
readCardanoNetwork "devnet" = Just Devnet
readCardanoNetwork _ = Nothing

networkOption :: Parser CardanoNetwork
networkOption =
    setting
        [ long "network"
        , short 'n'
        , conf "network"
        , help
            "Cardano network (mainnet, preprod, preview, devnet). \
            \Used for default peer node selection. Network magic \
            \and epoch slots are derived from the genesis file."
        , metavar "NETWORK"
        , reader $ maybeReader readCardanoNetwork
        , value Mainnet
        , option
        ]

nodeNameOption :: Parser String
nodeNameOption =
    setting
        [ long "node-name"
        , short 's'
        , conf "node-name"
        , help "Peer node hostname (n2n mode)"
        , metavar "HOSTNAME"
        , reader str
        , option
        ]

portNumberOption :: Parser PortNumber
portNumberOption =
    setting
        [ long "node-port"
        , short 'p'
        , conf "node-port"
        , help "Peer node port (n2n mode)"
        , metavar "INT"
        , reader auto
        , option
        ]

socketPathOption :: Parser FilePath
socketPathOption =
    setting
        [ long "socket-path"
        , conf "socket-path"
        , help "Path to node Unix socket (n2c mode)"
        , metavar "FILE"
        , reader str
        , option
        ]

-- | Parse connection mode: n2n (host+port) or n2c (socket path)
connectionModeParser :: Parser ConnectionMode
connectionModeParser =
    (N2C <$> socketPathOption)
        <|> (N2N <$> nodeNameOption <*> portNumberOption)

metricsSwitch :: Parser Bool
metricsSwitch =
    setting
        [ long "enable-metrics-reporting"
        , help "Enable metrics reporting on stdout"
        , reader auto
        , value False
        , switch True
        ]

eventQueueSizeOption :: Parser EventQueueLength
eventQueueSizeOption =
    EventQueueLength
        <$> setting
            [ long "headers-queue-size"
            , short 'q'
            , help "Size of the headers queue"
            , metavar "INT"
            , value 10
            , reader auto
            , option
            ]

apiPortOption :: Parser (Maybe PortNumber)
apiPortOption =
    optional
        $ setting
            [ long "api-port"
            , help "Port number for the API server"
            , metavar "INT"
            , reader auto
            , option
            ]

apiDocsPortOption :: Parser (Maybe PortNumber)
apiDocsPortOption =
    optional
        $ setting
            [ long "api-docs-port"
            , help "Port number for the API documentation server"
            , metavar "INT"
            , reader auto
            , option
            ]

syncThresholdOption :: Parser SyncThreshold
syncThresholdOption =
    SyncThreshold
        <$> setting
            [ long "sync-threshold"
            , help
                "Maximum slots behind chain tip to be considered synced \
                \(default: 100, ~5 blocks)"
            , metavar "SLOTS"
            , value 100
            , reader auto
            , option
            ]

genesisFileOption :: Parser FilePath
genesisFileOption =
    setting
        [ long "genesis-file"
        , conf "genesis-file"
        , help
            "Path to shelley-genesis.json. Required for \
            \reading the security parameter (k) and \
            \optionally bootstrapping from genesis."
        , metavar "FILE"
        , reader str
        , option
        ]

byronGenesisFileOption :: Parser (Maybe FilePath)
byronGenesisFileOption =
    optional
        $ setting
            [ long "byron-genesis-file"
            , conf "byron-genesis-file"
            , help
                "Path to byron-genesis.json for bootstrapping \
                \from genesis. Inserts nonAvvmBalances into \
                \the CSMT before chain sync starts from Origin."
            , metavar "FILE"
            , reader str
            , option
            ]

signingKeyOption :: Parser (Maybe ByteString)
signingKeyOption =
    fmap (>>= decodeHex) . optional
        $ setting
            [ long "signing-key"
            , help
                "Ed25519 secret key for signing merkle roots \
                \(32 bytes, base16-encoded). When set, each \
                \MerkleRootEntry includes a signature."
            , metavar "HEX"
            , reader str
            , option
            ]
  where
    decodeHex :: String -> Maybe ByteString
    decodeHex s = case convertFromBase Base16 (packBS s) of
        Left (_ :: String) -> Nothing
        Right bs -> Just bs
    packBS = BS.pack . map (fromIntegral . fromEnum)

-- | Main options parser with YAML config file support
optionsParser :: Parser Options
optionsParser = withYamlConfig configFileOption optionsParserCore

-- | Core options parser (used by withYamlConfig)
optionsParserCore :: Parser Options
optionsParserCore =
    mkOptions
        <$> networkOption
        <*> connectionModeParser
        <*> eventQueueSizeOption
        <*> dbPathOption
        <*> logPathOption
        <*> apiPortOption
        <*> apiDocsPortOption
        <*> metricsSwitch
        <*> syncThresholdOption
        <*> genesisFileOption
        <*> byronGenesisFileOption
        <*> signingKeyOption
  where
    mkOptions
        net
        connMode
        queue
        db
        logP
        api
        apiDocs
        metrics
        threshold
        genesis
        byronGenesis
        signing =
            Options
                { network = net
                , connectionMode = connMode
                , headersQueueSize = queue
                , dbPath = db
                , logPath = logP
                , apiPort = api
                , apiDocsPort = apiDocs
                , metricsOn = metrics
                , syncThreshold = threshold
                , genesisFile = genesis
                , byronGenesisFile = byronGenesis
                , signingKey = signing
                }
