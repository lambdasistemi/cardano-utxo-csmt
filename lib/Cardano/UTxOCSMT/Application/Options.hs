{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.UTxOCSMT.Application.Options
    ( Options (..)
    , ConnectionMode (..)
    , CardanoNetwork (..)
    , optionsParser
    , optionsParserCore

      -- * Derived option accessors
    , networkMagic
    , epochSlotsFor
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
import Data.Word (Word16, Word64)
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
import Ouroboros.Network.Magic (NetworkMagic (..))
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

-- | Get network magic for a Cardano network
networkMagicFor :: CardanoNetwork -> NetworkMagic
networkMagicFor Mainnet = NetworkMagic 764824073
networkMagicFor Preprod = NetworkMagic 1
networkMagicFor Preview = NetworkMagic 2
networkMagicFor Devnet = NetworkMagic 42

-- | Get Byron epoch slots for a Cardano network
epochSlotsFor :: CardanoNetwork -> Word64
epochSlotsFor Mainnet = 21600
epochSlotsFor Preprod = 21600
epochSlotsFor Preview = 4320
epochSlotsFor Devnet = 4320

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
    , syncThreshold :: Word64
    -- ^ Number of slots behind chain tip to consider synced (default: 100)
    , genesisFile :: FilePath
    -- ^ Path to shelley-genesis.json (required for security parameter)
    , byronGenesisFile :: Maybe FilePath
    -- ^ Path to byron-genesis.json for bootstrap from genesis
    }

-- | Get effective network magic from options
networkMagic :: Options -> NetworkMagic
networkMagic = networkMagicFor . network

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
            \Sets network magic and default peer node. \
            \Use devnet for local Yaci DevKit networks (magic 42)."
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

syncThresholdOption :: Parser Word64
syncThresholdOption =
    setting
        [ long "sync-threshold"
        , help
            "Number of slots behind chain tip to consider synced \
            \(default: 100, ~33 minutes)"
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
        byronGenesis =
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
                }
