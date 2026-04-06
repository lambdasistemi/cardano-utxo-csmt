module Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    , codecs
    , ConfigKey (..)
    )
where

import CSMT.Interface (Indirect, Key)
import Cardano.UTxOCSMT.Application.Database.Implementation.CSMTCodecs
    ( csmtCBORCodecs
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( WithSentinel
    , rollbackListPrism
    , withSentinelPrism
    )
import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation
    )
import ChainFollower.Rollbacks.Column (RollbackKV)
import Control.Lens (Prism', prism', type (:~:) (Refl))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Database.KV.Transaction
    ( Codecs (..)
    , DMap
    , DSum ((:=>))
    , GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , fromList
    )
import Text.Read (readMaybe)

-- | Single key for application configuration
data ConfigKey = AppConfigKey
    deriving (Eq, Ord, Show)

configKeyPrism :: Prism' ByteString ConfigKey
configKeyPrism = prism' encode decode
  where
    encode :: ConfigKey -> ByteString
    encode AppConfigKey = "app_config"

    decode :: ByteString -> Maybe ConfigKey
    decode bs
        | bs == "app_config" = Just AppConfigKey
        | otherwise = Nothing

-- | Structure of the database used by this application
data Columns slot hash key value x where
    KVCol :: Columns slot hash key value (KV key value)
        -- ^ Key-Value column for utxos
    CSMTCol :: Columns slot hash key value (KV Key (Indirect hash))
        -- ^ CSMT column for storing the CSMT of the UTxO set
    ConfigCol
        :: Columns slot hash key value (KV ConfigKey ByteString)
        -- ^ Column for storing serialized application configuration
    JournalCol
        :: Columns slot hash key value (KV key ByteString)
        -- ^ Journal column for KVOnly mode replay
    MetricsCol
        :: Columns slot hash key value (KV ByteString Int)
        -- ^ Metrics column for journal size counter
    Rollbacks
        :: Columns
            slot
            hash
            key
            value
            ( RollbackKV
                (WithSentinel slot)
                [Operation key value]
                (hash, Maybe hash)
            )
        -- ^ Rollback column for Runner API

instance GEq (Columns slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq ConfigCol ConfigCol = Just Refl
    geq JournalCol JournalCol = Just Refl
    geq MetricsCol MetricsCol = Just Refl
    geq Rollbacks Rollbacks = Just Refl
    geq _ _ = Nothing

instance GCompare (Columns slot hash key value) where
    gcompare KVCol KVCol = GEQ
    gcompare KVCol _ = GLT
    gcompare _ KVCol = GGT
    gcompare CSMTCol CSMTCol = GEQ
    gcompare CSMTCol _ = GLT
    gcompare ConfigCol CSMTCol = GGT
    gcompare ConfigCol ConfigCol = GEQ
    gcompare ConfigCol _ = GLT
    gcompare JournalCol JournalCol = GEQ
    gcompare JournalCol MetricsCol = GLT
    gcompare JournalCol Rollbacks = GLT
    gcompare JournalCol _ = GGT
    gcompare MetricsCol MetricsCol = GEQ
    gcompare MetricsCol Rollbacks = GLT
    gcompare MetricsCol _ = GGT
    gcompare Rollbacks Rollbacks = GEQ
    gcompare Rollbacks _ = GGT

-- | Prisms for serializing/deserializing keys and values
data Prisms slot hash key value = Prisms
    { slotP :: Prism' ByteString slot
    , hashP :: Prism' ByteString hash
    , keyP :: Prism' ByteString key
    , valueP :: Prism' ByteString value
    }

-- | Codecs for the database columns
codecs
    :: Prisms slot hash key value
    -> DMap (Columns slot hash key value) Codecs
codecs Prisms{keyP, hashP, slotP, valueP} =
    fromList
        [ KVCol :=> Codecs{keyCodec = keyP, valueCodec = valueP}
        , CSMTCol :=> csmtCBORCodecs hashP
        , ConfigCol
            :=> Codecs
                { keyCodec = configKeyPrism
                , valueCodec = prism' id Just
                }
        , JournalCol
            :=> Codecs
                { keyCodec = keyP
                , valueCodec = prism' id Just
                }
        , MetricsCol
            :=> Codecs
                { keyCodec = prism' id Just
                , valueCodec =
                    prism'
                        (BS8.pack . show)
                        (readMaybe . BS8.unpack)
                }
        , Rollbacks
            :=> Codecs
                { keyCodec = withSentinelPrism slotP
                , valueCodec =
                    rollbackListPrism hashP keyP valueP
                }
        ]
