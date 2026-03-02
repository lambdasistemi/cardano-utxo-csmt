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
    ( RollbackPointKV
    , rollbackPointPrism
    , withOriginPrism
    )
import Control.Lens (Prism', prism', type (:~:) (Refl))
import Data.ByteString (ByteString)
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
    RollbackPoints
        :: Columns slot hash key value (RollbackPointKV slot hash key value)
        -- ^ Column for storing rollback points
    ConfigCol
        :: Columns slot hash key value (KV ConfigKey ByteString)
        -- ^ Column for storing serialized application configuration

instance GEq (Columns slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq RollbackPoints RollbackPoints = Just Refl
    geq ConfigCol ConfigCol = Just Refl
    geq _ _ = Nothing

instance GCompare (Columns slot hash key value) where
    gcompare KVCol KVCol = GEQ
    gcompare KVCol _ = GLT
    gcompare _ KVCol = GGT
    gcompare CSMTCol CSMTCol = GEQ
    gcompare CSMTCol _ = GLT
    gcompare RollbackPoints CSMTCol = GGT
    gcompare RollbackPoints RollbackPoints = GEQ
    gcompare RollbackPoints ConfigCol = GLT
    gcompare ConfigCol ConfigCol = GEQ
    gcompare ConfigCol _ = GGT

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
        , RollbackPoints
            :=> Codecs
                { keyCodec = withOriginPrism slotP
                , valueCodec = rollbackPointPrism hashP keyP valueP
                }
        , ConfigCol
            :=> Codecs
                { keyCodec = configKeyPrism
                , valueCodec = prism' id Just
                }
        ]
