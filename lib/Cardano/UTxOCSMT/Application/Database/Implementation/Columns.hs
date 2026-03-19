module Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    , codecs
    , ConfigKey (..)
    , rollbackCounter
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
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word64)
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
import MTS.Rollbacks.Store (RollbackCounter (..))

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
    JournalCol
        :: Columns slot hash key value (KV key ByteString)
        -- ^ Journal column for KVOnly mode replay
    MetricsCol
        :: Columns slot hash key value (KV ByteString Int)
        -- ^ Metrics column for persistent counters

instance GEq (Columns slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq RollbackPoints RollbackPoints = Just Refl
    geq ConfigCol ConfigCol = Just Refl
    geq JournalCol JournalCol = Just Refl
    geq MetricsCol MetricsCol = Just Refl
    geq _ _ = Nothing

instance GCompare (Columns slot hash key value) where
    gcompare KVCol KVCol = GEQ
    gcompare KVCol _ = GLT
    gcompare _ KVCol = GGT
    gcompare CSMTCol CSMTCol = GEQ
    gcompare CSMTCol _ = GLT
    gcompare RollbackPoints CSMTCol = GGT
    gcompare RollbackPoints RollbackPoints = GEQ
    gcompare RollbackPoints _ = GLT
    gcompare ConfigCol RollbackPoints = GGT
    gcompare ConfigCol CSMTCol = GGT
    gcompare ConfigCol ConfigCol = GEQ
    gcompare ConfigCol _ = GLT
    gcompare JournalCol JournalCol = GEQ
    gcompare JournalCol MetricsCol = GLT
    gcompare JournalCol _ = GGT
    gcompare MetricsCol MetricsCol = GEQ
    gcompare MetricsCol _ = GGT

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
        , JournalCol
            :=> Codecs
                { keyCodec = keyP
                , valueCodec = prism' id Just
                }
        , MetricsCol
            :=> Codecs
                { keyCodec = prism' id Just
                , valueCodec = intPrism
                }
        ]

-- | Serialize Int as 8-byte big-endian.
intPrism :: Prism' ByteString Int
intPrism = prism' encode decode
  where
    encode n =
        let w = fromIntegral n :: Word64
        in  BS.pack
                [ fromIntegral (w `shiftR` 56)
                , fromIntegral (w `shiftR` 48)
                , fromIntegral (w `shiftR` 40)
                , fromIntegral (w `shiftR` 32)
                , fromIntegral (w `shiftR` 24)
                , fromIntegral (w `shiftR` 16)
                , fromIntegral (w `shiftR` 8)
                , fromIntegral w
                ]
    decode bs
        | BS.length bs == 8 =
            case map fromIntegral $ BS.unpack bs of
                [a, b, c, d, e, f, g, h] ->
                    Just
                        $ fromIntegral @Word64
                        $ (a `shiftL` 56)
                            .|. (b `shiftL` 48)
                            .|. (c `shiftL` 40)
                            .|. (d `shiftL` 32)
                            .|. (e `shiftL` 24)
                            .|. (f `shiftL` 16)
                            .|. (g `shiftL` 8)
                            .|. h
                _ -> Nothing
        | otherwise = Nothing

-- | Persistent rollback point counter.
rollbackCounter :: RollbackCounter (Columns slot hash key value)
rollbackCounter =
    RollbackCounter
        { rcSelector = MetricsCol
        , rcKey = "rollback_count"
        }
