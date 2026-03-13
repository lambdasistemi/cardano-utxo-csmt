{- |
Module      : Cardano.UTxOCSMT.Bootstrap.Genesis
Description : Genesis UTxO bootstrap for from-genesis sync

Parse genesis files and extract UTxO pairs ready for CSMT insertion.
Supports both Shelley genesis (@initialFunds@) and Byron genesis
(@nonAvvmBalances@). This allows syncing from genesis without Mithril
by pre-populating the tree with genesis UTxOs before chain sync starts.
-}
module Cardano.UTxOCSMT.Bootstrap.Genesis
    ( readShelleyGenesis
    , genesisSecurityParam
    , genesisStabilityWindow
    , genesisNetworkMagic
    , genesisEpochSlots
    , genesisUtxoPairs
    , readByronGenesisUtxoPairs
    )
where

import Cardano.Chain.Common (unsafeGetLovelace)
import Cardano.Chain.Genesis
    ( GenesisData (..)
    , GenesisDataError
    , GenesisNonAvvmBalances (..)
    )
import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hashing (serializeCborHash)
import Cardano.Ledger.Address
    ( Addr (..)
    , BootstrapAddress (..)
    )
import Cardano.Ledger.Api.Tx.In (mkTxIxPartial)
import Cardano.Ledger.Api.Tx.In qualified as Shelley
import Cardano.Ledger.Api.Tx.Out (mkBasicTxOut)
import Cardano.Ledger.BaseTypes (unNonZero, unboundRational)
import Cardano.Ledger.Binary (natVersion, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis (..)
    , initialFundsPseudoTxIn
    )
import Cardano.Ledger.Val (inject)
import Cardano.UTxOCSMT.Application.UTxOs
    ( byronTxIdToShelley
    , cborEncode
    )
import Control.Exception (Exception, throwIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (coerce)
import Data.ListMap (unListMap)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Ouroboros.Network.Magic (NetworkMagic (..))

{- | Read and parse a Shelley genesis JSON file.
Patches @systemStart@ if set to @\"PLACEHOLDER\"@.
-}
readShelleyGenesis :: FilePath -> IO ShelleyGenesis
readShelleyGenesis fp = do
    raw <- BSL.readFile fp
    val <- case Aeson.eitherDecode' raw of
        Left err ->
            error $ "Genesis JSON parse error: " <> err
        Right v -> pure (patchSystemStart v)
    case Aeson.fromJSON val of
        Aeson.Error err ->
            error $ "Genesis decode error: " <> err
        Aeson.Success g -> pure g

{- | Extract the security parameter k from genesis.

This is the maximum rollback depth in blocks (e.g. 2160 on
mainnet).
-}
genesisSecurityParam :: ShelleyGenesis -> Word64
genesisSecurityParam =
    unNonZero . sgSecurityParam

{- | Stability window in slots: @ceiling(3k\/f)@.

Within this many slots of the tip, the chain is
mutable (rollbacks possible). Outside this window,
blocks are final. Uses @sgActiveSlotsCoeff@ (f) and
@sgSecurityParam@ (k) from the Shelley genesis.
-}
genesisStabilityWindow :: ShelleyGenesis -> Word64
genesisStabilityWindow genesis =
    ceiling (3 * fromIntegral k / f :: Rational)
  where
    k = genesisSecurityParam genesis
    f = unboundRational (sgActiveSlotsCoeff genesis)

{- | Extract network magic from Shelley genesis.

The @networkMagic@ field in shelley-genesis.json identifies
the network (e.g. 764824073 for mainnet, 1 for preprod,
2 for preview).
-}
genesisNetworkMagic :: ShelleyGenesis -> NetworkMagic
genesisNetworkMagic =
    NetworkMagic . sgNetworkMagic

{- | Derive Byron epoch slots from the security parameter.

Byron epoch length is @10 * k@ slots where @k@ is the
security parameter from shelley-genesis.json.
-}
genesisEpochSlots :: ShelleyGenesis -> Word64
genesisEpochSlots genesis =
    10 * genesisSecurityParam genesis

{- | Extract genesis UTxO pairs as CBOR-encoded (TxIn, TxOut).
Each initial fund entry becomes a pseudo-TxIn (derived from the
address hash) paired with a Conway-era TxOut.
-}
genesisUtxoPairs
    :: ShelleyGenesis -> [(ByteString, ByteString)]
genesisUtxoPairs genesis =
    mkPair <$> unListMap (sgInitialFunds genesis)
  where
    ver = natVersion @11
    mkPair :: (Addr, Coin) -> (ByteString, ByteString)
    mkPair (addr, coin) =
        let txIn = initialFundsPseudoTxIn addr
            txOut =
                mkBasicTxOut @ConwayEra addr (inject coin)
        in  (serialize ver txIn, serialize ver txOut)

newtype GenesisDataException
    = GenesisDataException GenesisDataError
    deriving (Show)

instance Exception GenesisDataException

{- | Read a Byron genesis file and extract UTxO pairs as
CBOR-encoded (TxIn, TxOut) using the same encoding as chain
sync. Byron genesis UTxOs use
@TxInUtxo (serializeCborHash address) 0@ as TxIn, converted
to Shelley format for consistent CSMT keys.
Zero-balance entries are filtered out.
-}
readByronGenesisUtxoPairs
    :: FilePath -> IO [(ByteString, ByteString)]
readByronGenesisUtxoPairs fp = do
    r <- runExceptT . Byron.readGenesisData $ fp
    case r of
        Left err -> throwIO $ GenesisDataException err
        Right (gd, _) ->
            pure
                $ byronUtxoPairs
                $ gdNonAvvmBalances gd

byronUtxoPairs
    :: GenesisNonAvvmBalances
    -> [(ByteString, ByteString)]
byronUtxoPairs (GenesisNonAvvmBalances m) =
    mkPair <$> filter nonZero (Map.toList m)
  where
    nonZero (_, lovelace) = unsafeGetLovelace lovelace > 0
    mkPair (addr, lovelace) =
        let
            -- Byron genesis convention: TxId = hash(CBOR(address))
            byronTxId :: Byron.TxId
            byronTxId = coerce (serializeCborHash addr)
            -- Convert to Shelley TxIn format
            shelleyTxIn :: Shelley.TxIn
            shelleyTxIn =
                Shelley.TxIn
                    (byronTxIdToShelley byronTxId)
                    (mkTxIxPartial (0 :: Integer))
            -- Convert Byron TxOut to Conway-era
            txOut =
                mkBasicTxOut @ConwayEra
                    (AddrBootstrap (BootstrapAddress addr))
                    ( inject
                        $ Coin
                        $ toInteger
                        $ unsafeGetLovelace lovelace
                    )
        in
            (cborEncode shelleyTxIn, cborEncode txOut)

{- | Replace @\"PLACEHOLDER\"@ systemStart with a dummy UTC time
so that 'FromJSON' succeeds.
-}
patchSystemStart :: Aeson.Value -> Aeson.Value
patchSystemStart = \case
    Aeson.Object obj
        | Just (Aeson.String "PLACEHOLDER") <-
            KM.lookup "systemStart" obj ->
            Aeson.Object
                $ KM.insert
                    "systemStart"
                    (Aeson.String "2020-01-01T00:00:00Z")
                    obj
    v -> v
