{- |
Module      : Cardano.UTxOCSMT.Application.UTxOs
Description : UTxO extraction from Cardano blocks

This module extracts UTxO changes (spends and creates) from Cardano blocks
across all eras (Byron through Dijkstra). Each transaction produces:

* 'Spend' entries for consumed inputs
* 'Create' entries for new outputs

For failed Plutus transactions ('isValid' = 'False'), collateral inputs are
consumed and the collateral return output (if any) is created instead of the
regular inputs\/outputs.

UTxO references are CBOR-encoded for consistent storage and lookup.
-}
module Cardano.UTxOCSMT.Application.UTxOs
    ( uTxOs
    , uTxOsWithTxCount
    , Change (..)
    , mkShelleyTxIn
    , unsafeMkTxIn
    , cborEncode
    , byronTxIdToShelley
    )
where

import Cardano.Chain.Common (unsafeGetLovelace)
import Cardano.Chain.UTxO qualified as Byron
import Cardano.Crypto.Hash.Class (Hash (..))
import Cardano.Crypto.Hashing (abstractHashToShort)
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Api.Tx.In (mkTxIxPartial)
import Cardano.Ledger.Api.Tx.In qualified as Shelley
import Cardano.Ledger.Api.Tx.Out (mkBasicTxOut, upgradeTxOut)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Binary (EncCBOR, natVersion, serialize)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Hashes (unsafeMakeSafeHash)
import Cardano.Ledger.Val (inject)
import Cardano.Read.Ledger.Block.Block (fromConsensusBlock)
import Cardano.Read.Ledger.Block.Txs (getEraTransactions)
import Cardano.Read.Ledger.Eras.EraValue (applyEraFun)
import Cardano.Read.Ledger.Eras.KnownEras
    ( Era (..)
    , IsEra
    , theEra
    )
import Cardano.Read.Ledger.Tx.CollateralInputs
    ( CollateralInputs (..)
    , getEraCollateralInputs
    )
import Cardano.Read.Ledger.Tx.CollateralOutputs
    ( CollateralOutputs (..)
    , getEraCollateralOutputs
    )
import Cardano.Read.Ledger.Tx.Inputs (Inputs (..), getEraInputs)
import Cardano.Read.Ledger.Tx.Outputs (Outputs (..), getEraOutputs)
import Cardano.Read.Ledger.Tx.ScriptValidity
    ( ScriptValidity (..)
    , getEraScriptValidity
    )
import Cardano.Read.Ledger.Tx.Tx (Tx (..))
import Cardano.Read.Ledger.Tx.TxId (TxId (..), getEraTxId)
import Cardano.UTxOCSMT.Ouroboros.Types (Block)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (toList)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word16)

data Change = Spend ByteString | Create ByteString ByteString
    deriving (Eq)

viewBinary :: BL.LazyByteString -> String
viewBinary = B.unpack . convertToBase Base64 . BL.toStrict

instance Show Change where
    show (Spend txIn) = "d " ++ viewBinary txIn
    show (Create txIn txOut) =
        "i "
            ++ viewBinary txIn
            ++ " "
            ++ viewBinary txOut

uTxOs :: Block -> [Change]
uTxOs bl =
    (changes . getEraTransactions) `applyEraFun` fromConsensusBlock bl

-- | Like 'uTxOs' but also returns the transaction count for diagnostics
uTxOsWithTxCount :: Block -> (Int, [Change])
uTxOsWithTxCount bl =
    ((\txs -> (length txs, changes txs)) . getEraTransactions)
        `applyEraFun` fromConsensusBlock bl

changes :: IsEra era => [Tx era] -> [Change]
changes = concatMap $ \tx ->
    if scriptIsValid tx
        then validChanges tx
        else invalidChanges tx

{- | Check if a transaction's scripts validated successfully.
Pre-Alonzo transactions are always valid.
-}
scriptIsValid :: forall era. IsEra era => Tx era -> Bool
scriptIsValid tx = case theEra @era of
    Byron -> True
    Shelley -> True
    Allegra -> True
    Mary -> True
    Alonzo -> check tx
    Babbage -> check tx
    Conway -> check tx
    Dijkstra -> check tx
  where
    check t = case getEraScriptValidity t of
        ScriptValidity (IsValid v) -> v

{- | UTxO changes for a valid transaction: regular inputs consumed,
regular outputs created.
-}
validChanges :: IsEra era => Tx era -> [Change]
validChanges tx =
    let inputs =
            Spend <$> extractInputs (getEraInputs tx)
        outputs =
            zipWith (mkCreate tx) [0 ..]
                $ extractOutputs (getEraOutputs tx)
    in  inputs ++ outputs

{- | UTxO changes for a failed Plutus transaction (@isValid = False@).
Collateral inputs are consumed; the collateral return output (if
present, Babbage+) is created at index = number of regular outputs.
-}
invalidChanges :: forall era. IsEra era => Tx era -> [Change]
invalidChanges tx = case theEra @era of
    -- Pre-Alonzo: impossible, but safe fallback
    Byron -> validChanges tx
    Shelley -> validChanges tx
    Allegra -> validChanges tx
    Mary -> validChanges tx
    -- Alonzo: collateral consumed, no collateral return
    Alonzo ->
        let CollateralInputs colIns = getEraCollateralInputs tx
        in  Spend . cborEncode <$> toList colIns
    -- Babbage: collateral consumed, collateral return created
    Babbage ->
        let CollateralInputs colIns = getEraCollateralInputs tx
            CollateralOutputs mReturn = getEraCollateralOutputs tx
            Outputs outs = getEraOutputs tx
            returnIdx = fromIntegral (length outs) :: Word16
            inputs = Spend . cborEncode <$> toList colIns
            outputs = case mReturn of
                SJust returnOut ->
                    [ Create
                        (mkTxIn tx returnIdx)
                        (cborEncode $ upgradeTxOut returnOut)
                    ]
                SNothing -> []
        in  inputs ++ outputs
    -- Conway: collateral consumed, collateral return created
    Conway ->
        let CollateralInputs colIns = getEraCollateralInputs tx
            CollateralOutputs mReturn = getEraCollateralOutputs tx
            Outputs outs = getEraOutputs tx
            returnIdx = fromIntegral (length outs) :: Word16
            inputs = Spend . cborEncode <$> toList colIns
            outputs = case mReturn of
                SJust returnOut ->
                    [ Create
                        (mkTxIn tx returnIdx)
                        (cborEncode returnOut)
                    ]
                SNothing -> []
        in  inputs ++ outputs
    -- Dijkstra: collateral consumed, collateral return created
    Dijkstra ->
        let CollateralInputs colIns = getEraCollateralInputs tx
            CollateralOutputs mReturn = getEraCollateralOutputs tx
            Outputs outs = getEraOutputs tx
            returnIdx = fromIntegral (length outs) :: Word16
            inputs = Spend . cborEncode <$> toList colIns
            outputs = case mReturn of
                SJust returnOut ->
                    [ Create
                        (mkTxIn tx returnIdx)
                        (cborEncode returnOut)
                    ]
                SNothing -> []
        in  inputs ++ outputs

mkCreate :: IsEra era => Tx era -> Word16 -> ByteString -> Change
mkCreate tx index = Create (mkTxIn tx index)

unsafeMkTxId :: ShortByteString -> Shelley.TxId
unsafeMkTxId = Shelley.TxId . unsafeMakeSafeHash . UnsafeHash

unsafeMkTxIn :: ShortByteString -> Word16 -> ByteString
unsafeMkTxIn txId index = mkShelleyTxIn index (unsafeMkTxId txId)

{- | Convert a Byron TxId to a Shelley TxId.
Extracts the raw 32-byte Blake2b_256 hash and re-wraps it.
-}
byronTxIdToShelley :: Byron.TxId -> Shelley.TxId
byronTxIdToShelley = unsafeMkTxId . abstractHashToShort

{-# INLINEABLE mkTxIn #-}
mkTxIn :: forall era. IsEra era => Tx era -> Word16 -> ByteString
mkTxIn tx index = case theEra @era of
    Byron ->
        mkShelleyTxIn index
            $ byronTxIdToShelley (unTxId $ getEraTxId tx)
    Shelley -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Allegra -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Mary -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Alonzo -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Babbage -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Conway -> mkShelleyTxIn index $ unTxId $ getEraTxId tx
    Dijkstra -> mkShelleyTxIn index $ unTxId $ getEraTxId tx

mkShelleyTxIn :: Word16 -> Shelley.TxId -> ByteString
mkShelleyTxIn index h =
    cborEncode
        $ Shelley.TxIn h
        $ mkTxIxPartial
        $ fromIntegral index

{-# INLINEABLE extractInputs #-}
extractInputs :: forall era. IsEra era => Inputs era -> [ByteString]
extractInputs (Inputs ins) = case theEra @era of
    Byron -> cborEncode . byronTxInToShelley <$> toList ins
      where
        byronTxInToShelley :: Byron.TxIn -> Shelley.TxIn
        byronTxInToShelley (Byron.TxInUtxo txId idx) =
            Shelley.TxIn
                (byronTxIdToShelley txId)
                (mkTxIxPartial (fromIntegral idx :: Integer))
    Shelley -> cborEncode <$> toList ins
    Allegra -> cborEncode <$> toList ins
    Mary -> cborEncode <$> toList ins
    Alonzo -> cborEncode <$> toList ins
    Babbage -> cborEncode <$> toList ins
    Conway -> cborEncode <$> toList ins
    Dijkstra -> cborEncode <$> toList ins

cborEncode :: EncCBOR a => a -> ByteString
cborEncode = serialize (natVersion @11)

_txs :: forall era. IsEra era => Tx era -> ByteString
_txs (Tx tx) = case theEra @era of
    Byron -> cborEncode tx
    Shelley -> cborEncode tx
    Allegra -> cborEncode tx
    Mary -> cborEncode tx
    Alonzo -> cborEncode tx
    Babbage -> cborEncode tx
    Conway -> cborEncode tx
    Dijkstra -> cborEncode tx

{- | Extract outputs, projecting eras through Conway to @BabbageTxOut ConwayEra@
before CBOR encoding. This ensures 'addressPrefix' in Config.hs sees a
Conway-era TxOut for historical eras.
-}
{-# INLINEABLE extractOutputs #-}
extractOutputs
    :: forall era. IsEra era => Outputs era -> [ByteString]
extractOutputs (Outputs outs) = case theEra @era of
    Byron ->
        cborEncode . fromByronOutput <$> toList outs
    Shelley ->
        cborEncode
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            <$> toList outs
    Allegra ->
        cborEncode
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            <$> toList outs
    Mary ->
        cborEncode
            . upgradeTxOut
            . upgradeTxOut
            . upgradeTxOut
            <$> toList outs
    Alonzo ->
        cborEncode
            . upgradeTxOut
            . upgradeTxOut
            <$> toList outs
    Babbage ->
        cborEncode
            . upgradeTxOut
            <$> toList outs
    Conway ->
        cborEncode <$> toList outs
    Dijkstra ->
        cborEncode <$> toList outs

-- | Convert a Byron TxOut to a Conway-era TxOut.
fromByronOutput :: Byron.TxOut -> BabbageTxOut ConwayEra
fromByronOutput (Byron.TxOut addr lovelace) =
    mkBasicTxOut @ConwayEra
        (AddrBootstrap (BootstrapAddress addr))
        ( inject
            $ Coin
            $ toInteger
            $ unsafeGetLovelace lovelace
        )
