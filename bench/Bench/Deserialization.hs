{- |
Module      : Bench.Deserialization
Description : CBOR deserialization level benchmarks

Compares different strategies for extracting address bytes
from CBOR-encoded Conway TxOuts:

  * Full ledger decode via 'decodeFull' + lens
  * Partial CBOR decode using raw cborg combinators
  * No-op passthrough (lower bound)

Uses the golden UTxO fixtures which contain both legacy
array-format and Babbage\/Conway map-format TxOuts.
-}
module Bench.Deserialization
    ( benchDeserialization
    )
where

import Cardano.Ledger.Address (unCompactAddr)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.Binary
    ( decodeFull
    , natVersion
    )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (compactAddrTxOutL)
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.DeepSeq (NFData (..))
import Control.Lens ((^.))
import Control.Monad (void)
import Criterion.Main
    ( Benchmark
    , bench
    , bgroup
    , nf
    )
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short (fromShort)

-- | Wrapper for benchmarking strict bytestring results.
newtype AddrBytes = AddrBytes StrictByteString

instance NFData AddrBytes where
    rnf (AddrBytes bs) = bs `seq` ()

-- | Build deserialization benchmarks from golden UTxOs.
benchDeserialization
    :: [(ByteString, ByteString)]
    -> Benchmark
benchDeserialization utxos =
    let txOuts = snd <$> utxos
    in  bgroup
            "Deserialization"
            [ bgroup
                "address-extraction"
                [ bench "full-ledger-decode"
                    $ nf
                        (fmap extractAddressFull)
                        txOuts
                , bench "partial-cbor"
                    $ nf
                        (fmap extractAddressPartial)
                        txOuts
                , bench "passthrough"
                    $ nf
                        (fmap extractPassthrough)
                        txOuts
                ]
            , bgroup
                "single-txout"
                $ case txOuts of
                    [] -> []
                    (t : _) ->
                        [ bench "full-ledger-decode"
                            $ nf
                                extractAddressFull
                                t
                        , bench "partial-cbor"
                            $ nf
                                extractAddressPartial
                                t
                        ]
            ]

{- | Full ledger decode via 'decodeFull', then extract
address through compact address lens.
-}
extractAddressFull :: ByteString -> AddrBytes
extractAddressFull bs =
    case decodeFull (natVersion @11) bs of
        Left e ->
            error
                $ "extractAddressFull: "
                    <> show e
        Right
            (txOut :: BabbageTxOut ConwayEra) ->
                AddrBytes
                    $ fromShort
                    $ unCompactAddr
                        (txOut ^. compactAddrTxOutL)

{- | Partial CBOR: decode only the address field,
skip value\/datum\/script. Handles both legacy
array format and Babbage\/Conway map format.
-}
extractAddressPartial :: ByteString -> AddrBytes
extractAddressPartial bs =
    case CBOR.deserialiseFromBytes
        decodeAddressOnly
        bs of
        Left e ->
            error
                $ "extractAddressPartial: "
                    <> show e
        Right (_, addr) -> AddrBytes addr

-- | No-op: just force the bytestring. Lower bound.
extractPassthrough :: ByteString -> AddrBytes
extractPassthrough bs =
    AddrBytes (LBS.toStrict bs)

{- | CBOR decoder that extracts only the address bytes
from a TxOut, regardless of encoding format.

Array format (legacy): @[address, value, ?datum]@
Map format (Babbage+): @{0: address, ...}@
-}
decodeAddressOnly
    :: forall s. CBOR.Decoder s StrictByteString
decodeAddressOnly = do
    tokenType <- CBOR.peekTokenType
    case tokenType of
        CBOR.TypeListLen -> decodeArrayAddress
        CBOR.TypeMapLen -> decodeMapAddress
        other ->
            fail
                $ "decodeAddressOnly: "
                    <> "unexpected token: "
                    <> show other

-- | Address from array-format TxOut (first element).
decodeArrayAddress
    :: forall s. CBOR.Decoder s StrictByteString
decodeArrayAddress = do
    _len <- CBOR.decodeListLen
    CBOR.decodeBytes

-- | Address from map-format TxOut (key 0).
decodeMapAddress
    :: forall s. CBOR.Decoder s StrictByteString
decodeMapAddress = do
    mapLen <- CBOR.decodeMapLen
    findKey0 mapLen
  where
    findKey0
        :: Int
        -> CBOR.Decoder s StrictByteString
    findKey0 0 =
        fail
            "decodeMapAddress: key 0 not found"
    findKey0 n = do
        key <- CBOR.decodeWord
        if key == 0
            then CBOR.decodeBytes
            else do
                void CBOR.decodeTerm
                findKey0 (n - 1)
