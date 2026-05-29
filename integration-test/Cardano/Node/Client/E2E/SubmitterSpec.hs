{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Cardano.Node.Client.E2E.SubmitterSpec
Description : E2E tests for the N2C Submitter
License     : Apache-2.0
-}
module Cardano.Node.Client.E2E.SubmitterSpec
    ( spec
    ) where

import Lens.Micro ((&), (.~))
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    , shouldSatisfy
    )

import Cardano.Ledger.Allegra.Scripts
    ( ValidityInterval (..)
    )
import Cardano.Ledger.Api.Tx (mkBasicTx)
import Cardano.Ledger.Api.Tx.Body
    ( mkBasicTxBody
    , vldtTxBodyL
    )
import Cardano.Ledger.BaseTypes
    ( SlotNo (..)
    , StrictMaybe (SJust, SNothing)
    )

import Cardano.Node.Client.E2E.Setup
    ( addKeyWitness
    , genesisAddr
    , genesisSignKey
    , withDevnet
    )
import Cardano.Node.Client.N2C.Provider
    ( mkN2CProvider
    )
import Cardano.Node.Client.N2C.Submitter
    ( mkN2CSubmitter
    )
import Cardano.Node.Client.Provider
    ( Provider (..)
    )
import Cardano.Node.Client.Submitter
    ( SubmitResult (..)
    , Submitter (..)
    )
import Cardano.Tx.Balance (BalanceResult (..), balanceTx)

spec :: Spec
spec =
    around withDevnet'
        $ describe "Submitter.N2C"
        $ do
            it "submits a simple ADA transfer"
                $ \(lsq, ltxs) -> do
                    let provider =
                            mkN2CProvider lsq
                        submitter =
                            mkN2CSubmitter ltxs
                    utxos <-
                        queryUTxOs
                            provider
                            genesisAddr
                    pp <-
                        queryProtocolParams
                            provider
                    case utxos of
                        [] ->
                            error
                                "no genesis UTxOs"
                        (feeUtxo : _) -> do
                            let vldt =
                                    ValidityInterval
                                        SNothing
                                        ( SJust
                                            ( SlotNo
                                                100_000
                                            )
                                        )
                                body =
                                    mkBasicTxBody
                                        & vldtTxBodyL
                                            .~ vldt
                                tx =
                                    mkBasicTx body
                            case balanceTx
                                pp
                                [feeUtxo]
                                []
                                genesisAddr
                                tx of
                                Left err ->
                                    error
                                        $ "balanceTx \
                                          \failed: "
                                            <> show
                                                err
                                Right BalanceResult{balancedTx} ->
                                    do
                                        let signed =
                                                addKeyWitness
                                                    genesisSignKey
                                                    balancedTx
                                        result <-
                                            submitTx
                                                submitter
                                                signed
                                        result
                                            `shouldSatisfy` isSubmitted
  where
    withDevnet' action =
        withDevnet $ curry action

isSubmitted :: SubmitResult -> Bool
isSubmitted (Submitted _) = True
isSubmitted _ = False
