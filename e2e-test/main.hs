module Main (main) where

import Cardano.UTxOCSMT.E2E.CrashRecoverySpec qualified as CrashRecoverySpec
import Cardano.UTxOCSMT.E2E.GenesisChainSyncSpec qualified as GenesisChainSyncSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    GenesisChainSyncSpec.spec
    CrashRecoverySpec.spec
