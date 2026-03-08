module Main (main) where

import Cardano.UTxOCSMT.E2E.GenesisChainSyncSpec qualified as GenesisChainSyncSpec
import Cardano.UTxOCSMT.E2E.MapColumnsChainSyncSpec qualified as MapColumnsChainSyncSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    GenesisChainSyncSpec.spec
    MapColumnsChainSyncSpec.spec
