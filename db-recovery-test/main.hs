module Main (main) where

import DbRecoverySpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec DbRecoverySpec.spec
