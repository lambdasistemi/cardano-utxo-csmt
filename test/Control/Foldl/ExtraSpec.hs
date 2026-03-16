module Control.Foldl.ExtraSpec (spec) where

import Control.Foldl (fold)
import Control.Foldl.Extra (valueSpeedoMeter)
import Data.Time
    ( UTCTime
    , addUTCTime
    , secondsToNominalDiffTime
    )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | A base time for test data
t0 :: UTCTime
t0 = posixSecondsToUTCTime 0

-- | Add seconds to a UTCTime
addSecs :: Double -> UTCTime -> UTCTime
addSecs s = addUTCTime (secondsToNominalDiffTime $ realToFrac s)

{- | The fold needs window+2 events to produce a rate:
1 event initializes, window events fill the counter,
1 event triggers the snapshot.
-}
spec :: Spec
spec = describe "valueSpeedoMeter" $ do
    it "returns 0 for empty input" $ do
        fold (valueSpeedoMeter 2) [] `shouldBe` 0

    it "returns 0 for a single event" $ do
        fold (valueSpeedoMeter 2) [(t0, 100)] `shouldBe` 0

    it "returns 0 before window is full" $ do
        -- window=2: need 4 events to snapshot, 3 is not enough
        let events =
                [ (t0, 0)
                , (addSecs 1 t0, 10)
                , (addSecs 2 t0, 20)
                ]
        fold (valueSpeedoMeter 2) events `shouldBe` 0

    it "computes correct rate after one window" $ do
        -- window=2: E0 inits (cnt=0), E1 cnt→1, E2 cnt→2,
        -- E3 triggers snapshot (t0,0)→(t0+3,300) = 100/s
        let events =
                [ (t0, 0)
                , (addSecs 1 t0, 100)
                , (addSecs 2 t0, 200)
                , (addSecs 3 t0, 300)
                ]
        fold (valueSpeedoMeter 2) events `shouldBe` 100

    it "rate reflects latest window" $ do
        -- window=2
        -- E0 (t0,0): init (cnt=0)
        -- E1 (t1,50): cnt→1
        -- E2 (t2,100): cnt→2
        -- E3 (t3,200): snapshot1 (t0,0)→(t3,200)=66.7/s, reset(t3,200,cnt=0)
        -- E4 (t4,400): cnt→1
        -- E5 (t5,600): cnt→2
        -- E6 (t6,900): snapshot2 (t3,200)→(t6,900)=233.3/s
        let events =
                [ (t0, 0)
                , (addSecs 1 t0, 50)
                , (addSecs 2 t0, 100)
                , (addSecs 3 t0, 200)
                , (addSecs 4 t0, 400)
                , (addSecs 5 t0, 600)
                , (addSecs 6 t0, 900)
                ]
        let rate = fold (valueSpeedoMeter 2) events
        rate `shouldSatisfy` (\r -> abs (r - 233.33) < 0.01)

    it "handles zero time delta" $ do
        let events =
                [ (t0, 0)
                , (t0, 100)
                , (t0, 200)
                , (t0, 300)
                ]
        fold (valueSpeedoMeter 2) events `shouldBe` 0
