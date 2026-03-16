{- |
Module      : Control.Foldl.Extra
Description : Generic fold utilities for metrics collection

Provides utility folds for calculating rolling statistics over event streams:

* 'speedoMeter' - tracks the speed of events over a sliding window
* 'averageOverWindow' - calculates rolling averages over a window
-}
module Control.Foldl.Extra
    ( speedoMeter
    , valueSpeedoMeter
    , averageOverWindow
    )
where

import Control.Foldl (Fold (..))
import Data.Time (UTCTime, diffUTCTime)
import Data.Word (Word64)

{- | Track the speed of events over a sliding window.

The fold counts events and measures the time between the first
and last event in each window, returning events per second.
-}
speedoMeter :: Int -> Fold UTCTime Double
speedoMeter window = Fold count Nothing getSpeed
  where
    getSpeed Nothing = 0
    getSpeed (Just (Nothing, _, _)) = 0
    getSpeed (Just (Just (startTime, endTime, cnt), _, _)) =
        fromIntegral cnt / realToFrac (diffUTCTime endTime startTime)
    count acc time = case acc of
        Nothing -> Just (Nothing, time, 0)
        Just (speed, startTime, cnt)
            | cnt < window ->
                Just (speed, startTime, cnt + 1)
            | otherwise -> Just (Just (startTime, time, cnt), time, 0)

{- | Track the rate of change of a value over a sliding window.

Unlike 'speedoMeter' which counts events per second, this measures
the rate at which a numeric value changes per second.
-}
valueSpeedoMeter :: Int -> Fold (UTCTime, Word64) Double
valueSpeedoMeter window = Fold step Nothing getSpeed
  where
    getSpeed Nothing = 0
    getSpeed (Just (Nothing, _, _, _)) = 0
    getSpeed
        ( Just
                ( Just (startTime, startVal, endTime, endVal)
                    , _
                    , _
                    , _
                    )
            ) =
            let dt = realToFrac (diffUTCTime endTime startTime)
                dv =
                    fromIntegral endVal
                        - fromIntegral startVal
                        :: Double
            in  if dt > 0 then dv / dt else 0
    step acc (time, val) = case acc of
        Nothing -> Just (Nothing, time, val, 0 :: Int)
        Just (speed, startTime, startVal, cnt)
            | cnt < window ->
                Just (speed, startTime, startVal, cnt + 1)
            | otherwise ->
                Just
                    ( Just (startTime, startVal, time, val)
                    , time
                    , val
                    , 0
                    )

{- | Calculate average over a rolling window.

Keeps the last @window@ values and computes their average.
Returns 0 if no values have been seen.
-}
averageOverWindow :: Fractional a => Int -> Fold a a
averageOverWindow window = Fold step [] getAverage
  where
    step xs x = take window (x : xs)
    getAverage xs =
        let l = length xs
        in  if l == 0 then 0 else sum xs / fromIntegral l
