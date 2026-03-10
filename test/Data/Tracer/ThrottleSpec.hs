module Data.Tracer.ThrottleSpec
    ( spec
    )
where

import Control.Tracer (Tracer (..), traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Tracer.Throttle (Throttled (..), throttleByFrequency)
import Data.Tracer.Timestamp (Timestamped (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Create a tracer that collects events in a list
collectTracer :: IORef [a] -> Tracer IO a
collectTracer ref = Tracer $ \a -> modifyIORef' ref (a :)

-- | Helper to create a Timestamped with a specific time
mkTimestamp :: UTCTime -> String -> Timestamped UTCTime String
mkTimestamp = Timestamped

-- | Base time for tests
baseTime :: UTCTime
baseTime = read "2024-01-01 00:00:00 UTC"

-- | Diff function for throttle tests
diffSec :: UTCTime -> UTCTime -> Double
diffSec t1 t2 = realToFrac (diffUTCTime t2 t1)

-- | Add seconds to base time
addSeconds :: Double -> UTCTime -> UTCTime
addSeconds s = addUTCTime (realToFrac s)

spec :: Spec
spec = do
    describe "throttleByFrequency" $ do
        it "passes through non-matching events immediately" $ do
            ref <- newIORef []
            tracer <- throttleByFrequency diffSec [] (collectTracer ref)
            traceWith tracer (mkTimestamp baseTime "event1")
            traceWith tracer (mkTimestamp baseTime "event2")
            results <- reverse <$> readIORef ref
            length results `shouldBe` 2
            map throttledDropped results `shouldBe` [0, 0]

        it "emits first matching event immediately" $ do
            ref <- newIORef []
            let matcher _ = Just 1.0 -- 1 Hz = 1 event per second
            tracer <- throttleByFrequency diffSec [matcher] (collectTracer ref)
            traceWith tracer (mkTimestamp baseTime "event1")
            results <- readIORef ref
            case results of
                [r] -> throttledDropped r `shouldBe` 0
                _ -> fail "expected exactly one result"

        it "drops events within throttle interval" $ do
            ref <- newIORef []
            let matcher _ = Just 1.0 -- 1 Hz = 1 event per second
            tracer <- throttleByFrequency diffSec [matcher] (collectTracer ref)
            traceWith tracer (mkTimestamp baseTime "event1")
            traceWith tracer (mkTimestamp (addSeconds 0.5 baseTime) "event2")
            traceWith tracer (mkTimestamp (addSeconds 0.9 baseTime) "event3")
            results <- readIORef ref
            length results `shouldBe` 1 -- only first event emitted
        it "reports dropped count when interval passes" $ do
            ref <- newIORef []
            let matcher _ = Just 1.0 -- 1 Hz
            tracer <- throttleByFrequency diffSec [matcher] (collectTracer ref)
            traceWith tracer (mkTimestamp baseTime "event1")
            traceWith tracer (mkTimestamp (addSeconds 0.3 baseTime) "event2")
            traceWith tracer (mkTimestamp (addSeconds 0.6 baseTime) "event3")
            traceWith tracer (mkTimestamp (addSeconds 1.1 baseTime) "event4")
            results <- reverse <$> readIORef ref
            length results `shouldBe` 2
            map throttledDropped results `shouldBe` [0, 2]
            map (timestampedEvent . throttledEvent) results
                `shouldBe` ["event1", "event4"]

        it "handles multiple matcher categories independently" $ do
            ref <- newIORef []
            let matcherA "A" = Just 1.0
                matcherA _ = Nothing
                matcherB "B" = Just 2.0 -- 2 Hz = 0.5s interval
                matcherB _ = Nothing
            tracer <-
                throttleByFrequency diffSec [matcherA, matcherB] (collectTracer ref)
            traceWith tracer (mkTimestamp baseTime "A")
            traceWith tracer (mkTimestamp baseTime "B")
            traceWith tracer (mkTimestamp (addSeconds 0.3 baseTime) "A")
            traceWith tracer (mkTimestamp (addSeconds 0.3 baseTime) "B")
            traceWith tracer (mkTimestamp (addSeconds 0.6 baseTime) "B")
            results <- reverse <$> readIORef ref
            length results `shouldBe` 3 -- A(0s), B(0s), B(0.6s)
            map (timestampedEvent . throttledEvent) results
                `shouldBe` ["A", "B", "B"]
