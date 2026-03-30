module Cardano.UTxOCSMT.Application.Run.QuerySpec (spec) where

import Cardano.UTxOCSMT.Application.Run.Query (queryAwaitValue)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Mutable store backed by IORef for testing
newtype TestStore = TestStore (IORef [(String, String)])

mkStore :: IO TestStore
mkStore = TestStore <$> newIORef []

insertKey :: TestStore -> String -> String -> IO ()
insertKey (TestStore ref) k v = modifyIORef' ref ((k, v) :)

lookupKey :: TestStore -> String -> IO (Maybe String)
lookupKey (TestStore ref) k = lookup k <$> readIORef ref

-- | Simulate a commit by incrementing the TVar
notify :: TVar Int -> IO ()
notify tvar = atomically $ modifyTVar' tvar (+ 1)

spec :: Spec
spec = describe "queryAwaitValue" $ do
    it "returns immediately when key already exists" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        insertKey store "tx1" "output1"
        result <- queryAwaitValue tvar (lookupKey store) "tx1" (Just 1)
        result `shouldBe` Just "output1"

    it "blocks until key is inserted then returns value" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        -- Start awaiting in a separate thread
        awaiter <-
            async $ queryAwaitValue tvar (lookupKey store) "tx2" (Just 5)
        -- Give the awaiter time to enter the retry loop
        threadDelay 100_000
        -- Insert the key and notify
        insertKey store "tx2" "output2"
        notify tvar
        -- The awaiter should unblock and return the value
        result <- wait awaiter
        result `shouldBe` Just "output2"

    it "returns Nothing on timeout when key never appears" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        result <- queryAwaitValue tvar (lookupKey store) "tx3" (Just 1)
        result `shouldBe` Nothing

    it "wakes up on notification but retries if key still missing" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        awaiter <-
            async $ queryAwaitValue tvar (lookupKey store) "tx4" (Just 5)
        -- Notify without inserting the key — awaiter should go back to sleep
        threadDelay 100_000
        notify tvar
        threadDelay 100_000
        -- Now insert and notify — awaiter should wake and find it
        insertKey store "tx4" "output4"
        notify tvar
        result <- wait awaiter
        result `shouldBe` Just "output4"

    it "multiple awaiters for different keys" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        a1 <- async $ queryAwaitValue tvar (lookupKey store) "txA" (Just 5)
        a2 <- async $ queryAwaitValue tvar (lookupKey store) "txB" (Just 5)
        threadDelay 100_000
        -- Insert txB first
        insertKey store "txB" "outputB"
        notify tvar
        r2 <- wait a2
        r2 `shouldBe` Just "outputB"
        -- txA should still be waiting
        threadDelay 100_000
        insertKey store "txA" "outputA"
        notify tvar
        r1 <- wait a1
        r1 `shouldBe` Just "outputA"

    it "uses default 30s timeout when Nothing is passed" $ do
        store <- mkStore
        tvar <- newTVarIO (0 :: Int)
        -- Insert immediately so it returns fast
        insertKey store "tx5" "output5"
        result <- queryAwaitValue tvar (lookupKey store) "tx5" Nothing
        result `shouldSatisfy` (== Just "output5")
