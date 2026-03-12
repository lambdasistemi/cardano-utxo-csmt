{-# LANGUAGE ConstraintKinds #-}

module Cardano.UTxOCSMT.Application.Database.Properties
    ( propertyTipIsAfterFinalityOrMissing
    , propertyForwardBeforeTipIsNoOp
    , propertyForwardAfterTipAppliesChanges
    , propertyRollbackAfterTipDoesNothing
    , propertyRollbackAfterBeforeTipUndoesChanges
    , propertyBlocksWithinKAreRollbackable
    , propertyBlocksDeeperThanKArePruned
    , propertyFinalityEqualsOldestRollbackPoint
    , populateWithSomeContent
    , findValue
    , logOnFailure
    , assertingJust
    )
where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Dump (..)
    , Operation (..)
    , TipOf
    , emptyDump
    )
import Cardano.UTxOCSMT.Application.Database.Properties.Expected
    ( Generator (..)
    , PropertyConstraints
    , PropertyWithExpected
    , asksGenerator
    , asksSecurityParam
    , expectedFinality
    , expectedKeys
    , expectedRollbackDepth
    , forwardTip
    , getDump
    , getFinality
    , getTip
    , rollbackTip
    )
import Control.Monad (replicateM)
import Control.Monad.State
    ( MonadTrans (..)
    , gets
    )
import Data.List.NonEmpty (NonEmpty (..), tails, toList)
import GHC.Stack (HasCallStack)
import Ouroboros.Network.Point (WithOrigin (..))
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , NonNegative (..)
    , Positive (Positive)
    , counterexample
    , elements
    , suchThat
    )
import Test.QuickCheck.Gen (frequency)
import Test.QuickCheck.Monadic
    ( PropertyM
    , assertWith
    , monitor
    , pick
    )
import Prelude hiding (truncate)

findValue
    :: Ord key
    => key
    -> Maybe value
    -> [(key, value)]
    -> Maybe value
findValue _key candidate [] = candidate
findValue key candidate ((k, v) : rest)
    | key >= k = findValue key (Just v) rest
    | otherwise = candidate

assert :: Monad m => String -> Bool -> PropertyM m ()
assert = flip assertWith

logOnFailure :: Monad m => String -> PropertyM m ()
logOnFailure = monitor . counterexample

assertingJust
    :: Monad m
    => String
    -> Maybe a
    -> (a -> PropertyM m ())
    -> PropertyM m ()
assertingJust msg Nothing _ = assert msg False
assertingJust _ (Just x) f = f x

genWithOrigin
    :: Gen slot
    -> Gen (WithOrigin slot)
genWithOrigin genSlot =
    frequency
        [ (1, return Origin)
        , (8, fmap At genSlot)
        ]

propertyTipIsAfterFinalityOrMissing
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyTipIsAfterFinalityOrMissing = do
    tip <- getTip
    finality <- getFinality
    assert
        "Tip slot should be after or equal to finality slot"
        $ tip >= finality

genSlotAfter
    :: PropertyConstraints m slot key value
    => WithOrigin slot
    -> PropertyWithExpected m slot key value slot
genSlotAfter base = do
    Generator{genSlot} <- asksGenerator
    pick $ genSlot `suchThat` (\s -> At s > base)

generateOperations
    :: PropertyConstraints m slot key value
    => PropertyWithExpected
        m
        slot
        key
        value
        [Operation key value]
generateOperations = do
    Generator{genKey, genValue} <- asksGenerator
    let go (0 :: Int) _ = pure []
        go i available = do
            let deleting = do
                    key <- elements available
                    rest <- go (i - 1) (filter (/= key) available)
                    pure (Delete key : rest)
                inserting = do
                    key <- genKey `suchThat` (`notElem` available)
                    value <- genValue
                    rest <- go (i - 1) (key : available)
                    pure (Insert key value : rest)
            frequency
                $ [ (1, deleting) | not (null available)
                  ]
                    <> [ (3, inserting)
                       ]
    availableKeys <- lift $ gets (expectedKeys . fst)
    Positive n <- pick arbitrary
    pick $ go n availableKeys

{- | Populate the database with some more content. This should be freely interleaved
  with properties.
-}
populateWithSomeContent
    :: ( PropertyConstraints m slot key value
       , HasCallStack
       , TipOf slot ~ slot
       )
    => PropertyWithExpected
        m
        slot
        key
        value
        [(WithOrigin slot, Dump slot key value)]
populateWithSomeContent = do
    NonNegative n <- pick arbitrary
    replicateM n $ do
        tip <- getTip
        slot <- genSlotAfter tip
        ops <- generateOperations
        forwardTip slot ops
        dump <- getDump
        pure (At slot, dump)

{- | Property: forwarding at or before tip is a no-op
forwarding must move the tip ahead
-}
propertyForwardBeforeTipIsNoOp
    :: (PropertyConstraints m slot key value, TipOf slot ~ slot)
    => PropertyWithExpected m slot key value ()
propertyForwardBeforeTipIsNoOp = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    logOnFailure $ "Current tip: " ++ show tipBefore
    case tipBefore of
        Origin -> pure ()
        At tipSlot -> do
            slot <- pick $ genSlot `suchThat` (<= tipSlot)
            ops <- generateOperations
            logOnFailure $ "Forwarding to slot: " ++ show slot
            oldDump <- getDump
            logOnFailure $ "Old dump: " ++ show oldDump
            forwardTip slot ops
            newDump <- getDump
            logOnFailure $ "New dump: " ++ show newDump
            assert
                "Forwarding at or before tip should be no-op"
                $ newDump == oldDump

{- | Property: forwarding after tip applies the changes correctly
finality stays where it is
tip moves to the new slot
associations are updated by applying the changes
old associations that were not deleted remain
-}
propertyForwardAfterTipAppliesChanges
    :: (PropertyConstraints m slot key value, TipOf slot ~ slot)
    => PropertyWithExpected m slot key value ()
propertyForwardAfterTipAppliesChanges = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    slot <- case tipBefore of
        Origin -> pick genSlot
        At tipSlot -> pick $ genSlot `suchThat` (> tipSlot)
    ops <- generateOperations
    old@Dump
        { dumpFinality = oldFinality
        , dumpAssocs = oldContents
        } <-
        getDump
    forwardTip slot ops
    new@Dump
        { dumpFinality = newFinality
        , dumpTip = newTip
        , dumpAssocs = newContents
        } <-
        getDump

    logOnFailure $ "Forwarding to slot: " ++ show slot
    logOnFailure $ "Operations: " ++ show ops
    logOnFailure $ "Old dump: " ++ show old
    logOnFailure $ "New dump: " ++ show new
    assert
        "Finality should not regress after forwarding"
        $ newFinality >= oldFinality
    assert
        "Tip should be updated to the newest slot after forwarding"
        $ newTip == At slot
    assert
        "Changes were applied correctly after forwarding"
        $ flip all (tails ops)
        $ \case
            (Delete key : rs) ->
                key `notElem` fmap fst newContents
                    || key `elem` [k | Insert k _value <- rs]
            (Insert key value : rs) ->
                (key, value) `elem` newContents
                    || key `elem` [k | Delete k <- rs]
            [] -> True
    assert
        "Old contents should be preserved after forwarding"
        $ flip all oldContents
        $ \(k, v) ->
            (k, v) `elem` newContents
                || k `elem` [key | Delete key <- ops]

{- | Property: rolling back at or after tip does nothing
rollback to a slot after or at tip should preserve the database as is
-}
propertyRollbackAfterTipDoesNothing
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyRollbackAfterTipDoesNothing = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    logOnFailure $ "Current tip: " ++ show tipBefore
    slotToRollbackTo <- case tipBefore of
        Origin -> pure Origin
        At tipSlot -> pick $ At <$> genSlot `suchThat` (>= tipSlot)
    logOnFailure $ "Rolling back to slot: " ++ show slotToRollbackTo
    oldDump <- getDump
    logOnFailure $ "Old dump: " ++ show oldDump
    rollbackTip slotToRollbackTo
    newDump <- getDump
    logOnFailure $ "New dump: " ++ show newDump
    assert
        "Rollback at or after tip should be no-op"
        $ newDump == oldDump

{- | Property: rolling back to a slot before tip undoes the changes made after it
given we know some snapshots of the database at various slots,
rolling back after or at the first one will restore the database to that state
-}
propertyRollbackAfterBeforeTipUndoesChanges
    :: PropertyConstraints m slot key value
    => NonEmpty (WithOrigin slot, Dump slot key value)
    -> PropertyWithExpected m slot key value ()
propertyRollbackAfterBeforeTipUndoesChanges history = do
    tip <- getTip
    logOnFailure $ "Current tip: " ++ show tip
    finality <- getFinality
    logOnFailure $ "Current finality: " ++ show finality
    (slot, dump) <-
        pick
            $ elements (toList history)
                `suchThat` (\s -> fst s >= finality)
    logOnFailure $ "Rolling back to slot: " ++ show slot
    rollbackTip slot
    finalDump <- getDump
    keys <- lift $ gets (expectedKeys . fst)
    logOnFailure $ "Expected keys: " ++ show keys
    logOnFailure $ "Final dump after rollback: " ++ show finalDump
    -- After rollback, finality is the oldest remaining
    -- rollback point; assocs and tip should match the
    -- historical dump but finality may differ
    assert
        "Rollback restores associations"
        $ dumpAssocs finalDump == dumpAssocs dump
    assert
        "Rollback restores tip"
        $ dumpTip finalDump == slot

{- | Property: rolling back before finality truncates
Rolling back to a slot before the oldest rollback point
(finality) should truncate the database.
-}
propertyBlocksDeeperThanKArePruned
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyBlocksDeeperThanKArePruned = do
    finality <- getFinality
    logOnFailure $ "Current finality: " ++ show finality
    case finality of
        Origin -> pure ()
        At finalitySlot -> do
            Generator{genSlot} <- asksGenerator
            slot <-
                pick
                    $ genWithOrigin genSlot
                        `suchThat` (< At finalitySlot)
            logOnFailure
                $ "Rolling back before finality to: "
                    ++ show slot
            rollbackTip slot
            newDump <- getDump
            logOnFailure
                $ "Dump after rollback: " ++ show newDump
            assert
                "Rolling back before finality truncates"
                $ newDump == emptyDump

{- | Property: blocks within k are rollbackable
After forwarding n blocks (n <= k), all of them
should be reachable by rollback.
-}
propertyBlocksWithinKAreRollbackable
    :: PropertyConstraints m slot key value
    => NonEmpty (WithOrigin slot, Dump slot key value)
    -> PropertyWithExpected m slot key value ()
propertyBlocksWithinKAreRollbackable history = do
    k <- asksSecurityParam
    depth <- lift $ gets (expectedRollbackDepth . fst)
    logOnFailure $ "Security param k: " ++ show k
    logOnFailure $ "Current depth: " ++ show depth
    assert
        "Rollback depth never exceeds k"
        $ depth <= k
    -- Pick any slot from history that is within the
    -- rollback window and verify rollback works
    finality <- getFinality
    let reachable =
            filter (\(s, _) -> s >= finality) (toList history)
    if null reachable
        then pure ()
        else do
            (slot, dump) <- pick $ elements reachable
            logOnFailure
                $ "Rolling back to: " ++ show slot
            rollbackTip slot
            finalDump <- getDump
            assert
                "Rollback within k restores associations"
                $ dumpAssocs finalDump == dumpAssocs dump
            assert
                "Rollback within k restores tip"
                $ dumpTip finalDump == slot

{- | Property: finality equals oldest rollback point
After populating with content, the finality reported by
the database should match the oldest rollback point's slot,
which is determined by the security parameter k.
-}
propertyFinalityEqualsOldestRollbackPoint
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyFinalityEqualsOldestRollbackPoint = do
    k <- asksSecurityParam
    dbFinality <- getFinality
    expectedFin <-
        lift $ gets (expectedFinality k . fst)
    logOnFailure
        $ "DB finality: " ++ show dbFinality
    logOnFailure
        $ "Expected finality: " ++ show expectedFin
    assert
        "DB finality matches expected oldest rollback point"
        $ dbFinality == expectedFin
