module Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (..)
    , ArmageddonTrace (..)
    , armageddon
    , cleanup
    , setup
    , renderArmageddonTrace
    )
where

import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Columns (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( RunTransaction (..)
    )
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Function (fix)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Transaction
    ( KeyOf
    , delete
    , iterating
    )
import MTS.Rollbacks.Store qualified as Store
import Ouroboros.Network.Point (WithOrigin (..))

data ArmageddonTrace
    = ArmageddonStarted
    | ArmageddonCompleted
    | SetupDone
    deriving (Show)

renderArmageddonTrace :: ArmageddonTrace -> String
renderArmageddonTrace ArmageddonStarted = " Armageddon cleanup started."
renderArmageddonTrace ArmageddonCompleted = " Armageddon cleanup completed."
renderArmageddonTrace SetupDone = " Initial setup done."

-- | Parameters for performing an "armageddon" cleanup of the database
data ArmageddonParams hash = ArmageddonParams
    { armageddonBatchSize :: Int
    -- ^ Number of entries to delete per batch
    , noHash :: hash
    }

-- Clean up a non-rollback column in batches
cleanUpBatch
    :: (Ord (KeyOf x), Monad m)
    => RunTransaction cf op slot hash key value m
    -> Columns slot hash key value x
    -> ArmageddonParams hash
    -> m ()
cleanUpBatch
    RunTransaction{transact}
    column
    ArmageddonParams{armageddonBatchSize} = do
        fix $ \batch -> do
            r <- transact $ iterating column $ do
                me <- firstEntry
                ($ (me, 0)) $ fix $ \go -> \case
                    (Nothing, _) -> pure False
                    (_, m) | m >= armageddonBatchSize -> pure True
                    (Just Entry{entryKey}, count) -> do
                        lift $ delete column entryKey
                        next <- nextEntry
                        go (next, count + 1)
            when r batch

{- | Clean up all database columns by deleting all entries in batches.
Does not initialize the database afterwards - use 'armageddon' for
cleanup + setup, or call 'setup' separately after 'cleanup'.
THIS IS NOT GOING TO RUN ATOMICALLY
-}
cleanup
    :: (Ord key, Ord slot, Monad m)
    => Tracer m ArmageddonTrace
    -> RunTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
cleanup (traceWith -> trace) runTransaction@RunTransaction{transact} armageddonParams = do
    trace ArmageddonStarted
    cleanUpBatch runTransaction KVCol armageddonParams
    cleanUpBatch runTransaction CSMTCol armageddonParams
    fix $ \batch -> do
        more <-
            transact
                $ Store.armageddonCleanup
                    RollbackPoints
                    (armageddonBatchSize armageddonParams)
        when more batch
    cleanUpBatch runTransaction ConfigCol armageddonParams
    trace ArmageddonCompleted

{- | Perform an "armageddon" cleanup of the database
by deleting all entries in all columns in batches,
then initialize with Origin in RollbackPoints.
THIS IS NOT GOING TO RUN ATOMICALLY
-}
armageddon
    :: (Ord key, Ord slot, Monad m)
    => Tracer m ArmageddonTrace
    -> RunTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
armageddon tracer runTransaction armageddonParams = do
    cleanup tracer runTransaction armageddonParams
    setup tracer runTransaction armageddonParams

setup
    :: (Ord slot, Monad m)
    => Tracer m ArmageddonTrace
    -> RunTransaction cf op slot hash key value m
    -> ArmageddonParams hash
    -> m ()
setup (traceWith -> trace) (RunTransaction{transact}) armageddonParams = do
    transact
        $ Store.armageddonSetup
            RollbackPoints
            Origin
            (Just (noHash armageddonParams, Nothing))
    trace SetupDone
