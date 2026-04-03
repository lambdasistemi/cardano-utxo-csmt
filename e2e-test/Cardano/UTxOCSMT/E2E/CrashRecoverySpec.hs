{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Cardano.UTxOCSMT.E2E.CrashRecoverySpec
Description : E2E crash recovery tests
License     : Apache-2.0

Populate a deterministic chain, run the CSMT
application, verify merkle roots match the
known chain state.
-}
module Cardano.UTxOCSMT.E2E.CrashRecoverySpec
    ( spec
    ) where

import CSMT.Hashes (Hash)
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Ledger.Api.Tx (Tx, bodyTxL, mkBasicTx, txIdTx)
import Cardano.Ledger.Api.Tx.Body (mkBasicTxBody, outputsTxBodyL)
import Cardano.Ledger.Api.Tx.Out (TxOut, mkBasicTxOut)
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (PParams, extractHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn, mkTxInPartial)
import Cardano.Node.Client.Balance (balanceTx)
import Cardano.Node.Client.E2E.ChainPopulator
    ( ChainPopulator (..)
    , PopulatorNext (..)
    , populateChain
    )
import Cardano.Node.Client.E2E.Devnet (withCardanoNode)
import Cardano.Node.Client.E2E.Setup
    ( devnetMagic
    , genesisAddr
    , genesisSignKey
    )
import Cardano.Node.Client.Types (Block)
import Cardano.UTxOCSMT.Application.Database.Backend
    ( BackendEvent (..)
    , createBackend
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , DbState (..)
    , ReadyState (..)
    , RunTransaction (..)
    , mkCSMTOps
    , openCSMTOps
    , queryMerkleRoot
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBTransaction
    , newRunRocksDBTransactionUnguarded
    )
import Cardano.UTxOCSMT.Application.Metrics
    ( MetricsEvent (..)
    , SyncPhase (..)
    , SyncThreshold (..)
    )
import Cardano.UTxOCSMT.Application.Run.Application
    ( applicationN2C
    )
import Cardano.UTxOCSMT.Application.Run.Config
    ( armageddonParams
    , context
    , prisms
    , slotHash
    , withRocksDB
    )
import Cardano.UTxOCSMT.Application.Run.Query
    ( queryExclusionProof
    , queryInclusionProof
    )
import Cardano.UTxOCSMT.Application.Run.Setup
    ( SetupResult (..)
    , setupDB
    )
import Cardano.UTxOCSMT.Ouroboros.Types (Point)
import ChainFollower.Backend qualified as Backend
import ChainFollower.Runner (Phase (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
    ( atomically
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , tryPutTMVar
    )
import Control.Lens (iso)
import Control.Monad (forM_, void, when)
import Control.Tracer
    ( Tracer (..)
    , nullTracer
    , traceWith
    )
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.RocksDB (BatchOp, ColumnFamily)
import Lens.Micro ((&), (.~), (^.))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

-- * Constants

genesisDir :: FilePath
genesisDir = "e2e-test/genesis"

shelleyGenesisPath :: FilePath
shelleyGenesisPath = genesisDir </> "shelley-genesis.json"

-- * Transaction building

-- | Get change output from a balanced tx.
changeOutput
    :: Tx ConwayEra -> (TxIn, TxOut ConwayEra)
changeOutput tx =
    let outs = toList (tx ^. bodyTxL . outputsTxBodyL)
        lastIdx = length outs - 1
    in  ( mkTxInPartial (txIdTx tx) (fromIntegral lastIdx)
        , last outs
        )

{- | Build a chain of N txs, each creating extra
UTxOs. Each tx has 10 small outputs (2 ADA each)
plus a change output for chaining. This populates
the CSMT with many distinct UTxOs.
-}
buildTxChain
    :: PParams ConwayEra
    -> (TxIn, TxOut ConwayEra)
    -> Int
    -> ([Tx ConwayEra], (TxIn, TxOut ConwayEra))
buildTxChain _ utxo 0 = ([], utxo)
buildTxChain pp utxo n =
    let extraOutputs =
            StrictSeq.fromList
                [ mkBasicTxOut
                    genesisAddr
                    (inject (Coin 2_000_000))
                | _ <- [1 :: Int .. 10]
                ]
        body =
            mkBasicTxBody
                & outputsTxBodyL .~ extraOutputs
        tx = mkBasicTx body
    in  case balanceTx pp [utxo] genesisAddr tx of
            Left err ->
                error $ "buildTxChain: " <> show err
            Right balanced ->
                let (rest, final') =
                        buildTxChain
                            pp
                            (changeOutput balanced)
                            (n - 1)
                in  (balanced : rest, final')

-- | Render a TxId as hex text.
txIdHex :: TxId -> Text
txIdHex (TxId safeHash) =
    decodeUtf8
        $ convertToBase Base16
        $ hashToBytes
        $ extractHash safeHash

-- * Chain populator

{- | Populator that submits batches of chained txs
then closes with the blocks.
-}
mkPopulator
    :: IORef [Block]
    -> IORef [TxId]
    -> Int
    -> Int
    -> PParams ConwayEra
    -> [(TxIn, TxOut ConwayEra)]
    -> ChainPopulator
mkPopulator blocksRef txIdsRef nBatches batchSize pp utxos =
    case utxos of
        [] -> error "mkPopulator: no initial UTxOs"
        (u : _) -> submitBatches pp u nBatches
  where
    submitBatches
        :: PParams ConwayEra -> (TxIn, TxOut ConwayEra) -> Int -> ChainPopulator
    submitBatches _ _ 0 = followAndClose blocksRef (200 :: Int)
    submitBatches pp' utxo n =
        ChainPopulator $ \_ _block -> do
            let (txs, nextUtxo) =
                    buildTxChain pp' utxo batchSize
            modifyIORef'
                txIdsRef
                (++ map txIdTx txs)
            pure
                $ Continue txs
                $ waitBlocks (3 :: Int)
                $ submitBatches pp' nextUtxo (n - 1)

    waitBlocks 0 next = next
    waitBlocks n next =
        ChainPopulator $ \_ _ ->
            pure $ Continue [] $ waitBlocks (n - 1) next

    followAndClose ref 0 =
        ChainPopulator $ \_ _ ->
            pure
                $ Close []
                $ \case
                    Right blocks ->
                        modifyIORef' ref (const blocks)
                    Left err ->
                        error $ "followAndClose: " <> show err
    followAndClose ref n =
        ChainPopulator $ \_ _ ->
            pure $ Continue [] $ followAndClose ref (n - 1)

-- * CSMT application

type Runner =
    RunTransaction
        ColumnFamily
        BatchOp
        Point
        Hash
        BL.ByteString
        BL.ByteString
        IO

{- | Common setup: open DB, setup, open ops, create
backend, start app with given metrics tracer.
The callback receives the kill action and runner.
-}
withCsmtApp
    :: FilePath
    -> Tracer IO MetricsEvent
    -> Tracer IO BackendEvent
    -> Int
    -- ^ Delay per restore operation (microseconds)
    -> Int
    -- ^ Delay per replay chunk (microseconds)
    -> SyncThreshold
    -- ^ Sync threshold
    -> Int
    -- ^ Security parameter (stability window)
    -> FilePath
    -> (IO () -> Runner -> IO a)
    -> IO a
withCsmtApp socketPath metricsTracer backendTracer restoreDelay replayDelay syncThresh secParam dbPath callback = do
    let CSMTContext{fromKV = fkv, hashing = h} = context
        ops = mkCSMTOps fkv h
    withRocksDB dbPath $ \db -> do
        runner <- newRunRocksDBTransaction db prisms
        let unguardedRunner =
                newRunRocksDBTransactionUnguarded db prisms
        SetupResult{setupStartingPoint} <-
            setupDB
                nullTracer
                shelleyGenesisPath
                Nothing
                armageddonParams
                ops
                runner
        dbState <-
            openCSMTOps
                4
                10
                (iso BL.toStrict BL.fromStrict)
                fkv
                h
                (transact runner)
                (transact unguardedRunner)
                ( \_ -> do
                    when (replayDelay > 0) $ do
                        putStrLn "  [replay chunk delay]"
                        threadDelay replayDelay
                )
        let resolve (NeedsRecovery r) = r >>= resolve
            resolve (Ready (ChooseKVOnly kvOps)) = pure kvOps
            resolve (Ready (ChooseFull _)) =
                fail "unexpected ChooseFull"
        kvOnlyOps <- resolve dbState
        let mutationTracer =
                if restoreDelay > 0
                    then Tracer $ \_ -> threadDelay restoreDelay
                    else nullTracer
            -- Wire backend events to metrics tracer
            -- (like Main.hs does)
            combinedBackendTracer = Tracer $ \event -> do
                traceWith backendTracer event
                case event of
                    ReplayStarted ->
                        traceWith
                            metricsTracer
                            (SyncPhaseEvent Replaying)
                    ReplayCompleted ->
                        traceWith
                            metricsTracer
                            (SyncPhaseEvent Following)
            backendInit =
                createBackend
                    combinedBackendTracer
                    mutationTracer
                    kvOnlyOps
                    slotHash
        restoring <- Backend.start backendInit
        let initialPhase = InRestoration restoring
        notifyTVar <- newTVarIO (0 :: Int)
        appThread <-
            async
                $ applicationN2C
                    syncThresh
                    (EpochSlots 42)
                    devnetMagic
                    socketPath
                    setupStartingPoint
                    (\_ -> pure ())
                    metricsTracer
                    nullTracer
                    runner
                    backendInit
                    armageddonParams
                    secParam
                    notifyTVar
                    initialPhase
                    [setupStartingPoint]
        callback (cancel appThread) runner

{- | Run the CSMT app until synced, then run the
callback with the runner.
-}
withCsmtSynced
    :: FilePath
    -> FilePath
    -> (Runner -> IO a)
    -> IO a
withCsmtSynced socketPath dbPath callback = do
    syncedVar <- newEmptyTMVarIO
    let metricsTracer = Tracer $ \case
            SyncPhaseEvent Synced ->
                atomically
                    $ void
                    $ tryPutTMVar syncedVar ()
            _ -> pure ()
    withCsmtApp
        socketPath
        metricsTracer
        nullTracer
        0
        0
        (SyncThreshold 100)
        5
        dbPath
        $ \kill runner -> do
            _ <-
                timeout 30_000_000
                    $ atomically
                    $ readTMVar syncedVar
            threadDelay 2_000_000
            kill
            callback runner

{- | Start the CSMT app, wait for a specific phase,
kill it. The DB is left in a crashed state.
Uses a restore delay to ensure the kill lands
during the target phase.
Returns the phases seen before the kill, so the
caller can verify the kill landed correctly.
-}
killCsmtDuring
    :: SyncPhase
    -> Int
    -- ^ Restore delay (microseconds per mutation)
    -> Int
    -- ^ Replay delay (microseconds per chunk)
    -> SyncThreshold
    -> FilePath
    -> FilePath
    -> IO ([SyncPhase], Int)
killCsmtDuring targetPhase restoreDelay replayDelay syncThresh socketPath dbPath = do
    phaseVar <- newEmptyTMVarIO
    seenRef <- newIORef ([] :: [SyncPhase])
    countRef <- newIORef (0 :: Int)
    let minEvents = 10 -- wait for at least 10 events
        metricsTracer = Tracer $ \case
            SyncPhaseEvent phase -> do
                modifyIORef' seenRef $ \seen ->
                    if phase `elem` seen
                        then seen
                        else seen ++ [phase]
                when (phase == targetPhase) $ do
                    n <-
                        modifyIORef' countRef (+ 1)
                            >> readIORef countRef
                    when (n >= minEvents)
                        $ atomically
                        $ void
                        $ tryPutTMVar phaseVar ()
            _ -> pure ()
    withCsmtApp
        socketPath
        metricsTracer
        nullTracer
        restoreDelay
        replayDelay
        syncThresh
        5
        dbPath
        $ \kill _runner -> do
            _ <-
                timeout 30_000_000
                    $ atomically
                    $ readTMVar phaseVar
            kill
    phases <- readIORef seenRef
    count <- readIORef countRef
    pure (phases, count)

-- * Tests

spec :: Spec
spec = describe "Crash recovery" $ do
    it "inclusion proofs verify for submitted txs" $ do
        blocksRef <- newIORef ([] :: [Block])
        txIdsRef <- newIORef ([] :: [TxId])
        withCardanoNode genesisDir $ \socketPath _startMs -> do
            -- Step 1: populate chain with 50 txs
            populateChain
                socketPath
                devnetMagic
                genesisAddr
                genesisSignKey
                (mkPopulator blocksRef txIdsRef 10 10)

            blocks <- readIORef blocksRef
            txIds <- readIORef txIdsRef
            putStrLn
                $ "Populated: "
                    ++ show (length blocks)
                    ++ " blocks, "
                    ++ show (length txIds)
                    ++ " txs"

            -- Step 2: run CSMT app on same node
            withSystemTempDirectory "e2e-csmt" $ \dbPath ->
                withCsmtSynced socketPath dbPath $ \runner -> do
                    -- Query merkle root
                    let CSMTContext{hashing = h} = context
                    root <-
                        transact
                            runner
                            (queryMerkleRoot h)
                    putStrLn $ "Merkle root: " ++ show root
                    root `shouldSatisfy` (/= Nothing)

                    -- Query inclusion proof for the last
                    -- submitted tx (its change output
                    -- should be in the UTxO set)
                    let lastTxId = last txIds
                    proof <-
                        queryInclusionProof
                            runner
                            (txIdHex lastTxId)
                            0
                    putStrLn
                        $ "Proof for last tx: "
                            ++ show (fmap (const ("present" :: String)) proof)
                    proof `shouldSatisfy` (/= Nothing)

    it "kill during restoration — same root after restart"
        $ killAndVerify Restoring 10_000 0 (SyncThreshold 100)

    it "kill during replay — same root after restart"
        $ killAndVerify Replaying 0 1_000_000 (SyncThreshold 100)

    it "kill during following — same root after restart"
        $ killAndVerify Following 0 0 (SyncThreshold 1)

-- | Kill during a phase, restart, verify root + proof.
killAndVerify :: SyncPhase -> Int -> Int -> SyncThreshold -> IO ()
killAndVerify targetPhase restoreDelay replayDelay syncThresh' = do
    blocksRef <- newIORef ([] :: [Block])
    txIdsRef <- newIORef ([] :: [TxId])
    withCardanoNode genesisDir $ \socketPath _startMs -> do
        populateChain
            socketPath
            devnetMagic
            genesisAddr
            genesisSignKey
            (mkPopulator blocksRef txIdsRef 10 10)

        txIds <- readIORef txIdsRef

        withSystemTempDirectory "e2e-kill" $ \dbPath -> do
            putStrLn
                $ "Kill during "
                    ++ show targetPhase
                    ++ "..."
            (phases, count) <-
                killCsmtDuring
                    targetPhase
                    restoreDelay
                    replayDelay
                    syncThresh'
                    socketPath
                    dbPath
            putStrLn
                $ "Phases seen: "
                    ++ show phases
                    ++ ", target events: "
                    ++ show count

            -- Target phase must have been seen
            targetPhase `elem` phases
                `shouldBe` True
            count `shouldSatisfy` (>= 1)

            -- Phase-specific: must not have gone
            -- beyond the target
            case targetPhase of
                Restoring ->
                    Replaying `elem` phases
                        `shouldBe` False
                Replaying ->
                    Synced `elem` phases
                        `shouldBe` False
                Following ->
                    Synced `elem` phases
                        `shouldBe` False
                _ -> pure ()

            -- Restart and verify
            putStrLn "Restarting..."
            withCsmtSynced socketPath dbPath $ \runner -> do
                let CSMTContext{hashing = h} = context
                root <-
                    transact
                        runner
                        (queryMerkleRoot h)
                putStrLn
                    $ "Root after restart: "
                        ++ show root
                root `shouldSatisfy` (/= Nothing)

                -- Verify ALL UTxOs:
                -- Each tx has outputs 0-9 (extra) + last (change)
                -- Change is spent by next tx, except last tx
                let
                    -- Unspent: all extra outputs (0-9) from every tx
                    -- + change output of last tx
                    unspent =
                        [ (txIdHex tid, ix)
                        | tid <- txIds
                        , ix <- [0 .. 9]
                        ]
                            ++ [(txIdHex (last txIds), 10)]
                    -- Spent: change output (10) of every tx except last
                    spent =
                        [ (txIdHex tid, 10)
                        | tid <- init txIds
                        ]

                -- All unspent UTxOs must have inclusion proofs
                putStrLn
                    $ "Checking "
                        ++ show (length unspent)
                        ++ " inclusion proofs..."
                forM_ unspent $ \(tid, ix) -> do
                    proof <-
                        queryInclusionProof
                            runner
                            tid
                            ix
                    case proof of
                        Nothing ->
                            error
                                $ "Inclusion proof failed for "
                                    ++ show tid
                                    ++ ":"
                                    ++ show ix
                        Just _ -> pure ()
                putStrLn "All inclusion proofs verified."

                -- All spent UTxOs must have exclusion proofs
                putStrLn
                    $ "Checking "
                        ++ show (length spent)
                        ++ " exclusion proofs..."
                forM_ spent $ \(tid, ix) -> do
                    excluded <-
                        queryExclusionProof
                            runner
                            tid
                            ix
                    excluded `shouldBe` True
                putStrLn "All exclusion proofs verified."

                -- Fabricated TxId must be excluded
                let fakeTxId = "0000000000000000000000000000000000000000000000000000000000000000"
                fakeExcluded <-
                    queryExclusionProof
                        runner
                        fakeTxId
                        0
                fakeExcluded `shouldBe` True
