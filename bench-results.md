# KVOnly Benchmark Results

All measurements: 1 minute sample, solo on node, targeting slot ~12-14M zone.

| # | Variant | slots/sec | Slot range | Notes |
|---|---------|-----------|------------|-------|
| 1 | Noop (seq ops, no DB) | 14,120 | 13.2M→14.0M | Pure chain-sync ceiling |
| 2 | Empty transaction per block | 14,240 | 12.0M→12.9M | `transact $ pure ()` |
| 3 | Transaction + queryTip | 13,972 | 12.0M→12.8M | `transact $ queryTip` |
| 4 | Transaction + queryTip + kvOps inserts | 13,496 | 12.0M→12.8M | csmtInsert (KV + journal) per Insert op |
| 5 | Transaction + queryTip + kvOps inserts + deletes | 13,708 | 12.0M→12.8M | csmtInsert + csmtDelete (KV + journal) |
| 6 | Write every block: kvOps + rollback point | 810 | 1.3M→1.4M | WAL sync per block = 17x slowdown! |
| 7 | Skip-empty + kvOps + rollback point | 1,347 | 12.0M→12.1M | Only write on blocks with ops |
| 8 | Skip-empty + full forwardTip (KVOnly) | 1,183 | 12.0M→12.1M | forwardTip with kvOps + count tracking |
| 9 | Skip-empty + kvOps, NO rollback, NO queryTip | 12,795 | 12.2M→12.9M | Pure kvOps baseline |
| 10 | Skip-empty + forwardTip + queryTip, NO rollback | 9,979 | 12.3M→12.9M | queryTip adds ~20% overhead |
| 11 | Skip-empty + kvOps + rollback, NO queryTip | 1,291 | 12.0M→12.1M | storeRollbackPoint write cost isolated |
| 12 | Full mode (baseline) | 690 | 15.2M→15.3M | mkUpdate with full CSMT ops |

## Key Findings

1. **Tests 2-5 misleading**: Fast (~14K) because empty blocks = read-only transactions (no WAL sync).
2. **WAL sync per block kills perf**: Test 6 writes on every block → 17x slower.
3. **Skip-empty essential**: Only open write transactions for blocks with ops.
4. **Cost breakdown per block with ops at slot 12M**:
   - Ceiling (noop): 14,120 slots/sec
   - kvOps (KV + journal): 12,795 (~10% overhead)
   - + queryTip: 9,979 (~22% overhead)
   - + storeRollbackPoint: 1,183 (**10x** slowdown from rollback write!)
   - storeRollbackPoint w/o queryTip: 1,291 (confirms write is bottleneck, not read)
   - Full CSMT mode: 690 (at 15M, includes CSMT tree updates)
5. **storeRollbackPoint is the dominant bottleneck** in KVOnly mode.
   queryTip is NOT the issue (removing it barely helps: 1,183→1,291).
   The single `insert RollbackPoints` call causes a 10x slowdown.
   This is puzzling since `storeRollbackPoint = insert` (same as KVCol/JournalCol inserts).
   Investigation needed: CBOR serialization cost vs RocksDB CF-specific behavior.
