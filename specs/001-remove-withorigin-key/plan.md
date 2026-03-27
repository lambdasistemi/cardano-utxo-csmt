# Implementation Plan: Replace WithOrigin with WithSentinel

**Branch**: `001-remove-withorigin-key` | **Date**: 2026-03-27 | **Spec**: [spec.md](spec.md)

## Summary

Replace ouroboros's `WithOrigin` with a project-owned `WithSentinel` type for rollback column keys. Same CBOR wire format (backward compatible), clearer semantics, decoupled from ouroboros internals.

## Technical Context

**Language/Version**: Haskell (GHC 9.8.4)
**Primary Dependencies**: `mts:rollbacks` (generic over key), `rocksdb-kv-transactions`, `ouroboros-network-api` (`Point`, `WithOrigin`)
**Storage**: RocksDB with CBOR-encoded keys via `Prism' ByteString a`
**Testing**: HSpec + QuickCheck (unit-tests, database-tests)
**Target Platform**: Linux (NixOS)
**Project Type**: Library + HTTP service
**Constraints**: Wire format preserved — no DB wipe needed

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| Pure core / impure shell | Pass | Change is in pure type/codec layer |
| Correctness | Pass | Eliminates semantic confusion between chain origin and DB sentinel |
| Small focused commits | Pass | Single concern: replace WithOrigin with WithSentinel |
| Test separation | Pass | Unit tests and database tests updated separately |
| Storage breaking change | N/A | Wire format unchanged, no DB wipe |

No violations.

## Project Structure

### Documentation (this feature)

```text
specs/001-remove-withorigin-key/
├── plan.md
├── research.md
├── data-model.md
└── tasks.md
```

### Source Code (affected files)

```text
lib/Cardano/UTxOCSMT/Application/Database/Implementation/
├── Columns.hs          # GADT key types + codec registration
├── RollbackPoint.hs    # WithSentinel type + withSentinelPrism + RollbackPointKV
├── Armageddon.hs       # Sentinel initialization
└── Query.hs            # Tip/finality/merkle root queries

application/Cardano/UTxOCSMT/Application/Run/
└── Application.hs      # rollBackward handler

test/Cardano/UTxOCSMT/Application/Database/Implementation/
└── RollbackPointSpec.hs

database-test/Cardano/UTxOCSMT/Application/Database/
├── E2ERunnerSpec.hs
└── ServerSpec.hs (if it uses WithOrigin for rollback keys)

docs/
└── database-schema.md
```
