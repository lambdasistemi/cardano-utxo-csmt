# Research: Replace WithOrigin with WithSentinel

## Decision 1: WithSentinel Type

**Decision**: Define a project-owned `WithSentinel a` type with `Sentinel | Value a` constructors.

**Rationale**: `WithOrigin` from ouroboros conflates chain origin (a chain concept) with database sentinel (a storage concept). When `slot = Point` (which already contains `WithOrigin`), this creates double wrapping with a phantom gap. A dedicated `WithSentinel` type makes the database concern explicit.

**Alternatives considered**:
- Remove wrapping entirely, use `slot` directly — rejected because tests use `slot = SlotNo` which has no genesis representation. The sentinel concept is needed for all slot types.
- Keep `WithOrigin` but document the confusion — rejected because the type-level confusion is the root cause of ordering bugs.

## Decision 2: Wire Format Compatibility

**Decision**: Use the same CBOR encoding as `withOriginPrism` (tag 0 = Sentinel, [1, slot] = Value).

**Rationale**: The wire format is sound — it's just the Haskell type wrapping that's confusing. By keeping the same encoding, existing databases remain compatible and no DB wipe is needed.

**Alternatives considered**:
- New wire format — rejected because it forces a DB wipe for no benefit.

## Decision 3: Module Location

**Decision**: Define `WithSentinel` in `RollbackPoint.hs` alongside the codec.

**Rationale**: It's a small type used only by rollback column keys. No need for a separate module.
