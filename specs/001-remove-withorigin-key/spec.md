# Feature Specification: Replace WithOrigin with WithSentinel in Rollback Column Keys

**Feature Branch**: `001-remove-withorigin-key`
**Created**: 2026-03-27
**Status**: Draft
**Input**: Replace ouroboros `WithOrigin` with a project-owned `WithSentinel` type for rollback column keys, eliminating semantic confusion between chain origin and database sentinel.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Correct Rollback to Genesis (Priority: P1)

When the chain follower receives a `RollBackward Origin` from ChainSync, the system rolls back to genesis without encountering "rollback impossible" errors caused by the semantic mismatch between `WithOrigin.Origin` (chain concept) and the database sentinel.

**Why this priority**: The reuse of ouroboros's `WithOrigin` for the database sentinel creates a phantom gap in the key ordering when `slot = Point` (which already contains `WithOrigin` internally). This is a correctness bug.

**Independent Test**: Initialize a fresh database, insert blocks, roll back to sentinel, verify the state resets correctly.

**Acceptance Scenarios**:

1. **Given** a database with blocks at slots 1-10, **When** a rollback to sentinel is requested, **Then** the rollback succeeds and the tip returns to `Sentinel`.
2. **Given** a fresh database with only the sentinel, **When** the tip is queried, **Then** it returns `Sentinel`.

---

### User Story 2 - Clear Sentinel Semantics (Priority: P2)

The database sentinel is a distinct concept from chain origin. The type system makes this explicit via a project-owned `WithSentinel` type.

**Why this priority**: Using ouroboros's `WithOrigin` conflates two distinct concerns: chain position (origin = before genesis block) and database initialization marker (sentinel = before any data). The `WithSentinel` type makes the database concern explicit and decouples from ouroboros internals.

**Independent Test**: Verify that `Sentinel` is strictly less than any `Value slot` in key ordering, regardless of the concrete `slot` type.

**Acceptance Scenarios**:

1. **Given** `WithSentinel` with `Ord` instance, **When** comparing `Sentinel` with `Value x` for any `x`, **Then** `Sentinel < Value x`.
2. **Given** rollback column key type is `WithSentinel slot`, **When** the code is compiled, **Then** no `Ouroboros.Network.Point.WithOrigin` appears in rollback key types or codecs.

---

### Edge Cases

- Empty database (no sentinel): system fails with a clear error as today.
- Rollback to a point not in the store: `seekKey` returns `RollbackImpossible` as before.
- Existing databases with old codec: incompatible, must be wiped.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: A new `WithSentinel a` type MUST be defined with constructors `Sentinel` and `Value a`, with `Eq`, `Ord`, `Show` instances where `Sentinel < Value x` for all `x`.
- **FR-002**: The `RollbackPoints` and `Rollbacks` columns MUST use `WithSentinel slot` as their key type instead of `WithOrigin slot`.
- **FR-003**: A new `withSentinelPrism` codec MUST replace `withOriginPrism`, using the same CBOR wire format for backward compatibility (tag 0 = Sentinel, [1, slot] = Value).
- **FR-004**: All call sites MUST replace `Origin` with `Sentinel` and `At x` with `Value x`.
- **FR-005**: The `withOriginPrism` function MUST be removed.
- **FR-006**: The `WithOrigin` import from ouroboros MUST be removed from all rollback key code (it may remain for non-key uses like `pointSlot`).
- **FR-007**: The database schema documentation MUST reflect the new type name.

### Key Entities

- **WithSentinel a**: New sum type replacing `WithOrigin` for rollback keys. Sentinel = database initialization marker, Value = actual slot data.
- **RollbackPointKV**: Type alias updated from `WithOrigin slot` to `WithSentinel slot`.
- **Rollbacks/RollbackPoints**: GADT constructors updated similarly.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing unit and database tests pass with updated assertions.
- **SC-002**: `WithOrigin` does not appear in any rollback column key type or codec.
- **SC-003**: E2E runner tests demonstrate correct rollback to sentinel with the new type.
- **SC-004**: Wire format is identical to the old codec (no DB wipe needed if format preserved).

## Assumptions

- Wire format compatibility: if the CBOR encoding is identical (tag 0 = Sentinel, [1, slot] = Value), existing databases remain compatible and no wipe is needed.
- The upstream `mts:rollbacks` library is generic over `key` — no upstream changes needed.
- `WithSentinel` is defined in the RollbackPoint module alongside the codecs.
