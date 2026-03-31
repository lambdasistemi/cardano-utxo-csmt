# Feature Specification: Use Chain-Follower Restoration Checkpoint

**Feature Branch**: `feat/restoration-mode`
**Created**: 2026-03-31
**Status**: Draft
**Input**: lambdasistemi/cardano-utxo-csmt#205

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Restart resumes from last processed block (Priority: P1)

The application processes blocks during restoration. After a restart (crash or intentional), it resumes from the last fully processed block without reprocessing or corrupting the journal.

**Why this priority**: Without this, restarts corrupt the journal by reprocessing blocks with stale checkpoint.

**Independent Test**: Process blocks to slot N, restart, verify intersection at slot N.

**Acceptance Scenarios**:

1. **Given** the app syncs to slot 10000, **When** it restarts, **Then** chain sync resumes from slot 10000.
2. **Given** the app crashes mid-transaction, **When** it restarts, **Then** it resumes from the last committed block (not the one that crashed).

---

### User Story 2 - Remove manual checkpoint management (Priority: P2)

The application no longer calls `putBaseCheckpoint`, `setCheckpoint`, or passes `txCheckpoint` to the follower. The chain-follower Runner handles checkpoint storage internally.

**Why this priority**: Eliminates a class of bugs and simplifies the application code.

**Independent Test**: Application compiles without any `putBaseCheckpoint` imports or checkpoint parameters.

**Acceptance Scenarios**:

1. **Given** the chain-follower stores checkpoints internally, **When** the application calls `processBlock`, **Then** no checkpoint callback or action is needed.
2. **Given** the application starts up, **When** it calls `readCheckpoint`, **Then** it gets the last processed slot for chain sync intersection.

---

### Edge Cases

- First start with empty DB: `readCheckpoint` returns `Nothing`, start from genesis.
- Restart after transition to following: rollback points handle resume, checkpoint not needed.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Application MUST pin chain-follower to the branch with `readCheckpoint` and internal checkpoint (lambdasistemi/chain-follower#21)
- **FR-002**: Application MUST remove `txCheckpoint`, `putBaseCheckpoint` usage from `Application.hs` and `Main.hs`
- **FR-003**: Application MUST remove `setCheckpoint` IO action from `applicationN2C` parameters
- **FR-004**: Application MUST use `readCheckpoint` on startup to determine chain sync intersection point
- **FR-005**: Application MUST remove `onBlock` parameter from `processBlock` calls

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: No `putBaseCheckpoint` or `txCheckpoint` in application code
- **SC-002**: Restart after 60s of syncing resumes within 1 block of where it stopped
- **SC-003**: Application builds and deploys successfully

## Assumptions

- chain-follower PR #21 is available on the `feat/restoration-checkpoint` branch
- The `readCheckpoint` API matches the consumer's needs for intersection point
- The existing `setCheckpoint` for N2C skip mode may still be needed — investigate
