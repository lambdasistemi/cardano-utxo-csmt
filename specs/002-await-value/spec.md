# Feature Specification: Push-based UTxO Await via TVar Notification

**Feature Branch**: `002-await-value`
**Created**: 2026-03-27
**Status**: Draft
**Input**: Add a blocking `awaitValue` operation that waits for a key to appear in the UTxO set, using TVar-based notification instead of polling.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Await Transaction Indexing (Priority: P1)

After submitting a Cardano transaction, a downstream service (MPFS offchain) needs to confirm the transaction has been indexed in the CSMT before proceeding. Instead of polling `getValue` every second, it calls `awaitValue` which blocks until the key appears or a timeout expires.

**Why this priority**: This is the core use case — replacing the 1-second polling loop with near-instant push notification. Reduces latency from up to 1 second to near-zero.

**Independent Test**: Submit a key via `forwardTipApply`, then verify `awaitValue` returns immediately if the key already exists. In a separate thread, call `awaitValue` for a key that doesn't exist yet, then insert it via `forwardTipApply`, and verify the blocked thread unblocks and returns the value.

**Acceptance Scenarios**:

1. **Given** a key exists in the UTxO set, **When** `awaitValue` is called for that key, **Then** it returns the value immediately.
2. **Given** a key does not exist, **When** `awaitValue` is called and then the key is inserted via a chain update, **Then** `awaitValue` unblocks and returns the value.
3. **Given** a key does not exist and a timeout is specified, **When** the timeout expires before the key appears, **Then** `awaitValue` returns `Nothing`.

---

### User Story 2 - HTTP Await Endpoint (Priority: P2)

An HTTP endpoint exposes the await functionality so external services can block-wait for a transaction to be indexed without implementing their own polling loop.

**Why this priority**: The MPFS offchain service currently polls via HTTP. An HTTP await endpoint eliminates the polling entirely at the API boundary.

**Independent Test**: Make an HTTP request to the await endpoint for a non-existent key, insert the key via chain update in another thread, verify the HTTP response arrives with the value.

**Acceptance Scenarios**:

1. **Given** a key exists, **When** `GET /await/:txId/:txIx` is called, **Then** it returns 200 with the value immediately.
2. **Given** a key does not exist, **When** the endpoint is called and the key is inserted before timeout, **Then** it returns 200 with the value.
3. **Given** a key does not exist and the timeout expires, **When** the endpoint is called, **Then** it returns 408 Request Timeout.

---

### Edge Cases

- Multiple clients awaiting the same key: all should be notified when the key appears.
- Key appears then disappears (rollback): the await should return the value at the time of notification; subsequent rollbacks are a separate concern.
- Server shutdown while clients are waiting: blocked threads should be interrupted cleanly.
- Very high frequency of commits: notification should not cause thundering herd — clients re-check their specific key, not scan the whole store.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The `Query` interface MUST provide an `awaitValue :: key -> m (Maybe value)` operation that blocks until the key exists or a timeout expires.
- **FR-002**: The database update path MUST broadcast a notification after each successful `forwardTipApply` commit.
- **FR-003**: The notification mechanism MUST use STM (`TVar`) so that multiple waiting threads are woken efficiently.
- **FR-004**: `awaitValue` MUST check the store immediately (fast path) before blocking.
- **FR-005**: The HTTP API MUST expose an await endpoint with a configurable timeout.
- **FR-006**: The timeout MUST be configurable per-request (query parameter or header) with a server-side maximum.

### Key Entities

- **Notification TVar**: A counter or epoch incremented after each commit. Waiters retry their lookup when the counter changes.
- **Query (extended)**: Adds `awaitValue` alongside existing `getValue`.
- **HTTP Await Endpoint**: Long-poll GET endpoint that blocks until the key appears or timeout.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `awaitValue` returns within 100ms of the key being committed (vs up to 1 second with polling).
- **SC-002**: No CPU usage while waiting (STM retry, not busy-loop).
- **SC-003**: Multiple concurrent awaiters for different keys do not interfere with each other.
- **SC-004**: The HTTP await endpoint handles at least 100 concurrent waiting connections without degradation.

## Assumptions

- The notification mechanism is coarse-grained: a single TVar counter for all commits, not per-key notifications. Waiters re-check their specific key after each notification.
- The timeout is enforced via `System.Timeout.timeout` wrapping the STM retry loop.
- The HTTP endpoint uses standard Servant handler with `liftIO` for the blocking operation.
- The TVar is created at application startup and threaded through the Query/Update interfaces.
