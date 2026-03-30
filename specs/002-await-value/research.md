# Research: Push-based UTxO Await

## Decision 1: Notification Granularity

**Decision**: Single `TVar Int` counter for all commits (coarse-grained).

**Rationale**: Per-key TVars would require a concurrent map that grows unboundedly. A single counter is simple, efficient, and sufficient — waiters re-check their specific key after each notification. The re-check is a single RocksDB point lookup (microseconds).

**Alternatives considered**:
- Per-key TVar map — rejected: unbounded memory, cleanup complexity, no meaningful latency improvement since RocksDB lookups are fast.
- `TChan` broadcast — rejected: requires each waiter to create a channel and risks missed messages if the channel isn't set up before the commit.

## How STM Retry Wakes Waiters

The notification works via the STM runtime's built-in dependency tracking, not explicit signaling:

1. **Waiter** runs `atomically $ do { c <- readTVar tvar; when (c == lastSeen) retry }`. The `retry` tells the STM runtime: "park this thread and record that it read `tvar`."
2. **Writer** runs `atomically $ modifyTVar' tvar (+1)`. When this commits, the STM runtime detects that `tvar` changed and wakes all threads whose retried transaction read it.
3. **Woken waiter** re-runs its transaction: reads `tvar` (new value), `c /= lastSeen` so transaction succeeds, thread exits `atomically` and does IO (re-checks RocksDB for the key).

No explicit signals, no condition variables, no missed wakeups. The STM runtime guarantees that any thread blocked on `retry` is woken when any TVar in its read-set changes.

**Concurrency under load**: every commit wakes all waiters (thundering herd), but each waiter does a single RocksDB point lookup (microseconds) then goes back to sleep if its key wasn't in that block. This is acceptable for the expected load (handful of concurrent awaiters).

## Decision 2: Timeout Mechanism

**Decision**: `System.Timeout.timeout` wrapping the STM retry loop.

**Rationale**: Standard library, composable, well-understood. The STM loop retries on TVar changes; timeout kills the thread after the deadline.

**Alternatives considered**:
- Manual `registerDelay` + STM — more control but unnecessary complexity for this use case.

## Decision 3: HTTP Endpoint Style

**Decision**: Long-poll GET endpoint with timeout query parameter.

**Rationale**: Simplest for clients — standard HTTP GET, no WebSocket complexity. The server holds the connection open until the key appears or timeout expires. Compatible with any HTTP client.

**Alternatives considered**:
- WebSocket — rejected: overkill for single-value await; adds client-side complexity.
- Server-Sent Events — rejected: similar complexity to WebSocket for a one-shot use case.

## Decision 4: Where to Increment TVar

**Decision**: In `Application.hs` after the `transact $ processBlock` call.

**Rationale**: This is the exact point where a commit is finalized. The existing code already has the transaction boundary here. Adding `atomically $ modifyTVar' notificationTVar (+1)` after the commit is minimal and correct.

## Decision 5: Query Interface Extension

**Decision**: Add `awaitValue` as a new field in the `Query` record, not a separate type.

**Rationale**: Keeps the Query interface cohesive. Existing consumers are unaffected (they don't use the new field). The field is `Maybe` or the Query constructor is extended with a default.

**Alternative considered**:
- Separate `AwaitQuery` type — rejected: fragmenting the query interface makes wiring harder.
