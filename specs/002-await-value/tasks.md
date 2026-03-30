# Tasks: Push-based UTxO Await

**Input**: Design documents from `/specs/002-await-value/`

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

## Phase 1: Foundational — Notification Mechanism

**Purpose**: Create the TVar notification plumbing without changing any external interfaces.

- [ ] T001 [US1] Add `awaitValue` field to `Query` record in `lib/Cardano/UTxOCSMT/Application/Database/Interface.hs`
- [ ] T002 [US1] Update `hoistQuery` to transform the new `awaitValue` field in `lib/Cardano/UTxOCSMT/Application/Database/Interface.hs`
- [ ] T003 [US1] Add `TVar Int` parameter to `application` function and increment after each `processBlock` commit in `application/Cardano/UTxOCSMT/Application/Run/Application.hs`

**Checkpoint**: Query type extended, notification fires after commits. Existing code compiles with placeholder `awaitValue`.

---

## Phase 2: User Story 1 — awaitValue Implementation (Priority: P1)

**Goal**: `awaitValue` blocks until a key appears in the store, using STM retry on the notification TVar.

**Independent Test**: In two threads — one calls awaitValue, the other inserts a key. The blocked thread unblocks and returns the value.

### Implementation

- [ ] T004 [US1] Implement `queryAwaitValue` handler using STM retry loop + `System.Timeout.timeout` in `application/Cardano/UTxOCSMT/Application/Run/Query.hs`
- [ ] T005 [US1] Create `TVar Int` at startup in `application/Cardano/UTxOCSMT/Application/Run/Main.hs`, pass to `application` and wire into Query
- [ ] T006 [US1] Update `mkQuery` / `mkTransactionedQuery` to accept notification TVar and populate `awaitValue` field in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Query.hs`

**Checkpoint**: `awaitValue` works end-to-end in the application. Can be tested programmatically.

---

## Phase 3: User Story 2 — HTTP Await Endpoint (Priority: P2)

**Goal**: External clients can block-wait for a UTxO key via HTTP.

**Independent Test**: HTTP request to `/await/:txId/:txIx` blocks until key is inserted, returns 200 with value.

### Implementation

- [ ] T007 [US2] Add `GET /await/:txId/:txIx` endpoint with `timeout` query parameter to Servant API type in `http/Cardano/UTxOCSMT/HTTP/API.hs`
- [ ] T008 [US2] Add `AwaitResponse` type (or reuse existing proof/UTxO response) in `http/Cardano/UTxOCSMT/HTTP/API.hs`
- [ ] T009 [US2] Implement HTTP handler that calls `awaitValue` and returns 200 or 408 in `http/Cardano/UTxOCSMT/HTTP/Server.hs`
- [ ] T010 [US2] Wire await handler into server and pass Query with awaitValue to HTTP layer in `application/Cardano/UTxOCSMT/Application/Run/Query.hs`

**Checkpoint**: HTTP await endpoint works end-to-end.

---

## Phase 4: Tests

- [ ] T011 [P] [US1] Add unit test for awaitValue STM logic: fast path (key exists), slow path (key inserted after await), timeout in `test/`
- [ ] T012 [P] [US1] Add database-test for awaitValue with real RocksDB: insert key in one thread, await in another in `database-test/`

**Checkpoint**: All tests pass.

---

## Phase 5: Polish

- [ ] T013 Update Swagger documentation (`just update-swagger`)
- [ ] T014 Run `just format`, `just hlint`, `just cabal-check`
- [ ] T015 Build and run all test suites

---

## Dependencies & Execution Order

- **Phase 1** (T001-T003): Sequential — T001 first (type change), then T002-T003
- **Phase 2** (T004-T006): Depends on Phase 1. T004 and T006 can be parallel, T005 wires them.
- **Phase 3** (T007-T010): Depends on Phase 2. T007-T008 parallel, T009 after, T010 after.
- **Phase 4** (T011-T012): Depends on Phase 2 (US1 tests) and Phase 3 (US2 would need HTTP tests too). T011 and T012 parallel.
- **Phase 5** (T013-T015): Depends on all above.

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Phase 1: notification plumbing
2. Phase 2: awaitValue in Query interface
3. Phase 4 (T011-T012): tests for US1
4. Deploy — MPFS can use awaitValue programmatically

### Full Feature

5. Phase 3: HTTP endpoint
6. Phase 5: polish

---

## Notes

- 15 tasks across 5 phases
- 2 user stories: P1 (programmatic await), P2 (HTTP endpoint)
- MVP delivers programmatic await; HTTP endpoint is additive
- Commits: split by phase (notification mechanism, await impl, HTTP endpoint, tests)
