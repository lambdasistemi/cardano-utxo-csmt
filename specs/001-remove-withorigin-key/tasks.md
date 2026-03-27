# Tasks: Replace WithOrigin with WithSentinel

**Input**: Design documents from `/specs/001-remove-withorigin-key/`

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to

## Phase 1: Define WithSentinel Type

**Purpose**: Introduce the new type and codec before changing any consumers.

- [ ] T001 [US2] Define `WithSentinel a` type with `Sentinel | Value a` constructors (deriving `Eq`, `Ord`, `Show`) and `withSentinelPrism` codec in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/RollbackPoint.hs`
- [ ] T002 [US2] Export `WithSentinel (..)` and `withSentinelPrism` from module header, keep old exports temporarily

**Checkpoint**: New type exists alongside old code. Everything still compiles.

---

## Phase 2: Switch Column Types

**Purpose**: Update GADT and type alias to use `WithSentinel`. Compiler errors guide remaining work.

- [ ] T003 [US1] Change `RollbackPointKV` type alias from `WithOrigin slot` to `WithSentinel slot` in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/RollbackPoint.hs`
- [ ] T004 [US1] Change `Rollbacks` GADT constructor key type from `WithOrigin slot` to `WithSentinel slot` in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Columns.hs`
- [ ] T005 [US1] Update codec registration from `withOriginPrism slotP` to `withSentinelPrism slotP` in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Columns.hs`

**Checkpoint**: Types changed — compiler errors appear at all call sites.

---

## Phase 3: Fix Call Sites (Priority: P1)

**Goal**: All rollback operations compile and work with `WithSentinel`.

- [ ] T006 [US1] Replace `Origin` with `Sentinel` in sentinel setup in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Armageddon.hs`
- [ ] T007 [P] [US1] Update `getAllMerkleRoots` return type from `WithOrigin slot` to `WithSentinel slot` in `lib/Cardano/UTxOCSMT/Application/Database/Implementation/Query.hs`
- [ ] T008 [P] [US1] Replace `At point` with `Value point` in `processBlock` and `rollbackTo` calls in `application/Cardano/UTxOCSMT/Application/Run/Application.hs`

**Checkpoint**: Library and application compile.

---

## Phase 4: Update Tests

- [ ] T009 [P] [US1] Replace all `Origin` with `Sentinel` and `At x` with `Value x` in `database-test/Cardano/UTxOCSMT/Application/Database/E2ERunnerSpec.hs`
- [ ] T010 [P] [US1] Replace `Origin`/`At` with `Sentinel`/`Value` in `database-test/Cardano/UTxOCSMT/HTTP/ServerSpec.hs`
- [ ] T011 [P] [US2] Update `withOriginPrism` tests to test `withSentinelPrism` in `test/Cardano/UTxOCSMT/Application/Database/Implementation/RollbackPointSpec.hs`

**Checkpoint**: All tests compile and pass.

---

## Phase 5: Remove Dead Code

- [ ] T012 [US2] Remove `withOriginPrism` function and its export from `lib/Cardano/UTxOCSMT/Application/Database/Implementation/RollbackPoint.hs`
- [ ] T013 Remove unused `WithOrigin` imports from all changed files
- [ ] T014 Update CDDL schema — rename `WithOrigin` to `WithSentinel` in `docs/database-schema.md`

---

## Phase 6: Polish

- [ ] T015 Run `just format` and `just cabal-check`
- [ ] T016 Verify build: `cabal build unit-tests database-tests`
- [ ] T017 Run tests: `cabal test unit-tests` and `cabal test database-tests`

---

## Dependencies & Execution Order

- **Phase 1** (T001-T002): No deps — new code alongside old
- **Phase 2** (T003-T005): Sequential, depends on Phase 1
- **Phase 3** (T006-T008): Depends on Phase 2. T007 and T008 parallel.
- **Phase 4** (T009-T011): Depends on Phase 3. All three parallel.
- **Phase 5** (T012-T014): Depends on Phase 4.
- **Phase 6** (T015-T017): Depends on Phase 5.

## Notes

- Wire format is identical — no DB wipe needed
- Single commit: all tasks are one logical concern (replace WithOrigin with WithSentinel)
- 17 tasks across 6 phases
