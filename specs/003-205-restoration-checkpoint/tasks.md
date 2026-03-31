# Tasks: Use Chain-Follower Restoration Checkpoint

**Feature**: 003-205-restoration-checkpoint
**Plan**: [plan.md](plan.md)

### Task 1: Bump chain-follower pin
**File**: `cabal.project`
**Depends on**: nothing

Update tag to `8d2c5f920b53d14e486643bf6dffdf4ed091f50d` with correct sha256.

**Done when**: `cabal build` fetches the new dep.

---

### Task 2: Remove manual checkpoint from Application.hs
**File**: `application/Cardano/UTxOCSMT/Application/Run/Application.hs`
**Depends on**: Task 1

- Remove `checkpoint` / `txCheckpoint` parameters from `follower`, `intersector`, `application`, `applicationN2C`
- Remove `(checkpoint fetchedPoint)` from `processBlock` call
- Remove `(Point -> AppTx ())` type annotations
- processBlock call becomes: `processBlock atTip transact Rollbacks securityParam ...`

**Done when**: Application.hs compiles.

---

### Task 3: Remove txCheckpoint from Main.hs
**File**: `application/Cardano/UTxOCSMT/Application/Run/Main.hs`
**Depends on**: Task 2

- Remove `txCheckpoint` definition
- Remove `txCheckpoint` from `application`/`applicationN2C` call sites

**Done when**: Main.hs compiles.

---

### Task 4: Use readCheckpoint on startup
**File**: `application/Cardano/UTxOCSMT/Application/Run/Main.hs`
**Depends on**: Task 3

After DB open, call `readCheckpoint Rollbacks` via `transact runner`. Use the result as the intersection point for chain sync (in addition to or replacing `setupStartingPoint`).

**Done when**: startup uses checkpoint for intersection.

---

### Task 5: Build, deploy, verify restart
**Depends on**: Task 4

Build docker image, deploy to preprod, sync for 60s, restart, verify resume from correct slot.

**Done when**: restart resumes within 1 block.
