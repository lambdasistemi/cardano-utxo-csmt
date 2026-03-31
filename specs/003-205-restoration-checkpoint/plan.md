# Implementation Plan: Use Chain-Follower Restoration Checkpoint

**Branch**: `feat/restoration-mode` | **Date**: 2026-03-31 | **Spec**: [spec.md](spec.md)

## Research Summary

### Current State (on this branch)

- chain-follower pinned to `4ff9eb3` (has `onBlock` callback)
- `Application.hs` passes `txCheckpoint` to `follower`/`intersector`/`application`/`applicationN2C`
- `Main.hs` defines `txCheckpoint` using `putBaseCheckpoint` and passes it through
- `setCheckpoint` (IO action) still used for N2C skip mode checkpoint

### Target State

- chain-follower pinned to `8d2c5f9` (has internal checkpoint, no `onBlock`)
- `processBlock` has no `onBlock` — checkpoint is automatic
- `readCheckpoint` used on startup for intersection
- `txCheckpoint` / `putBaseCheckpoint` removed from app
- `setCheckpoint` (IO action) may still be needed for N2C skip mode — needs investigation

### N2C Skip Mode Checkpoint

`setCheckpoint` is called in `ChainSyncN2C.hs` line 184 when the Mithril skip phase completes. This is a different concern — it marks the point where skip ends and normal following begins. This is NOT the per-block checkpoint. It can stay as a separate IO action or be replaced by the chain-follower checkpoint once the first real block is processed.

**Decision**: Keep `setCheckpoint` for skip mode. It's orthogonal to the per-block checkpoint.

## Implementation Phases

### Phase 1: Bump chain-follower pin

Update `cabal.project` to `8d2c5f9`.

### Phase 2: Remove manual checkpoint from Application.hs

- Remove `checkpoint` parameter from `follower`, `intersector`
- Remove `(Point -> AppTx ())` from `application`, `applicationN2C` signatures
- Remove `txCheckpoint` from parameter lists and call sites
- Remove `onBlock` argument from `processBlock` calls (already gone in new chain-follower)

### Phase 3: Remove txCheckpoint from Main.hs

- Remove `txCheckpoint` definition
- Remove `txCheckpoint` from `application`/`applicationN2C` call sites

### Phase 4: Use readCheckpoint on startup

In `Main.hs`, after opening the DB, use `readCheckpoint Rollbacks` to get the last checkpoint. Use it as the starting point for chain sync intersection (replacing or supplementing the current `setupStartingPoint`).

### Phase 5: Build and deploy

Build docker image, deploy to preprod, verify restart behavior.
