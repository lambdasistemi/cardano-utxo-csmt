# cardano-utxo-csmt Constitution

## Core Principles

### I. Pure Core, Impure Shell

Business logic lives in pure functions. IO is confined to a thin outer shell (database transactions, network, HTTP). Pure functions are tested with QuickCheck properties; the impure shell is tested with E2E database tests in a separate test suite.

### II. Correctness Over Convenience

- `-Werror` is enforced in CI builds via a cabal flag. Warnings are errors.
- `cabal check` runs in CI — upper bounds on `base`, `autogen-modules` for `Paths_*`, no unconditional `-Werror`.
- Breaking changes to storage codecs require explicit DB wipe documentation.
- No `sorry` in Lean proofs, no `undefined` in Haskell, no partial functions.

### III. Small Focused Commits

Each commit addresses a single concern. Conventional Commits format (`feat:`, `fix:`, `refactor:`, etc.). When a change conceptually belongs to an earlier commit, use StGit to place it there retroactively.

### IV. Test Separation

- **unit-tests**: Pure, fast, no database dependencies. Must run in under 1 second.
- **database-tests**: Require RocksDB, temp directories, full state machine testing. Run separately.
- **e2e-tests**: Full system with docker/devnet.
- **integration-tests**: Cardano node client tests.

### V. Nix-First

Everything builds via `flake.nix`. CI and local development use the exact same nix shell. No `pip install`, no `curl | sh`, no setup steps beyond cachix. Build gate job populates the shared nix store before downstream CI jobs run.

## Storage & Serialization

- RocksDB with column families, CBOR-encoded keys and values.
- Serialization codecs defined via `Prism' ByteString a` (lens prisms).
- Column families defined in a GADT (`Columns`) with type-level key/value pairing.
- Schema changes are breaking — existing databases must be wiped. Document in issue and PR.

## Upstream Dependencies

- `mts:rollbacks` and `mts:csmt` are generic libraries — this project makes concrete type choices.
- `chain-follower` provides the chain sync state machine.
- `ouroboros-network-api` provides `Point`, `SlotNo`, `WithOrigin` types.
- Dependency versions pinned by nix. `cabal.project` uses `--sha256:` comments in nix32 format.

## Development Workflow

- PRs required for all changes, never push to main directly.
- Linear git history via rebase merge.
- One worktree per issue, strict naming (`<repo>-issue-<N>`).
- CI must be green before merge. Build gate is the sole required status check.
- `just ci` locally before pushing.
- Format with `fourmolu`, lint with `hlint`, check with `cabal check`.

## Governance

Constitution reflects the project's established practices. Amendments are committed alongside the changes that motivate them.

**Version**: 1.0.0 | **Ratified**: 2026-03-27
