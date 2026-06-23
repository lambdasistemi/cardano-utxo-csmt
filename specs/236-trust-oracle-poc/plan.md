# plan — #236

## Tech stack

Haskell (ghc9123), haskell.nix flake. `cardano-utxo-csmt` packages: `library`,
`http`, `application`, exe `cardano-utxo`. CSMT core via `mts` (lambdasistemi/haskell-mts)
as a cabal.project source-repository-package. Verifier: `mts:csmt-verify`.
Gate: `nix build .#cardano-utxo .#unit-tests .#database-tests`.

## Slices (one bisect-safe commit each)

### Slice 1 — align the mts pin (serial gate)
Bump `cabal.project` haskell-mts `9a510679` → `ab15f7b2` (the rev
`cardano-mpfs-offchain` already uses; reuse its proven sha256
`081wz3vq8d15wh1mziqnqhk2ai4i304gv7vxrhw9hqjr2sa9xy1a`). Adapt the two call sites
of the refactored completeness API — `lib/.../Database/Implementation/Query.hs`
and `Transaction.hs` — to the new `CompletenessProof` (Witness/Empty split, moved
to csmt-core, `CompletenessEmpty` for absent prefixes, subtree-to-root inclusion
order). Green build + unit/database tests. This is the determinism-by-linkage gate:
publisher now derives proofs with the same `mts` the verifier checks with.

### Slice 2 — serve root + proof by chainpoint
Extend the `http` API: `GET` the address-prefixed UTxO-CSMT **root** for a
chainpoint, and an on-demand **inclusion/exclusion proof** for a given key at a
chainpoint. (Uses the existing trie + DB; no new trie.)

### Slice 3 — usable demo / cross-check
A consumer path (integration test or small `verify` entrypoint) that fetches
root(X) + proof and verifies via `mts:csmt-verify`: pass for a present UTxO,
reject for an absent one. Demo tie-in: target the MPFS token UTxO at its script
address.

## Operator follow-up (not a code slice)
Deploy the server behind Traefik on our infra (e.g. `csmt-train.dev.plutimus.com`)
for the live self-served demo — `infrastructure` skill. Live-boundary smoke, not a
bisect gate.

## Order / parallelization
Strictly serial: S1 gates everything (a wrong pin makes S2/S3 meaningless). S2
before S3 (S3 consumes S2's endpoints).
