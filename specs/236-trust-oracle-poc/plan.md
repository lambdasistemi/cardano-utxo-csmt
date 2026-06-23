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

### Slice 2 — consumer verifies the served proof with the generic mts:csmt-verify  (RE-AIMED)
Recon: serving already exists and is HTTP-tested (`GET /merkle-roots`,
`GET /proof/:txId/:txIx` → `InclusionProofResponse{proofBytes, proofBlockHash}`).
The real gap is consumer-side verification with the **shared** verifier: nothing
calls `mts:csmt-verify` (`verifyInclusionProof :: ByteString -> ByteString -> Bool`)
on the served root+proof; the repo doesn't even depend on it. Add an HTTP-boundary
test that fetches `/merkle-roots` + `/proof`, decodes, and verifies with
`mts:csmt-verify` → True for a present UTxO; absent → 404 discriminates. This is
"the oracle is usable", and it exercises the determinism-by-linkage from slice 1.

### Slice 3 — (optional/stretch) cryptographic absence over HTTP
Route the existing `queryExclusionProof` as an HTTP endpoint so an absent UTxO
returns a verifiable exclusion proof (not just 404). Defer if S2 suffices.

## Operator follow-up (not a code slice)
Deploy the server behind Traefik on our infra (e.g. `csmt-train.dev.plutimus.com`)
for the live self-served demo — `infrastructure` skill. Live-boundary smoke, not a
bisect gate.

## Order / parallelization
Strictly serial: S1 gates everything (a wrong pin makes S2/S3 meaningless). S2
before S3 (S3 consumes S2's endpoints).
