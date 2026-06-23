# tasks — #236

## Slice 1 — align the mts pin
- [ ] T236-S1 Bump cabal.project haskell-mts to `ab15f7b2` (+ sha256 `081wz3vq8d15wh1mziqnqhk2ai4i304gv7vxrhw9hqjr2sa9xy1a`); adapt the two completeness call sites (`Query.hs`, `Transaction.hs`) to the new CompletenessProof API; green `./gate.sh` (build + unit + database tests).

## Slice 2 — serve root + proof by chainpoint
- [ ] T236-S2 HTTP endpoints: per-chainpoint address-prefixed UTxO-CSMT root + on-demand inclusion/exclusion proof for a key; tests.

## Slice 3 — usable demo / cross-check
- [ ] T236-S3 Consumer path verifies fetched root+proof via `mts:csmt-verify`: pass for present UTxO, reject for absent; demo targets the MPFS token UTxO.
