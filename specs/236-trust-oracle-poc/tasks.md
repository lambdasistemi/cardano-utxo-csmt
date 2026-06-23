# tasks — #236

## Slice 1 — align the mts pin
- [X] T236-S1 Bump cabal.project haskell-mts to `ab15f7b2` (+ sha256 `081wz3vq8d15wh1mziqnqhk2ai4i304gv7vxrhw9hqjr2sa9xy1a`); adapt the two completeness call sites (`Query.hs`, `Transaction.hs`) to the new CompletenessProof API; green `./gate.sh` (build + unit + database tests).

## Slice 2 — consumer verifies the served proof with the generic mts:csmt-verify  (RE-AIMED)
Recon finding: serving already exists (`GET /merkle-roots`, `GET /proof/:txId/:txIx`,
both HTTP-tested). The genuine gap is that nothing verifies a served proof against a
served root with the **generic shared `mts:csmt-verify`** — the repo doesn't even
depend on it. That consumer-side verification IS "the oracle is usable".
- [X] T236-S2 Add a consumer that depends on `mts:csmt-verify`; over the HTTP boundary (reuse the `ServerSpec`/Wai harness) fetch `/merkle-roots` + `/proof/:txId/:txIx`, decode the root + proof, and verify the inclusion proof with `mts:csmt-verify` (`CSMT.Verify.verifyCompletenessProof`) → PASS for a present UTxO; an absent UTxO → 404 discriminates. Green `./gate.sh`.

## Slice 3 — (optional/stretch) cryptographic absence over HTTP
- [ ] T236-S3 Route the existing `queryExclusionProof` as an HTTP endpoint so an absent UTxO returns a verifiable exclusion proof (not just 404), verifiable by the same consumer. Defer if S2 proves usability sufficiently for the PoC.

## Operator follow-up (not a code slice)
- Deploy the server behind Traefik on our infra for the live self-served demo.
