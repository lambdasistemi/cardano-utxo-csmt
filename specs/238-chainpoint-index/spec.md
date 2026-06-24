# spec — #238 key roots/proofs by full chain point (slot, blockHash)

## P1 user story

As a second-oracle consumer, I read `/merkle-roots` and an inclusion-proof response
and get a full chain point `{slotNo, blockHash}` for each root, so I can index and
order the 2160-root train by slot and resolve "the root as of slot X" without
resolving hash→slot out of band.

## Context

`66ad3f1` moved root/proof addressing toward a chain point but represented it as a
**bare block hash**, dropping `slotNo` from `MerkleRootEntry` and the inclusion-proof
response. A Cardano chain point is `(slot, headerHash)`. The slot is already produced
at both sites and discarded:
- `queryMerkleRoots`/`toMerkleRootEntry (slot, blockHash, merkleRoot)` drops `slot`.
- `queryInclusionProof` has the full `Point` (`pt`) but keeps only `slotHash pt`.

This is additive plumbing, not new computation.

## Functional requirements

- FR1 — `MerkleRootEntry` carries `slotNo` again (with `blockHash`, `merkleRoot`).
- FR2 — the inclusion-proof response carries the full chain point `{slotNo, blockHash}`.
- FR3 — a single shared chain-point representation is used by both endpoints.
- FR4 — JSON (de)serialization + Swagger schema match the new shapes.

## Success criteria

- `GET /merkle-roots` entries include `slotNo`.
- `GET /proof/:txId/:txIx` carries `{slotNo, blockHash}` (a chain point), not a bare hash.
- `database-tests` (incl. the #236 ServerSpec consumer-verify path) green.

## Non-goals

- No change to proof bytes / CSMT proof semantics — response **envelope** only.
- Snapshot signing, HTTP exclusion proofs, on-chain anchor (separate follow-ups).
