# tasks — #238

## Slice 1 — restore the full chain point on both endpoints
- [X] T238-S1 Add a shared `{slotNo, blockHash}` chain-point shape; restore `slotNo` to `MerkleRootEntry` (thread the already-available slot in `toMerkleRootEntry`); carry the full chain point in the inclusion-proof response (extract slot from the `Point` in `queryInclusionProof`); update ToJSON/FromJSON/ToSchema + Swagger; update the #236 ServerSpec consumer; green `./gate.sh`.
