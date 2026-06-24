# plan — #238

## Tech stack
Haskell (haskell.nix), `cardano-utxo-csmt` http + application packages. Gate:
`nix build .#cardano-utxo .#unit-tests .#database-tests`.

## Slice (one bisect-safe commit)

### Slice 1 — restore the full chain point on both endpoints
- Introduce a shared chain-point representation `{slotNo, blockHash}` (a `ChainPoint`
  type, or restore `slotNo` symmetrically on both response types — driver's call,
  but one shared shape).
- `MerkleRootEntry`: add `slotNo :: SlotNo` back (c784375 had exactly this); thread
  the already-available `slot` in `toMerkleRootEntry`. Update ToJSON/FromJSON/ToSchema.
- Inclusion-proof response: carry the full chain point `{slotNo, blockHash}` instead
  of the bare `proofBlockHash`; `queryInclusionProof` already has the full `Point`
  (`pt`) — extract the slot too. Update ToJSON/FromJSON/ToSchema.
- Update the `ServerSpec`/database-tests consumer (the #236 verify path) to the new
  shapes; it may now match the root by `(slotNo, blockHash)`.
- Update Swagger (`just update-swagger` if the CI swagger-drift check requires it).

Single slice: the wire-type change + its (de)serialization + the test consumer are
one coherent, bisect-safe unit.

## Operator follow-up (not a code slice)
Re-roll `utxo-csmt.plutimus.com` to the merged image (breaking wire change).
