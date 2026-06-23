# spec — #236 PoC: address-prefixed UTxO-CSMT as a second trust oracle for moog

## P1 user story

As a light client, I fetch the address-prefixed UTxO-CSMT root for a chainpoint
from the self-served `cardano-utxo` train and verify a membership proof for a
chosen UTxO against it with `mts:csmt-verify` — passing for a present UTxO and
discriminating against an absent one — proving the second trust oracle is usable
without a Cardano node and without trusting MPFS's offchain service.

## Context

- moog's first trust oracle is MPFS (a CSMT over moog *facts*, anchored on-chain).
- The second oracle is `cardano-utxo-csmt`: a Moore machine folding the **UTxO set**
  into a CSMT. Different tries over different data — roots never match; the value
  is independent attestation, not root equality.
- Each snapshot carries exactly one root per chainpoint: the **address-prefixed
  UTxO-CSMT root**. The moog payoff is derived consumer-side: prove "the UTxO at
  the MPFS script address is in the set at chainpoint X".
- The verifier already exists and is generic: `mts:csmt-verify`.

## Functional requirements

- FR1 — Publisher and verifier compute over **one aligned `mts` rev** (identical
  address-prefix key derivation + completeness-proof encoding/order).
- FR2 — `cardano-utxo` serves, over its HTTP API, the per-chainpoint **root** and
  an on-demand **inclusion/exclusion proof** for a given address-prefixed key.
- FR3 — A consumer verifies an inclusion proof for a present UTxO against the
  served root via `mts:csmt-verify` → pass.
- FR4 — An absent UTxO → exclusion verifies (or inclusion fails) → discriminates.

## Success criteria

- Repo builds + unit/database tests pass on the aligned `mts` rev.
- The root+proof endpoints answer by chainpoint.
- A round-trip (fetch root, fetch proof, verify with `mts:csmt-verify`) passes for
  a present UTxO and is rejected for an absent one.

## Non-goals (PoC)

- Snapshot signing / key distribution (self-served host is trusted for the PoC).
- R2 / external host / mirroring / lifecycle retention.
- On-chain anchor / equivocation guard.
- Any comparison to MPFS's facts root (different trie; not in snapshots).
