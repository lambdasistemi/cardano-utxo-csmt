#!/usr/bin/env bash
# Slice gate for PR #236. Present while the PR is in flight; dropped before
# mark-ready (chore: drop gate.sh). Proof = repo builds + unit/database tests
# pass on the aligned mts pin.
set -euo pipefail
cd "$(dirname "$0")"

echo "== build cardano-utxo =="
nix build --quiet .#cardano-utxo

echo "== unit + database tests =="
nix build --quiet .#unit-tests .#database-tests

echo "GATE GREEN ($(date -Is))"
