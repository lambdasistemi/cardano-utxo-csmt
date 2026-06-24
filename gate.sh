#!/usr/bin/env bash
# Slice gate for PR (#238). Present while in flight; dropped before mark-ready.
set -euo pipefail
cd "$(dirname "$0")"
echo "== build cardano-utxo =="
nix build --quiet .#cardano-utxo
echo "== unit + database tests =="
nix build --quiet .#unit-tests .#database-tests
echo "GATE GREEN ($(date -Is))"
