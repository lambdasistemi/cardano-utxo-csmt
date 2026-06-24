#!/usr/bin/env bash
# Script to record a demo
# Ensures nix is built before recording to avoid build output in demo
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "=== Pre-building with nix ==="
nix build --quiet .#cardano-utxo
echo "Build complete."
