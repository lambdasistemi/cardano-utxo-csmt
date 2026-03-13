#!/usr/bin/env bash
set -euo pipefail

# Run cardano-utxo against a local Cardano node
# Usage: ./run/cardano-utxo.sh <network> [extra-args...]
#   network: preview, preprod, or mainnet

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Function to wait for sync completion
# Usage: API_PORT=8081 wait_for_sync
wait_for_sync() {
    local api_url="http://localhost:${API_PORT:-8081}/ready"
    echo "Waiting for sync to complete..."
    while true; do
        response=$(curl -s "$api_url" 2>/dev/null || echo '{"ready":false}')
        ready=$(echo "$response" | jq -r '.ready // false')
        if [ "$ready" = "true" ]; then
            echo "Service is synced!"
            break
        fi
        slots_behind=$(echo "$response" | jq -r '.slotsBehind // "unknown"')
        echo "Still syncing... slots behind: $slots_behind"
        sleep 10
    done
}

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <network> [extra-args...]"
    echo "  network: preview, preprod, or mainnet"
    exit 1
fi

NETWORK="$1"
shift

case "$NETWORK" in
    preview)
        NODE_PORT=3037
        DEFAULT_API_PORT=8081
        DEFAULT_DOCS_PORT=8082
        DEFAULT_RUNNER=cabal
        ;;
    preprod)
        NODE_PORT=3038
        DEFAULT_API_PORT=8083
        DEFAULT_DOCS_PORT=8084
        DEFAULT_RUNNER=nix
        ;;
    mainnet)
        NODE_PORT=3039
        DEFAULT_API_PORT=8085
        DEFAULT_DOCS_PORT=8086
        DEFAULT_RUNNER=nix
        ;;
    *)
        echo "Unknown network: $NETWORK (use preview, preprod, or mainnet)"
        exit 1
        ;;
esac

DB_PATH=${DB_PATH:-/tmp/csmt-$NETWORK}
LOG_PATH=${LOG_PATH:-/tmp/csmt-$NETWORK.log}
API_PORT=${API_PORT:-$DEFAULT_API_PORT}
API_DOCS_PORT=${API_DOCS_PORT:-$DEFAULT_DOCS_PORT}
CLEAN=${CLEAN:-false}
RUN_WITH=${RUN_WITH:-$DEFAULT_RUNNER}
CONFIG_FILE=${CONFIG_FILE:-$SCRIPT_DIR/config/$NETWORK.yaml}

if [[ "$CLEAN" == "true" ]]; then
    echo "Removing existing database: $DB_PATH"
    rm -rf "$DB_PATH"
fi

mkdir -p "$DB_PATH"

echo "Starting cardano-utxo on $NETWORK network..."
echo "  Runner: $RUN_WITH"
echo "  Config: $CONFIG_FILE"
echo "  Node: localhost:$NODE_PORT"
echo "  Database: $DB_PATH"
echo "  Log: $LOG_PATH"
echo "  API: http://localhost:$API_PORT"
echo "  Docs: http://localhost:$API_DOCS_PORT"

args=(
    --config-file "$CONFIG_FILE"
    --node-name localhost
    --node-port "$NODE_PORT"
    --db-path "$DB_PATH"
    --log-path "$LOG_PATH"
    --api-port "$API_PORT"
    --api-docs-port "$API_DOCS_PORT"
    --enable-metrics-reporting
    "$@"
)

case "$RUN_WITH" in
    cabal)
        exec cabal run cardano-utxo -- "${args[@]}"
        ;;
    nix)
        exec nix run .#cardano-utxo -- "${args[@]}"
        ;;
    *)
        echo "Unknown RUN_WITH value: $RUN_WITH (use 'cabal' or 'nix')"
        exit 1
        ;;
esac
