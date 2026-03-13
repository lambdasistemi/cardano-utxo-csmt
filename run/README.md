# Local Run Script

Script to run cardano-utxo against local Cardano nodes running in Docker.

## Prerequisites

Local nodes must be running and exposing their ports:

| Network | Docker Container | Port |
|---------|-----------------|------|
| Preview | cardano-node-preview | 3037 |
| Preprod | cardano-node-preprod | 3038 |
| Mainnet | mainnet-cardano-node-mainnet-1 | 3039 |

## Usage

```bash
./run/cardano-utxo.sh <network> [extra-args...]
```

Examples:
```bash
# Run against preview network
./run/cardano-utxo.sh preview

# Run against preprod network
./run/cardano-utxo.sh preprod

# Run against mainnet network
./run/cardano-utxo.sh mainnet
```

## Configuration

Environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `RUN_WITH` | Build tool (`cabal` or `nix`) | `cabal` (preview), `nix` (others) |
| `DB_PATH` | RocksDB database path | `/tmp/csmt-{network}` |
| `LOG_PATH` | Log file path | `/tmp/csmt-{network}.log` |
| `API_PORT` | HTTP API port | 8081/8083/8085 |
| `API_DOCS_PORT` | Swagger docs port | 8082/8084/8086 |
| `CLEAN` | Remove existing database before start | `false` |

Examples:
```bash
# Run with cabal (faster iteration during development)
RUN_WITH=cabal ./run/cardano-utxo.sh preview

# Run with nix (reproducible build)
RUN_WITH=nix ./run/cardano-utxo.sh preview

# Custom paths
DB_PATH=/data/csmt-preview API_PORT=9000 ./run/cardano-utxo.sh preview

# Clean start
CLEAN=true ./run/cardano-utxo.sh preview
```

## Additional Options

Pass extra flags after the network:

```bash
# Provide genesis files for initial bootstrap
./run/cardano-utxo.sh preview --genesis-file /path/to/shelley-genesis.json --byron-genesis-file /path/to/byron-genesis.json
```

## Endpoints

Once running:

- **Metrics**: `http://localhost:{API_PORT}/metrics`
- **Ready**: `http://localhost:{API_PORT}/ready`
- **Swagger**: `http://localhost:{API_DOCS_PORT}/`
