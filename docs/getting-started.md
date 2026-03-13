# Getting Started

This guide covers installation, configuration and usage of the Cardano UTxO CSMT service.

## Prerequisites

- A trusted running Cardano node with secure node-to-node protocol access
- Network connectivity to the node's port (default: 3001)
- Shelley and Byron genesis files for your network

## Installation

### Nix

```bash
# Setup caching (recommended)
nix shell nixpkgs#cachix -c cachix use paolino

# Run on preprod with genesis bootstrap
nix run github:lambdasistemi/cardano-utxo-csmt -- \
  --network preprod \
  --socket-path /path/to/node.socket \
  --genesis-file /path/to/shelley-genesis.json \
  --byron-genesis-file /path/to/byron-genesis.json \
  --db-path /tmp/csmt-db \
  --api-port 8080
```

This will:

1. Read initial UTxOs from both Byron and Shelley genesis files
2. Insert them into the CSMT database
3. Start chain sync from Origin

!!! warning
    Starting without `--genesis-file` and `--byron-genesis-file` will crash
    on the first block that spends a genesis UTxO, because the UTxO won't
    exist in the database yet. Always provide genesis files when syncing
    from Origin.

The genesis files are part of the node configuration. For a local NixOS node
they are typically found alongside the node config (e.g.
`/path/to/node/configs/shelley-genesis.json`).

## Configuration Options

| Option | Description |
|--------|-------------|
| `--network` | Network: `mainnet`, `preprod`, `preview` (default: mainnet) |
| `--node-name` | Override peer node hostname |
| `--node-port` | Override peer node port |
| `--socket-path` | Node-to-Client Unix socket path |
| `--db-path` | RocksDB database path (required) |
| `--api-port` | HTTP API port for REST endpoints |
| `--api-docs-port` | HTTP port for Swagger UI documentation |
| `--genesis-file` | Path to shelley-genesis.json for genesis bootstrap (required) |
| `--byron-genesis-file` | Path to byron-genesis.json for genesis bootstrap (recommended) |

## Verifying the Service

Once running, check the service status:

```bash
# Check readiness
curl http://localhost:8080/ready

# Check metrics
curl http://localhost:8080/metrics

# View API documentation
open http://localhost:8080/api-docs/swagger-ui
```

## Next Steps

- For fast bootstrap via Mithril snapshots, see [cardano-mithril-client](https://github.com/lambdasistemi/cardano-mithril-client)
- [API Documentation](swagger-ui.md) - Explore the REST API
- [Architecture](architecture.md) - Understand how it works
