# Getting Started

This guide covers installation, configuration and usage of the Cardano UTxO CSMT service.

## Prerequisites

- A trusted running Cardano node, reachable either over node-to-node TCP
  (`--node-name` + `--node-port`, e.g. mainnet relay port 3001) or over
  its local node-to-client Unix socket (`--socket-path`)
- The Shelley genesis file for your network (always required); the Byron
  genesis file as well if you want to bootstrap from Origin

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

On a fresh database this will:

1. Read initial UTxOs from the Shelley genesis file (and the Byron
   genesis file's `nonAvvmBalances` when `--byron-genesis-file` is given)
2. Insert them into the CSMT database
3. Start chain sync from Origin

!!! note
    `--genesis-file` (Shelley) is **always required** — the service reads
    the security parameter `k`, network magic, and epoch slots from it and
    will not start without it. When syncing from Origin, also pass
    `--byron-genesis-file`; otherwise the first block that spends a Byron
    genesis UTxO will fail because that UTxO is not in the database.

The genesis files are part of the node configuration. For a local NixOS node
they are typically found alongside the node config (e.g.
`/path/to/node/configs/shelley-genesis.json`).

## Configuration Options

| Option | Description |
|--------|-------------|
| `--network`, `-n` | Network: `mainnet`, `preprod`, `preview`, `devnet` (default: mainnet) — selects the default peer node |
| `--node-name`, `-s` | Peer node hostname (node-to-node mode) |
| `--node-port`, `-p` | Peer node port (node-to-node mode) |
| `--socket-path` | Node-to-client Unix socket path |
| `--db-path`, `-d` | RocksDB database path (required) |
| `--genesis-file` | Path to `shelley-genesis.json` (required) |
| `--byron-genesis-file` | Path to `byron-genesis.json` for genesis bootstrap (recommended) |
| `--api-port` | HTTP API port for REST endpoints |
| `--api-docs-port` | HTTP port for the Swagger UI documentation server |
| `--config-file`, `-c` | YAML configuration file (`conf` options may be set here) |
| `--log-path`, `-l` | Log file path (logs to stdout if omitted) |
| `--headers-queue-size`, `-q` | Header queue size (default: 10) |
| `--sync-threshold` | Max slots behind tip to be considered synced (default: 100) |
| `--enable-metrics-reporting` | Emit metrics on stdout |

Provide exactly one connection mode: either `--socket-path` (node-to-client)
or `--node-name` + `--node-port` (node-to-node).

## Verifying the Service

Once running, check the service status:

```bash
# Check readiness
curl http://localhost:8080/ready

# Check metrics
curl http://localhost:8080/metrics
```

The Swagger UI is served by a separate documentation server on
`--api-docs-port`. Start the service with, for example,
`--api-docs-port 8081`, then open
`http://localhost:8081/api-docs/swagger-ui`.

## Next Steps

- For fast bootstrap via Mithril snapshots, see [cardano-mithril-client](https://github.com/lambdasistemi/cardano-mithril-client)
- [API Documentation](swagger-ui.md) - Explore the REST API
- [Architecture](architecture.md) - Understand how it works
