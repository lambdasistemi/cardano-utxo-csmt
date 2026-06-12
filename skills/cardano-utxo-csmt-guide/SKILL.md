---
name: cardano-utxo-csmt-guide
description: >-
  Orientation guide for the cardano-utxo-csmt repository — a Haskell HTTP
  service that maintains a Compact Sparse Merkle Tree (CSMT) over Cardano's
  UTxO set in RocksDB and serves inclusion proofs. Load this when working in
  cardano-utxo-csmt: building or testing it (just build, just unit, just
  database, nix run .#cardano-utxo), understanding chain sync (node-to-node
  ChainSync + BlockFetch, or node-to-client over a Unix socket via
  --socket-path), the CSMT engine, the RocksDB column families (kv, csmt,
  rollbacks, config, journal, metrics), genesis bootstrap (--genesis-file,
  --byron-genesis-file), the CLI options (--network, --db-path, --api-port,
  --api-docs-port, --sync-threshold, --headers-queue-size), or the REST API
  (/ready, /metrics, /metrics/prometheus, /merkle-roots, /proof, /await,
  /utxos-by-address). Also for questions about the cardano-utxo, db-query,
  and cardano-utxo-swagger executables, rollback handling, the Conway-era
  TxOut projection, or modules under Cardano.UTxOCSMT.*.
---

# cardano-utxo-csmt guide

A single Cabal package (`cardano-utxo-csmt`) building internal libraries
plus three executables. It follows the Cardano chain, tracks the UTxO
set, maintains a CSMT over it in RocksDB, and serves proofs over HTTP.

## Repository map

| Path | Purpose |
|------|---------|
| `lib/Cardano/UTxOCSMT/` | Core library (`library` component): chain sync, UTxO processing, CSMT-over-RocksDB, options, metrics |
| `lib/.../Application/ChainSync.hs`, `BlockFetch.hs`, `KeepAlive.hs` | Node-to-node protocol clients |
| `lib/.../Application/ChainSyncN2C.hs` | Node-to-client (full-block) chain sync |
| `lib/.../Application/Options.hs` | CLI/YAML option parser (`opt-env-conf`) |
| `lib/.../Application/Database/` | RocksDB backend, column codecs, rollback points, query implementation |
| `lib/.../Bootstrap/Genesis.hs` | Reads Shelley/Byron genesis (params + initial UTxOs) |
| `lib/.../Ouroboros/` | Ouroboros network connection, codecs, types |
| `http/Cardano/UTxOCSMT/HTTP/` | Servant API (`API.hs`), server (`Server.hs`), Swagger (`Swagger.hs`) |
| `application/Cardano/UTxOCSMT/Application/Run/` | Wiring: `Main.hs`, `Setup.hs` (genesis/checkpoint), `Config.hs` (DB schema), `Query.hs` (endpoint handlers) |
| `executables/chainsync/main.hs` | `cardano-utxo` service entry point (calls `Run.Main.main`) |
| `executables/db-query/main.hs` | `db-query` debug tool |
| `executables/swagger/main.hs` | `cardano-utxo-swagger` (prints OpenAPI JSON) |
| `test/`, `database-test/`, `integration-test/`, `e2e-test/` | hspec suites |
| `bench/` | criterion benchmarks |
| `config/<network>.yaml` | Default peer node per network |
| `run/cardano-utxo.sh` | Local run helper against Docker nodes |
| `docs/`, `mkdocs.yml` | MkDocs site (getting-started, architecture, database-schema, swagger) |

## Build, test, run

Inside `nix develop` (or with `cabal` directly):

```bash
just build            # cabal build all --enable-tests --enable-benchmarks
just unit             # unit-tests suite
just database         # database-tests (RocksDB-backed)
just format           # fourmolu + cabal-fmt + nixfmt
just hlint            # hlint
just CI               # full local gate (build, unit, fmt, hlint, swagger, diagrams)
just update-swagger   # regenerate docs/assets/swagger.json
just serve-docs       # mkdocs serve on a free port

nix run .#cardano-utxo -- --help          # service
nix run .#cardano-utxo-swagger            # print OpenAPI JSON
nix build .#docker-image                  # OCI image
```

Integration/E2E (`just integration`, `just e2e`, `just e2e-n2n`,
`just e2e-genesis`) need a `cardano-node` and Docker.

## Navigating the code

- **Entry point:** `executables/chainsync/main.hs` → `Run.Main.main`
  (`application/.../Run/Main.hs`). That module wires options, the tracer
  pipeline, RocksDB, genesis setup, the HTTP servers, and the chain
  follower; it dispatches on `ConnectionMode` to `application` (N2N) or
  `applicationN2C` (N2C).
- **Connection modes:** defined in `Application/Options.hs`
  (`ConnectionMode = N2N {..} | N2C {..}`). `--socket-path` selects N2C;
  `--node-name` + `--node-port` select N2N.
- **CSMT / RocksDB:** column families and codecs are in
  `Application/Database/Implementation/Columns.hs`; the physical CFs are
  opened in `Run/Config.hs` `withRocksDB` (kv, csmt, config, journal,
  metrics, rollbacks). Rollback encoding is in
  `Database/Implementation/RollbackPoint.hs`.
- **HTTP surface:** the Servant API type is in `http/.../HTTP/API.hs`;
  handlers and the synced-gate (HTTP 503) live in `http/.../HTTP/Server.hs`;
  the query implementations are in `application/.../Run/Query.hs`.
- **Genesis bootstrap:** `Run/Setup.hs` (`setupDB`) plus
  `Bootstrap/Genesis.hs`. Shelley genesis is always read for `k`, network
  magic, and epoch slots; Byron `nonAvvmBalances` are added when
  `--byron-genesis-file` is given.

## Using cardano-utxo-csmt

Run the service (node-to-client example, bootstrapping from genesis):

```bash
nix run .#cardano-utxo -- \
  --network preprod \
  --socket-path /path/to/node.socket \
  --genesis-file /path/to/shelley-genesis.json \
  --byron-genesis-file /path/to/byron-genesis.json \
  --db-path /tmp/csmt-db \
  --api-port 8080 --api-docs-port 8081
```

REST endpoints (all but `/ready` and the `/metrics*` pair return 503
until synced):

| Endpoint | Purpose |
|----------|---------|
| `GET /ready` | `{ready, tipSlot, processedSlot, slotsBehind}` |
| `GET /metrics` | sync metrics (JSON) |
| `GET /metrics/prometheus` | Prometheus exposition format |
| `GET /merkle-roots` | historical Merkle roots by block |
| `GET /proof/:txId/:txIx` | inclusion proof for a UTxO |
| `GET /utxos-by-address/:address` | UTxOs at an address |
| `GET /await/:txId/:txIx?timeout=N` | wait for a UTxO (HTTP 408 on timeout) |

Swagger UI is served on `--api-docs-port` at `/api-docs/swagger-ui`.
Inspect the database directly with
`db-query --db-path <dir> --txid <64-hex> [--index N]`.

## Answering questions

- "What is this / what does it do?" → README **What is this**;
  `docs/index.md`.
- "How do I install / run it?" → README **Install** / **Quickstart**;
  `docs/getting-started.md`; `run/README.md` for the local helper.
- "What CLI flags exist?" → README **Usage** and
  `docs/getting-started.md`; ground truth is
  `lib/Cardano/UTxOCSMT/Application/Options.hs`.
- "What REST endpoints exist?" → README **Usage**;
  `docs/architecture.md` (HTTP API); ground truth is
  `http/Cardano/UTxOCSMT/HTTP/API.hs` and the generated
  `docs/assets/swagger.json`.
- "How is data stored?" → `docs/database-schema.md` (CDDL); ground truth
  is `Database/Implementation/Columns.hs` and `Run/Config.hs`.
- "How does sync / rollback / bootstrap work?" →
  `docs/architecture.md`; ground truth is `Run/Main.hs`, `Run/Setup.hs`,
  and the `Application/ChainSync*` modules.
- Always verify a claim against the cited source file before stating it;
  the docs aim to track the code but the code is authoritative.
