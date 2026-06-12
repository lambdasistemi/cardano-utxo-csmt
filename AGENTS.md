# Repository Agent Guide

## What this repo is

`cardano-utxo-csmt` is a Haskell HTTP service that follows the Cardano
chain, maintains a Compact Sparse Merkle Tree (CSMT) over the live UTxO
set in a local RocksDB database, and serves cryptographic inclusion
proofs and historical Merkle roots over a REST API. It connects to a node
either node-to-node (TCP: ChainSync headers + BlockFetch) or
node-to-client (Unix socket: ChainSync full blocks). The repo is a single
Cabal package with internal libraries (`library`, `http`, `application`)
and three executables (`cardano-utxo`, `db-query`, `cardano-utxo-swagger`).

## How to work here

The project uses Nix and a `justfile`. Enter the dev shell with
`nix develop`, then use `just`:

- Build: `just build` (`cabal build all --enable-tests --enable-benchmarks`)
- Unit tests: `just unit`
- Database tests: `just database`
- Format: `just format` (fourmolu + cabal-fmt + nixfmt)
- Lint: `just hlint`
- Full local CI gate: `just CI`
- Regenerate the OpenAPI doc: `just update-swagger` (writes `docs/assets/swagger.json`)
- Serve docs locally: `just serve-docs`

Integration / end-to-end suites (`just integration`, `just e2e`,
`just e2e-n2n`, `just e2e-genesis`) need a `cardano-node` and Docker.
Nix flake outputs: `.#cardano-utxo`, `.#cardano-utxo-swagger`,
`.#unit-tests`, `.#database-tests`, `.#bench`, `.#e2e-tests`,
`.#docker-image`. Scope conventions: Haskell sources are under `lib/`,
`http/`, `application/`, and `executables/`; this is **not** a `src/`
layout.

Run the service locally against Docker-hosted nodes with
`run/cardano-utxo.sh <preview|preprod|mainnet>`.

## Skills

Activatable procedures live under `skills/`. Load the one whose
description matches your task:

- `skills/cardano-utxo-csmt-guide/` — orient in this repository: the code
  map, exact build/test/run commands, where chain-sync / CSMT / RocksDB /
  HTTP logic lives, the CLI options and REST endpoints as implemented,
  and where the answers to common user questions are documented.
