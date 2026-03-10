# Cardano UTxO CSMT

[![CI](https://github.com/lambdasistemi/cardano-utxo-csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/lambdasistemi/cardano-utxo-csmt/actions/workflows/CI.yaml)
[![Documentation](https://github.com/lambdasistemi/cardano-utxo-csmt/actions/workflows/deploy-docs.yaml/badge.svg)](https://github.com/lambdasistemi/cardano-utxo-csmt/actions/workflows/deploy-docs.yaml)

An HTTP service that maintains a Compact Sparse Merkle Tree (CSMT) over Cardano's UTxO set, enabling efficient cryptographic inclusion proofs.

## Features

- **Real-time Chain Sync**: Follows the blockchain via node-to-node protocol
- **Merkle Proofs**: Generate inclusion proofs for any UTxO
- **Multi-Era Support**: Byron through Conway
- **Rollback Handling**: Graceful chain reorganization support
- **REST API**: Simple HTTP interface with Swagger documentation

## Documentation

Full documentation available at **[lambdasistemi.github.io/cardano-utxo-csmt](https://lambdasistemi.github.io/cardano-utxo-csmt/)**

- [Getting Started](https://lambdasistemi.github.io/cardano-utxo-csmt/getting-started/)
- [Architecture](https://lambdasistemi.github.io/cardano-utxo-csmt/architecture/)
- [API Reference](https://lambdasistemi.github.io/cardano-utxo-csmt/swagger-ui/)

## Roadmap

See [Milestones](https://github.com/lambdasistemi/cardano-utxo-csmt/milestones) for planned releases and progress.

## License

Apache-2.0
