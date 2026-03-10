# Cardano UTxO CSMT

A service that maintains a Compact Sparse Merkle Tree over Cardano's UTxO set, enabling efficient inclusion proofs.

## Features

- **Real-time Sync**: Follows the Cardano blockchain via node-to-node protocol
- **Merkle Proofs**: Generate cryptographic inclusion proofs for any UTxO
- **Rollback Handling**: Gracefully handles chain reorganizations
- **REST API**: Simple HTTP interface for queries and proofs

## Presentation

<div style="position: relative; width: 100%; padding-bottom: 58%; margin-bottom: 1em;">
<iframe src="presentation/index.html" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: none; border-radius: 8px;"></iframe>
</div>

**Controls:** Arrow keys / Space (navigate) | F (fullscreen) | Esc (overview)

[Open fullscreen presentation](presentation/index.html){target="_blank"}

## How It Works

The service connects to a Cardano node and maintains:

1. **UTxO Set**: Current unspent transaction outputs
2. **CSMT**: A Merkle tree with path compression for efficient proofs
3. **Rollback History**: State snapshots for handling chain reorgs

Each block updates the tree, producing a new Merkle root that commits to the entire UTxO set state.

## Use Cases

- **Light Clients**: Verify UTxO existence without downloading the full chain
- **Bridges**: Prove UTxO state to external systems
- **Auditing**: Cryptographic proof of UTxO set at any point

## Project Status

- [x] Chain synchronization via node-to-node protocol
- [x] UTxO set tracking with RocksDB persistence
- [x] CSMT maintenance with Merkle root computation
- [x] HTTP API with Swagger documentation
- [x] Merkle roots endpoint
- [x] Inclusion proofs endpoint
- [ ] Bootstrap without mithril
- [ ] Exclusion proofs
- [ ] Address completeness proofs

## Links

- [GitHub Repository](https://github.com/lambdasistemi/cardano-utxo-csmt)
- [API Documentation](swagger-ui.md)
- [Architecture Overview](architecture.md)
