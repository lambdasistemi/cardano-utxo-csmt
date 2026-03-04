# Database Schema

The application stores data in RocksDB using four column families.
All structured values are CBOR-encoded. The schema below is specified
in [CDDL](https://www.rfc-editor.org/rfc/rfc8610) (RFC 8610).

## Column Families

| Column Family | Key | Value |
|---------------|-----|-------|
| `kv` | raw bytes (UTxO reference) | raw bytes (CBOR-encoded TxOut) |
| `csmt` | [Key](#key) | [Indirect](#indirect) |
| `rollbacks` | [WithOrigin](#withorigin) | [RollbackPoint](#rollbackpoint) |
| `config` | UTF-8 literal `"app_config"` | Serialise-encoded tuple |

## CDDL

```cddl
; ============================================================
; Column family: csmt
; ============================================================

; Navigation path in the Compact Sparse Merkle Tree.
; Each element is a direction: 0 = Left, 1 = Right.
Key = [* Direction]
Direction = 0 / 1

; Tree node referencing a subtree by path and hash.
Indirect = [
    jump: Key,          ; path to the referenced node
    value: bstr          ; hash digest (raw bytes)
]

; ============================================================
; Column family: rollbacks
; ============================================================

; Rollback point key — Origin or a concrete slot.
WithOrigin = Origin / AtSlot
Origin  = 0              ; bare uint, not wrapped in array
AtSlot  = [1, bstr]      ; tag 1 + slot encoded via slot prism

; Rollback point value — captures inverse operations for undo.
RollbackPoint = [
    hash: bstr,          ; block hash at this point
    operations: [* Operation],
    merkle-root: MaybeHash
]

Operation = DeleteOp / InsertOp
DeleteOp  = [0, bstr]       ; tag 0 + key bytes
InsertOp  = [1, bstr, bstr] ; tag 1 + key bytes + value bytes

MaybeHash = [] / [bstr]     ; empty = Nothing, one element = Just

; ============================================================
; Column family: config
; ============================================================

; Chain synchronization point.
Point = PointOrigin / PointBlock
PointOrigin = [0]                ; origin sentinel
PointBlock  = [1, uint, bstr]    ; tag 1 + slot number + block hash
```

## Notes

- **kv** column: keys and values pass through as raw lazy bytestrings
  (identity prism). The values are CBOR-encoded Cardano TxOuts but
  their internal structure is opaque to this application.
- **Hash**: 32-byte Blake2b-256 digest, stored as raw `bstr`.
- **Slot prism**: the `bstr` in `WithOrigin.AtSlot` is the slot
  encoded through the same prism used by the application — in
  practice a CBOR-encoded `Point` (see `Point` definition above).
- **BREAKING**: this schema replaces the previous cereal-based
  encoding. Existing databases are incompatible and must be re-synced.
