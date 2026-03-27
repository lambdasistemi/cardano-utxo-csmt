# Data Model: Replace WithOrigin with WithSentinel

## New Type

```haskell
data WithSentinel a = Sentinel | Value a
  deriving (Eq, Ord, Show)
```

`Ord` instance via deriving: `Sentinel < Value x` for all `x` (constructors ordered by declaration).

## Entity Changes

### RollbackPointKV (before)

```haskell
type RollbackPointKV slot hash key value =
    RollbackKV (WithOrigin slot) (Operation key value) (hash, Maybe hash)
```

### RollbackPointKV (after)

```haskell
type RollbackPointKV slot hash key value =
    RollbackKV (WithSentinel slot) (Operation key value) (hash, Maybe hash)
```

### Wire Format (unchanged)

```
Sentinel  →  CBOR uint 0
Value x   →  CBOR [1, x_bytes]
```

Identical to the old `withOriginPrism` encoding. Existing databases remain compatible.

## Mapping

| Before | After |
|--------|-------|
| `WithOrigin slot` | `WithSentinel slot` |
| `Origin` | `Sentinel` |
| `At x` | `Value x` |
| `withOriginPrism` | `withSentinelPrism` |
| `import Ouroboros.Network.Point (WithOrigin (..))` | `import ...RollbackPoint (WithSentinel (..))` |

## Affected Columns

Both `RollbackPoints` and `Rollbacks` in the `Columns` GADT.
