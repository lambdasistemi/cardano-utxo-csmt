# Data Model: Push-based UTxO Await

## New Entities

### CommitNotification

A `TVar Int` counter incremented after each successful `processBlock` commit.

- Created at application startup (Main.hs)
- Written by: Application chain sync loop (after each commit)
- Read by: Query.awaitValue (STM retry on counter change)

### awaitValue (Query field)

```
awaitValue :: key -> Maybe Int -> m (Maybe value)
```

- `key`: the UTxO key to wait for
- `Maybe Int`: optional timeout in seconds (Nothing = server default)
- Returns `Just value` if found, `Nothing` on timeout

### HTTP Endpoint

```
GET /await/:txId/:txIx?timeout=30
```

- Path parameters: transaction ID and index (same format as /proof/:txId/:txIx)
- Query parameter: timeout in seconds (default: 30, max: 120)
- Response 200: value found (same format as existing UTxO responses)
- Response 408: timeout expired

## State Flow

```mermaid
sequenceDiagram
    participant Client
    participant HTTP as HTTP Handler
    participant Await as awaitValue
    participant Store as RocksDB
    participant App as Application
    participant TVar as TVar Int

    Client->>HTTP: GET /await/txId/txIx timeout=30
    HTTP->>Await: awaitValue(key, Just 30)
    Await->>Store: getValue(key)
    alt Key exists (fast path)
        Store-->>Await: Just value
        Await-->>HTTP: Just value
        HTTP-->>Client: 200 OK
    else Key not found
        Store-->>Await: Nothing
        loop STM retry until timeout
            Await->>TVar: readTVar (blocks via STM retry)
            App->>Store: processBlock commit
            App->>TVar: modifyTVar (+1)
            TVar-->>Await: counter changed, wake up
            Await->>Store: getValue(key)
            alt Found
                Store-->>Await: Just value
                Await-->>HTTP: Just value
                HTTP-->>Client: 200 OK
            else Still not found
                Store-->>Await: Nothing
            end
        end
        Await-->>HTTP: Nothing (timeout)
        HTTP-->>Client: 408 Timeout
    end
```
