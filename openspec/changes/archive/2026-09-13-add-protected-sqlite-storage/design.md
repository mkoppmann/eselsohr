# Design

## Context

### Legacy persistence evidence

[File.hs](../../../../src/Lib/Infra/Persistence/File.hs) stores each collection as `.json.gz`, using Aeson, gzip, and a durable atomic whole-file write.
Loading materializes the collection, malformed data can reach `error`, and the module exposes arbitrary collection lookup through the shared data path.

[Queue.hs](../../../../src/Lib/Infra/Persistence/Queue.hs) uses a `TQueue` of commands and per-collection worker queues.
`commit` waits on a `TMVar` that `storeUpdate` fills only after the action succeeds, so exceptions have no corresponding failure result in that handshake.
The [persistence server](../../../../src/Lib/Infra/Persistence/Server.hs) also runs expiration cleanup.
These workers solve real whole-file update coordination, but do not provide cross-process locking or authorization-and-write atomicity.
SQLite transactions can replace their write-serialization purpose; a connection registry or shutdown counter could later need synchronization, but that is no reason to carry forward the queue or require STM.

The fake [article](../../../../test/Test/Infra/Repo/ArticleList.hs), [capability](../../../../test/Test/Infra/Repo/CapabilityList.hs), and [collection](../../../../test/Test/Infra/Repo/Collection.hs) repositories share one `IORef CollectionPm` and ignore collection IDs in key operations, so they cannot prove collection isolation.
ID-only equality also weakens naive persistence round-trip assertions.

## Goals / Non-Goals

**Goals:**

- One trustworthy path from a presented bearer to an authorized read or write of exactly the checked request.
- Storage representations and limits owned by the adapter, not the domain model.

**Non-Goals:**

- Durability guarantees beyond what the tests show; crash and power-loss behavior and secure erasure remain unverified.

## Decisions

### D3: exact-payload sealing behind named operations

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Authorization gate | Named operations with private checks; generic sealed actions; operation-specific sealed requests behind named operations | D3 selects the last: store the complete inputs in a private request that executes only in the transaction that checked them. Keeping constructors private and preventing execution in another transaction add implementation and verification work. |

Authorization checks the presented bearer, collection, and target before creating the request, and the request's type restricts execution to that transaction.
The persistence helper requires the request; pure domain transformations require no authorization argument.
The seal records the exact submitted revision but does not prove that it matches storage; the helper compares it after authorization.
There is no generic authorized-action framework, dispatcher, or type-level registry mapping operations to required permissions; plain types state the same fact readably.
Narrow operation families are useful when they remove real authority; proliferating effects per CRUD operation is not.
The legacy permission-specific witnesses are adapted rather than kept: the seals bind the collection, bearer, current state, and transaction that the witnesses lacked.

```haskell
readArticles :: Connection -> Bearer -> Scope -> IO (Either StoreError [StoredArticle])
runReadTransaction :: Connection -> Bearer -> IO UTCTime
                   -> (forall tx. ReadTx tx a) -> IO (Either StoreError a)
checkRead :: Scope -> ReadTx tx (ReadRequest tx)
selectArticles :: ReadRequest tx -> ReadTx tx [StoredArticle]

changeTitle :: Connection -> Bearer -> ArticleId -> Title -> Integer
            -> IO (Either StoreError StoredArticle)
runWriteTransaction :: Connection -> Bearer -> IO UTCTime
                    -> (forall tx. WriteTx tx a) -> IO (Either StoreError a)
checkTitleChange :: ArticleId -> Title -> Integer
                 -> WriteTx tx (TitleChangeRequest tx)
writeTitle :: TitleChangeRequest tx -> WriteTx tx StoredArticle
```

The named operations supply `getCurrentTime` and immediately compose the check with the persistence step.
Each runner fixes the connection, bearer, and trusted clock and introduces a type parameter `tx` that identifies the transaction.
`ReadTx` and `WriteTx` are distinct mode aliases of a private `Tx` type whose mode and transaction parameters have nominal roles, so `coerce` cannot change a read action into a write action or substitute another transaction.
They expose only Functor, Applicative, and Monad instances.
Request constructors are private to Articles; transaction constructors are private to Internal.Transaction, whose context access and IO lifting are available only within the adapter library.
Internal.Authority loads and validates stored grants for both runners.
The `SQLite.Internal` modules are hidden Cabal modules; the component's exposed modules form its interface for sibling components.

### D4: bundled SQLite

Use the SQLite engine bundled with `direct-sqlite`, with `systemlib` disabled, so deployment needs no separate SQLite library.
Verify the embedded engine's version when updating the dependency and rebuild to deliver SQLite fixes.
Cabal specification 3.0 enables private component visibility, keeping SQLite out of the public domain library.

### Storage boundary and SQL layer

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Storage boundary | One file per collection; one shared SQLite database | Prefer per-collection files to limit each connection to one collection and allow independent export. Accept extra open/migration/backup coordination; not OS isolation. |
| Central index | No index; non-secret catalogue; global grant/secret store | No index now. Operator enumeration suffices; no cross-collection product query earns a central authority. |
| SQL layer | sqlite-simple/explicit SQL; Beam; direct-sqlite throughout | Prefer sqlite-simple plus a small low-level opening adapter. Beam buys typed query composition but not authorization or migration correctness; the small fixed query set does not yet earn it. |
| Concurrency | Existing STM workers; SQLite transactions; pool/registry synchronization | Replace persistence workers with transactions. Add synchronization only for a named resource-lifetime invariant. |

```text
data/
  collections/
    <collection-id>.sqlite
```

Per-collection files avoid an extra schema, a central bearer store, and a cross-file transaction protocol.
A central database becomes reasonable if global search, quotas, accounts, or fleet-wide atomic administration become real requirements; none are in scope.
The operator CLI can enumerate files for explicitly requested maintenance.
Keep the directory private and disallow arbitrary ATTACH, extensions, or SQL through application interfaces.

The opener uses `direct-sqlite` with ReadWrite, PrivateCache, and NoFollow, never CREATE, and wraps the handle with a fresh `sqlite-simple` savepoint counter.
ReadWrite may fall back to a read-only connection.

### Schema

```sql
CREATE TABLE collection_meta (
  singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
  collection_id BLOB NOT NULL UNIQUE CHECK (length(collection_id) = 16)
) STRICT;

CREATE TABLE article (
  id BLOB PRIMARY KEY CHECK (length(id) = 16),
  title TEXT NOT NULL CHECK (length(title) > 0),
  url TEXT NOT NULL CHECK (length(url) > 0),
  state TEXT NOT NULL CHECK (state IN ('unread', 'read')),
  created_at INTEGER NOT NULL,
  revision INTEGER NOT NULL DEFAULT 0 CHECK (revision >= 0)
) STRICT;

CREATE TABLE capability (
  id BLOB PRIMARY KEY CHECK (length(id) = 16),
  token_hash BLOB NOT NULL UNIQUE CHECK (length(token_hash) = 32),
  parent_id BLOB REFERENCES capability(id) ON DELETE CASCADE,
  scope_article_id BLOB REFERENCES article(id) ON DELETE CASCADE,
  expires_at INTEGER,
  depth INTEGER NOT NULL CHECK (depth BETWEEN 0 AND 4),
  petname TEXT,
  CHECK (parent_id IS NULL OR parent_id <> id),
  CHECK (
    (parent_id IS NULL AND depth = 0 AND expires_at IS NULL
                       AND scope_article_id IS NULL)
    OR
    (parent_id IS NOT NULL AND depth > 0 AND expires_at IS NOT NULL)
  )
) STRICT;
CREATE UNIQUE INDEX one_root ON capability((1)) WHERE parent_id IS NULL;

CREATE TABLE capability_permission (
  capability_id BLOB NOT NULL REFERENCES capability(id) ON DELETE CASCADE,
  permission TEXT NOT NULL CHECK (permission IN
    ('read', 'add', 'title', 'state', 'delete', 'delegate', 'inspect', 'revoke')),
  PRIMARY KEY (capability_id, permission)
) STRICT;
```

NULL article scope means whole collection.
Exactly one root for a live collection is a creation and runtime invariant beyond the index.
[STRICT tables](https://www.sqlite.org/stricttables.html) restrict column storage types.

Private `ArticleRow`, `GrantRow`, and `PermissionRow` DTOs have mechanical `FromRow` and `ToRow` instances; separate fallible conversions produce validated values and the private `StoredArticle` and `StoredGrant`.
`StoredArticle` pairs an Article with its Integer revision, keeping revision metadata outside the domain model.
A private update DTO with a `ToRow` instance binds only the title, new revision, article ID, and current revision.

### Bearer encoding and digest

```haskell
parseBearer :: Text -> Either BearerError Bearer
renderBearer :: Bearer -> Text
bearerCollection :: Bearer -> CollectionId
lookupBearer :: Connection -> Bearer -> IO (Either LookupError (Maybe StoredGrant))
```

No password-style slow hash is needed for a uniformly random 256-bit secret.
Issuance, which must generate the secret with a CSPRNG, belongs to `add-link-sharing-and-owner-lifecycle`.

### Connection settings and transactions

The write runner requires existing DELETE journaling and sets synchronous EXTRA and secure_delete ON, checking the settings before BEGIN and rechecking inside the transaction.
Use WAL or a connection pool only after measuring a need and reviewing their backup, cleanup, and lifetime costs; see [SQLite pragmas](https://www.sqlite.org/pragma.html).
The title change uses `BEGIN IMMEDIATE` so authorization, ancestry, target and revision checks, and mutation form one transaction; SQLite serializes writers to each database, see [SQLite isolation](https://www.sqlite.org/isolation.html).
Return a retryable busy or conflict result after a bound, not an indefinite wait, and make no network requests while holding a transaction.
Initial deployment supports one application process per data directory; maintenance needs an explicit cross-process exclusion mechanism, not merely an in-process mutex.

### Representative flows

Loading a collection: the caller parses the bearer and opens the existing collection file; the protected read transaction verifies metadata, hash lookup, root and ancestor validity, expiry, and Read scope, and returns only authorized articles.
Application code never chooses an unrelated file.

Changing a title: parse the ID, expected revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus ChangeTitles and the target scope; load the current article and revision; reject a revision mismatch; apply `rename`; persist a changed article with an incremented revision or accept a matching no-op; commit.
Failures cannot leave partial state.

### Tests

Persistence tests use real temporary databases and independent connections: structural round trips, direct constraint violations, fail-closed decoding, existing-only opening, wrong metadata, no effects on another file, commit, rollback, and busy behavior, and barrier-coordinated revoke-versus-edit races.
Revision tests reject older and newer expected revisions even for no-ops, preserve the counter on matching no-ops, increment once per committed change, and handle representation limits without wrapping or partial writes.
Source review checks private component visibility, constructor exports, exact payload signatures, quantified runners, and nominal mode and transaction roles.

## Risks / Trade-offs

- [A version match does not attest every schema object] → Decoders validate every row they read; SQLite is not a malicious-file sandbox.
- [Wrapping a request hides its transaction type] → The caller can keep the value, but the API cannot execute it in another transaction.
- [Trusted clock and exclusive connection use are not enforced by types] → They remain interpreter responsibilities in the later runtime.
- [macOS `fsync` does not flush the drive's write cache] → Durability is not claimed; `fullfsync` stays at its default of OFF until a deployment decision needs it.
- [Immediate failure under contention] → Callers receive StoreBusy and decide whether to retry.
