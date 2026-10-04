# Design

## Context

Each collection is one SQLite file, `data/collections/<collection-id>.sqlite`; see [collection storage](../../specs/collection-storage/spec.md).
Its schema version is stored in SQLite's `user_version`, and opening accepts only version 1, rejecting zero, negative, and newer versions without initializing or migrating the file.
Initialization installs the schema and identity atomically into an already-open empty database, without creating a file or root grant.
Writes require existing DELETE journaling, and the adapter never converts journal modes.
Initial deployment supports one application process per data directory.

No central index is needed for normal requests: the capability carries the public locator.
The operator CLI can enumerate files for explicitly requested maintenance.
This avoids an extra schema, central bearer store, and cross-file transaction protocol, but does not remove the runtime's authority over the data directory.

## Goals / Non-Goals

**Goals:**

- Maintenance that preserves collection boundaries and fails without partial changes.
- Honest operator guidance about what backups and restores can and cannot undo.

**Non-Goals:**

- Scanning every database during a request.
- Protection against restored backups, which would need external trusted state.

## Decisions

### Offline migrations with a small explicit version mechanism

Schema migrations are explicit offline CLI operations, and opening a collection during a request must not silently migrate it.
Use a small explicit version mechanism such as `user_version`; never infer a migration from an exception in a request.
`Store.Migrate` is CLI-only authority, not an operation available to web application code.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Migrations | Explicit numbered SQL; migrant-sqlite-simple; automatic migration on request | Offline migration is agreed. Prefer tiny explicit transactional steps; evaluate [migrant-sqlite-simple](https://hackage.haskell.org/package/migrant-sqlite-simple) if it reduces bookkeeping without hiding failure semantics. |

The migration library's existence is not evidence that automatic request-time migrations are safe.

### Migration procedure

Offline migrations take maintenance exclusion, identify the exact file and schema version, back up before risky changes, apply ordered transactional steps, check constraints and integrity, and advance the version only on success.
They refuse newer or unknown schemas and concurrent application access.

### Cross-process maintenance exclusion

Maintenance needs an explicit cross-process exclusion mechanism, not merely an in-process mutex, because the application and the CLI are separate processes.
The mechanism needs review before this change depends on it.

### Backups and exports

For backups, use the [SQLite backup API](https://www.sqlite.org/backup.html) or a stopped application with all handles closed, not an arbitrary live-file copy.
Backups include grant hashes and metadata; article-only exports deliberately exclude them.
Use WAL or a connection pool only after measuring a need and reviewing their backup, cleanup, and lifetime costs.

### Root reset

The owner may inspect its own root metadata and manage all proper descendants, but root revocation and replacement belong to the CLI reset.
CLI root reset revokes the old tree and issues a new one-time owner URL.
As with any issuance, the new URL is shown once and only its hash is stored.

### Deleted-file cleanup

Under the D5 proposal, owner deletion marks the collection deleted and removes articles and grants in one transaction, keeping the tombstone file until controlled startup or offline cleanup can establish that no handles are active.
Do not unlink a live database while other requests may still hold it.
File removal and backup retention must be explained separately from logical deletion.

### No central index

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Central index | No index; non-secret catalogue; global grant/secret store | No index now. Operator enumeration suffices; no cross-collection product query earns a central authority. |

A central database becomes reasonable if global search, quotas, accounts, or fleet-wide atomic administration become real requirements; none are currently in scope.

### Tests

Migration tests cover old supported schemas, interrupted steps, future-version refusal, and unchanged neighboring collections.
Maintenance tests also cover failure recovery and restored revocation semantics, using real temporary databases.

## Risks / Trade-offs

- [An old backup restored brings back formerly revoked grants] → Operator guidance states it; preventing rollback needs external trusted state that this small design does not supply.
- [Deleting a collection does not erase backups] → Explain file removal and backup retention separately from logical deletion.
- [Backups contain article URLs and text that may themselves be sensitive] → Hash-only grant records do not disclose reusable bearer secrets, but backups still need the same protection as the data directory.
- [Per-collection files need extra open, migration, and backup coordination] → Accepted for limiting each connection to one collection and allowing independent export; it is not operating-system isolation.
- [A shared process and disk remain a cross-collection availability dependency] → Maintenance touches only the requested collection and leaves the others unchanged.

## Open Questions

- The cross-process exclusion mechanism and its review.
- Whether `migrant-sqlite-simple` reduces bookkeeping without hiding failure semantics.
- The restore procedure and how it interacts with the exclusion.
- Owner-link recovery UX around root reset.
- Deletion and backup retention expectations, and D5's cleanup lifecycle.
