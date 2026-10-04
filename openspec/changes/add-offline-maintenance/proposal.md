# Proposal

## Why

Collection files will outlive the schema version that created them, and operators need backups, exports, and a way to recover lost owner access.
Opening a collection during a request deliberately refuses other schema versions without migrating them, so maintenance needs its own explicit path.
This change covers milestone M9: make persisted data maintainable without undermining collection boundaries.

## What Changes

- Add explicit offline CLI migrations that take maintenance exclusion, identify the exact file and schema version, back up before risky changes, apply ordered transactional steps, check constraints and integrity, and advance the version only on success.
- Refuse newer or unknown schemas and concurrent application access.
- Add consistent backup and restore of one collection; backups include grant hashes and metadata.
- Add an article-only export that excludes grant hashes and metadata.
- Add a CLI root reset that revokes the old grant tree and issues a new one-time owner URL.
- Add cleanup of deleted collection files.
- State in operator guidance that restoring an old backup can restore revoked authority.

## Capabilities

### New Capabilities

- `collection-maintenance`: offline operator maintenance of collection files, covering migrations, backups, exports, root reset, and deleted-file cleanup.

### Modified Capabilities

None.

## Non-goals

- A central admin database, backup fleet, or admin UI.
- Automatic migrations during requests.
- Preventing rollback through restored backups, which needs external trusted state.
- Claiming that deletion removes backups.

## Dependencies and approval gates

Depends on a stable schema, the grant and deletion semantics of `add-link-sharing-and-owner-lifecycle` (M7), and a reviewed maintenance lock.
Root reset needs the operator CLI from `add-manual-bookmark-web` (M6).
This change consumes the agreed offline migrations and the preferred per-collection SQLite files.
D5 gates deleted-file cleanup; the other slices do not depend on it.
Owner-link recovery UX and deletion and backup retention expectations remain open human choices; see design.md.

## Impact

- New `Store.Migrate` module and CLI commands for migration, backup, restore, export, root reset, and cleanup; web application code gains no maintenance operation.
- A cross-process exclusion mechanism shared by the application and maintenance commands.
- Operator documentation for backup, restore, and their security limits.
