# Proposal

## Why

The pure operation policy already decides issuance, inspection, revocation, and owner deletion, but no protected operation or page performs them.
Sharing is the reason Eselsohr uses capabilities, and its limits only become observable once real links are issued, inspected, revoked, and deleted with real transactions and HTTP boundaries.
The legacy sharing model lists links by category and reconstructs usable bearer URLs from stored tokens, which lets access to one grant discover others.
This change covers milestone M7: make delegated access and its limits observable in real software.

## What Changes

- Issue bounded child links in a write transaction and show each new URL once.
- Let the owner issue everyday working links that default to seven days.
- Issue add-only links for contributors, who submit articles through the add-only receipt from `add-manual-bookmark-web`.
- Show descendant link metadata and revoke selected subtrees.
- Let the owner delete the whole collection through the web interface, once D5 settles the deletion lifecycle.

## Capabilities

### New Capabilities

- `link-sharing`: issuing child links, their lifetimes, inspecting descendant metadata, and revoking subtrees.
- `collection-deletion`: owner-only deletion of a whole collection and its limits.

### Modified Capabilities

None.

## Non-goals

- User accounts, reusable-token recovery, or any way to display an issued URL again.
- Root revocation or replacement, which belong to the CLI root reset in `add-offline-maintenance`.
- Deleted-file cleanup, backups, restore, and migrations, which belong to `add-offline-maintenance` (M9).
- Automatic metadata fetching or any dependency on `add-restricted-metadata-fetch`.
- Claims that deletion removes backups.

## Dependencies and approval gates

Depends on `add-scoped-store-effects` and `add-manual-bookmark-web` for the transaction and web foundations, and on the approved pure policy in [operation-policy](../../specs/operation-policy/spec.md) and [capability-grants](../../specs/capability-grants/spec.md).
D5 (deletion lifecycle) gates collection deletion: no deletion task starts until the author approves a deletion and cleanup contract.
Issuance, inspection, and revocation do not depend on D5.
Owner-link recovery UX and deletion and backup retention expectations remain open human choices; see `design.md`.

## Impact

- New protected issuance, inspection, revocation, and collection deletion operations in the private adapter, exposed through Store facets.
- New sharing and management pages and POST routes.
- Possibly a schema change for deletion metadata, depending on D5, which would need an explicit offline migration.
- SQLite race tests and HTTP tests for issuance, revocation, and deletion.
