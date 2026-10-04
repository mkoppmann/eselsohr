# Proposal

## Why

D2 makes manually entered URLs and titles the first usable version, but the replacement has no runtime, command line, or web interface through which an operator or holder can use it.
The legacy server already shows that escaped HTML, plain forms, and restrictive headers fit the product, while method override, unconstrained redirect targets, trusted forwarded headers, and silent configuration fallbacks must not be carried forward.
This change covers milestone M6: a small usable vertical slice with secure web defaults from its first endpoint.

## What Changes

- Validate configuration through environment variables and start a local listener.
- Add an operator command that creates a collection and shows its owner URL once.
- Give each bearer exactly one page, derived from its grant, so that no request reaches an operation unless a presented capability selects it.
- Add a protected read through which a grant describes its own permissions, scope, and expiration to its holder.
- Add protected listing and manual adding of articles, including add-only receipts and one article per canonical URL.
- Add title and read-state edits that reject stale forms.
- Add protected article deletion, which requires no Read permission and discloses no article contents.
- Build escaping, explicit bearer handling, safe methods and redirects, response headers, body limits, and leakage tests into these endpoints rather than a late hardening pass.

## Capabilities

### New Capabilities

- `web-interface`: capability navigation, server-rendered HTML forms, request methods and redirects, bearer handling, response headers, link construction, and leakage limits.
- `collection-administration`: operator creation of a collection and one-time display of its owner URL.
- `article-management`: what holders see and can do on their page: listing, manual adding, title and read-state edits, and deletion of articles.
- `runtime-configuration`: how the application reads and validates its configuration.

### Modified Capabilities

- `operation-policy`: a describe-own-grant operation that requires only a valid acting chain.
- `application-store`: a read-only grant interface, and the add, read-state, and delete operations of the write interface with their results.
- `protected-article-operations`: the protected grant description, addition, read-state change, and deletion, with their sealed requests and transactions.
- `collection-storage`: a UNIQUE index on the article URL in schema version 1.

## Non-goals

- JavaScript, automatic metadata fetching, or PublicWeb, which belong to `add-restricted-metadata-fetch`.
- The full management interface: link issuance, inspection, revocation, collection deletion, and a collection quota belong to `add-link-sharing-and-owner-lifecycle`.
- Deployment, TLS termination, HSTS, systemd units, or containers, which belong to `add-native-deployment`.
- Legacy removal, which belongs to `cut-over-to-root-package`.
- Migrations, backups, root reset, or a process lock for the data directory, which belong to `add-offline-maintenance`.
- Per-action capability URLs; per-action sharing uses the child grants of `add-link-sharing-and-owner-lifecycle`.

## Dependencies and approval gates

Depends on `add-protected-sqlite-storage` (M4) and `add-scoped-store-effects` (M5); it does not depend on `add-restricted-metadata-fetch` (M8) or on complete sharing.
Consumes D2 (manual bookmarking first) and D3, whose sealed requests the new description, add, read-state, and delete operations follow.
D5 is not consumed.
The URL form, configuration, Origin handling, body limit, duplicate enforcement, deletion semantics, and own-grant description contract are settled in `design.md`.
No approval gate remains open for this change; the collection quota and the process lock are deferred to the changes named above.

## Impact

- New runtime, configuration, CLI, web routing, HTML, and protected description, add, read-state, and delete operations in `v2/`.
- A UNIQUE index added to schema version 1 in place, since no collection exists outside tests.
- New dependencies `wai`, `warp`, `http-types`, `lucid`, and `crypton`, pinned through a deliberate freeze update.
- HTTP and end-to-end tests using real temporary collections and ephemeral listeners.
