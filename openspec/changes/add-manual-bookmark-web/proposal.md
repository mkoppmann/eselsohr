# Proposal

## Why

D2 makes manually entered URLs and titles the first usable version, but the replacement has no runtime, command line, or web interface through which an operator or holder can use it.
The legacy server already shows that escaped HTML, plain forms, and restrictive headers fit the product, while method override, unconstrained redirect targets, trusted forwarded headers, and silent configuration fallbacks must not be carried forward.
This change covers milestone M6: a small usable vertical slice with secure web defaults from its first endpoint.

## What Changes

- Validate configuration through one mechanism and start a local listener.
- Add an operator command that creates a collection and shows its owner URL once.
- Add protected listing and manual adding of articles, including add-only receipts.
- Add title and read-state edits that reject stale forms.
- Add protected article deletion, which requires no Read permission and discloses no article contents.
- Build escaping, explicit bearer handling, safe methods and redirects, response headers, and leakage tests into these endpoints rather than a late hardening pass.

## Capabilities

### New Capabilities

- `web-interface`: server-rendered HTML forms, request methods and redirects, bearer handling, response headers, link construction, and leakage limits.
- `collection-administration`: operator creation of a collection and one-time display of its owner URL.
- `article-management`: protected listing, manual adding, title and read-state edits, and deletion of articles.
- `runtime-configuration`: how the application reads and validates its configuration.

### Modified Capabilities

None.

## Non-goals

- JavaScript, automatic metadata fetching, or PublicWeb, which belong to `add-restricted-metadata-fetch`.
- The full management interface: link issuance, inspection, revocation, and collection deletion belong to `add-link-sharing-and-owner-lifecycle`.
- Deployment, TLS termination, systemd units, or containers, which belong to `add-native-deployment`.
- Legacy removal, which belongs to `cut-over-to-root-package`.
- Migrations, backups, or root reset, which belong to `add-offline-maintenance`.

## Dependencies and approval gates

Depends on `add-protected-sqlite-storage` (M4) and `add-scoped-store-effects` (M5); it does not depend on `add-restricted-metadata-fetch` (M8) or on complete sharing.
Consumes D2 (manual bookmarking first) and D3, whose sealed requests the new add, read-state, and delete operations follow.
D5 is not consumed.
Numeric input limits, missing-Origin handling, and the exact configuration names, formats, and defaults stay open until their slice; see `design.md`.

## Impact

- New runtime, configuration, CLI, web routing, HTML, and protected add, read-state, and delete operations in `v2/`.
- New web dependencies, pinned through a deliberate freeze update.
- HTTP and end-to-end tests using real temporary collections and ephemeral listeners.
