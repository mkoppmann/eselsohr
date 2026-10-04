# Proposal

## Why

The pure policy decides on grant data supplied by its caller; nothing yet selects a collection file, authenticates a bearer, or applies a checked operation to stored state.
Trustworthy file selection and one protected read and write path must exist before an application-facing Store API, effects, or web routes depend on them.
This change covers milestone M4.

## What Changes

- Bundle SQLite through `direct-sqlite` with `systemlib` disabled, and add the private Cabal library `sqlite-internal` with its own integration suite.
- Open only existing collection files, checking stored identity and schema version 1.
- Install the initial schema atomically, and convert storage rows to validated values through private DTOs with separate revision metadata.
- Parse canonical bearers and look up grants by a collection-bound SHA-256 digest inside caller-owned transactions.
- Implement the named protected operations `readArticles` and `changeTitle` with operation-specific sealed requests, transactional ancestry and policy checks, and revision conflict handling.

## Capabilities

### New Capabilities

- `collection-storage`: per-collection files, restricted opening, schema installation, and validated row codecs.
- `bearer-credentials`: bearer format, collection-bound digest, and digest lookup.
- `protected-article-operations`: sealed, transactional article reads and title changes.

### Modified Capabilities

- `security-boundaries`: adds revalidation of persisted authority, fail-closed decoding, and the private status of raw database authority.

## Non-goals

- A full CRUD API, other protected mutations, issuance or revocation APIs, the application-facing Store interface, effects, runtime, web, CLI, file creation, migrations, pooling, or an elaborate migration history.
- Further SQLite distribution experiments, adopting new journal modes, or retrying contention.

## Dependencies and approval gates

Depends on `establish-replacement-package` and the policy of `add-domain-and-policy`.
Consumes D3 (exact-payload sealing behind named operations) and D4 (bundled SQLite).
D5 deletion metadata is deferred to `add-link-sharing-and-owner-lifecycle`.

## Impact

- New: `v2/persistence/Eselsohr/Store/SQLite/` (Open, Schema, Codec, Bearer, Articles, and the hidden Internal.Transaction and Internal.Authority) and `v2/test-sqlite/`.
- Changed: the package, project, freeze file, and `hie.yaml`.
- Pinned dependencies: `direct-sqlite` 2.3.29, `sqlite-simple` 0.4.19.0, `base64-bytestring` 1.2.1.0, and `cryptohash-sha256` 0.11.102.1 with its bundled C implementation; the domain library stays independent of SQLite.
