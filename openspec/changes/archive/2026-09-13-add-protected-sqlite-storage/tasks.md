# Tasks

## 1. SQLite distribution

- [x] 1.1 Settle D4: use the SQLite engine bundled with `direct-sqlite`, with `systemlib` disabled; no further distribution comparison is required.

## 2. Bundled SQLite and restricted collection opening

- [x] 2.1 Add the private `sqlite-internal` library and integration suite, pinning `direct-sqlite` 2.3.29 and `sqlite-simple` 0.4.19.0 while preserving existing pins and the index state; verified by the solved plan showing `systemlib: false`, by the suite reporting SQLite 3.45.0 with its source ID and compile options, and by `ldd "$(cabal list-bin --project-file=cabal-v2.project test:eselsohr-v2-sqlite-test)"` and the macOS `otool -L` equivalent listing no SQLite library.
- [x] 2.2 Implement `mkCollectionDirectory` and `withExistingCollection` with existing-only opening, symlink and non-regular rejection, identity metadata checks, connection settings, and cleanup; verified by opening tests on real files, wrong identities, invalid UTF-8 metadata, unchanged rejected fixtures, contention, and callback exceptions and cancellation.

## 3. Initial schema and validated storage codecs

- [x] 3.1 Implement the atomic schema initializer and the schema version 1 check; verified by schema tests covering version refusal, nonempty and active-transaction refusal, constraints, cascades, and rollback after controlled failures, SQL exceptions, unexpected exceptions, cancellation, and real FULL, BEGIN Busy, and COMMIT Busy failures.
- [x] 3.2 Implement private row DTOs and checked conversions with exact timestamps and separate revisions; verified by codec tests covering golden representations, generated round trips, timestamp and revision extrema, malformed values and SQL types, invalid UTF-8, all 256 child permission subsets, oversized lists, and repeated article IDs in independent files.

## 4. Canonical bearer parsing and digest lookup

- [x] 4.1 Implement `parseBearer`, `renderBearer`, `bearerCollection`, and `lookupBearer`, pinning `base64-bytestring` 1.2.1.0 and `cryptohash-sha256` 0.11.102.1; verified by bearer tests covering canonical encoding, malformed fields, unused bits, oversized input, all secret byte values, 500 generated round trips, independent SHA-256 goldens, and lookup against real databases with repeated grant IDs, copied digests, expired grants, duplicates, malformed rows, transaction ownership, contention, and committed deletion.

## 5. Protected article read

- [x] 5.1 Implement `readArticles` with the read runner, sealed scope, transactional ancestry and policy checks, and validated selection; verified by 26 read tests covering root, delegated, and add-only authority, exact scopes, two collections with repeated IDs, strict expiry, maximum ancestry, malformed storage, absence, caller transactions, rejected settings, contention, exceptions, cancellation, COMMIT Busy, and read, update, and revocation ordering.

## 6. Protected title change

- [x] 6.1 Implement `changeTitle` with the write runner, sealed payload, write settings, and revision handling; verified by 32 title tests covering owner and delegated authority, permission combinations and no-ops, exact payloads, two files with repeated IDs, fresh expiration and ancestor checks, malformed storage, revision extrema, rejected journal modes, rollback after an UPDATE, affected-row checks, COMMIT Busy, competing edits, and ancestor revocation in both orderings; revocation fixtures use trusted SQL to exercise ordering without a revocation API.
- [x] 6.2 Run Build, Test, Version, offline Build and Test, formatting, linting, HLS (28 files), and Diff checks on Linux and macOS; all 132 domain and policy tests and 166 SQLite tests pass with bundled SQLite 3.45.0.

## 7. Exit review

- [x] 7.1 Review the protected read and write path with two real databases, repeated object IDs, missing and wrong files, malformed rows, rollback, and concurrent requests, and address the findings.
