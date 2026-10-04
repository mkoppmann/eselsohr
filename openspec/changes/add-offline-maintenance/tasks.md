# Tasks

These are outlined slices; before /opsx:apply, expand the next slice into bounded tasks naming scope, non-goals, likely files, completion checks, and security constraints, and settle the decisions it consumes.
Use a planning pass for the exclusion and recovery contracts first.
A `/goal` may suit a fixed, approved backup and migration failure matrix once the APIs, allowed fixes, and finish criteria exist.

## 1. Version checks and one transactional migration

- [ ] 1.1 Add the reviewed cross-process maintenance exclusion and refuse to migrate while the application holds the data directory; verified by tests with a running application process and a concurrent maintenance command.
- [ ] 1.2 Add explicit version checks and one ordered, transactional migration with a backup before risky steps and integrity checks before advancing the version; verified by real database tests for old supported schemas, interrupted steps, future and unknown versions, and an unchanged neighboring collection.

## 2. Consistent backup and restore

- [ ] 2.1 Add backup through the SQLite backup API and restore of one collection; verified by tests that back up during application use, restore, and show that a backup taken before a revocation restores the revoked grant.
- [ ] 2.2 Write operator guidance stating that restoring a backup can restore revoked authority and that deleting a collection does not erase backups; verified by following the guidance against a test collection.

## 3. Article-only export

- [ ] 3.1 Add an article-only export without grant hashes or metadata; verified by tests that inspect the export contents.

## 4. Root reset

- [ ] 4.1 Add a CLI root reset that revokes the old tree and prints a new one-time owner URL; verified by tests that the old owner URL and its descendants are denied and the new URL is stored only as a hash.

## 5. Deleted-file cleanup

- [ ] 5.1 Add cleanup of deleted collection files after D5 settles the lifecycle; verified by tests that cleanup never unlinks a file a request may still hold and leaves other collections unchanged.

## 6. Exit review

- [ ] 6.1 Review failure recovery, unknown and future versions, unchanged neighboring collections, and restored revocation semantics; verified by Build, Test, Quality, HLS, Specs, and Diff checks.
