## Purpose

Defines how an operator maintains collection files offline through the CLI: schema migrations, backups and restores, article-only exports, root reset, and cleanup of deleted collection files.

## ADDED Requirements

### Requirement: Migrations are explicit CLI operations
Schema migrations SHALL run only as explicit offline CLI operations.
Opening a collection during a request MUST NOT migrate it, and web application code MUST have no operation that starts a migration.
A migration is never inferred from an exception during a request.

#### Scenario: Request opens an older schema
- **WHEN** a request opens a collection file with an older supported schema version
- **THEN** opening fails without changing the file

#### Scenario: Operator migrates a collection
- **WHEN** the operator runs the migration command for that collection
- **THEN** the file is migrated to the current schema version

### Requirement: Maintenance exclusion
Maintenance commands that change a collection file SHALL take an exclusion that works across processes and MUST refuse to run while the application has access to the data directory.
An in-process mutex does not satisfy this requirement.

#### Scenario: Application is running
- **WHEN** the operator starts a migration while the application process is running on the same data directory
- **THEN** the command refuses and leaves the file unchanged

#### Scenario: Application starts during maintenance
- **WHEN** the application starts while a maintenance command holds the exclusion
- **THEN** the application does not open collection files while the exclusion is held

### Requirement: Exact migration target
A migration SHALL identify the exact collection file and its current schema version before changing anything.
It MUST act only on the explicitly requested collection.

#### Scenario: Migrate one requested collection
- **WHEN** the operator migrates one collection in a data directory containing several
- **THEN** the command reports that file and its schema version and changes no other file

### Requirement: Backup before risky changes
A migration SHALL take a consistent backup of the collection before applying a risky step.

#### Scenario: Risky step follows a backup
- **WHEN** a migration includes a risky step
- **THEN** a consistent backup of the collection exists before that step runs

### Requirement: Transactional migration steps
A migration SHALL apply its steps in order, each inside a transaction, and MUST advance the schema version only after every step succeeds.
A failed or interrupted step leaves the collection at its previous version and contents.

#### Scenario: Interrupted step
- **WHEN** a migration step fails or is interrupted
- **THEN** the collection keeps its previous schema version and contents

#### Scenario: Successful migration
- **WHEN** every migration step succeeds
- **THEN** the schema version advances to the target version

### Requirement: Integrity check before advancing
A migration SHALL check constraints and database integrity before advancing the schema version, and MUST NOT advance it when a check fails.

#### Scenario: Constraint violation after a step
- **WHEN** a migrated collection violates a constraint or fails the integrity check
- **THEN** the migration fails without advancing the schema version

### Requirement: Unknown schemas are refused
Maintenance commands SHALL refuse collection files with a newer or unknown schema version and MUST leave them unchanged.

#### Scenario: File from a newer version
- **WHEN** the operator runs a migration on a file whose schema version is newer than the application supports
- **THEN** the command refuses and leaves the file unchanged

### Requirement: Consistent backups
A backup SHALL be a consistent copy of one collection, taken through the database's online backup mechanism or from a stopped application with all handles closed.
Copying a live database file MUST NOT be offered as a backup method.

#### Scenario: Backup while articles change
- **WHEN** the operator backs up a collection that the application may be using
- **THEN** the backup uses the online backup mechanism and opens as a valid collection with its stored identity and schema version

### Requirement: Backup contents
A backup SHALL contain the complete collection, including grant hashes and grant metadata.
Backups and restores MUST NOT store recoverable bearer URLs.

#### Scenario: Restore a backup
- **WHEN** the operator restores a backup
- **THEN** the restored collection contains the articles, grant hashes, and grant metadata from the time of the backup

### Requirement: Article-only export
An article-only export SHALL contain the collection's articles and MUST exclude grant hashes and grant metadata.

#### Scenario: Export a collection
- **WHEN** the operator exports a collection's articles
- **THEN** the export contains the articles and no grant hash or grant metadata

### Requirement: Root reset
A CLI root reset SHALL revoke the collection's existing grant tree and issue a new root grant whose owner URL is shown once.
The previous owner URL and all its descendants MUST stop authorizing access.

#### Scenario: Reset a lost owner link
- **WHEN** the operator resets the root of a collection
- **THEN** the command prints a new owner URL once, and the old owner URL and its descendants are denied

### Requirement: Deleted-file cleanup
Cleanup of deleted collection files SHALL run only when it can establish that no request holds the database.
It MUST NOT unlink a live database.

#### Scenario: Cleanup with possible active handles
- **WHEN** cleanup cannot establish that no request holds a deleted collection's file
- **THEN** the file is not removed

### Requirement: Other collections remain unchanged
Maintenance of one collection SHALL leave every other collection file unchanged, including collections that reuse the same article or grant IDs.

#### Scenario: Neighbor of a migrated collection
- **WHEN** the operator migrates, restores, resets, or cleans up one collection
- **THEN** the bytes of every other collection file are unchanged

### Requirement: Restored backups can restore revoked authority
Operator guidance SHALL state that restoring an old backup can restore formerly revoked grants, and that deleting a collection does not erase its backups.
Preventing this rollback needs external trusted state that the design does not supply.

#### Scenario: Restore after revocation
- **WHEN** a backup taken before a revocation is restored
- **THEN** the revoked grant authorizes access again, as the operator guidance states
