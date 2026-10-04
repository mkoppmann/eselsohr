## ADDED Requirements

### Requirement: One article per stored URL
The schema SHALL allow at most one article per stored URL through a UNIQUE index on the article URL.
Stored URLs are canonical renderings, so the index compares canonical URLs.

#### Scenario: A second article with the same URL
- **WHEN** trusted SQL inserts an article whose stored URL equals that of an existing article
- **THEN** the article URL uniqueness index rejects it

## MODIFIED Requirements

### Requirement: Atomic schema initialization
Initialization SHALL install the four tables, the root uniqueness index, the article URL uniqueness index, the supplied collection identity, and schema version 1 in one immediate transaction on an already-open database.
It MUST require an empty database at version zero, no active transaction, enabled foreign keys, and a supported journal mode other than OFF.
It reads back the version before committing and leaves no partial schema, identity, version, or open transaction after any failure.

#### Scenario: Commit fails
- **WHEN** the disk is full or COMMIT reports Busy during initialization
- **THEN** the database keeps no partial schema or version, and a retry succeeds after the cause is removed

#### Scenario: A nonempty database
- **WHEN** initialization targets a database that already contains objects
- **THEN** it returns NonEmptyDatabase without changing it

#### Scenario: Initialization creates no authority
- **WHEN** initialization succeeds
- **THEN** the database contains no article, bearer, or owner grant, no file was created, and no protected operation was involved
