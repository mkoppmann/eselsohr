## Purpose

How each collection is stored in its own SQLite database: selecting and opening only existing files, checking their identity and schema version, installing the initial schema, and converting stored rows to validated values.

## ADDED Requirements

### Requirement: One database per collection
Each collection SHALL be stored in its own SQLite file named `<canonical-collection-id>.sqlite` inside the collections directory.
Normal requests MUST NOT need a central index or scan every database; the presented capability carries the public locator.
Separate files limit some storage mistakes but are not operating-system isolation, and the runtime keeps authority over the data directory.

#### Scenario: Two collections with repeated identifiers
- **WHEN** two collection files contain articles and grants with the same identifiers
- **THEN** opening and operating on one collection never reads or changes the other file

### Requirement: Existing-only opening
Opening SHALL resolve an existing collections directory to a canonical absolute path without creating it, and derive the file name from a validated collection identifier.
It MUST NOT create a database, and it rejects symlinks and targets that are not regular files.
Paths containing NUL or surrogate code points are rejected before reaching filesystem or SQLite APIs, and URI-significant characters in absolute paths stay literal.

#### Scenario: A missing collection file
- **WHEN** opening is requested for a collection whose file does not exist
- **THEN** it returns Unavailable and creates no file

#### Scenario: A symlinked collection file
- **WHEN** the collection file name is a symlink to a valid database
- **THEN** opening returns Unavailable

### Requirement: Stored identity and schema version
Before handing over a connection, opening SHALL require a real identity table containing exactly one row: integer singleton 1 and the requested collection's UUIDv4 as a 16-byte BLOB in network byte order.
It MUST then require schema version 1, rejecting version zero, negative versions, and newer versions without initializing or migrating them.
The identity check reads at most two rows and applies the domain identifier parser to the decoded bytes.

#### Scenario: A file belonging to another collection
- **WHEN** the file's stored identity differs from the requested collection
- **THEN** opening returns InvalidMetadata without invoking the callback

#### Scenario: An unsupported schema version
- **WHEN** a file with the matching identity has schema version 0 or 2
- **THEN** opening returns UnsupportedSchema without invoking the callback and leaves the file unchanged

### Requirement: Opening errors and cleanup
Opening SHALL return Unavailable, InvalidMetadata, UnsupportedSchema, Busy, or DatabaseFault without including paths or stored values.
Metadata conversion failures, including invalid UTF-8, MUST return InvalidMetadata.
The connection is closed after success, failure, callback exceptions, and cancellation, and callback exceptions propagate unchanged.
Callbacks must finalize their statements and must not close or retain the connection.

#### Scenario: A callback throws
- **WHEN** the trusted callback throws an exception
- **THEN** the connection is closed and the same exception propagates

#### Scenario: Rejected files stay untouched
- **WHEN** opening rejects a closed fixture file
- **THEN** the file's bytes and directory entries are unchanged, with hot-journal recovery outside this claim

### Requirement: Opening connection settings
Opening SHALL set and read back foreign keys ON, trusted schema OFF, and a busy timeout of 0 before validating identity.
Opening and metadata contention MUST fail immediately as Busy, without a retry loop.
Opening leaves journal mode, synchronous, and secure_delete unchanged, and a successful open does not promise that later writes will succeed.

#### Scenario: A locked database
- **WHEN** another connection holds a lock that blocks the identity check
- **THEN** opening returns Busy without waiting

### Requirement: Atomic schema initialization
Initialization SHALL install the four tables, the root uniqueness index, the supplied collection identity, and schema version 1 in one immediate transaction on an already-open database.
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

### Requirement: Storage constraints
The schema SHALL use STRICT tables with NOT NULL, CHECK, UNIQUE, and foreign-key constraints for the representable invariants of articles, grants, and permissions.
At most one root grant is allowed per database, and deleting a grant or scoped article cascades to its descendants and permissions.
The schema alone MUST NOT be relied on to forbid cycles, validate URLs, enforce permission inclusion or deadlines, or establish exactly one root; decoding and the pure grant checks validate those instead.

#### Scenario: A second root
- **WHEN** trusted SQL inserts a second parentless grant
- **THEN** the root uniqueness index rejects it

### Requirement: Validated article rows
Article rows SHALL decode only when the identifier is 16 bytes containing a UUIDv4, the title and URL are already canonical, and the state is `unread` or `read`.
Decoding MUST NOT silently normalize stored values.
Article revisions are metadata kept beside the article, ranging from zero through 9223372036854775807 and checked before narrowing; new articles default to revision zero.

#### Scenario: A noncanonical stored title
- **WHEN** a stored title has surrounding whitespace
- **THEN** decoding fails with InvalidValue instead of normalizing it

#### Scenario: An out-of-range revision
- **WHEN** a revision outside the signed 64-bit range is encoded
- **THEN** encoding fails with UnrepresentableValue

### Requirement: Exact timestamps
Timestamps SHALL be stored as signed 64-bit Unix microseconds using exact integer arithmetic.
Submicrosecond precision, leap seconds, and out-of-range times MUST be rejected rather than rounded; negative timestamps are supported.

#### Scenario: Submicrosecond time
- **WHEN** a time with nanosecond precision is encoded
- **THEN** encoding fails instead of rounding

### Requirement: Validated grant rows
Grant decoding SHALL check the 32-byte digest and the root and child field requirements, while still accepting expired grants for later management checks.
It MUST build the scope from the collection identifier already checked by the caller and assemble at most eight distinct, known permission rows keyed to that grant, independent of row order.
Decoding does not authenticate a bearer, check article existence, or validate ancestry.

#### Scenario: A root missing a permission row
- **WHEN** a stored root grant has only seven permission rows
- **THEN** decoding fails with InvalidValue

#### Scenario: Preserve a petname
- **WHEN** a grant with a petname is encoded and decoded
- **THEN** the petname is unchanged, and no label validation is applied

#### Scenario: Encoding a foreign grant
- **WHEN** a grant scoped to another collection is encoded
- **THEN** encoding fails with CollectionMismatch

### Requirement: Sanitized codec errors
Codec errors SHALL distinguish invalid values, values SQLite cannot represent, and collection mismatch without including submitted or stored values.
SQL row conversion diagnostics MUST be replaced with these errors before reaching the caller, and stored rows have no text representation.

#### Scenario: A malformed SQL type
- **WHEN** a stored column has an unexpected SQL type
- **THEN** the caller receives a codec error without the stored value

### Requirement: Bundled SQLite engine
The application SHALL link the SQLite engine bundled with its dependencies instead of a system SQLite library, using version 3.45.0 or a deliberately updated later version.
Deployment MUST NOT require a separate SQLite shared library, and SQLite fixes are delivered by updating the dependency and rebuilding.
STRICT tables require SQLite 3.37.0 or later; that is a feature minimum, not a security patch policy.

#### Scenario: Inspect the linked libraries
- **WHEN** `ldd` on Linux or `otool -L` on macOS inspects the SQLite test executable
- **THEN** it lists no SQLite shared library, and the suite reports SQLite 3.45.0 with its source ID and compile options
