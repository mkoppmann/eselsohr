## MODIFIED Requirements

### Requirement: Existing-only opening
Opening SHALL resolve an existing collections directory to a canonical absolute path without creating it, and derive the file name from a validated collection identifier.
It MUST NOT create a database; it reports a missing collection file as Missing, and it rejects symlinks and targets that are not regular files as Unavailable.
Paths containing NUL or surrogate code points are rejected before reaching filesystem or SQLite APIs, and URI-significant characters in absolute paths stay literal.

#### Scenario: A missing collection file
- **WHEN** opening is requested for a collection whose file does not exist
- **THEN** it returns Missing and creates no file

#### Scenario: A missing collections directory
- **WHEN** the configured collections directory does not exist
- **THEN** resolving it returns Unavailable and creates no directory

#### Scenario: A symlinked collection file
- **WHEN** the collection file name is a symlink to a valid database
- **THEN** opening returns Unavailable

### Requirement: Opening errors and cleanup
Opening SHALL return Missing, Unavailable, InvalidMetadata, UnsupportedSchema, Busy, or DatabaseFault without including paths or stored values.
Missing is reserved for a collection file that does not exist; other filesystem failures return Unavailable.
Metadata conversion failures, including invalid UTF-8, MUST return InvalidMetadata.
The connection is closed after success, failure, callback exceptions, and cancellation, and callback exceptions propagate unchanged.
Callbacks must finalize their statements and must not close or retain the connection.

#### Scenario: A callback throws
- **WHEN** the trusted callback throws an exception
- **THEN** the connection is closed and the same exception propagates

#### Scenario: Rejected files stay untouched
- **WHEN** opening rejects a closed fixture file
- **THEN** the file's bytes and directory entries are unchanged, with hot-journal recovery outside this claim
