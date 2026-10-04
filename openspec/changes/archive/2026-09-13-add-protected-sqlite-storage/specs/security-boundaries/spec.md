## ADDED Requirements

### Requirement: Persisted authority is revalidated
Every protected operation SHALL look up the presented bearer, load its grant and ancestors, and read the clock inside its transaction, then check policy and apply the operation to that same collection and target in the same transaction.
Persisted authority MUST be validated, including its ancestry, rather than trusted because it came from SQLite.
If a grant is missing or revoked in storage, the operation denies access instead of reusing grant data from an earlier read.

#### Scenario: Grant data from an earlier read
- **WHEN** a grant read in an earlier transaction has since been revoked
- **THEN** a new protected operation denies access

### Requirement: Corrupt state fails closed
Decoding SHALL reject invalid identifiers, enumerations, schema versions, grant chains, and identity metadata mismatches.
SQLite constraints MUST NOT be presented as a sandbox for a malicious database file, and SQLite itself must be patched.

#### Scenario: A malformed grant row
- **WHEN** a stored grant row violates its field requirements
- **THEN** the protected operation reports corruption and grants no access

### Requirement: Raw database authority is private
Opening helpers, connection callbacks, raw SQL, and directory access SHALL stay inside the private storage component.
They remain trusted IO with database authority, and private packaging MUST NOT be claimed to confine a compromised process.
The collections directory and its writers are trusted; the adapter is not a sandbox against hostile filesystem replacement.

#### Scenario: Application code wants a connection
- **WHEN** a sibling component imports the storage component
- **THEN** it can reach only the exposed modules, not the hidden transaction and authority helpers
