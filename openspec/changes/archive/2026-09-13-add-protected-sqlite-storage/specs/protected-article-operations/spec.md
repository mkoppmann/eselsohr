## Purpose

Named protected operations that authenticate a presented bearer, check current authority, and apply exactly the checked request in one SQLite transaction, currently reading articles and changing a title.

## ADDED Requirements

### Requirement: Named protected operations
Reading articles and changing a title SHALL each run as a named operation that starts its own transaction, authenticates the presented bearer, checks current authority, and immediately executes the checked request.
Every call MUST start a fresh transaction and reload authority, and no generic authorized-action framework or dispatcher is used.

#### Scenario: Repeated reads reload authority
- **WHEN** the same bearer reads twice and its grant is revoked between the calls
- **THEN** the second read is denied

### Requirement: Sealed requests
Each operation SHALL seal its exact inputs in a private, operation-specific request: a read seals the requested scope, and a title change seals the article identifier, validated title, and expected revision.
The persistence step MUST take its target and payload only from the seal, without accepting replacement values.
A seal is neither a reusable bearer token nor a proof of single use.

#### Scenario: Execute exactly the checked payload
- **WHEN** a title change is authorized for one article and title
- **THEN** the write uses that same article, title, and expected revision

#### Scenario: Forge a seal
- **WHEN** source review checks the exports of the protected operations
- **THEN** no sealed request constructor, record-update field, or reconstruction instance is available outside the trusted authorization code

### Requirement: Requests stay in their transaction
A sealed request and the actions that use it SHALL be executable only in the transaction that checked them, including through coercion.
Write actions MUST NOT run through the read transaction runner.
The interface exposes no connection access, context replacement, IO lifting, or runner for an individual action.
A caller that hides a request's transaction type can keep the value but cannot execute it elsewhere or unwrap it into independent IO.

#### Scenario: Reuse a request in another transaction
- **WHEN** source review checks the runner types and the nominal roles of the mode and transaction parameters
- **THEN** a request from one transaction cannot be executed in another, and a read action cannot be coerced into a write action

### Requirement: Transaction preconditions
A protected operation SHALL reject an existing transaction on its connection without changing it, returning StoreTransactionActive.
It MUST verify the opener's connection settings and a supported journal mode other than OFF before beginning, and it does not retry contention.
Reads begin a deferred transaction and change no connection settings.

#### Scenario: A caller-owned transaction
- **WHEN** a protected read is called on a connection inside an active transaction
- **THEN** it returns StoreTransactionActive and leaves that transaction unchanged

### Requirement: Current authority inside the transaction
Inside its transaction, a protected operation SHALL recheck collection identity and schema version, look up the bearer, and follow stored parent identifiers to the root.
Ancestry loading MUST detect cycles, load at most five grants with the per-lookup row limits, and require the terminal root to be the collection's sole root.
The operation then samples the clock once and checks the chain and the operation policy before sealing the request.

#### Scenario: A delegated reader at maximum depth
- **WHEN** a depth-four grant with valid ancestors and ReadArticles reads within its scope
- **THEN** the read succeeds

#### Scenario: An expired ancestor
- **WHEN** any ancestor's deadline is at or before the sampled time
- **THEN** the operation returns StoreDenied

### Requirement: Reading articles
A protected read SHALL require ReadArticles and containment of the requested scope in the grant's scope.
One-article reads MUST be restricted in SQL, and every returned row, including its revision, is validated before commit, with duplicate article identifiers treated as corruption.
An authorized missing article or empty collection returns an empty list; results include revisions, without pagination or a defined order.

#### Scenario: Add-only access cannot read
- **WHEN** a grant holding only AddArticles requests a read
- **THEN** the read returns StoreDenied

#### Scenario: Read a missing article
- **WHEN** an authorized one-article read names an article that does not exist
- **THEN** it returns an empty list

### Requirement: Changing a title
A protected title change SHALL run in an immediate transaction and require ReadArticles and ChangeTitles for the sealed article.
Only after authorization MUST it check that the submitted revision is in the range zero through 9223372036854775807, then load at most two matching rows.
Submitted revisions are untrusted concurrency metadata and supply no authority.

#### Scenario: Revision check after authorization
- **WHEN** a grant without ChangeTitles submits an out-of-range revision
- **THEN** the operation returns StoreDenied rather than StoreInvalidRevision

#### Scenario: A missing article
- **WHEN** an authorized title change names an article that does not exist
- **THEN** it returns StoreNotFound

### Requirement: Revision conflicts and increments
A title change SHALL return StoreConflict when the submitted revision differs from the stored revision, even if the title would be unchanged.
A matching no-op MUST return the current article without an UPDATE, including at the maximum revision.
A real change increments the revision once, and a real change at the maximum returns StoreRevisionExhausted without writing.
The conditional UPDATE must affect exactly one row, or the transaction fails as StoreCorrupt.

#### Scenario: A stale no-op
- **WHEN** a title change submits an older revision with the current title
- **THEN** it returns StoreConflict

#### Scenario: A matching no-op at the maximum revision
- **WHEN** a title change submits the current maximum revision and the current title
- **THEN** it returns the article unchanged without writing

#### Scenario: Retrying a completed change
- **WHEN** a request that already changed the article is repeated with its original revision
- **THEN** it returns StoreConflict and rolls back

### Requirement: Write connection settings
Before beginning a write, the operation SHALL require existing DELETE journaling and set and read back synchronous EXTRA and secure_delete ON.
Other journal modes MUST be rejected without converting the file, and the journal mode and opener settings are rechecked inside the transaction.
Crash and power-loss durability and secure erasure are unverified; on macOS, `fsync` does not ask the drive to flush its write cache, and `fullfsync` stays at its default of OFF.

#### Scenario: A WAL database
- **WHEN** a title change runs on a database in WAL journal mode
- **THEN** it returns StoreFault without changing the journal mode or the article

### Requirement: Commit and rollback
Success SHALL be returned only after commit, and failures roll back while SQLite still reports an active transaction, including after an UPDATE.
Unexpected exceptions and cancellation MUST propagate after cleanup.

#### Scenario: Cancellation after an UPDATE
- **WHEN** a title change is cancelled after its UPDATE but before commit
- **THEN** the title and revision are both rolled back

#### Scenario: COMMIT reports Busy
- **WHEN** COMMIT fails with Busy
- **THEN** the operation returns StoreBusy and no change persists

### Requirement: Error categories
Protected operation errors SHALL contain no submitted or stored values.
Unknown bearers, missing ancestors, rejected chains, and policy failures MUST deny access; malformed rows, duplicate matches, and inconsistent roots are corruption.
Busy and Locked are contention, while unsupported schema, other SQL failures, and cleanup failures are faults.

#### Scenario: A duplicate article row
- **WHEN** storage returns two rows for one article identifier
- **THEN** the operation returns StoreCorrupt

### Requirement: Ordering with revocation
Authorization and the article access SHALL read the same database state within one transaction.
An operation ordered before a revocation may finish, and an operation whose authorization occurs after the revocation commits MUST fail.
Competing title changes serialize, and a stale retry conflicts.

#### Scenario: Revocation commits first
- **WHEN** an ancestor revocation commits before a title change acquires the write lock
- **THEN** the title change returns StoreDenied

#### Scenario: Two competing edits
- **WHEN** a second connection submits a title change while the first holds the write lock
- **THEN** the second returns StoreBusy immediately, and retrying it with the old revision after the first commits returns StoreConflict

### Requirement: Caller contract
The caller SHALL use the connection exclusively for the operation and leave its opening callback after a failure so the connection closes.
The trusted clock and exclusive connection use MUST remain interpreter responsibilities; the types do not sandbox arbitrary IO.

#### Scenario: A failed operation
- **WHEN** a protected operation fails
- **THEN** the caller leaves the opening callback, and the opener closes the connection
