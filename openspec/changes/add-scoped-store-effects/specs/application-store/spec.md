## Purpose

Defines how application code runs protected Store operations for one presented capability and collection, without receiving database connections, raw SQL, or the data directory.

## ADDED Requirements

### Requirement: Handlers bound to one capability and collection
Each Store handler SHALL be bound to one presented bearer and the collection it names.
Operations run through the handler MUST act only on that collection, and the bound bearer MUST determine what they may do.
Using a Store operation in application code grants no authority by itself.

#### Scenario: Two collections with repeated article IDs
- **WHEN** a handler bound to a bearer for collection A runs a read or title change with an article ID that also exists in collection B
- **THEN** the operation reads or changes only collection A

#### Scenario: Bound bearer lacks the permission
- **WHEN** application code calls the title change through a handler bound to a bearer without ChangeTitles for that article
- **THEN** the operation is denied and nothing changes

### Requirement: No raw storage authority in application code
Application code MUST NOT receive a general collection-opening function, a database connection, raw SQL, or the data directory path.
Only the runtime SHALL turn a parsed collection locator into an open collection, and it MUST NOT choose a file other than the one the locator names.

#### Scenario: Inspect the application-facing interface
- **WHEN** the exports of the application-facing Store interface are reviewed and checked by an external consumer
- **THEN** no connection, opening function, SQL execution, or data path is reachable through them

### Requirement: Authorization inside each operation's transaction
Each Store operation SHALL look up the bound bearer, load its grant and ancestors, sample a fresh time, and check current authority inside the transaction that executes the operation.
Authorization MUST NOT be a separate step whose result a later storage call trusts.

#### Scenario: Revocation between two calls
- **WHEN** an ancestor of the bound bearer is revoked after one successful call through a handler and before the next
- **THEN** the next call is denied

#### Scenario: Expiry between two calls
- **WHEN** the bound grant's deadline passes between two calls through the same handler
- **THEN** the later call is denied

### Requirement: Revision metadata alongside articles
Reads through the Store SHALL return each article together with its stored revision, kept outside the article value.
Edits SHALL accept an expected revision with their other inputs.

#### Scenario: Read and edit with the returned revision
- **WHEN** application code reads an article and submits a title change with the returned revision
- **THEN** the change succeeds and the stored revision increments once

### Requirement: Submitted revisions supply no authority
A submitted revision SHALL be treated as untrusted concurrency metadata and compared with storage only after authorization succeeds.
A mismatch MUST be rejected as a conflict, even for an apparent no-op.

#### Scenario: Unauthorized caller with the current revision
- **WHEN** a caller without the required permission submits the current revision
- **THEN** the operation is denied without comparing the revision

#### Scenario: Stale revision for an unchanged title
- **WHEN** an authorized caller submits an older revision with the article's current title
- **THEN** the operation reports a conflict and nothing changes

### Requirement: Expected failures as small result types
Store operations SHALL report expected input, authorization, absence, and conflict failures as small result values that contain no submitted or stored data.
A storage fault MUST NOT be reported as a permission denial, and an unexpected exception MUST NOT become a domain result.

#### Scenario: Corrupt stored row
- **WHEN** an authorized read encounters a malformed stored row
- **THEN** the operation reports corruption, not denial or success

#### Scenario: Contention
- **WHEN** another connection holds the collection's write lock
- **THEN** the operation reports a retryable busy result rather than a denial or an indefinite wait

### Requirement: Cleanup on exceptions and cancellation
When an operation fails with an unexpected exception or is cancelled, the handler SHALL roll back its transaction and release the collection before the exception propagates.
It MUST NOT catch all exceptions and report success or a fallback value.

#### Scenario: Cancellation during a write
- **WHEN** a title change is cancelled after its update and before commit
- **THEN** the title and revision are unchanged, the connection is released, and the cancellation propagates

### Requirement: Same application function under production and test handlers
The production SQLite handler and a deterministic test handler SHALL run the same application function.
Test handlers MUST replace only external behavior, not reimplement application logic in a second hierarchy.

#### Scenario: Run one function with both handlers
- **WHEN** the common application function runs with the SQLite handler on a real temporary database and with the test handler
- **THEN** both runs exercise the same function and their results follow the same contract

### Requirement: No unrestricted IO in application logic
Application functions SHALL use only the Store facets and other narrow interfaces they need, and MUST NOT perform unrestricted IO.
Unrestricted IO belongs to handlers and runtime composition.
These interface restrictions limit cooperating code, not a compromised process.

#### Scenario: Review application signatures
- **WHEN** the signatures of application functions are reviewed
- **THEN** each names only the facets it uses and none permits arbitrary IO
