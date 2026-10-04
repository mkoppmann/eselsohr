## Purpose

Defines how application code reads articles and changes titles for one presented capability and collection, without receiving database connections, raw SQL, the data directory, or a clock.

## ADDED Requirements

### Requirement: Bindings for one presented bearer
Each Store binding SHALL hold one presented bearer, the collections directory, and the trusted clock, and its operations MUST act only on the collection the bearer names.
Application code selects all articles or one article identifier and never names a collection, so a binding cannot pair a bearer with another collection's file.
The bound bearer determines what each operation may do; using a Store operation grants no authority by itself.

#### Scenario: Two collections with repeated article IDs
- **WHEN** a binding for a bearer of collection A reads or changes the title of an article ID that also exists in collection B
- **THEN** the operation reads or changes only collection A

#### Scenario: Bound bearer lacks the permission
- **WHEN** application code changes a title through a binding whose bearer lacks ChangeTitles for that article
- **THEN** the operation returns EditDenied and nothing changes

### Requirement: Collection opened per operation
Each Store operation SHALL open the collection named by the bound bearer, run in one transaction, and close the connection before returning.
A binding MUST NOT hold a connection between operations, and operations under one binding do not share a snapshot.

#### Scenario: Collection file replaced between operations
- **WHEN** the operator replaces a collection file between two operations under one binding
- **THEN** the second operation opens and checks the replacement file

### Requirement: Missing collections are denied
An operation whose bound bearer names a collection without a database file SHALL report a denial identical to an unknown secret, and MUST NOT create a file.
Other failures to open the file, including symlinks, non-regular files, and filesystem errors, MUST be reported as storage faults, not denials.
A missing file is rejected before any database work, so its response time can differ from a bearer lookup.

#### Scenario: Bearer for a nonexistent collection
- **WHEN** application code reads through a binding whose bearer names a collection with no file
- **THEN** the operation returns ReadDenied and no file is created

#### Scenario: Unopenable collection file
- **WHEN** the bearer's collection file exists but is a symlink
- **THEN** the operation returns a fault, not a denial

### Requirement: No raw storage authority in application code
Application code MUST NOT receive a collection-opening function, a database connection, raw SQL, the data directory path, or a way to construct a binding.
Only the runtime and tests SHALL construct bindings, and opening MUST NOT choose a file other than the one the bound bearer names.

#### Scenario: Inspect the application-facing interface
- **WHEN** the exports of the application-facing Store interface are reviewed and checked by an external consumer
- **THEN** no connection, opening function, SQL execution, data path, clock, or binding constructor is reachable through them

### Requirement: Trusted clock from the binding
Each Store operation SHALL sample the binding's clock once inside its transaction, after the transaction begins.
The runtime binds the host clock, and tests bind fixed, advancing, or blocking clocks through the same binding.
Whoever constructs a binding chooses its clock, which runs while a write transaction holds the collection's lock, so only trusted runtime and test code supplies it.

#### Scenario: Clock sampled inside the transaction
- **WHEN** a bound clock records whether a transaction is active each time it is sampled
- **THEN** every sample during an operation occurs inside that operation's transaction

### Requirement: Authorization inside each operation's transaction
Each Store operation SHALL look up the bound bearer, load its grant and ancestors, sample a fresh time, and check current authority inside the transaction that executes the operation.
Authorization MUST NOT be a separate step whose result a later storage call trusts.

#### Scenario: Revocation between two calls
- **WHEN** an ancestor of the bound bearer is revoked after one successful call through a binding and before the next
- **THEN** the next call is denied

#### Scenario: Expiry between two calls
- **WHEN** the bound clock passes the grant's deadline between two calls through the same binding
- **THEN** the later call is denied

### Requirement: Separate read and write interfaces
Reading and writing articles SHALL be separate interfaces, each handled from a binding by its own runner.
Code run with only the read interface MUST NOT be able to call a write operation, whatever the bound bearer permits.
Type checking enforces this for cooperating code; the test suite checks a compiling positive control and does not run negative compilation tests.

#### Scenario: Read-only composition
- **WHEN** an application function is run with only the read interface handled
- **THEN** it compiles only if it uses no write operation

### Requirement: Revision metadata alongside articles
Reads through the Store SHALL return each article together with its stored revision, kept outside the article value.
Edits SHALL accept an expected revision with their other inputs and return the stored article with its resulting revision.

#### Scenario: Read and edit with the returned revision
- **WHEN** application code reads an article and submits a title change with the returned revision
- **THEN** the change succeeds and returns the article with its revision incremented once

### Requirement: Submitted revisions supply no authority
A submitted revision SHALL be treated as untrusted concurrency metadata and compared with storage only after authorization succeeds.
A mismatch MUST be rejected as a conflict, even for an apparent no-op.

#### Scenario: Unauthorized caller with the current revision
- **WHEN** a caller without the required permission submits the current revision
- **THEN** the operation returns EditDenied without comparing the revision

#### Scenario: Stale revision for an unchanged title
- **WHEN** an authorized caller submits an older revision with the article's current title
- **THEN** the operation returns EditConflict and nothing changes

### Requirement: Expected failures as small result types
Reads SHALL report ReadDenied or ReadFault, and edits EditDenied, EditNotFound, EditConflict, EditInvalidRevision, EditRevisionExhausted, or EditFault.
A fault carries Busy, Corrupt, or Unavailable; no failure contains submitted or stored data.
A storage fault MUST NOT be reported as a denial, and an unexpected exception MUST NOT become a result value.

#### Scenario: Corrupt stored row
- **WHEN** an authorized read encounters a malformed stored row
- **THEN** the operation returns ReadFault Corrupt

#### Scenario: Contention
- **WHEN** another connection holds the collection's write lock during a title change
- **THEN** the operation returns EditFault Busy immediately rather than a denial or an indefinite wait

### Requirement: Cleanup on exceptions and cancellation
When an operation fails with an unexpected exception or is cancelled, the handler SHALL roll back its transaction and close the connection before the exception propagates.
It MUST NOT catch all exceptions and report success or a fallback value.

#### Scenario: Cancellation during a write
- **WHEN** a title change is cancelled after its update and before commit
- **THEN** the title and revision are unchanged, the connection is closed, and the cancellation propagates

### Requirement: Tests through the production handler
Tests SHALL run application functions through the production SQLite handler with a test clock and a temporary collections directory.
External behavior MUST be replaced only through the binding, without a separate test handler or a second application hierarchy.

#### Scenario: Application function under a test binding
- **WHEN** the common application function runs through the SQLite handler with a fixed clock and a temporary database
- **THEN** it reads an article and changes its title through the same code path the runtime uses

### Requirement: No unrestricted IO in application logic
Application functions SHALL use only the Store interfaces and other narrow interfaces they need, and MUST NOT perform unrestricted IO.
Unrestricted IO belongs to handlers and runtime composition.
These interface restrictions limit cooperating code, not a compromised process.

#### Scenario: Review application signatures
- **WHEN** the signatures of application functions are reviewed
- **THEN** each names only the interfaces it uses and none permits arbitrary IO
