## ADDED Requirements

### Requirement: Own-grant description
A separate read-only grant interface SHALL describe the bound bearer's own grant: its permissions, its scope, and its expiration, if any.
It MUST NOT reveal the grant's petname, ancestors, or depth, or any other grant.
An unknown, expired, or revoked bearer and a bearer for a missing collection MUST all report ReadDenied.

#### Scenario: Describe a one-article grant
- **WHEN** application code describes a bound bearer whose grant has DeleteArticles on one article
- **THEN** it receives that permission, the article's scope, and the grant's expiration, and no petname or ancestor

#### Scenario: Describe a revoked grant
- **WHEN** application code describes a bound bearer whose ancestor has been revoked
- **THEN** it receives ReadDenied

### Requirement: Additions and deletions return no stored data
An addition SHALL succeed in the same way for a new URL and a URL the collection already stores, returning no article or revision.
A deletion SHALL succeed in the same way whether or not the article still existed.

#### Scenario: Add a stored URL
- **WHEN** application code adds a URL that the collection already stores
- **THEN** it receives the same result as for a new URL

#### Scenario: Delete a missing article
- **WHEN** application code with DeleteArticles on the whole collection deletes an article that does not exist
- **THEN** it receives the same result as for an existing article

## MODIFIED Requirements

### Requirement: Separate read and write interfaces
Describing the bound grant, reading articles, and writing articles SHALL be separate interfaces, each handled from a binding by its own runner.
Code run with only the read interfaces MUST NOT be able to call a write operation, whatever the bound bearer permits.
Type checking enforces this for cooperating code; the test suite checks a compiling positive control and does not run negative compilation tests.

#### Scenario: Read-only composition
- **WHEN** an application function is run with only the read interfaces handled
- **THEN** it compiles only if it uses no write operation

### Requirement: Revision metadata alongside articles
Reads through the Store SHALL return each article together with its stored revision, kept outside the article value.
Title and read-state edits SHALL accept an expected revision with their other inputs and return the stored article with its resulting revision.
Additions and deletions take no revision.

#### Scenario: Read and edit with the returned revision
- **WHEN** application code reads an article and submits a title change with the returned revision
- **THEN** the change succeeds and returns the article with its revision incremented once

### Requirement: Expected failures as small result types
Reads and grant descriptions SHALL report ReadDenied or ReadFault, and title and read-state edits EditDenied, EditNotFound, EditConflict, EditInvalidRevision, EditRevisionExhausted, or EditFault.
Additions and deletions SHALL report EditDenied or EditFault.
A fault carries Busy, Corrupt, or Unavailable; no failure contains submitted or stored data.
A storage fault MUST NOT be reported as a denial, and an unexpected exception MUST NOT become a result value.

#### Scenario: Corrupt stored row
- **WHEN** an authorized read encounters a malformed stored row
- **THEN** the operation returns ReadFault Corrupt

#### Scenario: Contention
- **WHEN** another connection holds the collection's write lock during a title change
- **THEN** the operation returns EditFault Busy immediately rather than a denial or an indefinite wait
