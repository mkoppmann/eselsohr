## ADDED Requirements

### Requirement: Describing the presented grant
A protected grant description SHALL run in a deferred read transaction and require only a valid chain for the presented bearer at the sampled time.
It returns the acting grant's permissions, scope, and expiration, and MUST NOT return its petname, depth, ancestors, or any other grant.

#### Scenario: Describe an add-only grant
- **WHEN** a valid grant with only AddArticles requests its description
- **THEN** it receives AddArticles, whole-collection scope, and its expiration

#### Scenario: Describe an expired grant
- **WHEN** the presented grant's deadline is at or before the sampled time
- **THEN** the operation returns StoreDenied

### Requirement: Adding an article
A protected addition SHALL run in an immediate transaction and require AddArticles on the whole collection.
It inserts a new unread article at revision zero with a fresh random UUIDv4 identifier, unless the collection already stores an article with the same canonical URL, in which case it changes nothing.
Both outcomes return the same result, and the operation fetches nothing.

#### Scenario: Add a new URL
- **WHEN** an authorized addition submits a URL the collection does not store
- **THEN** one unread article with that URL and title exists at revision zero after commit

#### Scenario: Add a stored URL
- **WHEN** an authorized addition submits a URL the collection already stores, with another title
- **THEN** the stored article's title, read state, and revision are unchanged, and the result equals that of a new addition

#### Scenario: Add without whole-collection scope
- **WHEN** a one-article grant holding AddArticles requests an addition
- **THEN** the operation returns StoreDenied and inserts nothing

### Requirement: Changing the read state
A protected read-state change SHALL run in an immediate transaction and require ReadArticles and ChangeReadState for the sealed article.
Only after authorization MUST it check that the submitted revision is in the range zero through 9223372036854775807, then load at most two matching rows.
A missing article returns StoreNotFound.

#### Scenario: Read-state change without Read
- **WHEN** a grant with ChangeReadState but without ReadArticles requests a read-state change
- **THEN** the operation returns StoreDenied and the article is unchanged

#### Scenario: Mark read at the current revision
- **WHEN** an authorized read-state change marks an unread article read with its current revision
- **THEN** the article is read and its revision is incremented once

### Requirement: Deleting an article
A protected deletion SHALL run in an immediate transaction and require DeleteArticles for the sealed article, without ReadArticles.
It removes the article and, through the schema's cascade, the grants scoped to it and their descendants.
It takes no revision and returns the same result whether or not the article existed.

#### Scenario: Delete without Read
- **WHEN** a grant holding only DeleteArticles on an article deletes it
- **THEN** the article and the grants scoped to it, including the presented grant, no longer exist after commit

#### Scenario: Delete a missing article
- **WHEN** a whole-collection grant holding DeleteArticles deletes an article that does not exist
- **THEN** the operation returns the same result as a deletion that removed a row

### Requirement: Seals of the new operations
A read-state change SHALL seal the article identifier, requested state, and expected revision; an addition the validated URL and title; a deletion the article identifier; and a grant description no input beyond the presented bearer.
Their persistence steps MUST take target and payload only from the seal, as for reads and title changes.

#### Scenario: Add exactly the checked input
- **WHEN** an addition is authorized for one URL and title
- **THEN** the insertion uses that same URL and title

## MODIFIED Requirements

### Requirement: Named protected operations
Describing the presented grant, reading articles, adding an article, changing a title, changing the read state, and deleting an article SHALL each run as a named operation that starts its own transaction, authenticates the presented bearer, checks current authority, and immediately executes the checked request.
Every call MUST start a fresh transaction and reload authority, and no generic authorized-action framework or dispatcher is used.

#### Scenario: Repeated reads reload authority
- **WHEN** the same bearer reads twice and its grant is revoked between the calls
- **THEN** the second read is denied

### Requirement: Revision conflicts and increments
A title or read-state change SHALL return StoreConflict when the submitted revision differs from the stored revision, even if the article would be unchanged.
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

#### Scenario: A stale read-state change
- **WHEN** a read-state change submits an older revision with the current read state
- **THEN** it returns StoreConflict

### Requirement: Commit and rollback
Success SHALL be returned only after commit, and failures roll back while SQLite still reports an active transaction, including after an UPDATE, INSERT, or DELETE.
A failed addition, edit, or deletion MUST leave the collection unchanged, including after storage faults, contention, exceptions, and cancellation.
Unexpected exceptions and cancellation MUST propagate after cleanup.

#### Scenario: Cancellation after an UPDATE
- **WHEN** a title change is cancelled after its UPDATE but before commit
- **THEN** the title and revision are both rolled back

#### Scenario: COMMIT reports Busy
- **WHEN** COMMIT fails with Busy
- **THEN** the operation returns StoreBusy and no change persists

#### Scenario: Cancellation after a DELETE
- **WHEN** a deletion is cancelled after its DELETE but before commit
- **THEN** the article and the grants scoped to it still exist
