## Purpose

Defines the protected article operations a holder performs through Eselsohr: listing articles, adding them manually, changing their titles and read state, and deleting them, each checked against current authority in its own transaction.

## ADDED Requirements

### Requirement: Protected article list
Listing SHALL require ReadArticles and return only the articles within the requested scope, which must lie within the holder's scope.
An authorized request for an empty collection or a missing article returns an empty list.

#### Scenario: Whole-collection reader
- **WHEN** a holder with ReadArticles on the whole collection opens the article list
- **THEN** the page shows every article in that collection and none from another collection

#### Scenario: Single-article reader
- **WHEN** a holder with ReadArticles on one article requests the whole collection
- **THEN** the request is denied

#### Scenario: Holder without Read
- **WHEN** a holder without ReadArticles requests the article list
- **THEN** the request is denied and no article is shown

### Requirement: Manual article addition
Adding SHALL accept a manually entered URL and title, validate them, and check collection-wide AddArticles inside the write transaction that inserts the article.
A new article starts unread at revision zero.
Adding requires no Read permission and never fetches the URL.

#### Scenario: Add an article
- **WHEN** a holder with collection-wide AddArticles submits a valid URL and title
- **THEN** the collection contains a new unread article with that URL and normalized title at revision zero

#### Scenario: Single-article holder
- **WHEN** a holder whose scope is one article submits an addition
- **THEN** the request is denied and nothing is inserted

#### Scenario: Invalid input
- **WHEN** the submitted URL or title fails validation
- **THEN** nothing is inserted

### Requirement: Duplicate URLs
Adding a URL whose canonical rendering matches an article already in the collection SHALL store no second article and leave the existing article unchanged, discarding the submitted title.
The duplicate check SHALL cover the whole collection, not only the holder's scope.
The response MUST NOT differ according to whether such an article exists.

#### Scenario: Add a URL twice
- **WHEN** a holder adds the same URL twice
- **THEN** the collection contains one article with that URL and both responses have the same form

#### Scenario: Add-only holder submits a stored URL
- **WHEN** a holder with only AddArticles submits a URL that the collection already contains, with a different title
- **THEN** the receipt matches the receipt for a new article, and the stored article's title, read state, and revision are unchanged

### Requirement: Add-only receipt
The response to an addition SHALL NOT disclose existing titles, article counts, other article details, or reusable read authority.
Submitting an article MUST NOT issue read authority to the submitter.

#### Scenario: Add-only contributor
- **WHEN** a holder with only AddArticles submits an article
- **THEN** the receipt confirms the submission without showing any stored article, count, or bearer URL with Read

### Requirement: Title edit
Changing a title SHALL require ReadArticles and ChangeTitles for the target article, checked inside the write transaction that applies the change.

#### Scenario: Title change without Read
- **WHEN** a holder with ChangeTitles but without ReadArticles submits a title change
- **THEN** the request is denied and the article is unchanged

#### Scenario: Authorized title change
- **WHEN** a holder with both permissions submits a new valid title with the current revision
- **THEN** the title changes, the revision increments once, and the response redirects to a server-generated page

### Requirement: Read-state edit
Changing an article's read state SHALL require ReadArticles and ChangeReadState for the target article, checked inside the write transaction that applies the change.

#### Scenario: Mark an article read
- **WHEN** a holder with both permissions marks an unread article read with the current revision
- **THEN** the article becomes read and its revision increments once

#### Scenario: Read-state change without Read
- **WHEN** a holder with ChangeReadState but without ReadArticles submits a read-state change
- **THEN** the request is denied and the article is unchanged

### Requirement: Stale edits rejected
Title and read-state edits SHALL compare the submitted revision with storage after authorization and reject a mismatch, even when the edit would change nothing.
A matching no-op MUST keep the revision unchanged.

#### Scenario: Stale form
- **WHEN** a holder submits an edit form whose revision is older than the stored revision
- **THEN** the edit is rejected as a conflict and the article is unchanged

#### Scenario: Matching no-op
- **WHEN** a holder submits the current title or read state with the current revision
- **THEN** the article and its revision are unchanged

### Requirement: Article deletion
Deleting an article SHALL require DeleteArticles on the target article, checked inside the write transaction that removes it; ReadArticles MUST NOT be required.
The deletion request SHALL use an explicit POST route and end with a server-generated redirect.
The response MUST NOT disclose the article's title, URL, or read state.

#### Scenario: Delete without Read
- **WHEN** a holder with only DeleteArticles on one article deletes it
- **THEN** the article is removed and the response shows none of its contents

#### Scenario: Holder without Delete
- **WHEN** a holder without DeleteArticles submits a deletion
- **THEN** the request is denied and the article remains

#### Scenario: Article outside the holder's scope
- **WHEN** a holder with DeleteArticles on one article submits the deletion of another article
- **THEN** the request is denied and the other article remains

### Requirement: No partial state on failure
A failed addition, edit, or deletion MUST leave the collection unchanged, including after storage faults, contention, exceptions, and cancellation.

#### Scenario: Failure after the write
- **WHEN** an edit fails or is cancelled after its update and before commit
- **THEN** neither the article nor its revision changes
