## Purpose

Defines what holders see and can do with articles on their bearer page: listing articles, adding them manually, changing their titles and read state, and deleting them.

## ADDED Requirements

### Requirement: Articles on the bearer page
A holder whose grant has ReadArticles SHALL see the articles of the grant's scope on its page: every article of the collection for a whole-collection grant, and only the scoped article for a one-article grant.
A holder without ReadArticles MUST see no article title, URL, read state, or count.

#### Scenario: Whole-collection reader
- **WHEN** a holder with ReadArticles on the whole collection opens its page
- **THEN** the page shows every article in that collection and none from another collection

#### Scenario: Single-article reader
- **WHEN** a holder with ReadArticles on one article opens its page
- **THEN** the page shows that article and no other

#### Scenario: Holder without Read
- **WHEN** a holder without ReadArticles opens its page
- **THEN** the page shows no article contents

### Requirement: Manual article addition
A holder with AddArticles on the whole collection SHALL be able to submit a manually entered URL and title, which are validated before the addition.
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
- **THEN** the collection contains one article with that URL and both responses are the same

#### Scenario: Add-only holder submits a stored URL
- **WHEN** a holder with only AddArticles submits a URL that the collection already contains, with a different title
- **THEN** the receipt matches the receipt for a new article, and the stored article's title, read state, and revision are unchanged

### Requirement: Add-only receipt
After an addition, the holder SHALL return to its page with a fixed notice that the submission was received.
The receipt MUST NOT disclose existing titles, article counts, other article details, or reusable read authority, and submitting an article MUST NOT issue read authority to the submitter.

#### Scenario: Add-only contributor
- **WHEN** a holder with only AddArticles submits an article
- **THEN** the page shows the notice and the add form, without any stored article, count, or bearer URL with Read

### Requirement: Title edit
The page SHALL offer a title form for each listed article when the grant has ReadArticles and ChangeTitles.
A title change MUST be denied without ReadArticles, and a successful change redirects to the holder's page.

#### Scenario: Title change without Read
- **WHEN** a holder with ChangeTitles but without ReadArticles submits a title change
- **THEN** the request is denied and the article is unchanged

#### Scenario: Authorized title change
- **WHEN** a holder with both permissions submits a new valid title with the current revision
- **THEN** the title changes, the revision increments once, and the response redirects to the holder's page

### Requirement: Read-state edit
The page SHALL offer a read-state form for each listed article when the grant has ReadArticles and ChangeReadState.
A read-state change MUST be denied without ReadArticles, and a successful change redirects to the holder's page.

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
The page SHALL offer a deletion form for each listed article when the grant has ReadArticles and DeleteArticles, and one form without article contents when the grant has DeleteArticles without ReadArticles on one article.
Deleting requires DeleteArticles on the target article but not ReadArticles, takes no revision, and answers the deletion of an already missing article like any other deletion.
The response MUST NOT disclose the article's title, URL, or read state.

#### Scenario: Delete without Read
- **WHEN** a holder with only DeleteArticles on one article opens its page and submits the deletion
- **THEN** the page shows no article contents, the article is removed, and the response redirects to the deletion notice

#### Scenario: Holder without Delete
- **WHEN** a holder without DeleteArticles submits a deletion
- **THEN** the request is denied and the article remains

#### Scenario: Article outside the holder's scope
- **WHEN** a holder with DeleteArticles on one article submits the deletion of another article
- **THEN** the request is denied and the other article remains

#### Scenario: Article already deleted
- **WHEN** a holder with DeleteArticles on the whole collection submits the deletion of an article that no longer exists
- **THEN** the response is the same as for a deletion that removed an article

#### Scenario: Owner deletes an article
- **WHEN** the owner deletes an article from its page
- **THEN** the article is removed and the response redirects to the owner's page
