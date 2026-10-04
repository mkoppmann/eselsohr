# article-model Specification

## Purpose
The article record and its pure state transitions, independent of storage, revision metadata, and authorization, so protected operations can apply them inside their transactions.

## Requirements

### Requirement: Article fields and equality
An article SHALL consist of an article identifier, a validated title, a validated URL, a read state of Unread or Read, and a creation time.
Article equality MUST compare all five fields; identity questions compare identifiers explicitly.
Articles have no derived text representation, so a containing value cannot accidentally render the URL.

#### Scenario: Equality detects any field change
- **WHEN** two articles share an identifier but differ in any other field
- **THEN** they are not equal

### Requirement: New articles start unread
Creating an article SHALL preserve the supplied identifier, title, URL, and creation time exactly, with the initial state Unread.
The caller MUST supply the time; article functions read no clock and choose no timestamp storage encoding.

#### Scenario: Create an article
- **WHEN** an article is created from an identifier, title, URL, and time
- **THEN** it has those exact values and is Unread

### Requirement: Field-preserving edits
Renaming and setting the read state SHALL each change only their named field and preserve all other fields.
Assigning the existing title or state MUST leave the article's value unchanged, and repeating an assignment is idempotent.
There is no URL editing operation.

#### Scenario: Rename an article
- **WHEN** an article is renamed
- **THEN** only its title changes

#### Scenario: Assign the current value
- **WHEN** an article is assigned its current title or read state
- **THEN** the result equals the original article

#### Scenario: Toggle the read state twice
- **WHEN** an article's read state is changed and then changed back
- **THEN** the result equals the original article

### Requirement: Persistence ignorance
Article values and transitions SHALL carry no revision metadata, concurrency errors, or database limits.
A pure transition MUST NOT require or grant storage authority; writing its result requires a protected operation that checks authority and revision metadata inside its transaction.

#### Scenario: Edit without storage
- **WHEN** code renames an article value in memory
- **THEN** no authorization, revision, or storage input is required, and nothing is persisted
