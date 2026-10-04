## Purpose

Defines how holders share access by issuing bounded child links, how long those links live, and how holders inspect and revoke the links descending from their own.

## ADDED Requirements

### Requirement: Child link issuance
Issuing a child link SHALL validate the presenting grant, its ancestors, and the requested child inside one write transaction before inserting the child's metadata, permissions, and hash.
The child MUST NOT gain permissions, widen its scope, outlive its parent, or exceed the delegation depth.

#### Scenario: Issue a narrower link
- **WHEN** a holder with Delegate requests a child with a subset of its permissions, a scope within its own, and a deadline no later than its own
- **THEN** the child is stored and its URL is shown

#### Scenario: Revocation during issuance
- **WHEN** an ancestor of the presenting grant is revoked before the issuance transaction begins
- **THEN** issuance is denied and no child is stored

### Requirement: Fresh link secrets
Each issued link's secret SHALL consist of 256 bits from a cryptographically secure random generator, generated in trusted issuance code.
Parsing or round-trip tests cannot establish this entropy.

#### Scenario: Two issued links
- **WHEN** a holder issues two links with identical settings
- **THEN** they have independently generated secrets and different grant IDs

### Requirement: One-time link display
A newly issued URL SHALL be shown once.
Storage MUST keep only the secret's hash, and later management can display metadata, revoke a link, or issue a replacement, but MUST NOT recover the original URL.

#### Scenario: Return to the management page
- **WHEN** a holder views the link list after issuing a link
- **THEN** the list shows the link's metadata and no usable bearer URL

### Requirement: No recoverable bearer after an interrupted response
An interrupted issuance response MUST NOT cause Eselsohr to store or later reveal a recoverable bearer.
The holder can revoke the unseen grant and issue another.

#### Scenario: Response lost after commit
- **WHEN** the connection drops after the issuance transaction commits and before the holder sees the URL
- **THEN** the stored grant has only its hash, and the holder can revoke it through the link list

### Requirement: Everyday link lifetimes
Links issued by the owner SHALL default to expiring seven days after issuance, and the owner MAY explicitly choose a longer lifetime.
Every child link MUST have a finite deadline, and a link is expired when the current time equals or passes its deadline.

#### Scenario: Default owner-issued link
- **WHEN** the owner issues a working link without choosing a lifetime
- **THEN** the link expires seven days after issuance

#### Scenario: At the deadline
- **WHEN** a request presents a link exactly at its deadline
- **THEN** the request is denied

### Requirement: Delegation controls issuance, not copying
Delegate SHALL control only the issuance of new links.
Copying an existing bearer URL remains possible without Delegate and grants exactly that URL's authority.

#### Scenario: Holder without Delegate
- **WHEN** a holder without Delegate requests a child link
- **THEN** issuance is denied

### Requirement: Descendant metadata inspection
Inspecting links SHALL require InspectLinks and SHALL show metadata only for proper descendants of the presenting grant, including expired ones; the owner may also see its own root metadata.
Inspection MUST NOT reconstruct bearer URLs or return ancestor or sibling grants.

#### Scenario: Delegated holder inspects links
- **WHEN** a delegated holder with InspectLinks opens its link list
- **THEN** it sees metadata for its descendants and nothing about its ancestors, its siblings, or itself

#### Scenario: Inspection grants no Read
- **WHEN** a holder with InspectLinks but without ReadArticles opens its link list
- **THEN** no article content is shown

### Requirement: Subtree revocation
Revoking a link SHALL require RevokeLinks and a proper descendant relationship, and SHALL delete the selected grant and all its descendants in one transaction.
The root cannot be revoked through this operation.

#### Scenario: Revoke an intermediate link
- **WHEN** a holder revokes a descendant that has children of its own
- **THEN** the descendant and every link below it stop authorizing requests, and other links remain unchanged

#### Scenario: Attempt to revoke an ancestor
- **WHEN** a delegated holder attempts to revoke its own grant, an ancestor, or a sibling
- **THEN** the request is denied and nothing is deleted

### Requirement: Revocation ordering
An operation whose protected authorization occurs after a revocation commits MUST fail.
An operation ordered before the revocation may finish, and revocation cannot recall data already delivered.

#### Scenario: Edit after revocation
- **WHEN** a revocation of a link's ancestor commits and the link's holder then submits an edit
- **THEN** the edit is denied

### Requirement: Immutable parent links
Stored grants SHALL keep the parent they were issued under.
Operations, including revocation, MUST NOT change a stored grant's parent.

#### Scenario: Revocation leaves other parents unchanged
- **WHEN** a subtree is revoked
- **THEN** every remaining grant keeps its original parent
