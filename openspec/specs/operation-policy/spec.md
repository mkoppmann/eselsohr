# operation-policy Specification

## Purpose
The pure decision of whether a validated grant chain may perform a named operation on its target, covering article access, issuance, link management, and owner deletion.

## Requirements

### Requirement: Actor revalidation
Every decision SHALL revalidate the acting chain at the supplied decision time before examining the operation.
The first grant in the chain supplies the acting grant identifier, collection, permissions, and scope.
Read and article targets MUST include their collection, and no article identifier is interpreted across collections.

#### Scenario: The actor expired after validation
- **WHEN** a chain validated earlier is decided at a time at or after the acting grant's expiration
- **THEN** the decision returns ActorChainRejected

#### Scenario: An article in another collection
- **WHEN** a grant for one collection requests an operation on an article identifier in another collection
- **THEN** the decision returns OperationDenied

### Requirement: Reading articles
Reading SHALL require ReadArticles and containment of the requested scope in the acting scope.
A one-article grant with ReadArticles can read that article but MUST NOT request a whole-collection read.

#### Scenario: A one-article reader asks for the collection
- **WHEN** a one-article grant with ReadArticles requests a whole-collection read
- **THEN** the decision returns OperationDenied

### Requirement: Adding articles
Adding an article SHALL require AddArticles on the whole target collection.
Add-only access MUST NOT allow reading other articles or issuing a child that contains ReadArticles.

#### Scenario: A one-article grant cannot add
- **WHEN** a one-article grant holding AddArticles requests an add
- **THEN** the decision returns OperationDenied

#### Scenario: An add-only grant cannot mint Read
- **WHEN** a grant holding only AddArticles and Delegate requests a child containing ReadArticles
- **THEN** issuance is rejected

### Requirement: Editing articles
Renaming an article SHALL require ReadArticles and ChangeTitles on that article, and changing its read state SHALL require ReadArticles and ChangeReadState on that article.
Read permission MUST be required even when the edit is a no-op.

#### Scenario: A write permission without Read
- **WHEN** a grant holding ChangeTitles but not ReadArticles requests a rename
- **THEN** the decision returns OperationDenied

### Requirement: Deleting articles
Deleting an article SHALL require DeleteArticles on that article; ReadArticles MUST NOT be required.

#### Scenario: Delete without Read
- **WHEN** a grant holding only DeleteArticles on an article requests its deletion
- **THEN** the decision allows it

### Requirement: Issuing a child
Issuance SHALL require Delegate and a valid complete chain with the proposed child prepended, checking the child's identifier, parent, collection, scope, permissions, depth, and deadline.
Reuse of any ancestor identifier MUST be rejected.
Article existence and identifier uniqueness outside that path remain transaction checks.

#### Scenario: A child reuses an ancestor identifier
- **WHEN** a proposed child has the identifier of one of its ancestors
- **THEN** the decision returns ChildChainRejected

### Requirement: Managing descendant links
Inspecting a link SHALL require InspectLinks and revoking a link SHALL require RevokeLinks, checked before the target path.
The target identifier and its complete leaf-to-root path MUST pass structural validation, which permits expired targets and intermediate descendants, and the path must contain the acting grant and all its ancestors with every field matching the acting chain.
Holders may manage proper descendants only, never themselves, ancestors, or siblings.
InspectLinks and RevokeLinks are effective only together with Delegate, because a grant without Delegate has no valid descendants.

#### Scenario: Matching identifiers with different fields
- **WHEN** a target path contains the acting grant's identifier but different permissions, scope, or deadline for it
- **THEN** the decision rejects the request

#### Scenario: Revoke an expired descendant
- **WHEN** an unexpired grant with RevokeLinks targets an expired proper descendant whose path is structurally valid
- **THEN** the decision allows it

#### Scenario: Management without Delegate
- **WHEN** a grant holding InspectLinks and RevokeLinks but not Delegate targets a claimed child
- **THEN** the decision rejects the request

#### Scenario: An expired manager
- **WHEN** an expired acting grant targets a descendant
- **THEN** the decision returns ActorChainRejected

### Requirement: Owner root metadata
The owner SHALL be able to inspect its own root metadata and manage all proper descendants.
The root MUST NOT be revocable through link revocation; root replacement belongs to an operator reset.
InspectLinks and RevokeLinks grant neither article Read nor recovery of an original bearer URL, and every parent on a managed path must still hold Delegate.

#### Scenario: The owner tries to revoke the root
- **WHEN** a root-only chain requests revocation of the root itself
- **THEN** the decision returns OperationDenied

### Requirement: Owner-only collection deletion
Deleting a whole collection SHALL require a root-only acting chain for that collection.
A delegated grant MUST NOT gain this authority, even with all eight permissions.

#### Scenario: A delegated full permission set
- **WHEN** a depth-one grant holding all eight permissions requests collection deletion
- **THEN** the decision returns OperationDenied

### Requirement: Monotone policy
Removing permissions from a grant SHALL never enable an operation.
A reduction that invalidates ancestry MUST deny access.
An ancestor in a valid chain SHALL be allowed every operation its descendant is allowed at the same time, except issuing a child that names the descendant as its parent.

#### Scenario: Subset pairs
- **WHEN** any operation is decided for each of the 6,561 pairs of nested permission sets across whole-collection and two article scopes
- **THEN** whenever the smaller set is allowed, the larger set is also allowed

#### Scenario: Ancestors dominate descendants
- **WHEN** a descendant in a generated grant tree is allowed an article, collection, or link-management operation
- **THEN** each of its ancestors is also allowed that operation

### Requirement: Policy errors and results
Policy errors SHALL contain no submitted grants, identifiers, article values, or bearer data.
ActorChainRejected, ChildChainRejected, and TargetChainRejected wrap the relevant chain error, and OperationDenied covers failed permission, scope, ownership, and relationship checks.
The actor MUST be checked first, and permission checks precede target-path validation.
Success returns no data and performs no effect.

#### Scenario: A malformed target path without permission
- **WHEN** a grant without InspectLinks requests inspection with a malformed target path
- **THEN** the decision returns OperationDenied rather than a path error

### Requirement: Policy inputs are not authority
The decision SHALL use only the grants and time supplied by its caller, without checking existence, quotas, schema state, global grant uniqueness, titles, URLs, read-state values, or revisions.
It MUST NOT be treated as an authorized-action wrapper or as a sealed request for a protected operation.
Grant records carry no revocation state, so callers MUST supply only grants that are currently stored; the decision cannot distinguish a revoked grant from a valid one.

#### Scenario: Pure decisions on fabricated grants
- **WHEN** a caller supplies grant records that were never stored
- **THEN** the decision evaluates them as policy data and establishes nothing about bearer possession

#### Scenario: A revoked grant supplied as policy data
- **WHEN** a caller supplies a grant that has been revoked in storage
- **THEN** the decision evaluates it like any other grant, and only the caller's reload inside the transaction can deny access
