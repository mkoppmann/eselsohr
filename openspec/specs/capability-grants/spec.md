# capability-grants Specification

## Purpose
Permissions, collection-bound scopes, and grant records, with the local and complete-chain checks that keep delegated authority from growing in rights, target, depth, or lifetime.

## Requirements

### Requirement: Permission vocabulary
Grants SHALL use sets drawn from eight permissions: ReadArticles, AddArticles, ChangeTitles, ChangeReadState, DeleteArticles, Delegate, InspectLinks, and RevokeLinks.
Every combination MUST be representable, including the empty set.
Permission inclusion is set inclusion, with the candidate first and its containing parent second; it adds no implied rights and does not require Delegate.
Ordering of sets or records MUST NOT express inclusion.

#### Scenario: An add-only set excludes Read
- **WHEN** a set containing only AddArticles is compared with a set containing ReadArticles
- **THEN** neither includes the other

#### Scenario: Inclusion agrees with membership
- **WHEN** any two of the 256 permission sets are compared
- **THEN** inclusion holds exactly when every member of the candidate is a member of the parent

### Requirement: Collection-bound scopes
A scope SHALL be either a whole collection or one article within a named collection.
Scopes from different collections MUST never include each other, even when their article identifiers match.
Within one collection, a whole-collection scope includes itself and every one-article scope, and a one-article scope includes only the same article.
Scope equality compares complete contents, and scopes carry no ordering.

#### Scenario: Repeated article identifiers across collections
- **WHEN** two one-article scopes name the same article identifier in different collections
- **THEN** neither includes the other

#### Scenario: An article scope cannot widen
- **WHEN** a whole-collection scope is compared as a candidate against a one-article scope in the same collection
- **THEN** inclusion fails

### Requirement: Root grant fields
A root grant SHALL have no parent, whole-collection scope, all eight permissions, no expiration, and depth zero.
A grant record that fails any of these field requirements MUST NOT be accepted as a root.

#### Scenario: A narrowed root is invalid
- **WHEN** a parentless grant lacks a permission, has an article scope, an expiration, or a nonzero depth
- **THEN** validation rejects it as a root

### Requirement: Child grant fields
A child grant SHALL name a parent different from its own identifier, have a finite expiration, and have depth one through four.
Depth bounds MUST be checked before incrementing, including for extreme integer inputs.

#### Scenario: A child without a deadline is invalid
- **WHEN** a grant names a parent but has no expiration
- **THEN** validation rejects it as a child

### Requirement: Local attenuation
A child grant SHALL be accepted under a parent only when it neither gains permissions, widens its scope, nor outlives its parent.
The parent MUST be unexpired, hold Delegate, and be at depth below four, and the child's depth is the parent's plus one.
Deadlines are strict: a grant is valid only while the check time is before its expiration, so equality is expired.

#### Scenario: A child may share its parent's deadline
- **WHEN** a child's expiration equals its parent's
- **THEN** attenuation accepts that edge if the other checks pass

#### Scenario: Expiration at the check time
- **WHEN** a parent or child expires exactly at the supplied time
- **THEN** attenuation rejects the edge as expired

#### Scenario: Depth-four grants cannot delegate
- **WHEN** a depth-four grant holding Delegate is supplied as a parent
- **THEN** attenuation rejects the edge with DepthExhausted

### Requirement: Parent attenuation errors
Local attenuation SHALL check the parent first and return the first applicable parent error in this order: InvalidParent (the parent fails the root or child field requirements), ParentExpired, DelegateRequired, and DepthExhausted (the parent is at depth four).
Errors MUST be nullary values without submitted input.

#### Scenario: Parent problems precede child problems
- **WHEN** an expired parent is supplied with a child that widens its scope
- **THEN** attenuation returns ParentExpired

### Requirement: Child attenuation errors
After the parent checks pass, local attenuation SHALL return the first applicable child error in this order: InvalidChild (the child fails the child field requirements, including a root supplied as a child), ChildExpired, ParentMismatch (the child names another parent, or both identifiers match), DepthMismatch, ScopeWidened, PermissionsIncreased, and ChildOutlivesParent.
It MUST return success only when all checks pass.

#### Scenario: Rights before lifetime
- **WHEN** a child both gains a permission and outlives its parent
- **THEN** attenuation returns PermissionsIncreased

### Requirement: Local attenuation establishes one edge
A successful attenuation check SHALL establish only that one parent-child edge is valid at the supplied time.
It MUST NOT be treated as checking ancestry, root uniqueness, existence, revocation, or bearer possession.
Grant records are publicly constructible without validation; the check reads no clock and issues or persists nothing.

#### Scenario: A valid edge under an invalid ancestor
- **WHEN** an edge passes attenuation but its parent's own parent is revoked
- **THEN** attenuation still succeeds, and only chain validation in a protected operation can deny access

### Requirement: Bounded chain input
Chain validation SHALL take the check time, expected collection, requested grant identifier, and one to five grants in leaf-to-root order.
It MUST inspect at most six list cells before reading grant fields and reject oversized input without inspecting its remaining tail.
This bounds the list traversal, not the evaluation of arbitrary undefined values.

#### Scenario: An infinite list is rejected
- **WHEN** validation receives an unbounded list of grants
- **THEN** it returns ChainTooLong after inspecting at most six list cells

### Requirement: Every edge is attenuated
Chain validation SHALL check every edge with local attenuation at the supplied time, from the root toward the leaf, preserving the attenuation error order.
Entries MUST NOT be sorted, truncated, repaired, or ignored.
A root-only chain is valid, and the leaf need not hold Delegate unless it is the root.

#### Scenario: A missing intermediate parent
- **WHEN** a chain omits a grant between the leaf and the root
- **THEN** validation rejects it

#### Scenario: A root-only chain
- **WHEN** a chain contains only a valid root
- **THEN** validation accepts it

### Requirement: Chain shape errors
Chain validation SHALL first return the applicable shape error in this order: EmptyChain, ChainTooLong (more than five grants), RequestedGrantMismatch (the first grant is not the requested one), and CollectionMismatch (any scope belongs to another collection).
Chain errors MUST contain no submitted grants or identifiers.

#### Scenario: A grant from another collection
- **WHEN** a chain for the expected collection contains a grant scoped to another collection
- **THEN** validation returns CollectionMismatch

### Requirement: Chain linkage errors
After the shape checks, chain validation SHALL return the applicable linkage error in this order: RepeatedGrantId (even with different contents), MissingRoot (the final grant names a parent), InvalidRoot, and InvalidEdge carrying the first failing edge's attenuation error.
Repeated-identifier checks and exact parent linkage to a terminal root MUST reject cycles without graph traversal.

#### Scenario: A cycle is rejected
- **WHEN** a chain repeats a grant identifier to form a cycle
- **THEN** validation returns RepeatedGrantId

### Requirement: Validated chain result
A successful chain validation SHALL return a validated chain that preserves the supplied grants, their order, and the check time.
It MUST NOT be constructible, updatable, or reconstructible outside validation.
Success establishes valid policy inputs at the check time, not bearer possession, current database state, or authority for an operation; protected operations must reload and revalidate the grants in each transaction.

#### Scenario: Consumers cannot forge a validated chain
- **WHEN** an external consumer tries to construct, coerce, record-update, read, or generically reconstruct a validated chain
- **THEN** compilation fails

### Requirement: Structural validation of management targets
Structural validation SHALL apply the chain bound, grant field checks, collection and parent identifier comparisons, and delegation limits while omitting only the expiration checks.
Children MUST still have finite deadlines, a child of an expiring parent cannot outlive it, and the root must meet all its field requirements.
It returns the grants in their supplied order rather than a validated chain.

#### Scenario: An expired descendant remains manageable
- **WHEN** structural validation receives a well-formed path to an expired descendant
- **THEN** it accepts the path
