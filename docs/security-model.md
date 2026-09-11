# Eselsohr 2 security model

The replacement implements validated values, article transitions, grant validation, and pure operation policy.
These checks use supplied snapshots; bearer authentication, persistence, protected transactions, HTTP, and PublicWeb remain unimplemented.
This document specifies the approved policy and its enforcement limits, not guarantees of the legacy application.
See the [design](design/eselsohr-2.md) for implementation contracts and architectural proposals, and the [roadmap](design/roadmap.md) for remaining work.

## Operation policy

Task 3.6 is implemented in [Authorization](../v2/src/Eselsohr/Authorization.hs), with [policy tests](../v2/test/Authorization.hs).
Its operation values describe eligibility and targets, without carrying titles, URLs, read-state values, or revisions.
The decision API is:

```haskell
decide :: UTCTime -> ValidChain -> Operation -> Either PolicyError ()
```

Every call revalidates the acting chain at the supplied decision time before examining the operation.
The acting leaf determines the holder, collection, permissions, and scope.
The saved check time is metadata, not a cache lifetime or evidence of current storage.
Read and article-mutation targets include their collection; no article ID is interpreted across collections.

| Operation | Required policy |
| --- | --- |
| ReadArticlesIn scope | ReadArticles and containment of the requested scope in the acting scope. |
| AddArticle collection | AddArticles on WholeCollection for that collection. |
| RenameArticle collection article | ReadArticles and ChangeTitles on that article. |
| ChangeArticleReadState collection article | ReadArticles and ChangeReadState on that article. |
| DeleteArticle collection article | DeleteArticles on that article; ReadArticles is not required. |
| IssueChild snapshot | Delegate and a valid complete chain with the proposed child prepended. |
| InspectLink target path | InspectLinks and a proper descendant relationship; the root owner may also inspect its own root metadata. |
| RevokeLink target path | RevokeLinks and a proper descendant relationship; the root cannot be revoked through this operation. |
| DeleteCollection collection | A root-only acting chain for that collection. |

With ReadArticles, OneArticle authority can read that article, but cannot request a whole-collection read or add an article.
Read permission is required for renaming and read-state changes, including no-ops.
DeleteArticles alone permits in-scope deletion; future responses to a holder without Read must not disclose article contents.
Add-only access cannot read other articles or issue a child containing Read.
A full permission set on a delegated grant does not confer owner deletion authority.

Issuance validates the complete proposed chain, including the new child's ID, parent, collection, scope, permissions, depth, and deadline.
It rejects reuse of any ancestor ID.
Article existence and ID uniqueness outside that path remain transaction checks.

Link operations take a requested grant ID and its complete leaf-to-root path.
They check the corresponding management permission before examining that path.
[Structural validation](design/eselsohr-2.md#bounded-ancestry-validation) permits expired targets and expired intermediate descendants, but still requires all non-time ancestry and attenuation rules.
The suffix from the acting grant through the target's root must equal the entire acting chain structurally.
Matching IDs alone cannot combine inconsistent snapshots.

Delegated holders may manage proper descendants, including deeper descendants, but cannot manage themselves, ancestors, or siblings.
The owner may inspect its own root metadata and manage all proper descendants.
Root replacement belongs to future CLI reset; collection deletion is a separate owner-only operation.
An expired acting holder is denied even when the target is inspectable or revocable by a current holder.
Malformed target paths are rejected, not repaired.
Management grants metadata eligibility, not article Read or recovery of a bearer URL.
Every parent on a structurally valid descendant path must still have Delegate.

`PolicyError` has Eq and Show and contains no submitted snapshots, identifiers, article values, or bearer data.
ActorChainRejected, ChildChainRejected, and TargetChainRejected wrap the relevant ChainError.
OperationDenied covers failed permission, scope, ownership, and relationship checks.
The actor is checked first; permission checks precede target-path validation, whose existing error order is preserved.
Success is `Right ()` and neither returns data nor performs an effect.

## Authority and protected state

A capability is more than its permission set.
Effective authority is the tuple of collection, target scope, operations, lifetime, valid ancestors, delegation budget, and nondelegable owner status.
A public collection ID locates a file; only a valid bearer secret authorizes access to it.

Indirect authority is compatible with object capabilities.
A holder who can mint a Read capability can obtain Read authority, even if an API calls this only management.
Maintaining separate use/grant sets would make that distinction easy to misdescribe and would add policy states without confining the holder.
The agreed single set makes use and delegable rights explicit.
`Delegate` allows issuance of a subset; it never creates a right absent from the parent.
Copying the parent's own bearer URL remains possible without `Delegate`.

Distinct ID types prevent accidental substitution; IDs do not authenticate a bearer.
Article, grant snapshot, and validated-chain equality are structural; identity checks compare IDs explicitly.
Database decoders must apply the implemented value validation rules.

The future Store must obtain current snapshots and a fresh time inside the protected transaction, authenticate the presented bearer, check policy, and apply the operation to the same collection and target.
Revision metadata belongs alongside the article at the Store boundary; Article and its pure transitions have no revision checks or database limits.
After authorization, the Store must reject a submitted revision that differs from current storage, even for an apparent no-op.
Matching no-ops retain the revision, and committed changes increment it once without wrapping or partial writes.
Submitted revisions provide no authority; an eligibility decision does not bind a mutation payload or prove its revision is current.
The metadata DTO, concurrency checks, and storage representation validation remain unimplemented and belong to M4.
The pure policy does not check existence, quotas, schema state, or global grant uniqueness.
Root uniqueness and immutable persisted parents remain Store invariants.
Missing or revoked stored grants must not be replaced with retained snapshots.
D3 still decides the protected-operation interface; Operation is not an authorized-action wrapper.

Add receipts must not disclose existing titles, counts, article details, or read authority.
Link lists may return only permitted target metadata, without reconstructing bearers or returning ancestor/sibling grants as reusable alternatives.
These response and transaction properties require later HTTP and database tests.

The policy tests cover every permission set and all 6,561 subset pairs with consistent actor/target snapshots.
Reducing rights cannot enable an operation; a reduction that invalidates ancestry denies access.
Generated trees cover valid, expired, and corrupted target paths and branch relationships.
Examples retain the legacy laws about required permissions and exact article targeting without importing the legacy runtime or fake repositories.
Tests of these pure records do not establish authentic remote authority.

## Security boundaries

Treat malicious requests, stolen URLs, leaked logs or referrers, malformed or compromised databases, races, and SSRF as threats.
Authority must remain bound to its collection and object.
Authorization and protected state changes must use a consistent transaction view; an earlier check cannot justify a later mutation after revocation.
Persisted authority needs validation, including its ancestry, rather than trust because it came from SQLite.

Types and private constructors can prevent some misuse by cooperating Haskell code.
Effects restrict the authority exposed by an interface.
Neither contains arbitrary malicious code running with the application's process privileges.
Separate files limit some storage mistakes, not access by a process that can open every file.
Hash-only capability records do not disclose reusable bearer secrets, but database contents can still contain sensitive URLs or text.

A bearer URL can be copied.
Delegation controls newly issued authority, not copying of existing authority.
Revocation cannot retract downloaded data, and deleting a collection does not erase backups.
Restoring an old backup can restore revoked authority.
These limits must remain explicit in tests and operator guidance.

## Threat model

Protect article contents, bearer secrets, grant relationships, and the integrity and availability of each collection.
Trust the operator, the operating system, the process's trusted handlers, the cryptographic implementation, and an adequately maintained SQLite/TLS stack.
Do not mistake those assumptions for properties established by Haskell types.

| Actor or event | Required response and limit |
| --- | --- |
| Malicious remote client | Parse totally, bound input/work, deny unauthorized reads, mutations, and fetches. An identifier alone supplies no authority. |
| Stolen capability URL | Treat possession as authority until expiration or revocation. No account identity can distinguish thief from intended holder. |
| Logs, browser history, referrers, extensions, screenshots | Minimize propagation and lifetime. No-store/no-referrer and log redaction help; they cannot remove all copies of a URL. |
| Read-only copy of one database | Article data and grant metadata are disclosed. Hash-only grant records should not reveal reusable secrets, nor authority for other collection databases. Article URLs/text might themselves contain secrets. |
| Writable compromise of one database | Its integrity and authorization are lost: an attacker could insert a chosen secret hash. Validate malformed state and confine database access, but do not promise integrity against its writer. |
| Whole data directory or process compromise | All accessible collections are at risk. Separate files and effect signatures do not sandbox the process. |
| Hostile remote website/DNS | Prevent SSRF through the actual socket and redirect path; bound work and treat metadata as untrusted text. |
| Concurrent revoke, edit, delegate, delete | Define transaction ordering, revalidate authority at use, and reject stale updates. Avoid network work inside database transactions. |
| Corrupt or attacker-controlled persisted state | Fail closed on invalid IDs, enums, schema, grant chains, or metadata mismatch. SQLite itself must be patched; constraints are not a malicious-file sandbox. |
| Buggy cooperating application module | Give it only the protected operation families it needs. No global connection, data-directory lookup, or arbitrary IO in ordinary logic. |
| Resource exhaustion | Bound request bodies, query results, grant depth/count, database work, and fetch concurrency. A shared process/disk remains a cross-collection availability dependency. |
| Old backup restored | State, including formerly revoked grants, can return. Preventing rollback needs external trusted state; the proposed small design does not supply it. |

## Security properties and their enforcement

Pure value and policy checks are implemented; the runtime, storage, HTTP, crypto, and network enforcement in this table remain requirements for later work.
Tests provide evidence and regression detection, not a proof against all programs, inputs, machines, or deployment mistakes.

| Property | Primary enforcement | Limits and evidence |
| --- | --- | --- |
| Different identifier kinds cannot be accidentally substituted | Distinct abstract Haskell ID types | Compile-time protection for ordinary typed code, not token authenticity; inspect exports/coercions. |
| Titles, URLs, states are validated before ordinary use | Smart constructors and small ADTs; validated database decoders | SQL NOT NULL/CHECK/STRICT adds independent checks where expressible. URL syntax is not destination safety. |
| A bearer cannot feasibly be guessed or recovered from its stored digest | CSPRNG secret and domain-separated cryptographic hash | Computational guarantee assuming entropy and primitives; parsing/round-trip tests cannot prove randomness. |
| A capability for collection/object A cannot act on B | Collection-bound handler, checked database identity, scope policy, bound action target | Runtime checks and module API; integration tests with two files and repeated article IDs. No global Store escape hatch. |
| Delegation never increases effective authority | Pure subset/scope/lifetime/depth checks plus transaction validation of ancestors | SQL foreign keys preserve relationships, not arbitrary cross-row subset laws; exhaustive/property tests. |
| Removing rights cannot enable an operation | Monotone operation policy over actual permission sets | Exhaustive finite checks plus scoped/ancestry generators. Owner status comes from a root-only chain, never from a full permission set. |
| Unauthorized requests cannot reach protected persistence mutation | Only guarded operations exported from the trusted store implementation | Closed witnesses may add a static gate if selected at D3. Neither witnesses nor tests prove freshness without transactional checking. |
| Revocation/expiry invalidates descendants | Immutable parent chain, bounded validation, cascade deletion or equivalent checked state | Same transaction snapshot as use. Already completed or earlier-linearized work is not retracted; clock is trusted. |
| One collection open/migration cannot select or mutate another | Canonical locator, existing-only open, metadata check, no exposed ATTACH/raw SQL, explicit CLI target | Filesystem ownership/no-symlink policy and two-file failure tests. The runtime still holds directory authority. |
| Concurrent writes preserve authorization and domain invariants | SQLite transactions, revision check, constraints | Multi-connection race tests with barriers, not sleeps. Return success only after commit. |
| PublicWeb cannot connect to forbidden destinations | Production resolver/connector, redirect checks, bounded HTTP behavior | Real connector tests. Egress firewall is defense-in-depth; a policy-only fake is insufficient. |
| Capability URLs do not appear in routine logs or outbound requests | Allowlisted logging fields, redacted errors, canonical routing, no referrer forwarding | HTTP/proxy integration checks; operational log configuration. Browser-side copying remains possible. |
| One database copy does not expose another's grant secrets | Independent random secrets, collection-bound hashing, no central reusable secret store | Storage/content inspection tests demonstrate format properties, not OS isolation or absence of user-pasted secrets. |
| Deleted collections stop authorizing requests | Owner-only deletion; D5 must settle the transaction and file cleanup lifecycle | Integration/race tests; backup retention is a separate concern. |
