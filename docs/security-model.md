# Eselsohr 2 security model

The replacement implements validated values, article transitions, grant validation, and pure operation policy.
These checks use grant fields and times supplied by the caller.
The private SQLite adapter selects existing collection files, validates identity and schema version, installs an initial schema atomically, converts storage rows to validated values, and resolves canonical bearers by digest inside caller-owned transactions.
Its protected read and title change reload the bearer and bounded ancestry, check current operation authority, and execute the exact sealed payload in one transaction.
Other protected mutations, the application-facing Store API, HTTP, and PublicWeb remain unimplemented.
This document specifies the approved policy and its enforcement limits, not guarantees of the legacy application.
See the [design](design/eselsohr-2.md) for implementation contracts and architectural proposals, and the [roadmap](design/roadmap.md) for remaining work.

## Operation policy

Task 3.6 is implemented in [Authorization](../v2/src/Eselsohr/Authorization.hs), with [policy tests](../v2/test/Authorization.hs).
Its operation values name the requested action and target, without carrying titles, URLs, read-state values, or revisions.
The decision API is:

```haskell
decide :: UTCTime -> ValidChain -> Operation -> Either PolicyError ()
```

Every call revalidates the acting chain at the supplied decision time before examining the operation.
The first grant in the chain supplies the acting grant ID, collection, permissions, and scope.
The saved check time records when the supplied chain was validated; it does not detect later changes in storage.
Read and article-mutation targets include their collection; no article ID is interpreted across collections.

| Operation | Required policy |
| --- | --- |
| ReadArticlesIn scope | ReadArticles and containment of the requested scope in the acting scope. |
| AddArticle collection | AddArticles on WholeCollection for that collection. |
| RenameArticle collection article | ReadArticles and ChangeTitles on that article. |
| ChangeArticleReadState collection article | ReadArticles and ChangeReadState on that article. |
| DeleteArticle collection article | DeleteArticles on that article; ReadArticles is not required. |
| IssueChild child | Delegate and a valid complete chain with the proposed child prepended. |
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
The target's path must include the acting grant and all its ancestors, with every field matching the acting chain.
Matching IDs alone is insufficient when permissions, scope, deadlines, or other fields differ.

Delegated holders may manage proper descendants, including deeper descendants, but cannot manage themselves, ancestors, or siblings.
The owner may inspect its own root metadata and manage all proper descendants.
Root replacement belongs to future CLI reset; collection deletion is a separate owner-only operation.
An expired acting holder is denied even when the target is inspectable or revocable by a current holder.
Malformed target paths are rejected, not repaired.
InspectLinks permits viewing grant metadata, and RevokeLinks permits revoking links allowed by the policy.
Neither permission grants article Read or allows recovery of the original bearer URL.
Every parent on a structurally valid descendant path must still have Delegate.

`PolicyError` has Eq and Show and contains no submitted grants, identifiers, article values, or bearer data.
ActorChainRejected, ChildChainRejected, and TargetChainRejected wrap the relevant ChainError.
OperationDenied covers failed permission, scope, ownership, and relationship checks.
The actor is checked first; permission checks precede target-path validation, whose existing error order is preserved.
Success is `Right ()` and neither returns data nor performs an effect.

## Authority and protected state

A capability is more than its permission set.
A holder may act only on the grant's collection and scope, using its allowed operations while the grant and all its ancestors remain valid.
Delegation is also limited by depth, and whole-collection deletion requires the root grant.
A public collection ID locates a file; only a valid bearer secret authorizes access to it.
The [opening adapter](../v2/persistence/Eselsohr/Store/SQLite/Open.hs) checks file type, stored identity, and schema version 1 before handing a connection to trusted code, but performs no bearer authentication.
Its raw helper and connection callback belong to a private Cabal component.
They remain trusted IO with database authority; private packaging does not confine a compromised process.

The private [bearer parser and lookup](../v2/persistence/Eselsohr/Store/SQLite/Bearer.hs) use the approved [canonical encoding and collection-bound digest](design/eselsohr-2.md#bearer-format).
Parsing checks representation, not secret entropy or stored authority, and Bearer has no revealing Show instance.
Lookup requires a transaction already started by the caller and rechecks collection identity and version.
It finds the grant by the bearer's hash, loads its permissions, and validates the stored fields before returning StoredGrant.
It accepts expired grants and does not check ancestors or permission for an operation; protected operations perform those checks using grants read in their own transaction.
Expected lookup errors contain no submitted or stored values; unexpected exceptions and cancellation propagate.

The private [protected read](design/eselsohr-2.md#protected-read) owns a deferred transaction and validates the complete acting chain and sole root before selecting articles.
It reads the clock after loading the grant and its ancestors, then checks the requested scope through the pure operation policy.
The request's transaction type prevents its use in another transaction, and the runner fixes the connection and bearer.
The selection helper takes its scope only from that request.
The Articles interface exposes no IO lifting, context replacement, or individual action runner, and ReadRequest exposes no constructor, record updates, or reconstruction instances.
Articles owns the read and title-change implementations and seals; Internal.Transaction and Internal.Authority are hidden Cabal modules used by this trusted implementation.
Even if a caller wraps a request or action to hide its transaction type, the API cannot execute it in another transaction.
The trusted clock and exclusive connection use remain interpreter responsibilities; types do not sandbox arbitrary IO.
Successful results include validated article revisions after commit, with no ordering or pagination contract.

The private [protected title change](design/eselsohr-2.md#protected-title-change) owns an immediate transaction and requires ReadArticles plus ChangeTitles for the sealed article.
ReadTx and WriteTx share a private implementation, but their types prevent a write action from running through the read runner, including through `coerce`.
Its sealed request supplies the complete article ID, validated title, and expected revision to the persistence helper.
Only after authorization does the operation check that the submitted revision is in range and equals the stored revision.
Matching no-ops issue no UPDATE, including at the maximum revision; real changes increment once and exhaustion fails without writing.
Failures after an UPDATE roll back the title and revision together, including cancellation and COMMIT contention.
Writes require existing DELETE journaling and verified synchronous EXTRA and secure_delete ON settings; other journal modes are rejected without conversion.
Multi-connection tests cover competing edits and ancestor revocation ordered before or after an edit.
Already ordered writes may finish, but new writes after committed revocation deny access.

Indirect authority is compatible with object capabilities.
A holder who can mint a Read capability can obtain Read authority, even if an API calls this only management.
Maintaining separate use/grant sets would make that distinction easy to misdescribe and would add policy states without confining the holder.
The agreed single set makes use and delegable rights explicit.
`Delegate` allows issuance of a subset; it never creates a right absent from the parent.
Copying the parent's own bearer URL remains possible without `Delegate`.

Distinct ID types prevent accidental substitution; IDs do not authenticate a bearer.
Article and grant equality compare every field; ValidChain equality also compares the check time and grant order.
Identity checks compare IDs explicitly.
Database decoders must apply the implemented value validation rules.

Every protected operation must look up the presented bearer, load its grant and ancestors, and read the clock inside its transaction.
It must check policy and apply the operation to that same collection and target within the transaction.
The private read and title change implement these checks; other protected mutations remain deferred.
Revision metadata belongs alongside the article at the Store boundary; Article and its pure transitions have no revision checks or database limits.
After authorization, the Store must reject a submitted revision that differs from current storage, even for an apparent no-op.
Matching no-ops retain the revision, and committed changes increment it once without wrapping or partial writes.
The policy check does not check the submitted title or revision; the sealed request keeps those inputs together, and the persistence helper compares the revision with storage.
The private adapter keeps revision metadata outside Article and returns it from protected reads and title changes; the application-facing Store API remains unimplemented.
Grant codecs check IDs, hash length, parent ID, depth, deadline, and permission rows, including the root's full permission set, while permitting expired grants for later management checks.
Decoding builds the scope using the collection ID checked by the caller and returns grant data without loading ancestors or authenticating a bearer.
The pure policy does not check existence, quotas, schema state, or global grant uniqueness.
Root uniqueness and immutable persisted parents remain Store invariants.
If a grant is missing or revoked in storage, a protected operation must deny access rather than reuse grant data from an earlier read.
Operation remains pure policy input and is not an authorized-action wrapper.

D3 selects exact-payload sealing behind named protected operations.
Private, operation-specific seals bind the operation, exact payload, target, collection, presented bearer, and transaction.
A read seals its requested scope; a title change seals its article ID, validated title, and expected revision.
The protected persistence helper requires the seal and takes its target and payload from it without accepting replacement values.
Checks and execution remain inside the same transaction, and seals cannot authorize execution outside it.
The seal supplies no proof that a submitted revision matches storage and no proof of single use.
Pure domain transformations require no sealed request, and no generic authorized-action framework or dispatcher is selected.
Tasks 4.5–4.6 implement these restrictions for reads and title changes; runtime tests check authorization and the exact inputs used, and source review checks private constructors and the types that restrict execution to one transaction.

Add receipts must not disclose existing titles, counts, article details, or read authority.
Link lists may return only permitted target metadata, without reconstructing bearers or returning ancestor/sibling grants as reusable alternatives.
These response and transaction properties require later HTTP and database tests.

The policy tests cover every permission set and all 6,561 subset pairs, with matching fields for grants shared by the acting and target chains.
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
| Buggy cooperating application module | Give it only the protected operation families it needs. No global connection, data-directory lookup, or arbitrary IO in application logic. |
| Resource exhaustion | Bound request bodies, query results, grant depth/count, database work, and fetch concurrency. A shared process/disk remains a cross-collection availability dependency. |
| Old backup restored | State, including formerly revoked grants, can return. Preventing rollback needs external trusted state; the proposed small design does not supply it. |

## Security properties and their enforcement

Pure value and policy checks, restricted collection opening, initial schema installation, validated storage codecs, bearer parsing/digest lookup, and protected article reads and title changes are implemented.
The remaining runtime, other protected mutations, HTTP, secret generation, and network enforcement in this table are requirements for later work.
Tests provide evidence and regression detection, not a proof against all programs, inputs, machines, or deployment mistakes.

| Property | Primary enforcement | Limits and evidence |
| --- | --- | --- |
| Different identifier kinds cannot be accidentally substituted | Distinct Haskell ID types with private constructors | Compile-time protection against mixing ID types; inspect exports/coercions. IDs do not authenticate a bearer. |
| Application values contain validated titles, URLs, and states | Smart constructors and small ADTs; validated database decoders | SQL NOT NULL/CHECK/STRICT adds independent checks where expressible. URL syntax is not destination safety. |
| A bearer cannot feasibly be guessed or recovered from its stored digest | CSPRNG secret and domain-separated cryptographic hash | Computational guarantee assuming entropy and primitives; parsing/round-trip tests cannot prove randomness. |
| A capability for collection/object A cannot act on B | Collection-bound handler, checked database identity, scope policy, bound action target | Runtime checks and module API; integration tests with two files and repeated article IDs. No global Store escape hatch. |
| Delegation never increases effective authority | Pure subset/scope/lifetime/depth checks plus transaction validation of ancestors | SQL foreign keys preserve relationships, not arbitrary cross-row subset laws; exhaustive/property tests. |
| Removing rights cannot enable an operation | Monotone operation policy over actual permission sets | Exhaustive finite checks plus scoped/ancestry generators. Owner status comes from a root-only chain, never from a full permission set. |
| Unauthorized requests cannot reach protected article reads or mutations | Named operations; persistence helpers require private operation-specific seals binding the exact request and transaction | Tasks 4.5–4.6 test reads and title changes, including current ancestry and exact inputs. Source review checks that requests cannot cross transactions and writes cannot use the read runner. Other mutations remain deferred; each protected operation must reload and check the grants. |
| Revocation/expiry invalidates descendants | Immutable parent chain, bounded validation, cascade deletion or equivalent checked state | Grant checks and use happen in the same transaction. Revocation does not undo completed work, and an operation ordered before it may finish; the clock is trusted. |
| Collection opening selects the requested existing file and checks its identity and version | Canonical locator, existing-only open, non-regular/symlink rejection, UUID metadata and schema version checks | Tasks 4.2–4.3 test real files, wrong identities, and unsupported versions. The directory and its writers are trusted; raw SQL is private adapter authority. Migrations and CLI targeting remain unimplemented. |
| Initial schema installation is atomic | One transaction for tables, identity, and version, including rollback after commit failure | Task 4.3 checks real FULL and COMMIT/BEGIN Busy failures, controlled errors, SQL exceptions, and cancellation. This is not a protected operation. |
| Concurrent writes preserve authorization and domain invariants | SQLite transactions, revision check, constraints | Task 4.6 tests title-change conflicts, rollback, and edit/revoke ordering using barriers between connections. Success returns only after commit; crash/power-loss durability remains unverified. |
| PublicWeb cannot connect to forbidden destinations | Production resolver/connector, redirect checks, bounded HTTP behavior | Real connector tests. Egress firewall is defense-in-depth; a policy-only fake is insufficient. |
| Capability URLs do not appear in routine logs or outbound requests | Allowlisted logging fields, redacted errors, canonical routing, no referrer forwarding | HTTP/proxy integration checks; operational log configuration. Browser-side copying remains possible. |
| One database copy does not expose another's grant secrets | Independent random secrets, collection-bound hashing, no central reusable secret store | Storage/content inspection tests demonstrate format properties, not OS isolation or absence of user-pasted secrets. |
| Deleted collections stop authorizing requests | Owner-only deletion; D5 must settle the transaction and file cleanup lifecycle | Integration/race tests; backup retention is a separate concern. |
