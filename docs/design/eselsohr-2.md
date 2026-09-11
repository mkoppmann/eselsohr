# Eselsohr 2

Eselsohr stores web articles to read later or share with others.
It should be a small reference application for domain-driven design, secure functional software, and object-capability ideas, understandable by one developer.
It remains a bookmark manager, not an archiving service or account platform.

This document describes the current planning decisions.
It is a direction to review as work progresses, not permission to implement every milestone at once.
The legacy implementation remains unchanged.
The replacement has an independent package with a version command, validated values, pure article transitions, grant validation, operation policy, and tests.
Bearer authentication, persistence, protected transactions, and web behavior remain unimplemented.
Read the [investigation](investigation.md) for evidence about the old source, and the [roadmap](roadmap.md) for implementation tasks.
The validated values, article transitions, inclusion predicates, local child attenuation, bounded ancestry validation, and operation policy below are implemented; the remaining code and SQL are design sketches.

## Agreed decisions

The rewrite can start fresh: no running instances, stored data, or old capability URLs require compatibility.
Existing behavior, comments, and tests remain useful evidence, but the module layers need not survive.
Keep the README's concise, concrete, understated prose.

D1 is approved: start with a temporary independent `v2/` package selected by `cabal-v2.project`, with no imports from the old `Lib` tree or compatibility adapters.
Cut over to the root package in a later task.

D2 is approved: the first usable version supports manually entered URLs and titles.
Automatic metadata fetching remains intended functionality, but does not block that delivery.
Investigate the restricted fetcher separately.

Pure domain transformations stay ordinary functions.
Persistence ignorance is an agreed constraint: domain values and transitions do not carry revision metadata or enforce database representation limits.
The protected Store owns concurrency checks, and the persistence adapter owns storage representations and their bounds.
Use `effectful` for meaningful external authority; the exact effect interfaces remain undecided.
Do not turn every dependency into an effect or expose arbitrary `IO` to ordinary application logic.

Capabilities have one permission set, including permission to delegate, rather than separate sets for use and granting.
The eight permissions and collection-bound scopes in the [inclusion contract](#permission-and-scope-inclusion) are approved.
Delegation is recursive and limited to four edges from the root at depth zero.
A child cannot gain permissions, widen its target, or outlive its parent.
Revocation and expiration of an ancestor deny access through its descendants.
The [operation policy](../security-model.md#operation-policy) is approved and implemented.

Keep a long-lived, bookmarkable owner link for management and issuing shorter everyday links.
The root has all eight permissions and no expiration; every child has a finite deadline, with equality to that deadline already expired.
Everyday links default to seven days; the owner may explicitly choose longer lifetimes.
Delegated management applies to proper descendants, including expired targets and intermediate descendants.
The owner can manage descendants and inspect its own root metadata; root revocation and replacement belong to the future CLI reset.

Store cryptographic hashes of high-entropy bearer secrets, not reusable tokens.
Show a newly issued URL once.
Later management can display metadata, revoke a link, or issue a replacement, but cannot recover the original URL.

Support add-only links: a contributor can submit an article without reading other articles.
Submission must not automatically issue read authority.
Renaming an article or changing its read state requires read permission and the relevant write permission.
Deleting an article requires DeleteArticles alone, within the holder's scope.
Collection creation is an operator CLI action.
The owner may delete the whole collection through the web interface; this authority is nondelegable.

Schema migrations are explicit offline CLI operations.
Opening a collection during a request must not silently migrate it.

## Preferred direction

Prefer one SQLite database per collection, explicit SQL through `sqlite-simple`, small migrations, and SQLite constraints.
Introduce a central index or secret store only for a demonstrated need.
Test database opening, transaction, and maintenance rules before adopting the full design.

Prefer concept-oriented modules such as Article, Capability, Authorization, Store, Fetch, and Web.
Retain separation where it protects an invariant, not merely to reproduce the old Domain/Application/Infrastructure hierarchy.
Servant, Lucid, and server-rendered HTML without JavaScript remain the preferred web stack.
Dependencies and abstractions must earn their complexity.

Expose metadata retrieval through a restricted `PublicWeb` interface.
Its handler must enforce destination restrictions at the actual connection, including redirects and DNS behavior, and bound time and response size.
Parsing a URL does not establish that fetching it is safe.

Native GHCup and Cabal should be the contributor path on Linux and macOS.
Nix remains optional.
Prefer one executable, one configuration mechanism, a private data directory, and a reverse proxy where appropriate.

## Decisions before dependent work

Three choices remain for dependent work.
D3: before fixing the protected persistence API, compare named transactional operations with private authorization checks against sealed authorized actions.
Use the same protected read and title change to assess binding, freshness, and complexity.
A generic `Authorized action` wrapper is not yet agreed.
D4: before SQLite integration, verify system SQLite against a maintained bundled version on Linux and macOS; system SQLite is preferred pending those checks.
D5: before owner deletion, review transactional deletion with later controlled file cleanup against draining requests and deleting the file immediately.
The first approach is preferred, but retention and cleanup behavior remain open.

## Security boundaries

The [security model](../security-model.md) defines the approved operation policy, threat model, and enforcement requirements.
Pure policy uses supplied snapshots; authentic bearer possession and transaction freshness remain responsibilities of the future protected Store.
D3–D5 remain open.

## Implementation sequence

The [roadmap](roadmap.md) defines the milestones, first task cards, parallel experiments, and review gates.
It is the source for task numbering.
Approval of this design is not authorization to run that whole sequence autonomously.

## Existing baseline

The legacy code uses MTL-style interfaces and compressed JSON collection files, not SQLite.
See the [build baseline](investigation.md#build-ci-and-nix-baseline) for its compiler constraint and the replacement's verification results.

The independent `eselsohr-v2` package builds and tests on Linux with GHC 9.10.3 and Cabal 3.16.1.0.
It provides `--version`, the values, transitions, grant validation, and operation policy below, and a Tasty suite with examples, exhaustive permission checks, and QuickCheck properties.
Dependency versions and the Hackage index state are pinned; offline build/test checks pass with populated dependency caches.
The [roadmap](roadmap.md#task-contract-and-verification-vocabulary) lists the verified commands.
A developer's container setup and machine paths must not become project requirements.

## Validated values

Task 3.1 is implemented in [Id](../../v2/src/Eselsohr/Id.hs), [Article](../../v2/src/Eselsohr/Article.hs), and [WebUrl](../../v2/src/Eselsohr/WebUrl.hs).
The identifier, title, and URL types are abstract values with total constructors returning `Either`, explicit text renderers, and small input errors without the submitted text.
No constructor, `Read`, `IsString`, or generic reconstruction instance bypasses validation.

`CollectionId`, `ArticleId`, and `CapabilityId` are distinct UUID newtypes.
Their parsers accept exactly 36-character lowercase, hyphenated UUIDv4 text with the standard variant.
Other versions, nil, mixed case, and whitespace are rejected.
An ID establishes neither existence nor authority.

`Title` accepts at most 4,096 input code points, collapses Unicode White_Space runs to one ASCII space, and trims both ends.
The result must contain 1–500 Unicode code points and no other control characters.
Other characters retain their case and normalization form.

`WebUrl` requires an absolute HTTP(S) URL with a nonempty host and no userinfo, literal whitespace, controls, or backslashes.
Ports may be omitted or numerically equal to the scheme's default, 80 for HTTP and 443 for HTTPS; rendering removes explicit default ports.
Input is limited to 8,192 code points and the rendered ASCII URL to 8,192 characters.
`network-uri` validates URI syntax; `idna2008` validates domain labels with strict IDNA2008 checks and case/NFC mappings, rendering lowercase punycode.
Emoji domains, malformed labels, invalid UTF-8 in encoded hosts, and DNS presentation escapes are rejected.
IPv6 literals use bracketed address syntax; IPvFuture and zone identifiers are rejected.
Unicode in paths, queries, and fragments becomes UTF-8 percent encoding, while existing escapes, path separators, query order, and fragments are preserved.
Domain trailing dots are preserved.
`WebUrl` has no `Show` instance; callers render it explicitly.
Local/private hosts are accepted without DNS or address classification: URL syntax does not establish a safe network destination.

## Article transitions

Task 3.2 is implemented in [Article](../../v2/src/Eselsohr/Article.hs), with examples and properties in the [Article test group](../../v2/test/Articles.hs).
`Article` is a public record containing an `ArticleId`, `Title`, `WebUrl`, `ArticleState`, and creation time as `UTCTime`.
Its equality compares all five fields; use an explicit ID comparison when identity is the question.
It has no `Show` instance, so a containing value cannot accidentally render its URL through derived `Show`.
`ArticleState` is `Unread` or `Read`.

The article model has no revision metadata, concurrency errors, or database limits.
Its transitions are total functions over validated values:

```haskell
newArticle :: ArticleId -> Title -> WebUrl -> UTCTime -> Article
rename :: Title -> Article -> Article
setReadState :: ArticleState -> Article -> Article
```

`newArticle` preserves the supplied ID, title, URL, and creation time exactly, with initial state `Unread`.
The caller supplies time; these functions do not read a clock or choose a timestamp storage encoding.
Each edit changes only its named field and preserves all other fields.
Assigning the existing title or state preserves the article's value; repeating an assignment is idempotent.
Title equality uses its normalized value.
There is no URL editing operation.

The future protected Store must load current state and check authority and revision metadata within the transaction.
Its metadata DTO and implementation are deferred to M4.
These pure functions do not settle D3's protected-operation API.

## Permission and scope inclusion

Task 3.3 is implemented in [Capability](../../v2/src/Eselsohr/Capability.hs), with exhaustive checks in the [Permission and Scope test groups](../../v2/test/Capabilities.hs).
The public values and predicates are:

```haskell
data Permission
  = ReadArticles | AddArticles | ChangeTitles | ChangeReadState
  | DeleteArticles | Delegate | InspectLinks | RevokeLinks
  deriving stock (Eq, Ord, Enum, Bounded, Show)
type Permissions = Set Permission

data Scope
  = WholeCollection CollectionId
  | OneArticle CollectionId ArticleId
  deriving stock (Eq, Show)

permissionsSubsetOf :: Permissions -> Permissions -> Bool
scopeSubsetOf :: Scope -> Scope -> Bool
```

Both predicates take the candidate first and its containing parent second.
Permission inclusion uses `Set.isSubsetOf`; ordering of aggregate sets or records does not express inclusion.
`Ord Permission` serves set storage, and Scope has no `Ord` instance.
Every permission combination is representable, including the empty set; inclusion adds no implied rights and does not require `Delegate`.
For example, an add-only set cannot contain a request for Read.
Whole-collection deletion remains owner authority outside this vocabulary.

Scopes include their collection ID, and equality compares their complete contents.
Different collections never include each other's scopes, even when article IDs match.
Within one collection, WholeCollection includes itself and every OneArticle; OneArticle includes only the same article and never WholeCollection.
The public constructors supply policy values, not evidence of article existence or bearer possession.
Operation eligibility is defined by the [operation policy](#operation-policy); bearer issuance and transaction checks remain unimplemented.

## Local child attenuation

Task 3.4 is implemented in [Capability](../../v2/src/Eselsohr/Capability.hs), with examples and properties in the [Attenuation test group](../../v2/test/Attenuation.hs).
The public snapshot and validator are:

```haskell
data GrantSnapshot = GrantSnapshot
  { grantId :: CapabilityId
  , grantParentId :: Maybe CapabilityId
  , grantScope :: Scope
  , grantPermissions :: Permissions
  , grantExpiresAt :: Maybe UTCTime
  , grantDepth :: Int
  }
  deriving stock (Eq, Show)

maxDelegationDepth :: Int
maxDelegationDepth = 4

attenuates :: UTCTime -> GrantSnapshot -> GrantSnapshot
           -> Either AttenuationError ()
```

The caller supplies the current time, then the parent and child snapshots.
A local root shape has no parent, WholeCollection scope, all eight permissions, no expiration, and depth zero.
A local child shape has a parent ID different from its own, a finite expiration, and depth 1–4.
Depth bounds are checked before incrementing, including for extreme Int inputs.

`attenuates` returns the first applicable error in this order, or `Right ()` when all checks pass:

| Error | Rejection condition |
| --- | --- |
| InvalidParent | Parent fails the local root/child shape rules. |
| ParentExpired | Parent deadline is at or before the supplied time. |
| DelegateRequired | Parent lacks Delegate. |
| DepthExhausted | Parent is at depth four. |
| InvalidChild | Child fails the child shape rules, including a root supplied as a child. |
| ChildExpired | Child deadline is at or before the supplied time. |
| ParentMismatch | Child names a different parent, or parent and child IDs match. |
| DepthMismatch | Child depth is not parent depth plus one. |
| ScopeWidened | Child scope is not included in parent scope. |
| PermissionsIncreased | Child permissions are not included in parent permissions. |
| ChildOutlivesParent | Child deadline exceeds the parent's finite deadline. |

Errors are nullary values with Eq and Show, without submitted input.
A child may share its parent's deadline; a root may delegate any finite future deadline.
The seven-day default belongs to later issuance/UI work, not this validator.
A depth-four child may retain Delegate, but cannot delegate further.

Snapshots are publicly constructible, unvalidated policy inputs with structural equality and no bearer secrets or independent owner flag.
Collection identity comes from Scope.
Success establishes only one local edge; ancestry, root uniqueness, existence, revocation, and bearer possession remain unchecked.
There is no reparenting operation, and the future store must enforce immutable persisted parent links.
These functions read no clock and do not issue or persist a grant.

## Bounded ancestry validation

Task 3.5 is implemented in [Capability](../../v2/src/Eselsohr/Capability.hs), with examples and generated trees in the [Chains test group](../../v2/test/Chains.hs).
The caller supplies the check time, expected collection, requested grant ID, and every snapshot from that grant back to its root:

```haskell
validateChain :: UTCTime -> CollectionId -> CapabilityId -> [GrantSnapshot]
              -> Either ChainError ValidChain

chainGrants :: ValidChain -> NonEmpty GrantSnapshot
chainCheckedAt :: ValidChain -> UTCTime

validateGrantStructure :: CollectionId -> CapabilityId -> [GrantSnapshot]
                       -> Either ChainError (NonEmpty GrantSnapshot)
```

The list contains one to five grants in leaf-to-root order.
The size check inspects at most six list constructors, before reading snapshot contents, and rejects oversized input without inspecting its remaining tail.
This bounds the list traversal, not the evaluation of arbitrary undefined Haskell values.
Validation returns the first applicable error in this order:

| Error | Rejection condition |
| --- | --- |
| EmptyChain | No snapshots were supplied. |
| ChainTooLong | More than five snapshots were supplied. |
| RequestedGrantMismatch | The first snapshot does not have the requested grant ID. |
| CollectionMismatch | Any scope belongs to a different collection. |
| RepeatedGrantId | An ID occurs more than once, even with different contents. |
| MissingRoot | The final snapshot still names a parent. |
| InvalidRoot | The final snapshot fails the approved root shape. |
| InvalidEdge AttenuationError | The first failing edge, checked from root toward leaf, fails local attenuation. |

Each edge uses `attenuates` at the supplied time and preserves its error order.
A root-only chain is valid; an ordinary leaf need not retain Delegate.
Repeated-ID checks and exact parent linkage to a terminal root reject cycles without graph traversal.
Missing intermediate parents, reordered entries, and extra roots fail; the validator does not sort, truncate, repair, or ignore entries.

Task 3.6 adds `validateGrantStructure` for management targets that may have expired.
It shares the bounded traversal, shape, binding, and attenuation checks, omitting only ParentExpired and ChildExpired.
Finite child deadlines, lifetime inclusion, root shape, and every other ordered failure remain required.
It returns ordinary snapshots, never a ValidChain or evidence of current authority.

`ValidChain` has a private constructor, ordinary read-only accessors, structural Eq, and Show.
It preserves the supplied snapshots and their order together with the check time.
It has no record-update fields, Read, Generic, or unchecked reconstruction API.
`ChainError` has Eq and Show and contains no submitted snapshots or identifiers.

Success establishes valid policy inputs at the supplied time, not bearer possession, current database state, or authority to perform an operation.
The check time is metadata and does not make a retained result fresh automatically.
The future store must supply current snapshots and revalidate inside the protected transaction; revocation execution, collection-wide root uniqueness, and immutable persisted parents remain its responsibility.
This result does not decide D3 or implement an authorized-action wrapper.

## Operation policy

Task 3.6 is implemented in [Authorization](../../v2/src/Eselsohr/Authorization.hs), with exhaustive permission checks and generated path tests.
The [security model](../security-model.md#operation-policy) is the operation table and decision contract.
Operation values describe eligibility and targets; article payloads belong to domain transitions, while revision metadata belongs to the future Store API.
Pure decisions do not perform protected effects or settle D3.

## Bearer format

Proposed bearer format is `v2.<canonical-collection-uuid>.<base64url-secret>` with 256 random secret bits.
The database stores an independent public grant ID and `SHA256(domain-separator || collection-id || secret)`.
Use unambiguous fixed encodings and a maintained crypto library; no password-style slow hash is needed for a uniformly random secret.
Parse version, length, encoding, and structure totally, with bounded input and generic invalid-capability responses.
Do not derive a revealing `Show` instance.
Exact encoding is a proposal, not an existing URL compatibility commitment.

## Effects and the protected store API

`effectful` was chosen for the project's teaching purpose, not because ordinary records cannot express this application.
Start with `effectful-core` and small dynamic effects; avoid generated plumbing unless it demonstrably helps.
See the [effectful project](https://github.com/haskell-effectful/effectful) for the library model.
Concrete signatures should remain readable without requiring the reader to know a new architecture framework.

Store handlers should be bound to one presented capability and collection.
Only the runtime can turn a parsed locator into a connection.
Application code must not receive a global `openCollection`, `Connection`, raw SQL, or data path.
A Store effect with every operation on every collection would mostly rename dependency injection; it would not establish least authority.

A protected read must supply articles with revision metadata kept outside Article so an edit can carry the expected stored version.
A protected title change accepts an article ID, a validated title, and an expected revision.
The metadata DTO and concrete read/change signatures belong to M4 and D3.

An illustrative metadata-fetch operation, not a committed effect declaration:

```haskell
fetchMetadata :: PublicWeb :> es
              => WebUrl -> Eff es (Either FetchFailure Metadata)
```

The Store interpreter validates bearer possession and current authority inside each protected operation's transaction.
It compares the expected revision with the current stored revision after authorization and before applying an edit, rejecting any mismatch even for an apparent no-op.
Submitted revisions are untrusted concurrency metadata and supply no authority.
Newly stored articles start at revision zero; matching no-ops retain the revision, and committed changes increment it once.
The adapter must reject revisions it cannot represent and refuse counter exhaustion without wrapping or leaving partial writes.
These checks and their database tests remain unimplemented.
Pure transitions such as `rename` do not need an authorization witness: transforming an in-memory value is not persisting it.
The protected persistence interface is the gate.

D3 compares two realizations of that gate:

* Named protected transactional operations keep checks beside SQL and expose no unguarded mutation method.
  This is the current simpler candidate.
* A closed `Action a` GADT and opaque `Authorized a` can seal the exact operation, payload, collection, target, and transaction context.
  Such a value must not support arbitrary mapping, deserialization, coercion, or reuse across transactions.
  A wrapper around freely replaceable data loses the guarantee.

Both need the same runtime freshness checks.
Generic authorized actions, purpose-specific Store facets, and fetch permits are candidates, not a bundle to adopt by default.
Compare one read and one title change with the same misuse/race tests before deciding.
Narrow operation families are useful when they remove real authority; proliferating effects per CRUD operation is not.

Keep `IOE` at interpreters and composition.
Use a Clock only where application policy actually needs time; transactional authorization must use an appropriate fresh time sample.
Secret/ID generation can stay within trusted creation and issuance operations rather than becoming a global Random effect.
Start logging at boundaries; add an application logging effect only for a concrete need.
Test handlers replace external behavior, not a second App/TestApp hierarchy.

Use `Either` and small ADTs for expected input, domain, authorization, conflict, and fetch failures.
A storage fault is not a permission denial; an unexpected exception is not a domain result.
Translate a small `StoreFault` family at the handler boundary, keep HTTP redirects in Web, and let asynchronous cancellation unwind bracketed resources.
Do not catch all exceptions as success/fallback or dump raw SQL/HTTP exceptions containing URLs and parameters into logs.

## Proposed source layout

This is the intended module shape, not a scaffold to create in advance.
Under the approved D1 choice it initially lives in `v2/`; cutover later moves it to the root.
Add each module when a task needs it.

```text
app/Main.hs
src/Eselsohr/
  Id.hs
  Article.hs
  WebUrl.hs
  Capability.hs
  Authorization.hs
  Store.hs
  Store/SQLite.hs
  Store/Migrate.hs
  Fetch.hs
  Fetch/Http.hs
  Web.hs
  Web/Routes.hs
  Web/Html.hs
  Cli.hs
  Config.hs
  Runtime.hs
test/
```

Article owns validated article values and pure transitions.
Capability owns permission/scope/grant values and attenuation; Authorization owns the operation policy.
Store exposes protected semantic operations, while Store.SQLite owns connections, decoded persisted state, and transactional checks.
Store.Migrate is CLI-only authority, not an operation available to web application code.
Fetch is the narrow metadata interface; Fetch.Http owns the network handler.
Web translates HTTP inputs/results, Routes owns typed links, and Html renders already-authorized view data.
Runtime alone composes directory, network, clock, and process authority; Config validates its inputs.
Keep helper types close to their concept and avoid generic Repository, DTO, Controller, or Service layers.

## Per-collection SQLite

Recommended layout:

```text
data/
  collections/
    <collection-id>.sqlite
```

No central index is needed for normal requests: the capability carries the public locator.
The operator CLI can enumerate files for explicitly requested maintenance.
This avoids an extra schema, central bearer store, and cross-file transaction protocol.
It does not remove the runtime's authority over the data directory.
A central database becomes reasonable if global search, quotas, accounts, or fleet-wide atomic administration become real requirements; none are currently in scope.

Use canonical filenames constructed from parsed IDs, not user path fragments.
Open existing files without CREATE or URI interpretation; reject symlinks and non-regular targets, and verify the stored collection ID.
A prior existence check followed by ordinary create-capable open is insufficient.
Investigate a small `direct-sqlite` opening adapter feeding `sqlite-simple`; verify the exact selected binding API and flags on both platforms before making this a guarantee.
Keep the directory private and disallow arbitrary ATTACH, extensions, or SQL through application interfaces.
Do not scan every database during a request.

The following four-table schema illustrates the intended constraints.
Timestamp units and storage bounds must be finalized before implementation; the depth limit is four edges.

```sql
CREATE TABLE collection_meta (
  singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
  collection_id BLOB NOT NULL UNIQUE CHECK (length(collection_id) = 16),
  deleted_at INTEGER
) STRICT;

CREATE TABLE article (
  id BLOB PRIMARY KEY CHECK (length(id) = 16),
  title TEXT NOT NULL CHECK (length(title) > 0),
  url TEXT NOT NULL CHECK (length(url) > 0),
  state TEXT NOT NULL CHECK (state IN ('unread', 'read')),
  created_at INTEGER NOT NULL,
  revision INTEGER NOT NULL CHECK (revision >= 0)
) STRICT;

CREATE TABLE capability (
  id BLOB PRIMARY KEY CHECK (length(id) = 16),
  token_hash BLOB NOT NULL UNIQUE CHECK (length(token_hash) = 32),
  parent_id BLOB REFERENCES capability(id) ON DELETE CASCADE,
  scope_article_id BLOB REFERENCES article(id) ON DELETE CASCADE,
  expires_at INTEGER,
  depth INTEGER NOT NULL CHECK (depth BETWEEN 0 AND 4),
  petname TEXT,
  CHECK (parent_id IS NULL OR parent_id <> id),
  CHECK (
    (parent_id IS NULL AND depth = 0 AND expires_at IS NULL
                       AND scope_article_id IS NULL)
    OR
    (parent_id IS NOT NULL AND depth > 0 AND expires_at IS NOT NULL)
  )
) STRICT;
CREATE UNIQUE INDEX one_root ON capability((1)) WHERE parent_id IS NULL;

CREATE TABLE capability_permission (
  capability_id BLOB NOT NULL REFERENCES capability(id) ON DELETE CASCADE,
  permission TEXT NOT NULL CHECK (permission IN
    ('read', 'add', 'title', 'state', 'delete', 'delegate', 'inspect', 'revoke')),
  PRIMARY KEY (capability_id, permission)
) STRICT;
```

NULL article scope means whole collection.
At most one root is enforced by the index; exactly one root for a live collection is a creation/runtime invariant.
The schema alone does not forbid every cycle, validate a URL, enforce parent permission inclusion, ensure the root has the intended rights, or prove a deadline is no later than its parent's.
Validate those in pure code and inside issuance transactions, and revalidate decoded state.
Decode SQL revision metadata separately from Article and check its nonnegative integer representation in the adapter; storage decoders remain unimplemented.

SQLite's [STRICT tables](https://www.sqlite.org/stricttables.html) require SQLite 3.37.0 or later and restrict column storage types.
That is a feature minimum, not a security patch policy.
D4 must verify the actual linked SQLite version; installing a recent CLI executable does not prove the Haskell binding uses it.
The [direct-sqlite package](https://hackage.haskell.org/package/direct-sqlite) offers a system-library build option; do not assume its bundled engine is current merely because a Haskell dependency resolves.

Start with bracketed connections per operation, private cache, short transactions, and bounded busy handling.
Proposed settings are foreign keys ON, trusted schema OFF, rollback journal DELETE, synchronous EXTRA, and secure_delete ON.
Read back important settings: some pragmas have connection-local or transaction-sensitive behavior.
These are candidates requiring platform tests, not a claim of secure erasure.
See [SQLite pragmas](https://www.sqlite.org/pragma.html).
Use WAL or a connection pool only after measuring a need and reviewing their backup, cleanup, and lifetime costs.

Use `BEGIN IMMEDIATE` for writes so authorization, ancestry, target/revision checks, and mutation form one transaction.
Read authorization and protected data from one snapshot.
SQLite serializes writers to each database, allowing the old STM persistence queues to go; see [SQLite isolation](https://www.sqlite.org/isolation.html).
Return a retryable busy/conflict result after a bound, not an indefinite wait.
No network requests while holding a transaction.
Initial deployment supports one application process per data directory; maintenance needs an explicit cross-process exclusion mechanism, not merely an in-process mutex.

## Representative flows

Loading a collection: Web totally parses the bearer; Runtime derives and opens the existing collection file; the protected read transaction verifies metadata, hash lookup, root/ancestor validity, expiry, and Read scope; it returns only authorized articles; Web renders them with escaped text.
Ordinary application logic never chooses an unrelated file.
Invalid locators must not create files or provide a useful collection-existence oracle.

Changing a title: parse the ID, expected revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus title permission and target scope; load the current article and separate revision metadata; reject a revision mismatch; apply `rename title article`; persist a changed article with an incremented revision or accept a matching no-op; commit; issue a server-generated same-application redirect.
Failures cannot leave partial state.

Adding manually: validate submitted values and call the protected Add operation.
Check collection-wide Add authority, quota, and input inside the write path.
Proposed duplicate handling permits duplicate URLs, avoiding a response oracle about whether another article exists.
An add-only receipt must not disclose existing titles, counts, details, or reusable read authority.
Numeric resource limits remain decisions for the relevant task.

Adding with metadata: perform a protected eligibility/quota precheck, fetch via PublicWeb outside the transaction, then recheck authority and quota in the insertion transaction.
An optional opaque fetch permit can bind the precheck to one request/URL but is not durable authorization and is not yet selected.
Revocation during fetching must prevent the final insert.
A supplied manual title avoids fetching; fetch failure should allow manual entry or a hostname fallback rather than preventing bookmarking.

Delegating: generate a fresh secret in trusted code, validate the parent and requested child in a write transaction, insert child metadata/permissions/hash, commit, and show the new URL once.
An interrupted response does not justify storing a recoverable bearer: the operator/user can revoke the unseen grant and issue another.
Parent and child IDs used by the UI are metadata, not secrets.

Revoking: check descendant management authority and delete the selected subtree transactionally.
Apply the approved strict deadline rule, `now < expiresAt`; equality is expired.
An operation ordered before revocation may finish; one whose protected authorization occurs after revocation commits must fail.
Revocation cannot recall bytes already delivered.
Avoid cached witnesses or automatic reparenting.

Deleting the collection (D5): proposed owner-only POST marks it deleted and removes articles and grants in one transaction, then returns a receipt without depending on the now-invalid owner URL.
Keep the tombstone file until controlled startup/offline cleanup can establish no active handles.
Do not unlink a live database while other requests may still hold it.
File removal and backup retention must be explained separately from logical deletion.

## PublicWeb and the browser boundary

PublicWeb grants retrieval of bounded public-site metadata, not arbitrary HTTP.
The production handler should allow only absolute HTTP/HTTPS URLs on the selected standard ports, reject userinfo, resolve and classify IPv4/IPv6 addresses, and exclude loopback, private, link-local, cloud metadata, and other non-public destinations.
Redirects undergo the same checks.
Pin the actual connection to validated addresses while retaining TLS hostname verification and SNI; a second uncontrolled DNS lookup defeats the check.
Handle mixed DNS answers and mapped addresses explicitly.
These are requirements informed by the [OWASP SSRF guidance](https://cheatsheetseries.owasp.org/cheatsheets/Server_Side_Request_Forgery_Prevention_Cheat_Sheet.html), not a claim that a URL validator or stock HTTP manager establishes them.

Proposed limits are three redirects, a five-second total budget including DNS and body reading, and one MiB each for encoded and decoded response bodies.
Accept HTML/XHTML for metadata; fetch no images, scripts, or linked resources.
Do not forward cookies, authorization, referrers, or environment proxy settings.
Review connection reuse before enabling it.
Exact limits and connector APIs belong to the fetch experiment.
If actual connection confinement cannot be demonstrated, retain manual bookmarking and do not ship automatic fetching.
Egress filtering is an additional operational control, not a substitute.

Keep plain HTML.
No concrete interaction examined so far requires JavaScript; this is not a usability-study result.
One-time capability display creates a copy/bookmark step, but it works without scripting.
If that proves awkward, first consider a small handwritten enhancement.
htmx brings a broader DOM and request mechanism; any script on a capability-bearing page can read its bearer links and forms.
No third-party scripts.
A SPA is outside the present need.

Keep GET read-only and use explicit POST mutation routes, removing `_method` override and arbitrary `goto`.
Use a configured canonical public origin, not untrusted Host/forwarded headers, to construct absolute links.
Keep bearer credentials explicit rather than adding ambient authentication cookies.
Origin checks can add defense-in-depth, but are not the authorization mechanism.
Define missing-Origin handling in the web task without breaking ordinary forms.

Apply no-store, no-referrer, nosniff, and a CSP with no scripts, no base URL override, no framing, and same-origin form actions.
Minimize assets; embedding small CSS may remove the runtime static directory, with a matching CSP policy.
Do not put bearer URLs in logs, analytics, exception messages, or outbound metadata requests.
Proxy access logs need the same redaction contract.
HTTPS is mandatory for deployed bearer URLs; localhost development is distinct.

## Alternatives and classification

These comparisons preserve the reasoning; they are not invitations to reopen agreed choices without evidence.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Effects | Existing MTL classes; ReaderT/records or direct function arguments; effectful | effectful is agreed for teaching. Records could suffice for this scope and remain the simplicity benchmark. Broad handlers remain broad authority in either design. |
| Authorization gate | Many permission witnesses; Set plus closed authorized actions; named protected operations | Replace permission records with sets. D3 compares the latter two; preserve exact target binding and fresh checks, not just fewer witness names. |
| Storage boundary | One file per collection; one shared SQLite database | Prefer per-collection files for understandable confinement and independent export. Accept extra open/migration/backup coordination; not OS isolation. |
| Central index | No index; non-secret catalogue; global grant/secret store | No index now. Operator enumeration suffices; no cross-collection product query earns a central authority. |
| SQL layer | sqlite-simple/explicit SQL; Beam; direct-sqlite throughout | Prefer sqlite-simple plus a small low-level opening adapter. Beam buys typed query composition but not authorization or migration correctness; the small fixed query set does not yet earn it. |
| Migrations | Explicit numbered SQL; migrant-sqlite-simple; automatic migration on request | Offline migration is agreed. Prefer tiny explicit transactional steps; evaluate [migrant-sqlite-simple](https://hackage.haskell.org/package/migrant-sqlite-simple) if it reduces bookkeeping without hiding failure semantics. |
| Framework | Servant; a smaller direct WAI/Scotty-style router | Keep Servant unless the first HTML slice demonstrates a clear simplification. Typed routes/link generation already serve capability forms. Avoid elaborate API-type machinery. |
| Browser | Plain HTML; small handwritten enhancement; htmx | Keep no JavaScript until a specific UX problem warrants more page authority. |
| Concurrency | Existing STM workers; SQLite transactions; pool/registry synchronization | Replace persistence workers with transactions. Add synchronization only for a named resource-lifetime invariant. |
| DX | Native Cabal/GHCup; optional small Nix flake; Nix-first/haskell.nix | Native path is canonical. Optional Nix must exercise the same package, not maintain a second architecture or mask broken bounds. |

The [sqlite-simple package](https://hackage.haskell.org/package/sqlite-simple) provides the intended mid-level binding; the adapter/prototype must still prove safe opening, parameter binding, transaction cleanup, and selected compiler compatibility.
The migration library's existence is not evidence that automatic request-time migrations are safe.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Layered module tree | REPLACE | Organize by concepts and concrete trusted boundaries, without controller/DTO/repository duplication by default. |
| Article/read-state/domain values | SIMPLIFY | Keep pure transitions, typed IDs, and non-empty values; tighten constructors/decoders and use structural equality. |
| Permission records and derived ordering | REPLACE | Actual sets with explicit subset semantics; derive ordering only for set elements. |
| Permission-specific witnesses | SIMPLIFY | Keep operation/target binding where it pays; resolve D3 before choosing a generic replacement. |
| Bearer capabilities | KEEP | Preserve possession-based sharing, but replace encoding/storage and add ancestry, bounded attenuation, revocation semantics. |
| SharedRef categories and reusable-link discovery | REPLACE | Immutable parent graph, descendant metadata only, one-time secrets. |
| MTL port/runtime scheme | REPLACE | Selected effectful handlers must narrow authority rather than merely change syntax. |
| App/TestApp duplication | REMOVE | Compose production and test handlers around the same application functions. |
| Compressed aggregate persistence | REPLACE | Per-collection SQLite, explicit SQL, constraints, and transactions. |
| STM persistence queue | REMOVE | Its write-serialization purpose moves to SQLite; keep no educational queue without a remaining need. |
| General errors and redirects | SIMPLIFY | Small expected result types; boundary faults and HTTP control flow kept separate. |
| Configuration | REPLACE | Fail on malformed explicit values; no global environment clearing or file/directory confusion. |
| Scraper/network authority | REPLACE | Restricted PublicWeb handler with actual connector tests. |
| Servant | KEEP | Typed routes and link construction remain useful; assess complexity in a real slice. |
| Lucid HTML | KEEP | Escaped server rendering and plain forms fit the product. |
| No-JavaScript frontend | KEEP | No demonstrated problem currently justifies script authority or a build pipeline. |
| Test suite | REPLACE | Extract durable laws and behavior; remove repeated permutations and isolation-blind fakes. |
| Nix | SIMPLIFY | Optional small flake; remove existing overrides only after validation. |
| CI | REPLACE | Native fast feedback, honest cache inputs, expensive work on separate events. |
| Deployment | SIMPLIFY | Retain non-root/private-state intent, validate one executable and explicit operational dependencies. |

## Tests, maintenance, and delivery

Keep security laws small and reusable.
Article tests check construction, all five equality fields, field preservation, no-ops, idempotence, and restoration after toggling read state twice.
Permission tests exhaust all 65,536 pairs of the eight-permission vocabulary's 256 subsets and all 65,536 nested subset chains.
Scope tests cover inclusion, structural equality, and order laws over two collections and three article IDs reused across them.
Attenuation tests cover local snapshot shapes, ordered failures, deadline/depth boundaries, and generated valid or malformed edges.
Ancestry tests cover complete paths in generated trees, corrupted or missing ancestors, collection binding, deadline/depth boundaries, and bounded traversal of infinite lists.
External consumer checks exercise the ValidChain construction boundary; its check time and snapshots remain policy data, not authentic authority.
Operation tests cover the table over all 256 permission sets and monotonicity over all 6,561 subset pairs, each across whole-collection and two article scopes.
Examples and generated trees cover owner and descendant management, expired targets, actor expiration rechecks, inconsistent ancestor snapshots, and issuance failures.
Structural validation tests preserve non-time checks and bounded traversal while allowing expired targets.
Pure tests exercise policy; separate compile-time/export checks exercise the selected protected API.
Neither replaces HTTP and database tests.

Persistence tests use real temporary databases and independent connections: structural round-trips; direct constraint violations; fail-closed decoding; existing-only opening; wrong metadata; no effects on another file; commit/rollback and busy behavior; controlled revoke-versus-edit/delegate races.
Revision tests belong here: reject older and newer expected revisions even for no-ops, preserve the counter on matching no-ops, increment once per committed change, and handle representation limits without wrapping or partial writes.
Use barriers to establish ordering.
Migration tests cover old supported schemas, interrupted steps, future-version refusal, and unchanged neighboring collections.

HTTP tests cover add-only receipts, cross-object attempts, server-generated redirects, methods, escaping, response headers, malformed/oversized tokens, one-time issuance, and leakage from error paths.
The production fetch handler needs actual resolver/connector tests for forbidden destinations, redirects, TLS hostnames, decompression limits, and timeout cleanup.
End-to-end coverage can stay a few owner/delegate/contributor flows using ordinary HTML forms.

Offline migrations should take maintenance exclusion, identify the exact file and schema version, back up before risky changes, apply ordered transactional steps, check constraints/integrity, and advance the version only on success.
Refuse newer/unknown schemas and concurrent application access.
Use a small explicit version mechanism such as `user_version`; never infer a migration from an exception in a request.
For backups, use the [SQLite backup API](https://www.sqlite.org/backup.html) or a stopped application with all handles closed, not an arbitrary live-file copy.
Backups include grant hashes/metadata; article-only exports deliberately exclude them.
CLI root reset should revoke the old tree and issue a new one-time owner URL.

Use native Cabal on Linux and macOS.
GHC 9.10.3 and Cabal 3.16.1.0 build and test the replacement package on Linux.
HLS 2.14.0.0 loads its library, executable, and tests through the root `hie.yaml`; macOS remains unverified.
This is not yet a wider support matrix.
The independent project and its freeze file pin dependency versions and the index state.
Update them deliberately; do not resolve a moving snapshot every CI run.
Fourmolu 0.20.1.0 and HLint 3.10 check every v2 Haskell source and pass on Linux with the existing formatter style and HLint defaults.
The [roadmap](roadmap.md#21-formatting-and-linting-for-the-new-package) lists the verified commands.
Keep these explicit versions and limit quality checks to the replacement source.
A wrapper/Makefile/Justfile is unnecessary until it shortens a real workflow.
Document the SQLite development dependency if D4 selects the system library.

GitHub CI work is deferred while the local development experience is the priority.
The existing local build, test, formatting, linting, and HLS checks support the current domain work.
When CI work resumes, PR/push CI should run native build/tests and parallel formatting/linting, with least-privilege permissions, cancellation of superseded work, and cache keys including OS, architecture, compiler, Cabal, and dependency inputs.
Keep Haddock, release artifacts/static linking, packaging, optional Nix checks, and wider compiler/platform matrices on suitable main/scheduled/tag jobs.
Measure cold and warm feedback rather than promising a duration.
Do not leave the native path untested because Nix happens to build.

Deployment starts with one non-root process, loopback listener behind a configured TLS proxy, private writable state, and bounded graceful shutdown.
Proposed env names are `ESELSOHR_DATA_DIR`, `ESELSOHR_LISTEN`, `ESELSOHR_PUBLIC_ORIGIN`, and `ESELSOHR_LOG_LEVEL`; exact formats/defaults await the web/config task.
Keep one configuration mechanism.
State the actual runtime libraries and CA-certificate requirements instead of claiming a dependency-free binary.
An optional container packages the same executable.
A small optional Nix flake should support relevant Linux/macOS architectures without blanket broken-package overrides; no heavy framework is justified yet.

Alongside D3–D5, remaining human choices are local to their milestones: numeric input/grant/fetch limits, owner-link recovery UX, deletion/backup retention expectations, and platform support proven affordable by CI.
Preserve those as open questions until the task needs them.
Do not create ADRs for unresolved alternatives or treat this document's examples as approval.
