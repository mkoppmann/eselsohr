# Eselsohr 2

Eselsohr stores web articles to read later or share with others.
It should be a small reference application for secure functional software and object-capability ideas, understandable by one developer.
It remains a bookmark manager, not an archiving service or account platform.

This document describes the current planning decisions.
It is a direction to review as work progresses, not permission to implement every milestone at once.
The legacy implementation remains unchanged.
The replacement has an independent build seed with a version command and a smoke test; no bookmark functionality is implemented.
Read the [investigation](investigation.md) for evidence about the old source, and the [roadmap](roadmap.md) for implementation tasks.
Code and SQL below are design sketches, not compiled or tested interfaces.

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
Use `effectful` for meaningful external authority; the exact effect interfaces remain undecided.
Do not turn every dependency into an effect or expose arbitrary `IO` to ordinary application logic.

Capabilities have one permission set, including permission to delegate, rather than separate sets for use and granting.
Delegation is bounded and recursive.
A child cannot gain permissions, widen its target, or outlive its parent.
Revocation and expiration of an ancestor deny access through its descendants.
The numerical depth limit and complete operation policy still need review.

Keep a long-lived, bookmarkable owner link for management and issuing shorter everyday links.
Everyday links default to seven days; the owner may explicitly choose longer lifetimes.
Delegated management applies to the delegate's own descendants, while the owner can manage the whole tree.

Store cryptographic hashes of high-entropy bearer secrets, not reusable tokens.
Show a newly issued URL once.
Later management can display metadata, revoke a link, or issue a replacement, but cannot recover the original URL.

Support add-only links: a contributor can submit an article without reading other articles.
Submission must not automatically issue read authority.
Editing existing articles requires read permission and the relevant write permission.
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
A detailed security document belongs with the approved operation policy.
Until then, the threat model and property table below preserve the proposed requirements without claiming they are established.

## Implementation sequence

The [roadmap](roadmap.md) defines the milestones, first task cards, parallel experiments, and review gates.
It is the source for task numbering.
Approval of this design is not authorization to run that whole sequence autonomously.

## Existing baseline

The legacy code uses MTL-style interfaces and compressed JSON collection files, not SQLite.
See the [build baseline](investigation.md#build-ci-and-nix-baseline) for its compiler constraint and the replacement's verification results.

The independent `eselsohr-v2` seed builds and tests on Linux with GHC 9.10.3 and Cabal 3.16.1.0.
It provides only `--version` and an assertion-based smoke test.
Dependency versions and the Hackage index state are pinned; offline build/test checks pass using the existing compiler and package-index cache.
The [roadmap](roadmap.md#task-contract-and-verification-vocabulary) lists the verified commands.
A developer's container setup and machine paths must not become project requirements.

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

These are targets.
Tests provide evidence and regression detection, not a proof against all programs, inputs, machines, or deployment mistakes.

| Property | Primary enforcement | Limits and evidence |
| --- | --- | --- |
| Different identifier kinds cannot be accidentally substituted | Distinct abstract Haskell types; nominal roles if a parameterized ID is retained | Compile-time protection for ordinary typed code, not token authenticity; inspect exports/coercions. |
| Titles, URLs, states are validated before ordinary use | Smart constructors and small ADTs; validated database decoders | SQL NOT NULL/CHECK/STRICT adds independent checks where expressible. URL syntax is not destination safety. |
| A bearer cannot feasibly be guessed or recovered from its stored digest | CSPRNG secret and domain-separated cryptographic hash | Computational guarantee assuming entropy and primitives; parsing/round-trip tests cannot prove randomness. |
| A capability for collection/object A cannot act on B | Collection-bound handler, checked database identity, scope policy, bound action target | Runtime checks and module API; integration tests with two files and repeated article IDs. No global Store escape hatch. |
| Delegation never increases effective authority | Pure subset/scope/lifetime/depth checks plus transaction validation of ancestors | SQL foreign keys preserve relationships, not arbitrary cross-row subset laws; exhaustive/property tests. |
| Removing rights cannot enable an operation | Monotone operation policy over actual permission sets | Exhaustive finite checks plus scoped/ancestry generators. Owner status is separate, never inferred from a full set. |
| Unauthorized requests cannot reach protected persistence mutation | Only guarded operations exported from the trusted store implementation | Closed witnesses may add a static gate if selected at D3. Neither witnesses nor tests prove freshness without transactional checking. |
| Revocation/expiry invalidates descendants | Immutable parent chain, bounded validation, cascade deletion or equivalent checked state | Same transaction snapshot as use. Already completed or earlier-linearized work is not retracted; clock is trusted. |
| One collection open/migration cannot select or mutate another | Canonical locator, existing-only open, metadata check, no exposed ATTACH/raw SQL, explicit CLI target | Filesystem ownership/no-symlink policy and two-file failure tests. The runtime still holds directory authority. |
| Concurrent writes preserve authorization and domain invariants | SQLite transactions, revision check, constraints | Multi-connection race tests with barriers, not sleeps. Return success only after commit. |
| PublicWeb cannot connect to forbidden destinations | Production resolver/connector, redirect checks, bounded HTTP behavior | Real connector tests. Egress firewall is defense-in-depth; a policy-only fake is insufficient. |
| Capability URLs do not appear in routine logs or outbound requests | Allowlisted logging fields, redacted errors, canonical routing, no referrer forwarding | HTTP/proxy integration checks; operational log configuration. Browser-side copying remains possible. |
| One database copy does not expose another's grant secrets | Independent random secrets, collection-bound hashing, no central reusable secret store | Storage/content inspection tests demonstrate format properties, not OS isolation or absence of user-pasted secrets. |
| Deleted collections stop authorizing requests | Owner-only transactional tombstone and grant removal; controlled later cleanup | Integration/race tests; filesystem deletion and backup retention are separate operations. |

## Capability model and indirect authority

A capability is more than its permission set.
Effective authority is the tuple of collection, target scope, operations, lifetime, valid ancestors, delegation budget, and nondelegable owner status.
A public collection ID locates a file; only a valid bearer secret authorizes access to it.

Indirect authority is compatible with object capabilities.
A holder who can mint a Read capability can obtain Read authority, even if an API calls this only management.
Maintaining separate use/grant sets would make that distinction easy to misdescribe and would add policy states without confining the holder.
The agreed single set makes use and delegable rights explicit.
`Delegate` allows issuance of a subset; it never creates a right absent from the parent.
Copying the parent's own bearer URL remains possible without `Delegate`.

Proposed values, with constructors private where they protect invariants:

```haskell
newtype CollectionId = CollectionId UUID
newtype ArticleId = ArticleId UUID
newtype CapabilityId = CapabilityId UUID
newtype Title = Title Text
newtype WebUrl = WebUrl URI
newtype Revision = Revision Word64

data ArticleState = Unread | Read
data Scope = WholeCollection | OneArticle ArticleId
data Permission
  = ReadArticles | AddArticles | ChangeTitles | ChangeReadState
  | DeleteArticles | Delegate | InspectLinks | RevokeLinks
  deriving stock (Eq, Ord, Enum, Bounded)
type Permissions = Set Permission

data Action a where
  ListArticles :: Action [Article]
  ChangeTitle :: ArticleId -> Revision -> Title -> Action ()
  IssueChild :: ChildRequest -> Action IssuedLink

mkTitle :: Text -> Either InvalidInput Title
parseWebUrl :: Text -> Either InvalidInput WebUrl
rename :: Title -> Article -> Article
setReadState :: ArticleState -> Article -> Article

attenuates :: GrantSnapshot -> ChildRequest -> Either Rejection ()
validateChain :: Instant -> CollectionId -> [GrantSnapshot]
              -> Either Rejection ValidChain
decide :: Instant -> ValidChain -> Action a -> Either Rejection ()
```

The sketch's `Ord Permission` only orders set elements; inclusion uses `Set.isSubsetOf`, not ordering of sets or records.
Distinct ID newtypes avoid accidental mixing; IDs are not capabilities.
Prefer structural equality for articles and grant snapshots, and an explicit ID comparison when identity is the question.
Bound and normalize titles at their smart constructor; exact length/whitespace policy belongs in the domain task, not in hidden SQL behavior.

`ValidChain` means checked policy input, not proof that a remote client possesses the corresponding secret.
It must not become a long-lived authorization cache.
Validate missing parents, cycles, cross-collection inputs, every attenuation edge, ancestor deadlines, and depth.
Proposed maximum depth is four delegation edges; this number is not approved.
Root is the unique owner, proposed without expiration; children have finite deadlines.
Parent links cannot be reparented.

The proposed operation table needs approval in M3:

| Operation | Required authority |
| --- | --- |
| List/read articles | ReadArticles, with every returned article in scope |
| Add article | AddArticles on WholeCollection; not on an existing OneArticle target |
| Rename/change read state | ReadArticles plus the matching write permission on that article |
| Delete article | ReadArticles plus DeleteArticles on that article; confirm this conjunction with the complete policy |
| Issue child | Delegate, valid ancestry, subset rights, narrower/equal scope, no later expiration, remaining depth |
| Inspect issued links | InspectLinks, limited to own descendants; metadata only |
| Revoke issued link | RevokeLinks, limited to own descendants; cannot revoke ancestor or sibling |
| Delete collection | Nondelegable root-owner status, not merely all eight permissions |

A parent can delegate an article-scoped child only for an article in its own scope.
Scope membership and article existence must be checked transactionally.
Owner management may operate on the complete tree.
Link lists must never reconstruct a bearer or expose sibling grants as reusable alternatives.

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

Illustrative application-facing operations, not a committed effect declaration:

```haskell
loadArticles :: Store :> es => Eff es (Either Rejection [Article])
changeTitle :: Store :> es
            => ArticleId -> Revision -> Title
            -> Eff es (Either ChangeFailure ())
fetchMetadata :: PublicWeb :> es
              => WebUrl -> Eff es (Either FetchFailure Metadata)
```

The Store interpreter validates bearer possession and current authority inside each protected operation's transaction.
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
Timestamp units, bounds, and the example depth limit must be finalized before implementation.

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
A SQL revision integer also needs a checked Haskell conversion and overflow policy.

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

Changing a title: parse the ID, revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus title permission and target scope; load the article and reject a stale revision; apply the pure transformation; persist the new revision; commit; issue a server-generated same-application redirect.
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
Proposed expiry is `now < expiresAt`; equality is expired.
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
Exhaust all pairs of the proposed eight permission sets (65,536 pairs), with representative scope cases and generated valid/invalid grant trees.
Test lost permissions, mismatched collection/article IDs, missing/cyclic/expired ancestors, child expiry/depth, and descendant-only management.
Pure tests exercise policy; separate compile-time/export checks exercise the selected protected API.
Neither replaces HTTP and database tests.

Persistence tests use real temporary databases and independent connections: structural round-trips; direct constraint violations; fail-closed decoding; existing-only opening; wrong metadata; no effects on another file; commit/rollback and busy behavior; controlled revoke-versus-edit/delegate races.
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
GHC 9.10.3 and Cabal 3.16.1.0 build and test the seed on Linux.
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
The existing local build, test, formatting, linting, and HLS checks are sufficient to begin domain work.
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

Alongside D3–D5, remaining human choices are local to their milestones: the complete operation table, numeric input/grant/fetch limits, owner-link recovery UX, deletion/backup retention expectations, and platform support proven affordable by CI.
Preserve those as open questions until the task needs them.
Do not create ADRs for unresolved alternatives or treat this document's examples as approval.
