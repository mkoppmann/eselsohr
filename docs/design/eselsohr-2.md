# Eselsohr 2

Eselsohr stores web articles to read later or share with others.
It should be a small reference application for domain-driven design, secure functional software, and object-capability ideas, understandable by one developer.
It remains a bookmark manager, not an archiving service or account platform.

This document describes the current planning decisions.
It is a direction to review as work progresses, not permission to implement every milestone at once.
The legacy implementation remains unchanged.
The replacement has an independent package with a version command, validated values, pure article transitions, grant validation, operation policy, and a private SQLite adapter with opening, schema installation, validated row codecs, bearer parsing/digest lookup, and protected article reads and title changes.
Other protected mutations, the application-facing Store API, and web behavior remain unimplemented.
Read the [investigation](investigation.md) for evidence about the old source, and the [roadmap](roadmap.md) for implementation tasks.
The validated values, article transitions, inclusion predicates, local child attenuation, bounded ancestry validation, operation policy, restricted collection opening, schema, storage codecs, bearer parsing/digest lookup, and protected read and title change below are implemented.
The remaining code describes proposed interfaces.

## Agreed decisions

The rewrite can start fresh: no running instances, stored data, or old capability URLs require compatibility.
Existing behavior, comments, and tests remain useful evidence, but the module layers need not survive.
Keep the README's concise, concrete, understated prose.

D1 is approved: start with a temporary independent `v2/` package selected by `cabal-v2.project`, with no imports from the old `Lib` tree or compatibility adapters.
Cut over to the root package in a later task.

D2 is approved: the first usable version supports manually entered URLs and titles.
Automatic metadata fetching remains intended functionality, but does not block that delivery.
Investigate the restricted fetcher separately.

D3 is approved: use exact-payload sealing behind named protected operations.
Each operation uses a sealed request: a value with a private constructor that holds the complete inputs for that operation.
Authorization checks the presented bearer, collection, and target before creating the request; its type restricts execution to that transaction.
The persistence helper requires this request; pure domain transformations require no authorization argument.
Tasks 4.5–4.6 implement private protected reads and title changes together in Articles, with operation-specific seals and shared transaction and authority helpers.
The application-facing Store API remains deferred to M5.

D4 is approved: use the SQLite engine bundled with `direct-sqlite`, with `systemlib` disabled.
Compiling the engine into the application removes the need to install a separate SQLite library at deployment.
Task 4.2 pins `direct-sqlite` 2.3.29 and `sqlite-simple` 0.4.19.0; the opening tests run against bundled SQLite 3.45.0.
Update the bundled engine with the application's dependencies and rebuild to deliver SQLite fixes.

Persistence ignorance is an agreed constraint: domain values and transitions do not carry revision metadata or enforce database representation limits.
The protected Store owns concurrency checks, and the persistence adapter owns storage representations and their bounds.
Use `effectful` for meaningful external authority; the exact effect interfaces remain undecided.
Do not turn every dependency into an effect or expose arbitrary `IO` to application logic.

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

Organize the application around named operations: the meaningful things a holder can do, such as adding an article, changing its title or read state, deleting it, delegating, and revoking a link.
Create, update, and delete on stored records are not the organizing unit.
Frameworks such as Elixir's Ash declare such operations and derive interfaces from those declarations; Eselsohr expresses them as ordinary Haskell types and functions instead of a resource/action language.
Validated values and meaningful states belong in types, decisions in pure functions, and each operation in a typed function whose effects name only the authority it uses.
Handlers own SQLite, network, and clock access; Web and the CLI translate inputs and results.
Derive mechanical representations, such as row DTOs, typed routes and links, and decoding of raw form input.
Do not derive domain behavior or bypass validated constructors.
Introduce a shared abstraction only after repeated operations show that it removes real duplication.

Expose metadata retrieval through a restricted `PublicWeb` interface.
Its handler must enforce destination restrictions at the actual connection, including redirects and DNS behavior, and bound time and response size.
Parsing a URL does not establish that fetching it is safe.

Native GHCup and Cabal should be the contributor path on Linux and macOS.
Nix remains optional.
Prefer one executable, one configuration mechanism, a private data directory, and a reverse proxy where appropriate.

## Decisions before dependent work

D5: before owner deletion, review transactional deletion with later controlled file cleanup against draining requests and deleting the file immediately.
The first approach is preferred, but retention and cleanup behavior remain open.

## Security boundaries

The [security model](../security-model.md) defines the approved operation policy, threat model, and enforcement requirements.
Pure policy checks the grant fields supplied by its caller.
The private protected read and title change look up the bearer and load its grant and ancestors from storage inside each transaction before checking permission for the operation.
D5 remains open.

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

Protected mutations must load current state and check authority and revision metadata within the transaction.
The adapter's private `StoredArticle` pairs an Article with an Integer revision and is returned by the protected read and title change; the application-facing Store API remains unimplemented.

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
The [operation policy](#operation-policy) defines the permissions and targets allowed for each operation.
The private read and title change check that policy inside their transactions; issuance and other mutations remain unimplemented.

## Local child attenuation

Task 3.4 is implemented in [Capability](../../v2/src/Eselsohr/Capability.hs), with examples and properties in the [Attenuation test group](../../v2/test/Attenuation.hs).
Attenuation prevents a child grant from gaining permissions, widening its scope, or outliving its parent.
The grant record and validator are:

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

The caller supplies the current time, then the parent and child grants.
A root must have no parent, WholeCollection scope, all eight permissions, no expiration, and depth zero.
A child must have a parent ID different from its own, a finite expiration, and depth 1–4.
Depth bounds are checked before incrementing, including for extreme Int inputs.

`attenuates` returns the first applicable error in this order, or `Right ()` when all checks pass:

| Error | Rejection condition |
| --- | --- |
| InvalidParent | Parent fails the root or child field requirements above. |
| ParentExpired | Parent deadline is at or before the supplied time. |
| DelegateRequired | Parent lacks Delegate. |
| DepthExhausted | Parent is at depth four. |
| InvalidChild | Child fails the child field requirements above, including a root supplied as a child. |
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

`GrantSnapshot` has a public constructor; callers can supply grant fields without first validating them.
Equality compares every field, and the record contains no bearer secret or separate owner flag.
Collection identity comes from Scope.
Success establishes only one local edge; ancestry, root uniqueness, existence, revocation, and bearer possession remain unchecked.
There is no reparenting operation, and the future store must enforce immutable persisted parent links.
These functions read no clock and do not issue or persist a grant.

## Bounded ancestry validation

Task 3.5 is implemented in [Capability](../../v2/src/Eselsohr/Capability.hs), with examples and generated trees in the [Chains test group](../../v2/test/Chains.hs).
The caller supplies the check time, expected collection, requested grant ID, and every grant from that grant back to its root:

```haskell
validateChain :: UTCTime -> CollectionId -> CapabilityId -> [GrantSnapshot]
              -> Either ChainError ValidChain

chainGrants :: ValidChain -> NonEmpty GrantSnapshot
chainCheckedAt :: ValidChain -> UTCTime

validateGrantStructure :: CollectionId -> CapabilityId -> [GrantSnapshot]
                       -> Either ChainError (NonEmpty GrantSnapshot)
```

The list contains one to five grants in leaf-to-root order.
The size check inspects at most six list constructors, before reading grant fields, and rejects oversized input without inspecting its remaining tail.
This bounds the list traversal, not the evaluation of arbitrary undefined Haskell values.
Validation returns the first applicable error in this order:

| Error | Rejection condition |
| --- | --- |
| EmptyChain | No grants were supplied. |
| ChainTooLong | More than five grants were supplied. |
| RequestedGrantMismatch | The first grant does not have the requested grant ID. |
| CollectionMismatch | Any scope belongs to a different collection. |
| RepeatedGrantId | An ID occurs more than once, even with different contents. |
| MissingRoot | The final grant still names a parent. |
| InvalidRoot | The final grant fails the root field requirements. |
| InvalidEdge AttenuationError | The first failing edge, checked from root toward leaf, fails local attenuation. |

Each edge uses `attenuates` at the supplied time and preserves its error order.
A root-only chain is valid; the leaf need not retain Delegate unless it is also the root.
Repeated-ID checks and exact parent linkage to a terminal root reject cycles without graph traversal.
Missing intermediate parents, reordered entries, and extra roots fail; the validator does not sort, truncate, repair, or ignore entries.

Task 3.6 adds `validateGrantStructure` for management targets that may have expired.
It applies the same list bound, grant field checks, collection and parent ID comparisons, and delegation limits, omitting only ParentExpired and ChildExpired.
Children still need finite deadlines, and a child of an expiring parent cannot outlive it.
The root must meet all its field requirements.
It returns the grants in the supplied order rather than constructing a ValidChain.

`ValidChain` has a private constructor, read-only accessors, structural Eq, and Show.
It preserves the supplied grants and their order together with the check time.
It has no record-update fields, Read, Generic, or unchecked reconstruction API.
`ChainError` has Eq and Show and contains no submitted grants or identifiers.

Success establishes valid policy inputs at the supplied time, not bearer possession, current database state, or authority to perform an operation.
Keeping the check time does not detect later expiration, revocation, or changes in storage.
Protected operations must reload the grants and validate them inside each transaction.
The private read and title change implement this and check the sole root; revocation execution and immutable persisted parents remain future write-operation responsibilities.
This result is policy input, not a sealed request for a protected operation.

## Operation policy

Task 3.6 is implemented in [Authorization](../../v2/src/Eselsohr/Authorization.hs), with exhaustive permission checks and generated path tests.
The [security model](../security-model.md#operation-policy) is the operation table and decision contract.
Operation values carry the operation and target to check; article edits use domain values, while revision metadata belongs to the future Store API.
Pure decisions do not perform protected effects or construct the Store's sealed requests.

## Bearer format

Task 4.4 implements the approved format in the private [Bearer module](../../v2/persistence/Eselsohr/Store/SQLite/Bearer.hs): `v2.<base64url-uuid>.<base64url-secret>`.
The UUID is 16 network-order UUIDv4 bytes encoded as 22 characters; the secret is 32 bytes encoded as 43 characters, making 69 characters in total.
Both fields use canonical unpadded base64url, including zero unused bits.
The parser checks total length before splitting or decoding and rejects padding, whitespace, unsupported versions, invalid UUIDs, and noncanonical encodings with `InvalidBearer`.
It accepts every 32-byte secret value; parsing cannot establish entropy.
Future issuance must generate 256 random secret bits with a CSPRNG.

The stored digest is exactly `SHA256(ASCII("eselsohr:bearer:v2") || NUL || uuid16 || secret32)`.
The database keeps an independent public grant ID, which is not part of that preimage.
The private component uses pinned `base64-bytestring` 1.2.1.0 and `cryptohash-sha256` 0.11.102.1 with its bundled C implementation enabled.
No password-style slow hash is needed for a uniformly random secret.

```haskell
parseBearer :: Text -> Either BearerError Bearer
renderBearer :: Bearer -> Text
bearerCollection :: Bearer -> CollectionId
lookupBearer :: Connection -> Bearer -> IO (Either LookupError (Maybe StoredGrant))
```

Bearer retains the canonical credential, validated collection ID, and derived digest behind a private constructor.
It has no record-update fields, Show, Read, Generic, or SQL instances; rendering explicitly returns the credential.
Lookup requires an active caller-owned transaction and rechecks collection identity before schema version in that transaction.
It binds the digest as a BLOB and loads at most two matching grant rows and nine permission rows, rejecting duplicate matches and malformed rows through the existing DTO conversions.
An unknown digest returns `Right Nothing`; a match returns a StoredGrant containing the grant fields, token hash, and optional petname.
Decoding checks the stored field values and permission rows, but accepts expired grants.
No transaction is started, committed, or rolled back by lookup.
`TransactionRequired`, `CorruptData`, `LookupBusy`, and `LookupFault` contain no submitted or stored data; unsupported schema versions and other SQL failures are faults, while Busy/Locked are contention.
Conversion errors are sanitized and unexpected exceptions or cancellation propagate.
Ancestry, expiration, fresh time, operation policy, and sealed requests belong to protected operations; retaining a lookup result does not keep authority current.

## Protected read

Task 4.5 implements the named read operation in [Articles](../../v2/persistence/Eselsohr/Store/SQLite/Articles.hs) inside `sqlite-internal`.
Articles owns the named operations, sealed requests, and article SQL; [Internal.Transaction](../../v2/persistence/Eselsohr/Store/SQLite/Internal/Transaction.hs) owns connection settings and transaction lifetime, and [Internal.Authority](../../v2/persistence/Eselsohr/Store/SQLite/Internal/Authority.hs) owns stored ancestry and current policy validation.
The adapter source root is `v2/persistence`.
The `SQLite.Internal` modules are hidden within the adapter library; its exposed modules form the interface for sibling components.
The application-facing Store API remains deferred to M5.

```haskell
readArticles :: Connection -> Bearer -> Scope -> IO (Either StoreError [StoredArticle])

runReadTransaction :: Connection -> Bearer -> IO UTCTime
                   -> (forall tx. ReadTx tx a) -> IO (Either StoreError a)
checkRead :: Scope -> ReadTx tx (ReadRequest tx)
selectArticles :: ReadRequest tx -> ReadTx tx [StoredArticle]
```

`readArticles` supplies `getCurrentTime` and immediately composes `checkRead` with `selectArticles`.
The transaction runner fixes the connection, bearer, and trusted clock and introduces a type parameter `tx` that identifies that transaction.
ReadTx and WriteTx are distinct mode aliases for the private Tx type.
Its mode and transaction parameters have nominal roles: `coerce` cannot change a read action into a write action or substitute a different transaction parameter.
Both expose only Functor, Applicative, and Monad instances.
ReadRequest has a private constructor and nominal transaction parameter.
Read operations require ReadTx; title-change operations require WriteTx, so writes cannot run through the read runner.
The Articles interface exports no connection accessor, context replacement, IO lifting, or runner for an individual region's action.
A ReadRequest seals the exact requested Scope, without record-update fields or reconstruction instances; selection accepts no replacement payload.
Wrapping a request or action to hide its `tx` parameter allows a caller to keep it after the transaction ends.
The interface still cannot execute that value in another transaction or unwrap it into independent IO.

The runner rejects an existing transaction without changing it, verifies the opener's required settings and supported journaling with OFF rejected, and begins a DEFERRED transaction.
It does not change connection settings or retry contention.
Bearer lookup rechecks identity and schema version inside that transaction.
Ancestry follows stored parent IDs to the root, detecting cycles and loading at most five grants, with two-row match and nine-row permission limits per lookup.
A bounded root query requires that the terminal root be the collection's sole root.
After loading the grant and its ancestors, the shared authorizer samples the clock once and runs `validateChain` and `decide` for the requested read scope before Articles constructs the request.
Every named call starts a fresh transaction and reloads authority.

Selection restricts OneArticle reads in SQL and validates every returned row, including revision metadata and duplicate article IDs, before commit.
An authorized missing article or empty collection returns an empty list; results are unpaginated with unspecified ordering.
Success is returned after commit; failures roll back if SQLite still reports an active transaction, and unexpected exceptions or cancellation propagate after cleanup.
The caller must use the connection exclusively and leave its opening callback on failure.
`StoreDenied`, `StoreBusy`, `StoreCorrupt`, `StoreFault`, and `StoreTransactionActive` contain no submitted or stored data.
Unknown bearers, missing ancestors, rejected chains, and policy failures deny access; malformed rows, duplicate matches, and inconsistent roots are corruption.
Busy/Locked are contention; unsupported schema, other SQL failures, and cleanup failures are faults.
Authorization and article selection read the same database state within one transaction.
A read ordered before revocation may finish; a new read after revocation commits must fail.

## Protected title change

Task 4.6 implements the named title change alongside reads in [Articles](../../v2/persistence/Eselsohr/Store/SQLite/Articles.hs).

```haskell
changeTitle :: Connection -> Bearer -> ArticleId -> Title -> Integer
            -> IO (Either StoreError StoredArticle)
runWriteTransaction :: Connection -> Bearer -> IO UTCTime
                    -> (forall tx. WriteTx tx a) -> IO (Either StoreError a)
checkTitleChange :: ArticleId -> Title -> Integer
                 -> WriteTx tx (TitleChangeRequest tx)
writeTitle :: TitleChangeRequest tx -> WriteTx tx StoredArticle
```

`changeTitle` supplies `getCurrentTime` and immediately checks and executes the exact submitted payload.
The write runner starts an IMMEDIATE transaction and fixes its connection, bearer, and clock as the read runner does.
The operation reloads the grant chain, reads the clock, and validates the grants through the shared authorization helper.
The runner commits or rolls back the transaction using the same cleanup code as the read runner.
Requests and actions remain restricted to that transaction.
The runner requires the [write connection settings](#per-collection-sqlite) before executing the operation.
Authorization checks RenameArticle for the bearer's collection and sealed article, requiring ReadArticles and ChangeTitles within scope.
TitleChangeRequest has a private constructor and nominal transaction parameter, with no record-update fields or reconstruction instances.

After authorization, `writeTitle` rejects submitted revisions outside zero through 9223372036854775807 as StoreInvalidRevision.
It loads at most two matching rows, returning StoreNotFound for absence and StoreCorrupt for duplicate or malformed rows.
A revision differing from current storage yields StoreConflict, even if the title would be unchanged.
A matching no-op returns the current StoredArticle without an UPDATE, including at the maximum revision.
A real change at the maximum yields StoreRevisionExhausted without writing.
Otherwise `rename` supplies the changed Article and the adapter increments its separate revision once.
A private update DTO with a ToRow instance binds only the title, new revision, article ID, and current revision; the conditional UPDATE must affect exactly one row or the transaction fails as StoreCorrupt.
StoreError contains no submitted or stored values, and success returns only after commit.

Tests cover exact payloads, authorization before article reads and revision checks, representation limits, and rollback after an UPDATE.
Barrier-coordinated connections verify that competing edits serialize, stale retries conflict, and writes after committed ancestor revocation deny access.
A write ordered before revocation may finish; the revoker must acquire the writer lock afterwards.

## Effects and the protected store API

`effectful` was chosen for the project's teaching purpose; records and function arguments could also express the required interfaces.
Start with `effectful-core` and small dynamic effects; avoid generated plumbing unless it demonstrably helps.
See the [effectful project](https://github.com/haskell-effectful/effectful) for the library model.
Concrete signatures should remain readable without requiring the reader to know a new architecture framework.

Store handlers should be bound to one presented capability and collection.
Only the runtime can turn a parsed locator into a connection.
Application code must not receive a global `openCollection`, `Connection`, raw SQL, or data path.
A Store effect with every operation on every collection would mostly rename dependency injection; it would not establish least authority.

An application operation's constraints should show which Store facet it uses.
An illustrative title change, not a committed effect declaration:

```haskell
changeArticleTitle :: ArticleEditor :> es
                   => ArticleId -> Title -> Integer
                   -> Eff es (Either TitleChangeFailure ())
```

The constraint does not grant authority: the handler's bound bearer determines what the operation may do.
Authorization is not a separate effect called before storage.
The Store handler checks it inside the transaction that executes the sealed request, as D3 requires.

The private protected read supplies articles with revision metadata kept outside Article so an edit can carry the expected stored version.
A protected title change accepts an article ID, a validated title, and an expected revision.
Both operations return the private `StoredArticle` metadata DTO; the application-facing Store interfaces remain deferred.

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
The schema defaults revisions to zero, the codecs check representability, and the protected title change implements transactional comparison, increment, and exhaustion handling.
Pure transitions such as `rename` change an in-memory value.
Writing that value to storage requires a sealed request at the protected persistence interface.

D3 selects named protected operations with private, operation-specific sealed requests.
Each named operation authenticates the presented bearer and checks current authority inside its transaction, then immediately executes the sealed request.
Protected persistence helpers require that request and take their target and payload from it, without accepting replacement values or exposing an unguarded path.
Only the trusted authorization code can construct a seal.

A read request seals the requested scope; a title-change request seals the article ID, validated title, and expected revision.
Both bind the operation to the transaction's fixed presented bearer and collection.
The seal records the exact submitted revision; it does not prove that the revision matches storage.
Policy still checks only policy-relevant inputs, and the persistence helper compares the revision after authorization before applying the change.

The sealing interface must prevent unchecked reconstruction, payload replacement, and execution outside the originating transaction, including through coercion or escaped transaction actions.
Seals remain private to the protected operation; they are not reusable bearer tokens or a proof of single use.
The private read and title change enforce these restrictions through the request types and transaction runners described under [protected read](#protected-read).
The selected design adds no generic authorized-action framework or dispatcher.
It also adds no type-level registry mapping operations to required effects or permissions; plain constraints state the same fact readably.
Purpose-specific Store facets and fetch permits remain separate candidates.
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

This is the intended module layout, not a scaffold to create in advance.
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
Store exposes operations such as reading articles and changing titles, while Store.SQLite owns connections, row decoding, and transaction checks.
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

Task 4.2 implements [restricted opening](../../v2/persistence/Eselsohr/Store/SQLite/Open.hs) in the private Cabal library `sqlite-internal`, with [integration tests](../../v2/test-sqlite/Main.hs).
The public domain library stays independent of SQLite.
`mkCollectionDirectory` resolves an existing directory to a canonical absolute path without creating it.
`withExistingCollection` derives `<canonical-collection-id>.sqlite` from a validated ID, rejects symlinks and non-regular targets, and checks the stored identity followed by schema version 1 before invoking its trusted callback.
The underlying helper uses `direct-sqlite` with ReadWrite, PrivateCache, and NoFollow, then wraps the handle with a fresh `sqlite-simple` savepoint counter.
It never enables CREATE; absolute paths preserve literal URI-significant characters even with SQLite URI support compiled in.
Both path boundaries reject NUL and surrogate code points before passing paths to filesystem or SQLite APIs.
ReadWrite may fall back to a read-only connection; successful opening does not promise that later writes will succeed.
Opening returns Unavailable, InvalidMetadata, UnsupportedSchema, Busy, or DatabaseFault without including paths or stored values.
Metadata query conversion failures, including invalid UTF-8, return InvalidMetadata.
It closes the connection after success, metadata failure, callback exceptions, and cancellation; callback exceptions propagate unchanged.
Callbacks must finalize their statements and must not close or retain the connection.
The directory and its writers are trusted; the adapter is not a sandbox against hostile filesystem replacement.
Tests preserve the bytes and directory entries of closed rejected fixtures; hot-journal recovery is outside that claim.
Keep the directory private and disallow arbitrary ATTACH, extensions, or SQL through application interfaces.
Do not scan every database during a request.

Opening requires a real `collection_meta` table with exactly one row: integer singleton 1 and a 16-byte BLOB containing the requested UUIDv4 in network byte order.
The decoder reads at most two rows and applies the domain ID parser after decoding the bytes.
The private [schema initializer](../../v2/persistence/Eselsohr/Store/SQLite/Schema.hs) installs the four tables below, the supplied collection identity, and `PRAGMA main.user_version = 1` in one `BEGIN IMMEDIATE` transaction.
It requires an already-open empty database at version zero, no active transaction, enabled foreign keys, and supported journaling with OFF rejected.
It reads back the version before committing and rolls back body failures, commit failures, and cancellation before commit, checking SQLite's autocommit state after automatic rollback.
Unexpected exceptions propagate after cleanup; callers must leave the opening callback on failure so the handle closes.
The initializer creates no file, article, bearer, or owner grant.
Opening rejects version zero, negative versions, and newer versions without initializing or migrating them.
Migrations and deletion metadata remain deferred; D5 will settle the deletion lifecycle.

```sql
CREATE TABLE collection_meta (
  singleton INTEGER PRIMARY KEY CHECK (singleton = 1),
  collection_id BLOB NOT NULL UNIQUE CHECK (length(collection_id) = 16)
) STRICT;

CREATE TABLE article (
  id BLOB PRIMARY KEY CHECK (length(id) = 16),
  title TEXT NOT NULL CHECK (length(title) > 0),
  url TEXT NOT NULL CHECK (length(url) > 0),
  state TEXT NOT NULL CHECK (state IN ('unread', 'read')),
  created_at INTEGER NOT NULL,
  revision INTEGER NOT NULL DEFAULT 0 CHECK (revision >= 0)
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

The private [codecs](../../v2/persistence/Eselsohr/Store/SQLite/Codec.hs) use `FromRow` and `ToRow` on `ArticleRow`, `GrantRow`, and `PermissionRow` containing primitive storage fields.
Separate fallible conversions validate those rows and the private `StoredArticle` and `StoredGrant` values; domain modules have no SQL instances or storage limits.
IDs use exactly 16 bytes in network order and pass the existing UUIDv4 parsers.
Title and URL decoding requires valid canonical text rather than silently normalizing stored values.
States and permissions use the explicit strings above.
Timestamps use signed 64-bit Unix microseconds with exact integer arithmetic; submicrosecond precision, leap seconds, and out-of-range times are rejected rather than rounded.
Negative timestamps are supported, and revisions range from zero through 9223372036854775807, checked before narrowing.
Grant decoding checks the 32-byte digest and the [root and child field requirements](#local-child-attenuation), including parent ID, scope, permissions, depth, and deadline, while allowing expired grants.
It reconstructs scope from the caller's checked collection context and assembles at most eight distinct, known permissions keyed to that grant, independent of row order.
Encoding also rejects a grant from a different collection.
Petnames are optional text and are preserved unchanged; no label validation is applied.
These conversions do not authenticate a bearer, check article existence, or validate a child's complete ancestry.
CodecError distinguishes invalid values, values SQLite cannot represent, and collection mismatch without including submitted or stored values.
The adapter replaces SQL row conversion diagnostics with these errors before returning them to its caller.
Rows and stored-value DTOs have no Show instance.

SQLite's [STRICT tables](https://www.sqlite.org/stricttables.html) require SQLite 3.37.0 or later and restrict column storage types.
That is a feature minimum, not a security patch policy.
Use the [direct-sqlite package](https://hackage.haskell.org/package/direct-sqlite) with `systemlib` explicitly disabled, as approved in D4.
Verify the embedded engine's version when updating the dependency and rebuild the application to deliver SQLite fixes.

The opening adapter sets and reads back foreign keys ON, trusted schema OFF, and busy_timeout 0 before validating identity.
Opening and metadata contention fail immediately as Busy; there is no retry loop.
The opening adapter and read runner leave journal mode, synchronous, and secure_delete unchanged.
The write runner requires existing DELETE journaling, sets and reads back synchronous EXTRA and secure_delete ON, and rejects other modes without converting the file.
It checks required settings before BEGIN and rechecks the journal mode and opener settings inside the transaction.
An existing transaction is rejected before any configuration changes.
These settings and transaction behavior are verified on Linux with bundled SQLite 3.45.0; macOS, crash/power-loss durability, and secure erasure remain unverified.
Future protected operations should use short transactions and read back important connection settings.
See [SQLite pragmas](https://www.sqlite.org/pragma.html).
Use WAL or a connection pool only after measuring a need and reviewing their backup, cleanup, and lifetime costs.

The protected title change uses `BEGIN IMMEDIATE` so authorization, ancestry, target/revision checks, and mutation form one transaction.
Read the grants and articles in the same transaction so authorization and article selection see the same database state.
SQLite serializes writers to each database, allowing the old STM persistence queues to go; see [SQLite isolation](https://www.sqlite.org/isolation.html).
Return a retryable busy/conflict result after a bound, not an indefinite wait.
No network requests while holding a transaction.
Initial deployment supports one application process per data directory; maintenance needs an explicit cross-process exclusion mechanism, not merely an in-process mutex.

## Representative flows

Loading a collection: Web totally parses the bearer; Runtime derives and opens the existing collection file; the protected read transaction verifies metadata, hash lookup, root/ancestor validity, expiry, and Read scope; it returns only authorized articles; Web renders them with escaped text.
Application logic never chooses an unrelated file.
Invalid locators must not create files or provide a useful collection-existence oracle.

Changing a title: parse the ID, expected revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus title permission and target scope; load the current article and separate revision metadata; reject a revision mismatch; apply `rename title article`; persist a changed article with an incremented revision or accept a matching no-op; commit; issue a server-generated same-application redirect.
Failures cannot leave partial state.

Adding manually: validate submitted values and call the protected Add operation.
Check collection-wide Add authority, quota, and input inside the write path.
Proposed duplicate handling permits duplicate URLs, avoiding a response oracle about whether another article exists.
An add-only receipt must not disclose existing titles, counts, details, or reusable read authority.
Numeric resource limits remain decisions for the relevant task.

Adding with metadata: check permission to add and the collection quota, fetch via PublicWeb outside the transaction, then repeat those checks in the insertion transaction.
A fetch permit with a private constructor could record the checked request and URL, but would still require those later checks; this option is not yet selected.
Revocation during fetching must prevent the final insert.
A supplied manual title avoids fetching; fetch failure should allow manual entry or a hostname fallback rather than preventing bookmarking.

Delegating: generate a fresh secret in trusted code, validate the parent and requested child in a write transaction, insert child metadata/permissions/hash, commit, and show the new URL once.
An interrupted response does not justify storing a recoverable bearer: the operator/user can revoke the unseen grant and issue another.
Parent and child IDs used by the UI are metadata, not secrets.

Revoking: check descendant management authority and delete the selected subtree transactionally.
Apply the approved strict deadline rule, `now < expiresAt`; equality is expired.
An operation ordered before revocation may finish; one whose protected authorization occurs after revocation commits must fail.
Revocation cannot recall bytes already delivered.
Reload and check the grants for each operation; do not change parent IDs automatically.

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
If tests cannot demonstrate that the connector rejects forbidden destinations, retain manual bookmarking and do not ship automatic fetching.
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
Define missing-Origin handling in the web task without breaking HTML forms.

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
| Application model | CRUD repositories; a declarative resource/action framework such as Ash; named operations as typed functions | Prefer named operations with plain effect constraints. Declarative frameworks derive interfaces from operation descriptions, but add machinery each reader must learn; derive only mechanical representations. |
| Authorization gate | Named operations with private checks; generic sealed actions; operation-specific sealed requests behind named operations | D3 selects the last: store the complete inputs in a private request that executes only in the transaction that checked it. Keeping constructors private and preventing execution in another transaction add implementation and verification work. |
| Storage boundary | One file per collection; one shared SQLite database | Prefer per-collection files to limit each connection to one collection and allow independent export. Accept extra open/migration/backup coordination; not OS isolation. |
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
| Permission-specific witnesses | ADAPT | D3 uses sealed requests at the protected persistence boundary to keep the checked inputs together within one transaction. Pure transformations require no such request. |
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
Attenuation tests cover root and child field requirements, ordered failures, deadline/depth boundaries, and generated valid or malformed parent-child pairs.
Ancestry tests cover complete paths in generated trees, corrupted or missing ancestors, collection binding, deadline/depth boundaries, and bounded traversal of infinite lists.
External consumer checks confirm that callers cannot construct ValidChain without validation; its check time and grants do not establish bearer possession.
Operation tests cover the table over all 256 permission sets and monotonicity over all 6,561 subset pairs, each across whole-collection and two article scopes.
Examples and generated trees cover owner and descendant management, expired targets, actor expiration rechecks, conflicting fields for shared ancestors, and issuance failures.
Structural validation tests preserve non-time checks and bounded traversal while allowing expired targets.
Pure tests exercise policy; source review checks the protected API's exports, payload types, and nominal roles.
Neither replaces HTTP and database tests.
The SQLite opening suite exercises real temporary files, invalid metadata, two collection identities with repeated object IDs, lock contention, and connection cleanup after exceptions and cancellation.

Persistence tests use real temporary databases and independent connections: structural round-trips; direct constraint violations; fail-closed decoding; existing-only opening; wrong metadata; no effects on another file; commit/rollback and busy behavior; controlled revoke-versus-edit/delegate races.
Revision tests belong here: reject older and newer expected revisions even for no-ops, preserve the counter on matching no-ops, increment once per committed change, and handle representation limits without wrapping or partial writes.
Use barriers to establish ordering.
Migration tests cover old supported schemas, interrupted steps, future-version refusal, and unchanged neighboring collections.

HTTP tests cover add-only receipts, cross-object attempts, server-generated redirects, methods, escaping, response headers, malformed/oversized tokens, one-time issuance, and leakage from error paths.
The production fetch handler needs actual resolver/connector tests for forbidden destinations, redirects, TLS hostnames, decompression limits, and timeout cleanup.
End-to-end coverage can stay a few owner/delegate/contributor flows using HTML forms.

Offline migrations should take maintenance exclusion, identify the exact file and schema version, back up before risky changes, apply ordered transactional steps, check constraints/integrity, and advance the version only on success.
Refuse newer/unknown schemas and concurrent application access.
Use a small explicit version mechanism such as `user_version`; never infer a migration from an exception in a request.
For backups, use the [SQLite backup API](https://www.sqlite.org/backup.html) or a stopped application with all handles closed, not an arbitrary live-file copy.
Backups include grant hashes/metadata; article-only exports deliberately exclude them.
CLI root reset should revoke the old tree and issue a new one-time owner URL.

Use native Cabal on Linux and macOS.
GHC 9.10.3 and Cabal 3.16.1.0 build and test the replacement package on Linux.
HLS 2.14.0.0 loads its two libraries, executable, and two test suites through the root `hie.yaml`; macOS remains unverified.
This is not yet a wider support matrix.
The independent project and its freeze file pin dependency versions and the index state.
Update those pins deliberately rather than selecting new dependency versions on each CI run.
Fourmolu 0.20.1.0 and HLint 3.10 check every v2 Haskell source and pass on Linux with the existing formatter style and HLint defaults.
The [roadmap](roadmap.md#21-formatting-and-linting-for-the-new-package) lists the verified commands.
Keep these explicit versions and limit quality checks to the replacement source.
A wrapper/Makefile/Justfile is unnecessary until it shortens a real workflow.
SQLite integration uses the bundled engine and requires no separate system SQLite development package.

GitHub CI work is deferred while the local development experience is the priority.
The existing local build, test, formatting, linting, and HLS checks cover the current domain and SQLite opening, schema, and codec work.
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

Alongside D5, remaining human choices are local to their milestones: numeric input/grant/fetch limits, owner-link recovery UX, deletion/backup retention expectations, and platform support proven affordable by CI.
Preserve those as open questions until the task needs them.
Do not create ADRs for unresolved alternatives or treat this document's examples as approval.
