# Design

## Context

The private `sqlite-internal` component implements two named protected operations in its Articles module.
`readArticles` and `changeTitle` take a connection, a parsed bearer, and the operation's inputs; each starts a fresh transaction, reloads the bearer's grant and ancestors, samples the clock, checks the operation policy, and executes a private, operation-specific sealed request in that transaction.
Both return the private `StoredArticle`, which pairs an Article with its revision, and a `StoreError` whose values contain no submitted or stored data.
The trusted clock and exclusive use of the connection remain interpreter responsibilities.
No application-facing interface, runtime, or web layer exists yet.

### Legacy evidence

[App](../../../src/Lib/Infra/Monad.hs) wraps `ReaderT AppEnv IO`, implements the port and repository classes, and derives `MonadIO` and `MonadUnliftIO`.
[TestApp](../../../test/Test/Infra/Monad.hs) duplicates much of this structure.
The classes in [Port.hs](../../../src/Lib/App/Port.hs) express useful intent, but the actual constraints matter more than the class names.
For example, `Page.Shared.WithQuery` includes `WithFile`, which includes the data-directory environment and `MonadUnliftIO`.
Ordinary page queries therefore have much broader storage and IO authority than their semantic operation requires.
Renaming these classes to effects without narrowing their handlers would not correct that boundary.

[Domain.Error](../../../src/Lib/Domain/Error.hs) mixes expected validation and authorization failures, storage and internal failures, and redirects.
[Infra.Error](../../../src/Lib/Infra/Error.hs) and `App` add exception-based transport, and the comments acknowledge redirects as a workaround rather than domain failures.
The replacement separates these concerns without introducing dozens of trivial errors.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| MTL port/runtime scheme | REPLACE | Selected effectful handlers must narrow authority rather than merely change syntax. |
| App/TestApp duplication | REMOVE | Compose production and test handlers around the same application functions. |
| General errors and redirects | SIMPLIFY | Small expected result types; boundary faults and HTTP control flow kept separate. |

## Goals / Non-Goals

**Goals:**

- One named operation that application code can call with only the authority its handler is bound to.
- Readable signatures that do not require knowing a new architecture framework.

**Non-Goals:**

- Choosing the final set of Store facets for every later operation; later milestones add facets when their operations need them.
- Treating effects as a sandbox: they restrict the authority an interface exposes, not arbitrary code running with the process's privileges.

## Decisions

### effectful with small dynamic effects

`effectful` was chosen for the project's teaching purpose; records and function arguments could also express the required interfaces.
Start with `effectful-core` and small dynamic effects, and avoid generated plumbing unless it demonstrably helps.
See the [effectful project](https://github.com/haskell-effectful/effectful) for the library model.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Effects | Existing MTL classes; ReaderT/records or direct function arguments; effectful | effectful is agreed for teaching. Records could suffice for this scope and remain the simplicity benchmark. Broad handlers remain broad authority in either design. |

Organize the application around named operations, each a typed function whose effects name only the authority it uses.
Handlers own SQLite, network, and clock access; Web and the CLI translate inputs and results.
Do not turn every dependency into an effect or expose arbitrary IO to application logic.

### Handlers bound to one capability and collection

Store handlers are bound to one presented capability and its collection.
Only the runtime can turn a parsed locator into a connection, and application code never receives a global collection-opening function, a `Connection`, raw SQL, or a data path.
A Store effect offering every operation on every collection would mostly rename dependency injection; it would not establish least authority.
Runtime alone composes directory, network, clock, and process authority.

### Facet constraints show what an operation uses

An application operation's constraints show which Store facet it uses.
An illustrative title change, not a committed effect declaration:

```haskell
changeArticleTitle :: ArticleEditor :> es
                   => ArticleId -> Title -> Integer
                   -> Eff es (Either TitleChangeFailure ())
```

The constraint does not grant authority: the handler's bound bearer determines what the operation may do.
Authorization is not a separate effect called before storage; the Store handler checks it inside the transaction that executes the sealed request, as D3 requires.
Purpose-specific Store facets remain candidates; narrow operation families are useful when they remove real authority, while proliferating effects per CRUD operation is not.
No type-level registry maps operations to required effects or permissions; plain constraints state the same fact readably.

### Exposing D3's sealed requests

D3 selects named protected operations with private, operation-specific sealed requests, and the private adapter already implements them for reads and title changes.
This change does not add a second authorization path: the SQLite handler calls those named operations, so authentication, current authority, and execution of the exact payload stay in one transaction.
Sealed requests stay private to the adapter; they are not reusable bearer tokens or a proof of single use.
No generic authorized-action framework or dispatcher is added.
Pure transitions such as `rename` change an in-memory value; writing it to storage still requires a sealed request at the protected persistence interface.

### Revision metadata at the Store boundary

The protected read supplies articles with revision metadata kept outside Article, so an edit can carry the expected stored version.
A protected title change accepts an article ID, a validated title, and an expected revision.
The interpreter validates bearer possession and current authority inside each operation's transaction, then compares the expected revision with storage before applying an edit, rejecting any mismatch even for an apparent no-op.
Newly stored articles start at revision zero, matching no-ops retain the revision, and committed changes increment it once; the adapter rejects revisions it cannot represent and refuses exhaustion without wrapping or partial writes.

### Time, randomness, and logging

Keep unrestricted IO at interpreters and composition.
Use a Clock only where application policy actually needs time; transactional authorization must use a fresh time sample, which the protected operations already take inside their transactions.
Secret and ID generation can stay within trusted creation and issuance operations rather than becoming a global Random effect.
Start logging at boundaries, and add an application logging effect only for a concrete need.
Test handlers replace external behavior, not a second App/TestApp hierarchy.

### Error handling

Use `Either` and small ADTs for expected input, domain, authorization, conflict, and fetch failures.
A storage fault is not a permission denial, and an unexpected exception is not a domain result.
Translate a small StoreFault family at the handler boundary, keep HTTP redirects in Web, and let asynchronous cancellation unwind bracketed resources.
Do not catch all exceptions as success or fallback, or dump raw SQL or HTTP exceptions containing URLs and parameters into logs.

### Slices

Start from one named operation, give it only the effects it uses, and keep authorization inside the Store handler's transaction.
Separate slices: the smallest semantic effect or interface; the SQLite handler and runtime binding; a deterministic test handler; one common application function with its authority and export tests.
Competing interface worktrees can help the comparison, but select one before expanding coverage.

## Risks / Trade-offs

- [The effect machinery adds concepts each reader must learn] → Keep concrete signatures readable and treat records as the simplicity benchmark.
- [A broad handler renamed as an effect keeps broad authority] → Bind each handler to one bearer and collection, and test exports and authority.
- [A caller wraps a request or action to hide its transaction parameter and keeps it after the transaction ends] → The private interface still cannot execute it in another transaction or unwrap it into independent IO.
- [Retaining a handler looks like retaining current authority] → Every call starts a fresh transaction and reloads the grants.
- [Types and effects are mistaken for a sandbox] → State that they restrict cooperating code only; process compromise reaches every file the process can open.

## Open Questions

- The exact effect interfaces remain undecided: facet granularity and names, the result types exposed to application code, and how the runtime binds a handler to a parsed locator.
  The M5 focused Plan comparison settles them before the first implementation slice.
