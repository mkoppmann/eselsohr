# Design

## Context

The private `sqlite-internal` component implements two named protected operations in its Articles module.
`readArticles` and `changeTitle` take a connection, a parsed bearer, and the operation's inputs; each starts a fresh transaction, reloads the bearer's grant and ancestors, samples the clock, checks the operation policy, and executes a private, operation-specific sealed request in that transaction.
Both return the private `StoredArticle`, which pairs an Article with its revision, and a `StoreError` whose values contain no submitted or stored data.
The transaction runners take the clock as an argument, but both named operations pass `getCurrentTime`, although the protected-article-operations spec leaves the trusted clock to the interpreter.
`withExistingCollection` opens the file named by a collection identifier and returns `Unavailable` for a missing file and for every other filesystem failure alike.
The bearer carries the collection identifier, so it is also the locator that selects the file.
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
| App/TestApp duplication | REMOVE | Tests run the production handler with a test clock and directory instead of a second hierarchy. |
| General errors and redirects | SIMPLIFY | Small expected result types; boundary faults and HTTP control flow kept separate. |

## Goals / Non-Goals

**Goals:**

- Application code reads articles and changes titles with only the authority its binding carries.
- Readable signatures that do not require knowing a new architecture framework.

**Non-Goals:**

- Choosing the effects for every later operation; later milestones place their operations in these effects or add effects when they need them.
- Treating effects as a sandbox: they restrict the authority an interface exposes, not arbitrary code running with the process's privileges.
- A separate test handler; the M6 web tests may add a scripted one when forcing faults into responses needs it.

## Decisions

### effectful with small dynamic effects

`effectful` was chosen for the project's teaching purpose; records and function arguments could also express the required interfaces.
Start with `effectful-core` and small dynamic effects, and avoid generated plumbing unless it demonstrably helps.
See the [effectful project](https://github.com/haskell-effectful/effectful) for the library model.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Effects | Existing MTL classes; ReaderT/records or direct function arguments; effectful | effectful is agreed for teaching. Records could suffice for this scope and remain the simplicity benchmark. Broad handlers remain broad authority in either design. |

Every effect operation is first-order and performs one complete protected operation.
No operation takes an action as an argument, so no transaction scope, connection, or callback reaches application code.
Handlers own SQLite and clock access; Web and the CLI translate inputs and results.
Do not turn every dependency into an effect or expose arbitrary IO to application logic.

### Bindings open the collection per operation

A `Binding` holds the canonical collections directory, the parsed bearer, and the trusted clock.
Only the runtime and tests construct one; application code receives effects already handled by it and cannot name a directory, collection, or clock.
The bearer is the locator: the handler takes the collection from the bearer, so a binding cannot pair a bearer with another collection's file.

Each operation opens the file named by the bearer's collection, runs the named protected operation in one transaction, and closes the connection.
Cleanup after exceptions and cancellation stays inside the existing opening and transaction brackets, the handler needs no unlifting from `Eff` into `IO`, and the exclusive use of a connection holds by construction.
Repeated opening, connection setup, and identity checks cost little for the one or two operations of a web request, and each operation already rechecks the identity inside its transaction.
Two operations under one binding run in separate transactions and do not share a snapshot.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Connection lifetime | One connection held for the handler scope | Open per operation. A held connection needs `withEffToIO` around the opening callback, reports opening failures once, and relies on the `Eff` scope for exclusive use. |

### Missing collections are denied

A bearer that names a collection without a file authenticates nothing, so the handler reports it as a denial, identical to an unknown secret.
`withExistingCollection` gains a `Missing` result for a collection path that does not exist; symlinks, other non-regular files, and other filesystem failures stay `Unavailable`, which the handler reports as a fault.
`mkCollectionDirectory` keeps reporting a missing collections directory as `Unavailable`, because that is an operator configuration error.
A missing file is rejected before any database work, so the response time still differs from a bearer lookup; responses are identical, but no constant-time claim is made.

### The binding supplies the trusted clock

`readArticles` and `changeTitle` take the clock as an `IO UTCTime` argument instead of calling `getCurrentTime`.
It stays an action rather than a time value, so each operation samples it inside its transaction, after it begins.
The runtime binds `getCurrentTime`; tests bind fixed, advancing, or blocking clocks to check expiry between calls and cancellation at chosen points.
Whoever constructs a binding chooses its clock, and setting it back revives expired grants, as the security-boundaries spec states for the host clock.
The clock runs while a write transaction holds the collection's lock, so only trusted runtime and test code supplies it.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Clock source | `getCurrentTime` inside the named operations; a Clock effect | Binding field. The hard-coded clock prevents deterministic handler tests. A Clock effect would need unlifting into the transaction's `IO`, and these operations give application code no use for the time. |

### Tests use the production handler

There is no separate test handler.
Tests run application functions through the SQLite handler with a test clock and a temporary collections directory, so they exercise the real authorization, revisions, and cleanup with no second Store implementation to drift.
Existing fixtures create the databases; external behavior is replaced only through the binding.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Test handler | Faithful in-memory model; scripted stub | Production handler with test bindings, at no extra handler code. An in-memory model is a second Store implementation of roughly 150 to 250 lines and would move the title-change revision logic out of the adapter. A scripted stub of roughly 30 to 50 lines can force faults but shares no contract with SQLite; it is deferred to M6. M5 therefore does not demonstrate swapping interpreters. |

### Read and write effects

`ArticleReader` lists articles, and `ArticleWriter` changes them; both are handled from the same binding.

```haskell
data ArticleSelection = AllArticles | SingleArticle ArticleId

data ArticleReader :: Effect where
    ListArticles :: ArticleSelection -> ArticleReader m (Either ReadFailure [Revised Article])

data ArticleWriter :: Effect where
    ChangeTitle :: ArticleId -> Title -> Revision -> ArticleWriter m (Either EditFailure (Revised Article))

runArticleReader :: IOE :> es => Binding -> Eff (ArticleReader : es) a -> Eff es a
runArticleWriter :: IOE :> es => Binding -> Eff (ArticleWriter : es) a -> Eff es a
```

A read selects all articles or one article identifier, and the handler builds the scope from the bound collection; application code never names a collection.
`ListArticles` avoids a clash with the `ReadArticles` permission.

The split removes real authority: code run with only the reader cannot change anything, even under an owner bearer.
Link previewers, crawlers, and prefetching follow bearer URLs with GET, so the read routes of M6 must not be able to mutate.
The enforcement point is the composition of runners for a route; an application function's constraints state which effects it uses, and its route discharges only those.
The effects are independent: an edit form's GET uses the reader, and its POST uses the writer.
The constraint grants no authority, because the binding's bearer decides what each operation may do.

| Line considered | Authority removed | Decision |
| --- | --- | --- |
| Read and write | Read routes cannot mutate under any bearer. | Split. |
| Add and other writes | Little: add-only bearers are already denied reads, and owners may read. | Not now; M6 may split adding if it finds a reason. |
| Edit and delete | Nothing the bearer does not already check. | No split. |
| Link management | Article routes cannot issue or revoke links. | Left to M7. |

M6 places adding, read-state changes, and article deletion in `ArticleWriter` unless it finds a reason to separate adding.
No type-level registry maps operations to required effects or permissions; plain constraints state the same fact readably.

The effects, `ArticleSelection`, `Revision`, `Revised`, and the result types live in a module of the public library that does not import `sqlite-internal`; it is the subject of the export tests.
`Binding` and the runners live in a runtime module that imports `sqlite-internal`.

### Exposing D3's sealed requests

D3 selects named protected operations with private, operation-specific sealed requests, and the private adapter already implements them for reads and title changes.
This change does not add a second authorization path: the SQLite handler calls those named operations, so authentication, current authority, and execution of the exact payload stay in one transaction.
Authorization is not a separate effect called before storage.
Sealed requests stay private to the adapter; they are not reusable bearer tokens or a proof of single use.
No generic authorized-action framework or dispatcher is added.
Pure transitions such as `rename` change an in-memory value; writing it to storage still requires a sealed request at the protected persistence interface.

### Revision metadata at the Store boundary

The application-facing module pairs a value with its revision as `Revised a`, replacing the private `StoredArticle` at the boundary; the handler converts between them.
`Revision` is a newtype over `Integer` with a public constructor and no range check, so a revision cannot be confused with another number.
The adapter checks the range only after authorization, as the protected-article-operations spec orders it.
The interpreter validates bearer possession and current authority inside each operation's transaction, then compares the expected revision with storage before applying an edit, rejecting any mismatch even for an apparent no-op.
Newly stored articles start at revision zero, matching no-ops retain the revision, and committed changes increment it once; the adapter rejects revisions it cannot represent and refuses exhaustion without wrapping or partial writes.
A successful title change returns the stored article and its new revision.

### Result types

Each effect operation returns an `Either` whose failure type lists the expected failures of its operation family and one constructor for storage faults.

```haskell
data StoreFault = Busy | Corrupt | Unavailable

data ReadFailure = ReadDenied | ReadFault StoreFault

data EditFailure
    = EditDenied
    | EditNotFound
    | EditConflict
    | EditInvalidRevision
    | EditRevisionExhausted
    | EditFault StoreFault
```

Reads have no expected failure besides denial, because an authorized read of a missing article returns an empty list.
Title changes and the planned read-state change fail in the same ways, so they share `EditFailure`.
All constructors are nullary, so failures carry no submitted or stored data into logs.

The handler translates opening and transaction results as follows.

| Adapter result | Application result |
| --- | --- |
| `Missing` when opening, `StoreDenied` | `ReadDenied`, `EditDenied` |
| `StoreNotFound`, `StoreConflict`, `StoreInvalidRevision`, `StoreRevisionExhausted` | the matching `Edit` constructor |
| `Busy` when opening, `StoreBusy` | fault `Busy` |
| `InvalidMetadata` when opening, `StoreCorrupt` | fault `Corrupt` |
| `Unavailable`, `UnsupportedSchema`, or `DatabaseFault` when opening; `StoreFault`; `StoreTransactionActive` | fault `Unavailable` |

`StoreTransactionActive` cannot occur while the handler owns its connection; it maps to a fault rather than a domain result.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Fault channel | effectful `Error StoreFault`; IO exceptions thrown by the handler | Fault constructor inside the `Either`. Callers handle faults explicitly, which costs little while routes call one operation each, and contention stays testable by equality. An `Error` effect adds a concept and a `runError` each route must remember. Exceptions would hide failures from signatures. |
| Failure types | Re-exporting `StoreError`; one shared `Failure e` type | One type per operation family. `StoreError` forces callers to handle impossible cases, and a parameterized type adds a concept to save two constructors per family. |

Web maps denials to one invalid-link response, `Busy` to a retryable response, and other faults to an error response with an operator log, and keeps redirects to itself.
The handler catches no exceptions beyond the adapter's existing classification; unexpected exceptions and asynchronous cancellation propagate after the brackets roll back and close the connection.
Do not catch all exceptions as success or fallback, or write raw SQL or HTTP exceptions containing URLs and parameters to logs.

### Time, randomness, and logging

Keep unrestricted IO at interpreters and composition.
Secret and ID generation can stay within trusted creation and issuance operations rather than becoming a global Random effect.
Start logging at boundaries, and add an application logging effect only for a concrete need.

### Slices

1. Adapter prerequisites: the clock argument of the named operations and the `Missing` opening result.
2. The application-facing module with its effects and result types, after the `effectful-core` dependency is pinned.
3. The binding, runners, and result translation, tested with real temporary databases.
4. One application function using both effects, with authority and export tests.

## Risks / Trade-offs

- [The effect machinery adds concepts each reader must learn] → Keep concrete signatures readable and treat records as the simplicity benchmark.
- [A broad handler renamed as an effect keeps broad authority] → Bind each handler to one bearer, split reads from writes, and test exports and authority.
- [A caller wraps a request or action to hide its transaction parameter and keeps it after the transaction ends] → The private interface still cannot execute it in another transaction or unwrap it into independent IO.
- [Retaining a binding looks like retaining current authority] → Every call opens the collection, starts a fresh transaction, and reloads the grants.
- [Opening per operation repeats setup and identity checks] → The cost is small for one or two operations per request; revisit with measurements if a route needs many.
- [Operations under one binding do not share a snapshot] → Edits carry the revision they were based on, and conflicts are reported.
- [A missing collection is rejected faster than a bearer lookup] → Responses are identical; the timing difference is a stated limit.
- [Types and effects are mistaken for a sandbox] → State that they restrict cooperating code only; process compromise reaches every file the process can open.

## Open Questions

- The runtime module holding `Binding` and the runners lives either in a new private library or in the executable; the binding slice settles this when its tasks are expanded.
