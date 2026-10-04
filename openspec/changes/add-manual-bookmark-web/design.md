# Design

## Context

The private adapter opens existing collection files, installs the schema into an already-open empty database, parses bearers, and implements protected reads and title changes.
The schema initializer creates no file, article, bearer, or owner grant; this change adds the operator command that does.
`add-scoped-store-effects` provides the application-facing Store interface bound to one presented capability and collection.
Adding articles, changing read state, deleting articles, and describing a presented grant are not yet protected operations; this change adds them following D3.

### Legacy evidence

The legacy [server](../../../src/Lib/Ui/Server.hs) sets no-store, no-referrer, nosniff, and a restrictive CSP, and Lucid provides escaped rendering.
These are worthwhile choices to keep.
The same server applies POST method override and trusts a forwarded client-IP header without a configured trusted-proxy boundary.
The override serves the Servant `Delete` and `Patch` endpoints in the [routes](../../../src/Lib/Ui/Web/Route.hs), which HTML forms reach only through a hidden `_method` field; see [Shared](../../../src/Lib/Ui/Web/Page/Shared.hs).
The same routes take the bearer as an optional `QueryParam "acc"`, so a handler can run without one.
The legacy [access token](../../../src/Lib/Ui/Dto/Accesstoken.hs) serializes a resource and capability identifier and is presented to these fixed routes, so the request, not the capability, selects the operation.
Controllers, such as the [article list controller](../../../src/Lib/Ui/Web/Controller/ArticleList.hs), use unconstrained form `goto` values for redirects.
HTTPS and HSTS behavior also needs an explicit proxy contract; see [AddHsts](../../../src/Network/Wai/Middleware/AddHsts.hs), which enables HSTS from an untrusted `X-Forwarded-Proto` header.

[Config](../../../src/Config.hs) silently falls back for several malformed values.
`clearEnv` unsets the entire process environment, not just Eselsohr variables.
`sanitizePath` adds a trailing directory separator and is also used for TLS certificate and key paths.
The legacy [README](../../../README.md) listens on `127.0.0.1:6979` by default, defaults its base URL to `http://localhost`, and also offers optional public collection creation.

The [HTTP test setup](../../../test/Test/Ui/Web/Controller/Shared.hs) brackets its data directory but starts asynchronous servers without retaining and cancelling their handles.
It uses fixed port 6980 and bypasses production URI validation, so the suite does not establish the production restrictions.
Replacement integration tests need scoped lifetimes, ephemeral ports where a real listener is necessary, multiple collection files, and actual handlers.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Configuration | REPLACE | Fail on malformed explicit values; no global environment clearing or file/directory confusion. |
| Servant | REPLACE | Its REST verbs led to method override, and explicit POST routes with uniform invalid-locator responses gain little from typed routes; see the WAI decision below. |
| Fixed routes with a token parameter | REPLACE | Each bearer has one page derived from its grant; see capability navigation below. |
| Lucid HTML | KEEP | Escaped server rendering and plain forms fit the product. |
| No-JavaScript frontend | KEEP | No demonstrated problem currently justifies script authority or a build pipeline. |

## Goals / Non-Goals

**Goals:**

- A real owner can create a collection, add articles manually, list them, edit titles and read state, and delete articles through plain HTML forms.
- Escaping, explicit bearers, safe methods and redirects, and headers are present from the first endpoint.
- No request reaches an operation unless a presented capability selects it.

**Non-Goals:**

- Usability claims: no concrete interaction examined so far requires JavaScript, but that is not a usability-study result.
- A deployment procedure, TLS termination, or HSTS; they belong to `add-native-deployment` (M11).
- A collection quota; it arrives with the add-only links of `add-link-sharing-and-owner-lifecycle` (M9).
- Enforcing one server process per data directory; the lock belongs to `add-offline-maintenance`, which must exclude a running server anyway.

## Decisions

### D2: manual bookmarking first

The first usable version supports manually entered URLs and titles.
Automatic metadata fetching remains intended functionality, but it does not block this delivery and is investigated separately through the restricted PublicWeb interface.
A supplied manual title avoids fetching altogether.

### Plain WAI and Lucid

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Framework | Plain WAI and Warp with a small router; Servant | Plain WAI. Servant's typed routes and links cover none of this slice's security requirements, and two of its defaults would need overrides. Reconsider Servant when the route count grows well past twenty or a JSON API becomes a goal. |

The web-interface requirements sit in WAI middleware, Warp settings, or handler code under either choice: response headers, body limits, Warp's own error responses, explicit methods, cookies, the public origin, and redirect targets.
Servant adds decoding and typed links on top of them.
In `servant-server-0.20.3.0`, its default 400 formatter returns the parser's message as the response body, and `ReqBody` reads the request body without a size limit.
A typed bearer capture fails in the router while an unknown bearer is denied in the handler, so one response for every invalid locator would need either a global formatter or a `Text` capture parsed in the handler.
An offline solve against the pinned index state counts 38 non-boot packages today, 78 with `wai`, `warp`, `http-types`, and `lucid`, and 131 with `servant-server` and `servant-lucid` added.

Routing is a pure, total function from the request method and path segments to a route.
Routes builds links by hand from the bearer prefix and the configured public origin; unit tests of the routing function and HTTP tests that submit every form action keep links and routes in agreement.
Decode raw form input mechanically but not domain behavior; decoded input still passes through the validated constructors.
Web translates HTTP inputs and results, Routes owns links, and Html renders already-authorized view data.

### Plain HTML without JavaScript

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Browser | Plain HTML; small handwritten enhancement; htmx | Keep no JavaScript until a specific UX problem warrants more page authority. |

One-time capability display creates a copy or bookmark step, but it works without scripting.
If that proves awkward, first consider a small handwritten enhancement.
htmx brings a broader DOM and request mechanism, and any script on a capability-bearing page can read its bearer links and forms.
No third-party scripts; a single-page application is outside the present need.

### Capability navigation

No code path runs unless a presented capability selects it.
Each request passes through three steps:

```
request
   |
   v
route (pure, total) --> invalid-link page | deletion notice | (raw bearer, action)
   |
   v
entry: parse the bearer, construct the binding, run the Store runners    runtime composition
   |
   v
handler: action -> Eff es Page, with only Store interfaces in es           application code
```

The route function recognizes the bearer pages and the deletion notice; everything else receives the invalid-link page.
The entry step parses the bearer totally before any file is opened and is the only code that constructs bindings, as the application-store spec already requires of runtime composition.
Handlers carry no `IOE`, so for cooperating code they can reach articles and grants only through effects bound to the presented bearer.
The entry step authorizes nothing: each Store call remains a named protected operation with its own sealed request, so this is not the generic authorized-action framework that D3 rules out.
The web component is the private library `web` in `web/`, holding `Eselsohr.Config` and the `Eselsohr.Web` modules; the executable depends on it, and the test suite `eselsohr-v2-web-test` in `test-web/` covers it.
Its `build-depends` exclude `sqlite-internal`, `direct-sqlite`, and `sqlite-simple`, so no web module can open a file or a connection, and collection creation stays in code the web component cannot import.
`Eselsohr.Store.Runtime` gives the entry step an opaque collections directory, a function that resolves it once when the server starts, and `bindPresented`, which turns the presented bearer text into a binding with the host clock or into nothing when the text is not a well-formed bearer.
The web component never sees the bearer type, the bearer parser, or a function that opens files.
A property test checks that every path without a well-formed bearer segment, other than the deletion notice, receives the invalid-link response.
These boundaries prevent honest mistakes such as a route that forgets authorization; handlers run in a process with IO, so they are not a sandbox.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Navigation | One page per bearer with fixed POST verbs; per-action capability URLs (web-keys) | One page per bearer. Per-action URLs add no authority separation for the person viewing the page, and stable per-action URLs conflict with hash-only storage. |

Per-action capability URLs, as in Waterken's web-keys, give every action on every target its own unguessable URL.
A page is rendered many times, so its per-action URLs must be reproducible, which requires a stored secret, a server key that would act as a master link, or fresh rows written on every GET.
Signing facets with a key derived from the presented secret and storing only a public key avoids stored secrets, but each facet then lives as long as its parent and cannot be revoked or inspected on its own.
A facet derived from the owner URL would never expire.
The page that shows per-action URLs was loaded with the parent bearer, so they only reduce authority when handed to someone else; that is link issuance in M9, where a child grant can hold one permission for one article and remains inspectable and revocable.
The route vocabulary is public in the repository anyway, and knowing it gives nothing without a 256-bit bearer.

### One page per bearer

A bearer URL is `<public origin>/b/<bearer>`, and every page and form target follows from it:

```
GET  /b/<bearer>           page derived from the grant
POST /b/<bearer>/add       url, title
POST /b/<bearer>/title     article, revision, title
POST /b/<bearer>/state     article, revision, state
POST /b/<bearer>/delete    article
GET  /deleted              public deletion notice, no bearer
```

Each path has exactly one form; a trailing slash or extra segment receives the invalid-link page rather than a redirect that would echo the bearer in a `Location` header.
In-page links and form actions are root-relative, so they never depend on the Host header; absolute URLs from the configured origin are needed only outside pages, such as the owner URL the command prints.
The only query parameter interpreted is the server-generated `added` notice flag, and other query parameters are ignored, so tracking parameters appended by chat applications do not break links.
HEAD requests receive the status and headers of the matching GET without a body, because WAI leaves HEAD to the application.
Forms always carry the article identifier they target, rendered by the server; the protected operation checks it against the grant's scope.

The page is a pure function of the grant description and, when the grant has ReadArticles, of the articles in the grant's scope:

| Grant has | Whole-collection scope | One-article scope |
| --- | --- | --- |
| ReadArticles | list all articles | list that article |
| ChangeTitles or ChangeReadState, with Read | edit forms per listed article | edit forms for that article |
| DeleteArticles, with Read | a deletion form per listed article | a deletion form for that article |
| DeleteArticles, without Read | nothing, because no target can be named | a deletion form for that article, without contents |
| AddArticles | the add form | nothing, because adding needs the whole collection |
| Delegate, InspectLinks, or RevokeLinks | nothing until M9 | nothing until M9 |
| Nothing usable | a neutral page without forms | a neutral page without forms |

A page needs two Store calls: the grant description, then the article read when the grant has ReadArticles.
The M5 design already accepts one or two operations per request; a revocation between them makes the read fail, and the page becomes the invalid-link page.
The page function lists articles newest first by creation time; the Store keeps returning rows without a defined order.
Each slice adds the forms of the operations it implements, so no page offers a form whose route still answers with the invalid-link page.

The add form requires both the URL and the title; automatic titles belong to `add-restricted-metadata-fetch`.
When a submitted value fails validation, the response is 422 with the holder's page again, a fixed message naming the field, and the submitted values filled back into the form as escaped text, so nothing typed is lost without JavaScript.
The page shows the holder only their own input and what the grant already permits them to see.

Redirects after a mutation are a pure function of the action and the grant's scope.
An addition redirects to `/b/<bearer>?added`, whose page shows a fixed notice that is the same for new and stored URLs.
Edits redirect to `/b/<bearer>`.
A deletion redirects to `/b/<bearer>`, except when the grant's scope is the deleted article: deleting a scoped article cascades to the grants scoped to it, so the presented bearer no longer exists and the redirect goes to the public deletion notice.
Scope cannot widen down a grant chain, so a whole-collection grant never loses itself through an article deletion.

### Own-grant description

The page needs to know what the presented grant allows, so a new protected read describes the acting grant to its holder.

| Aspect | Decision | Reason |
| --- | --- | --- |
| Requires | a valid acting chain at the sampled time, and no permission | expired, revoked, and unknown bearers all receive the same invalid-link page |
| Reveals | the acting grant's permissions, scope, and expiration | exactly what the page needs; showing the expiration helps holders |
| Never reveals | the petname, ancestors, depth, or any other grant | the petname is the issuer's private note about the holder, not the holder's data |
| Policy | a new operation that `decide` allows after revalidating the actor | the decision stays in the one policy function and its property tests |
| Interface | a separate read-only grant interface beside `ArticleReader` and `ArticleWriter` | the spec keeps article reads and writes separate, and M9's link inspection extends this interface |
| Transaction | its own deferred read transaction | a describe-then-read page needs no combined operation |

The interface is the effect `GrantReader` in `Eselsohr.Store`, whose `describeGrant` returns a `GrantView` of permissions, scope, and expiration, or `ReadDenied` or `ReadFault`.
A separate module `Eselsohr.Store.Grants` and an operation inside `ArticleReader` were considered; the first splits a small interface across modules, and the second would give every article reader a description operation outside the article interface.
The protected read reuses the authorization step through a variant that also returns the acting grant, from which the view is taken.

### Methods, redirects, origin, and bearers

Keep GET and HEAD read-only and use explicit POST mutation routes, removing `_method` override and arbitrary `goto`.
After a mutation, issue a server-generated same-application redirect.
Use a configured canonical public origin, not untrusted Host or forwarded headers, to construct absolute links.
Keep bearer credentials explicit rather than adding ambient authentication cookies.

Do not check the Origin, Referer, or Fetch Metadata headers.
Without ambient credentials, a forged cross-site request must already contain the bearer, and whoever has the bearer can send the request directly.
Under `Referrer-Policy: no-referrer`, browsers send `Origin: null` with form POSTs, even to the same origin, so a strict Origin match would reject the application's own forms.
`frame-ancestors 'none'` covers clickjacking.

### Headers, assets, and leakage

Apply no-store, no-referrer, nosniff, `X-Robots-Tag: noindex, nofollow`, and a CSP with no scripts, no base URL override, no framing, and same-origin form actions.
Warp answers malformed headers, oversized header blocks, and uncaught exceptions itself, outside WAI middleware, so its exception-response hook renders the same error page with the same headers.
A request line that Warp cannot split into method, path, and version gets no response at all: Warp closes the connection.
In `warp-3.4.15`, the default exception logger hides `InvalidRequest`, whose `BadFirstLine` case carries the raw request line; the replacement logger writes only a fixed category for every exception and never the request.
Minimize assets; embedding small CSS may remove the runtime static directory, with a matching CSP.
Do not put bearer URLs in logs, analytics, or exception messages; proxy access logs need the same redaction contract.
The application keeps no access log, and operator messages, such as startup and storage faults with the collection identifier, go to standard error at one level.

A denial from any Store call becomes the invalid-link page.
`Busy` becomes 503 with `Retry-After: 1` and a fixed page asking the holder to try again; `Corrupt` and `Unavailable` become 500 with the error page and an operator message naming the fault and the collection identifier.
The runtime exposes the collection identifier of a binding for this message, never the bearer; the identifier is half of a bearer URL but grants nothing alone.

Request bodies are read with a limit of 160 KiB and rejected beyond it before decoding.
The limit covers the domain limits in the worst case: 4096 title characters and 8192 URL characters, each up to 4 bytes in UTF-8 and each byte percent-encoded as 3 characters.

HTTPS is mandatory for deployed bearer URLs, and localhost development is distinct.
The application refuses a public origin with plain HTTP unless its host is loopback, and it trusts no forwarded header.
TLS termination and HSTS belong to the reverse proxy contract of `add-native-deployment`.

### Representative flows

Loading a page: the route function matches `GET /b/<bearer>`; the entry step parses the bearer and binds it; the grant description verifies metadata, hash lookup, root and ancestor validity, and expiry; with Read, the protected read selects all articles or the one article of the grant's scope; the pure page function renders escaped text and only the forms the grant permits.
Application logic never chooses an unrelated file, and invalid locators must not create files or provide a useful collection-existence oracle.

Changing a title: parse the ID, expected revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus title permission and target scope; load the current article and its separate revision; reject a revision mismatch; apply `rename`; persist a changed article with an incremented revision or accept a matching no-op; commit; issue a server-generated redirect.
Changing the read state follows the same steps with ChangeReadState and `setReadState`.
Failures cannot leave partial state.

Deleting an article: parse the ID; enter a write transaction; resolve the presented capability and current ancestry; check DeleteArticles and target scope; delete the article and, through the cascade, the grants scoped to it; commit; issue the redirect chosen from the grant's scope.
Read is not required, so the response and the page that offers deletion show no article contents.

Adding manually: validate the submitted values and call the protected Add operation, which checks collection-wide Add authority inside the write transaction and inserts with `INSERT ... ON CONFLICT(url) DO NOTHING`.
If the collection already contains an article with the same canonical URL, the operation stores nothing and leaves that article unchanged.
The duplicate check covers the whole collection, and the receipt is the same as for a new article, so an add-only holder learns nothing about stored articles and cannot change their titles.
An add-only receipt must not disclose existing titles, counts, details, or reusable read authority.

### Protected add, read-state, delete, and description operations

Each is a named protected operation with a private, operation-specific sealed request, like the existing title change.
By D3, each request holds the complete inputs of its operation: for an add, the validated URL and title; for a read-state change, the article ID, the requested state, and the expected revision; for a deletion, the article ID; a description has no input beyond the bearer.
The add, read-state, and delete operations run in an immediate write transaction, and the description in a deferred read transaction; each reloads the bearer and its ancestry, samples fresh time, and checks the operation policy before executing the sealed request.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| One article per URL | UNIQUE index with `ON CONFLICT(url) DO NOTHING`; SELECT then INSERT; both | The index and conflict clause. One statement serves new and stored URLs, keeping their timing close, and a primary-key collision still raises an error. A SELECT would scan the URL column on every add. |
| Deletion revision | none; required only for forms rendered with Read | None. A holder without Read cannot see revisions, and deletion applies to whatever the article currently is. |
| Deleting a missing article | the same result as a deletion; not found | The same result. A holder with DeleteArticles but without Read gets no existence oracle for article identifiers. |

The UNIQUE index on `article(url)` is added to schema version 1 in place, because no collection exists outside tests and there is no compatibility requirement.
Stored URLs are already canonical, so the index compares canonical renderings.
A successful add returns no stored data, so its result cannot reveal whether the URL was new.
ID generation stays within the trusted add operation rather than becoming a global Random effect: a UUIDv4 article identifier comes from 16 random bytes with the version and variant bits set.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Add in the interface | `addArticle` in `ArticleWriter`; a separate `ArticleAdder` effect | `ArticleWriter`, as the M5 design planned. The bearer already decides what a holder may do; a separate effect would add a runner and interface without adding a check. |
| Identifier source | a generator parameter like the clock; drawn inside the operation | A parameter. The named SQLite operation takes an `IO ArticleId` and samples it inside the transaction, like the clock; the runtime runner passes a `crypton` generator, and SQLite tests pass fixed identifiers, so a primary-key collision can be tested. |

The article's creation time is the clock sample of the add transaction.

### Operator collection creation

Collection creation is an operator CLI action, not a web route.
The executable parses `serve`, `create-collection`, and `--version` by pattern matching on its arguments; three commands without options need no parsing library.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Location | a module in `sqlite-internal`; a private administration library; `store-runtime` | `Eselsohr.Store.SQLite.Create` in `sqlite-internal`. The web component does not depend on `sqlite-internal`, so it cannot import creation; a separate library would hold one function, and `store-runtime` is a dependency of the web component. |
| Randomness | inside the creation module; in the executable | Inside the module, so the trusted creation code owns it and the test suite can check that two collections differ; a variant taking explicit identifiers and secret serves tests. |
| Atomicity | one transaction; `initializeSchema` followed by a second transaction | One transaction, so a collection never exists without its owner grant. |

`createCollection` takes the resolved collections directory and generates the collection identifier, the root grant identifier, and the 256-bit secret from `getRandomBytes` of `crypton`, which reads the operating system's generator; UUIDv4 identifiers come from 16 random bytes with the version and variant bits set.
`warp` already brings `crypton` into the plan, so the direct dependency of `sqlite-internal` adds no package.
It creates `<collection-id>.sqlite` exclusively with mode 0600, so it never opens or overwrites an existing file, and other local users cannot read titles and URLs.
In one immediate transaction it installs the schema and identity and inserts a root grant with all eight permissions, whole-collection scope, no expiration, depth zero, and no petname.
The schema module therefore provides its installation as a step without its own transaction, which both `initializeSchema` and creation use.
Any failure after the file exists deletes the file, so a failed creation leaves nothing behind.
The bearer module builds the owner bearer from the collection identifier and secret with the same hashing as `parseBearer`, and only the hash is stored.

The command prints the owner URL, `<public origin>/b/<bearer>`, once on standard output, together with text saying that the URL grants full access and cannot be shown again.
The owner link is long-lived and bookmarkable; later management can display metadata or issue replacements but cannot recover the original URL.

### Configuration

Both commands read the same environment variables, and nothing else configures the application:

| Variable | Format | Default |
| --- | --- | --- |
| `ESELSOHR_DATA_DIR` | an existing directory containing an existing `collections/` directory | none; required |
| `ESELSOHR_LISTEN` | `host:port`, where the host is a name, a dotted IPv4 address, or a bracketed IPv6 address, such as `[::1]:6979` | `127.0.0.1:6979`, as in the legacy application |
| `ESELSOHR_PUBLIC_ORIGIN` | exactly a lowercase `http` or `https` scheme, a lowercase ASCII host name (punycode for international names) or IP literal, and an optional port, without trailing slash, path, query, fragment, or user information | none; required |

A set but malformed value, a missing required value, or a missing directory stops the command before it does anything else.
Nothing creates the data or collections directory.
A listener host name is resolved when the server starts, and a name that cannot be resolved stops it.
The public origin is not normalized: a trailing slash or uppercase letters stop the command with a message naming the variable, so the configured text is the text every printed URL starts with.
Configuration loading takes the variable lookup as a function, so tests supply values without changing the process environment.
`http` is accepted only for loopback hosts: `localhost`, `127.0.0.0/8`, and `::1`.
There is no log level setting.
The data directory leaves room for files beside `collections/`, such as the process lock of `add-offline-maintenance`.
Initial deployment supports one application process per data directory, and this change does not enforce it.

### Tests

HTTP tests cover add-only receipts, cross-object attempts, server-generated redirects, methods including HEAD, escaping, response headers on page, error, and Warp-generated responses, malformed and oversized tokens and bodies, one-time issuance, and leakage from error paths.
Unit tests cover the route function, including the property that pages without a well-formed bearer receive the invalid-link response, and the pure page and redirect functions over the grant table above.
End-to-end coverage stays a few owner and contributor flows using HTML forms.
Tests use scoped server lifetimes, ephemeral ports, and several real collection files.
Servers run under Warp's `testWithApplicationSettings`, and a small helper over `network` writes raw requests and parses status lines and headers, so tests can also send malformed request lines; no HTTP client library is added.
The server takes its operator-message sink as a function, so tests capture what would reach standard error.
The invalid-link page answers 404, an oversized body 413, a malformed request 400, and an uncaught exception 500, each with the same headers.

## Risks / Trade-offs

- [A bearer URL leaks through browser history, extensions, or screenshots] → no-store, no-referrer, and log redaction reduce propagation; they cannot remove every copy.
- [Pasting a bearer URL into a chat application lets its link previewer fetch the page, including titles and URLs] → `X-Robots-Tag` stops well-behaved crawlers from indexing, not previewers; a click-through page before contents is a trade-off for M9, when links are shared.
- [A proxy logs bearer URLs] → Document the redaction contract for proxy access logs; the application cannot enforce it.
- [Hand-written links drift from their routes] → Unit-test the routing function and submit every form action in HTTP tests; reconsider Servant when the route table grows.
- [A new route bypasses the entry step] → The route function's result type admits only bearer pages and the deletion notice, and its property test covers every other path.
- [New and stored URLs differ in timing] → One conflict-clause statement serves both; index work and page caching can still differ slightly, and the difference is not measured.
- [Plain HTML makes one-time URL display awkward] → Accept the copy or bookmark step; consider a small handwritten enhancement only for a specific problem.
- [Tests pass against fakes that bypass production validation] → Use actual handlers, real files, and ephemeral listeners.

## Open Questions

None block this change.
The collection quota is deferred to `add-link-sharing-and-owner-lifecycle`; once it exists, manual and fetched adds both check it, with Add authority and input, inside the insertion transaction.
Enforcing one process per data directory is deferred to `add-offline-maintenance`.
