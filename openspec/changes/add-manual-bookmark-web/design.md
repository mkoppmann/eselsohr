# Design

## Context

The private adapter opens existing collection files, installs the schema into an already-open empty database, parses bearers, and implements protected reads and title changes.
The schema initializer creates no file, article, bearer, or owner grant; this change adds the operator command that does.
`add-scoped-store-effects` provides the application-facing Store interface bound to one presented capability and collection.
Adding articles and changing read state are not yet protected operations; this change adds them following D3.

### Legacy evidence

The legacy [server](../../../src/Lib/Ui/Server.hs) sets no-store, no-referrer, nosniff, and a restrictive CSP, and Lucid provides escaped rendering.
These are worthwhile choices to keep.
The same server applies POST method override and trusts a forwarded client-IP header without a configured trusted-proxy boundary.
Controllers, such as the [article list controller](../../../src/Lib/Ui/Web/Controller/ArticleList.hs), use unconstrained form `goto` values for redirects.
HTTPS and HSTS behavior also needs an explicit proxy contract; see [AddHsts](../../../src/Network/Wai/Middleware/AddHsts.hs).

[Config](../../../src/Config.hs) silently falls back for several malformed values.
`clearEnv` unsets the entire process environment, not just Eselsohr variables.
`sanitizePath` adds a trailing directory separator and is also used for TLS certificate and key paths.
The legacy [README](../../../README.md) also offers optional public collection creation.

The [HTTP test setup](../../../test/Test/Ui/Web/Controller/Shared.hs) brackets its data directory but starts asynchronous servers without retaining and cancelling their handles.
It uses fixed port 6980 and bypasses production URI validation, so the suite does not establish the production restrictions.
Replacement integration tests need scoped lifetimes, ephemeral ports where a real listener is necessary, multiple collection files, and actual handlers.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Configuration | REPLACE | Fail on malformed explicit values; no global environment clearing or file/directory confusion. |
| Servant | KEEP | Typed routes and link construction remain useful; assess complexity in a real slice. |
| Lucid HTML | KEEP | Escaped server rendering and plain forms fit the product. |
| No-JavaScript frontend | KEEP | No demonstrated problem currently justifies script authority or a build pipeline. |

## Goals / Non-Goals

**Goals:**

- A real owner can create a collection, add articles manually, list them, edit titles and read state, and delete articles through plain HTML forms.
- Escaping, explicit bearers, safe methods and redirects, and headers are present from the first endpoint.

**Non-Goals:**

- Usability claims: no concrete interaction examined so far requires JavaScript, but that is not a usability-study result.
- A deployment procedure; it belongs to `add-native-deployment` (M11).

## Decisions

### D2: manual bookmarking first

The first usable version supports manually entered URLs and titles.
Automatic metadata fetching remains intended functionality, but it does not block this delivery and is investigated separately through the restricted PublicWeb interface.
A supplied manual title avoids fetching altogether.

### Servant and Lucid

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Framework | Servant; a smaller direct WAI/Scotty-style router | Keep Servant unless the first HTML slice demonstrates a clear simplification. Typed routes/link generation already serve capability forms. Avoid elaborate API-type machinery. |

Derive mechanical representations such as typed routes and links and the decoding of raw form input, but not domain behavior; decoded input still passes through the validated constructors.
Web translates HTTP inputs and results, Routes owns typed links, and Html renders already-authorized view data.

### Plain HTML without JavaScript

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Browser | Plain HTML; small handwritten enhancement; htmx | Keep no JavaScript until a specific UX problem warrants more page authority. |

One-time capability display creates a copy or bookmark step, but it works without scripting.
If that proves awkward, first consider a small handwritten enhancement.
htmx brings a broader DOM and request mechanism, and any script on a capability-bearing page can read its bearer links and forms.
No third-party scripts; a single-page application is outside the present need.

### Methods, redirects, origin, and bearers

Keep GET read-only and use explicit POST mutation routes, removing `_method` override and arbitrary `goto`.
After a mutation, issue a server-generated same-application redirect.
Use a configured canonical public origin, not untrusted Host or forwarded headers, to construct absolute links.
Keep bearer credentials explicit rather than adding ambient authentication cookies.
Origin checks can add defense in depth, but they are not the authorization mechanism.

### Headers, assets, and leakage

Apply no-store, no-referrer, nosniff, and a CSP with no scripts, no base URL override, no framing, and same-origin form actions.
Minimize assets; embedding small CSS may remove the runtime static directory, with a matching CSP.
Do not put bearer URLs in logs, analytics, or exception messages; proxy access logs need the same redaction contract.
HTTPS is mandatory for deployed bearer URLs, and localhost development is distinct.

### Representative flows

Loading a collection: Web totally parses the bearer; Runtime derives and opens the existing collection file; the protected read transaction verifies metadata, hash lookup, root and ancestor validity, expiry, and Read scope; it returns only authorized articles; Web renders them with escaped text.
Application logic never chooses an unrelated file, and invalid locators must not create files or provide a useful collection-existence oracle.

Changing a title: parse the ID, expected revision, and Title; enter a write transaction; resolve the presented capability and current ancestry; check Read plus title permission and target scope; load the current article and its separate revision; reject a revision mismatch; apply `rename`; persist a changed article with an incremented revision or accept a matching no-op; commit; issue a server-generated redirect.
Changing the read state follows the same steps with ChangeReadState and `setReadState`.
Failures cannot leave partial state.

Deleting an article: parse the ID; enter a write transaction; resolve the presented capability and current ancestry; check DeleteArticles and target scope; delete the article; commit; issue a server-generated redirect.
Read is not required, so the response and the page that offers deletion show no article contents.

Adding manually: validate the submitted values and call the protected Add operation, which checks collection-wide Add authority and the input inside the write path.
If the collection already contains an article with the same canonical URL, the operation stores nothing and leaves that article unchanged.
The duplicate check covers the whole collection, and the receipt is the same as for a new article, so an add-only holder learns nothing about stored articles and cannot change their titles.
An add-only receipt must not disclose existing titles, counts, details, or reusable read authority.

### Protected add, read-state, and delete operations

Each is a named protected operation with a private, operation-specific sealed request, like the existing title change.
By D3, each request holds the complete inputs of its operation: for an add, the validated URL and title; for a read-state change, the article ID, the requested state, and the expected revision; for a deletion, the article ID.
Each runs in an immediate write transaction that reloads the bearer and its ancestry, samples fresh time, and checks the operation policy before executing the sealed request.
ID generation can stay within the trusted add operation rather than becoming a global Random effect.

### Operator collection creation

Collection creation is an operator CLI action, not a web route.
The command creates `<collection-id>.sqlite` under the collections directory, installs the schema and identity, and inserts a root grant with all eight permissions and no expiration.
It generates the 256-bit secret with a CSPRNG in trusted code, stores only its hash, and prints the owner URL once.
The owner link is long-lived and bookmarkable; later management can display metadata or issue replacements but cannot recover the original URL.

### Configuration

Keep one configuration mechanism and fail on malformed explicit values.
Proposed environment names are `ESELSOHR_DATA_DIR`, `ESELSOHR_LISTEN`, `ESELSOHR_PUBLIC_ORIGIN`, and `ESELSOHR_LOG_LEVEL`; their exact formats and defaults await the configuration slice.
Initial deployment supports one application process per data directory.

### Tests

HTTP tests cover add-only receipts, cross-object attempts, server-generated redirects, methods, escaping, response headers, malformed and oversized tokens, one-time issuance, and leakage from error paths.
End-to-end coverage stays a few owner and contributor flows using HTML forms.
Tests use scoped server lifetimes, ephemeral ports, and several real collection files.

## Risks / Trade-offs

- [A bearer URL leaks through browser history, extensions, or screenshots] → no-store, no-referrer, and log redaction reduce propagation; they cannot remove every copy.
- [A proxy logs bearer URLs] → Document the redaction contract for proxy access logs; the application cannot enforce it.
- [Servant's type machinery outweighs its benefit in a small slice] → Assess it in the first HTML slice and keep the API types simple.
- [Plain HTML makes one-time URL display awkward] → Accept the copy or bookmark step; consider a small handwritten enhancement only for a specific problem.
- [Tests pass against fakes that bypass production validation] → Use actual handlers, real files, and ephemeral listeners.

## Open Questions

- The add slice must decide whether a check inside the write transaction or a UNIQUE index on the article URL enforces one article per URL; an index changes the schema.
  The duplicate and insertion paths may still differ in timing; the slice should keep that difference small and document it.
- Numeric resource limits, such as request body size and a collection quota, remain decisions for the slice that needs them.
  Once a quota exists, manual and fetched adds both check it, with Add authority and input, inside the insertion transaction.
- The deletion slice must decide whether deletion compares an expected revision, which a holder without Read cannot see, how it answers for an article that is already gone, and how a holder without Read reaches the deletion form.
- Missing-Origin handling must be defined in the web slice without breaking HTML forms.
- The configuration names above are proposals; their formats and defaults remain open.
- How an instance enforces HTTPS for deployed bearer URLs, including any HSTS behavior, needs an explicit proxy contract.
