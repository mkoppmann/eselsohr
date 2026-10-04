# Design

## Context

The legacy application fetches a submitted URL and extracts the first `h1`, not an article archive; see [the scraper](../../../src/Lib/Infra/Adapter/Scraper.hs).
`Command.createArticle` in [Command.hs](../../../src/Lib/App/Command.hs) calls the scraper before `canCreateArticles`, so an unauthorized add attempt can reach the network-fetch path.
[Uri.hs](../../../src/Lib/Domain/Uri.hs) checks schemes, ports, literal addresses, and the name `localhost`, but does not bind a DNS result to the actual socket.
It also exports its constructor and `unfilteredUri`, and the [controller](../../../src/Lib/Ui/Web/Controller/ArticleList.hs) uses unfiltered parsing in Dev and Test modes.
The validation even permits a missing scheme.

The production scraper calls Scalpel's ordinary `scrapeURL` with `MonadIO`.
The inspected code does not establish redirect revalidation, DNS-rebinding protection, public-address socket pinning, or application-specific response limits.
Literal-address validation tests cannot establish those properties.
The [HTTP test setup](../../../test/Test/Ui/Web/Controller/Shared.hs) starts asynchronous servers on fixed port 6980 without retaining and cancelling their handles, and bypasses production URI validation.
The legacy suite therefore does not establish the fetcher's restrictions.

The replacement's `WebUrl` requires an absolute HTTP(S) URL with a nonempty host, no userinfo, and an omitted or default port; see [validated values](../../specs/validated-values/spec.md).
It accepts local and private hosts without DNS or address classification, because URL syntax does not establish a safe network destination.
Protected operations check authority inside the transaction that executes their sealed request, as D3 requires.

## Goals / Non-Goals

**Goals:**

- Metadata for new articles without giving application code arbitrary network IO.
- Evidence from the real resolver and connector, not from a policy-only fake.

**Non-Goals:**

- Making URL parsing responsible for destination safety.
- A general HTTP client for other features.

## Decisions

### A restricted PublicWeb interface

Expose metadata retrieval through a restricted `PublicWeb` interface.
`Fetch` is the narrow metadata interface, `Fetch.Http` owns the network handler, and Runtime alone composes network authority.
Application code never receives a connection manager or arbitrary `IO`.
An illustrative metadata-fetch operation, not a committed effect declaration:

```haskell
fetchMetadata :: PublicWeb :> es
              => WebUrl -> Eff es (Either FetchFailure Metadata)
```

Use `Either` and a small ADT for expected fetch failures.
Do not catch all exceptions as success or fallback, and do not dump raw HTTP exceptions containing URLs and parameters into logs.
This replaces the legacy scraper's broad network authority, classified REPLACE: a restricted PublicWeb handler with actual connector tests.

### Enforce restrictions at the actual connection

The handler must enforce destination restrictions at the actual connection, including redirects and DNS behavior.
Parsing a URL does not establish that fetching it is safe.
Pin the actual connection to validated addresses while retaining TLS hostname verification and SNI; a second uncontrolled DNS lookup defeats the check.
Handle mixed DNS answers and mapped addresses explicitly.
These requirements are informed by the [OWASP SSRF guidance](https://cheatsheetseries.owasp.org/cheatsheets/Server_Side_Request_Forgery_Prevention_Cheat_Sheet.html), not a claim that a URL validator or stock HTTP manager establishes them.

### Proposed limits

Proposed limits are three redirects, a five-second total budget including DNS and body reading, and one MiB each for encoded and decoded response bodies.
These numbers are proposals; the exact limits and connector APIs belong to the fetch experiment.
Accept HTML and XHTML for metadata, and fetch no images, scripts, or linked resources.
Do not forward cookies, authorization, referrers, or environment proxy settings.
Review connection reuse before enabling it.

### Authorization around the fetch

Adding with metadata checks permission to add and the collection quota, fetches through PublicWeb outside the transaction, then repeats those checks in the insertion transaction.
Revocation during fetching must prevent the final insert.
The first check keeps unauthorized requests away from the network, unlike the legacy order.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| Authority across the fetch | Repeat the checks in the insertion transaction; a fetch permit; fetch inside the transaction | Repeat the checks: no network requests while holding a transaction. A fetch permit with a private constructor could record the checked request and URL, but would still require the later checks; this option is not yet selected. |

### Manual fallback

Under D2, manual bookmarking remains the baseline.
A supplied manual title avoids fetching; fetch failure should allow manual entry or a hostname fallback rather than preventing bookmarking.
If tests cannot demonstrate that the connector rejects forbidden destinations, retain manual bookmarking and do not ship automatic fetching.

### Egress filtering

Egress filtering is an additional operational control, not a substitute for the handler's checks.
The security model trusts the operating system and the process's trusted handlers; effects do not sandbox a compromised process.

### Real connector tests

The production fetch handler needs actual resolver and connector tests for forbidden destinations, redirects, TLS hostnames, decompression limits, and timeout cleanup.
Review real forbidden-destination, redirect, rebinding, size and decompression, timeout, and cancellation tests before enabling automatic fetching.
Integration tests need scoped lifetimes, ephemeral ports where a real listener is necessary, and the actual handlers.
A policy-only fake is insufficient evidence.

## Risks / Trade-offs

- [A stock HTTP client resolves the name again or follows redirects internally] → Pin the connection to the validated addresses, apply the checks to each redirect in the handler, and test with the real connector.
- [The connector cannot demonstrate rejection of forbidden destinations] → Keep manual bookmarking and do not ship automatic fetching.
- [A hostile site returns slow, large, or highly compressed responses] → Bound total time and encoded and decoded size, and treat metadata as untrusted text.
- [Authority can change while the fetch runs outside the transaction] → Repeat Add permission and quota checks in the insertion transaction.
- [Application checks do not confine a compromised process] → Egress filtering adds an operational layer; neither replaces the other.
- [Connection reuse is unreviewed] → Keep it disabled until reviewed.

## Open Questions

- The exact redirect, time, size, and concurrency limits, and the connector, TLS, and HTML parsing libraries.
- Which standard ports the handler selects for redirect targets and URLs.
- Whether a mixed DNS answer rejects the name or connects only to its public addresses.
- Whether a fetch failure leads to manual entry, a hostname fallback, or a choice between them.
- Which page elements supply the title; the legacy scraper uses the first `h1`.
- Whether a fetch permit earns its complexity.
- Whether connection reuse can be enabled after review.
