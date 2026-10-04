# Proposal

## Why

D2 lets the first usable version store manually entered URLs and titles, but automatic metadata fetching remains intended functionality.
Fetching a submitted URL from the server invites server-side request forgery (SSRF): a hostile website or DNS answer can direct the request at loopback, private, or cloud metadata addresses.
This change covers milestone M8: add metadata without broadening application code to arbitrary network IO.

## What Changes

- Add a restricted `PublicWeb` interface that grants retrieval of bounded public-site metadata, not arbitrary HTTP.
- Admit only absolute HTTP and HTTPS URLs on selected standard ports, without userinfo, whose resolved IPv4 and IPv6 addresses are public; redirects undergo the same checks.
- Pin each connection to the validated addresses while retaining TLS hostname verification and SNI.
- Bound fetch time and encoded and decoded response size, accept only HTML and XHTML, and fetch no linked resources.
- Forward no cookies, authorization, referrers, environment proxy settings, or bearer URLs.
- Check Add permission and the collection quota before fetching outside the transaction, and again in the insertion transaction, so that revocation during a fetch prevents the insert.
- Keep manual bookmarking: a supplied title avoids fetching, and a fetch failure allows manual entry or a hostname fallback.
- Enable automatic fetching only after real connector tests show that forbidden destinations are rejected.

## Capabilities

### New Capabilities

- `public-web-fetch`: restricted retrieval of public-site metadata when adding an article, and the authorization checks around that fetch.

### Modified Capabilities

None.

## Non-goals

- A crawler, a body archive, or a production flag that bypasses the destination checks.
- Fetching images, scripts, or other linked resources.
- Claims based solely on a fake HTTP handler or on URL validation.
- Replacing manual bookmarking; it remains usable if the experiment fails.
- Egress firewall rules, which are an additional operational control rather than a substitute for these checks.

## Dependencies and approval gates

The address, connector, and response experiment can start after `establish-replacement-package` (M1).
Integration into adding articles needs `add-scoped-store-effects` (M5) and `add-manual-bookmark-web` (M6), not completion of `add-link-sharing-and-owner-lifecycle` (M7).
This change consumes D2, which keeps automatic fetching from blocking manual bookmarking, and D3, whose sealed requests apply to the final insert.
It leaves open the exact redirect, time, size, and concurrency limits, the connector API, connection reuse, and the fetch permit option; see design.md.
It does not consume D5.

## Impact

- New `Fetch` interface and `Fetch.Http` network handler; Runtime alone composes network authority for that handler.
- The Add operation and its web form gain an optional fetch path; manual adding keeps working without it.
- New DNS, HTTP, TLS, and HTML parsing dependencies, chosen and pinned in the connector and parser slices.
- Outbound TLS verification needs trusted CA certificates at runtime; `add-native-deployment` states the actual requirement.
