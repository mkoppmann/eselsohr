## Purpose

Defines how Eselsohr retrieves bounded metadata from public websites when adding an article, without giving application code arbitrary network access.

## ADDED Requirements

### Requirement: Metadata-only network authority
The `PublicWeb` interface SHALL grant only retrieval of bounded metadata from a public website, not arbitrary HTTP.
Application code MUST NOT obtain a connection, a raw response, or other network access through it.
Returned metadata is untrusted text from a possibly hostile website.

#### Scenario: Fetch returns metadata or a failure
- **WHEN** application code requests metadata for a URL through `PublicWeb`
- **THEN** it receives either extracted metadata or a fetch failure, and no connection or raw response

### Requirement: Accepted URL forms
The fetch handler SHALL request only absolute HTTP and HTTPS URLs on the selected standard ports.
It MUST reject URLs containing userinfo before resolving or connecting.
URL syntax validation alone does not establish that a destination is safe to fetch.

#### Scenario: Userinfo is rejected
- **WHEN** a URL to fetch contains userinfo
- **THEN** the fetch fails without a DNS lookup or connection

#### Scenario: Unsupported scheme or port is rejected
- **WHEN** a URL to fetch uses a scheme other than HTTP or HTTPS, or a port outside the selected set
- **THEN** the fetch fails without a connection

### Requirement: Public destinations only
The fetch handler SHALL resolve the host and classify every IPv4 and IPv6 address before connecting.
It MUST NOT connect to loopback, private, link-local, cloud metadata, or other non-public addresses, whether the URL names them literally or through DNS.

#### Scenario: Name resolving to loopback
- **WHEN** a URL's hostname resolves only to a loopback address
- **THEN** the fetch fails without a connection

#### Scenario: Literal private address
- **WHEN** a URL names a private IPv4 or IPv6 address literally
- **THEN** the fetch fails without a connection

#### Scenario: Cloud metadata address
- **WHEN** a URL names or resolves to a cloud metadata address
- **THEN** the fetch fails without a connection

### Requirement: Mixed answers and mapped addresses
The fetch handler SHALL classify IPv6 addresses that embed an IPv4 address, such as IPv4-mapped addresses, so that they cannot bypass the IPv4 restrictions.
It MUST handle DNS answers that contain both public and non-public addresses explicitly and never connect to a non-public address from such an answer.

#### Scenario: Mapped loopback address
- **WHEN** a URL's hostname resolves to an IPv4-mapped IPv6 loopback address
- **THEN** the fetch fails without a connection

#### Scenario: Mixed DNS answer
- **WHEN** a URL's hostname resolves to both public and private addresses
- **THEN** the handler does not connect to any private address

### Requirement: Redirect checks
The fetch handler SHALL apply the same URL and destination checks to every redirect target before connecting to it.
It MUST follow only a bounded number of redirects.

#### Scenario: Redirect to a private address
- **WHEN** a public site redirects to a URL whose host resolves to a private address
- **THEN** the fetch fails without connecting to that address

#### Scenario: Too many redirects
- **WHEN** a redirect chain exceeds the redirect bound
- **THEN** the fetch fails

### Requirement: Connections pinned to validated addresses
The fetch handler SHALL connect only to the addresses it validated for that request.
It MUST NOT perform a second, uncontrolled DNS lookup between validation and connection.

#### Scenario: DNS answer changes after validation
- **WHEN** a hostname's DNS answer changes to a forbidden address after the handler validated its earlier answer
- **THEN** the connection uses only the validated address

### Requirement: TLS hostname verification
HTTPS fetches over a pinned connection SHALL send the requested hostname through SNI and MUST verify the server certificate against that hostname.

#### Scenario: Certificate for another name
- **WHEN** an HTTPS server at a validated address presents a certificate that does not match the requested hostname
- **THEN** the fetch fails

### Requirement: Bounded time and response size
Each fetch SHALL have a total time budget that includes DNS resolution and body reading.
It MUST limit both the encoded and the decoded response body size.
Exceeding a bound, or cancelling the fetch, fails it and releases its connection.

#### Scenario: Slow server
- **WHEN** a server sends its response more slowly than the time budget allows
- **THEN** the fetch fails within the budget and releases its connection

#### Scenario: Decompression beyond the limit
- **WHEN** a compressed response body decodes to more than the decoded size limit
- **THEN** the fetch fails without retaining the excess

#### Scenario: Cancelled fetch
- **WHEN** the request that started a fetch is cancelled
- **THEN** the fetch stops and releases its connection

### Requirement: HTML metadata only
The fetch handler SHALL accept only HTML and XHTML responses for metadata extraction.
It MUST NOT fetch images, scripts, or other linked resources.

#### Scenario: Non-HTML response
- **WHEN** a server responds with a content type other than HTML or XHTML
- **THEN** the fetch fails

#### Scenario: Page with linked resources
- **WHEN** an HTML page references images, scripts, or stylesheets
- **THEN** the handler makes no further request for them

### Requirement: No forwarded credentials or proxy settings
Outbound metadata requests MUST NOT carry cookies, authorization headers, or referrers.
The fetch handler MUST ignore proxy settings from the process environment.

#### Scenario: Proxy variables in the environment
- **WHEN** the process environment names an HTTP proxy
- **THEN** the handler connects directly to the validated address and sends no cookie, authorization, or referrer header

### Requirement: No bearer URLs in outbound requests
Outbound metadata requests MUST NOT contain bearer URLs or bearer tokens in their URL or headers.

#### Scenario: Add through a capability-bearing page
- **WHEN** a holder adds an article with fetching from a page whose URL contains a bearer
- **THEN** the outbound request contains no part of that bearer

### Requirement: Bounded fetch concurrency
The application SHALL bound the number of metadata fetches in progress at once.

#### Scenario: Many simultaneous add requests
- **WHEN** more add requests with fetching arrive than the concurrency bound allows
- **THEN** no more fetches than the bound are in progress at any time

### Requirement: Authorization before and after fetching
Adding an article with fetched metadata SHALL check Add permission and any collection quota before fetching, perform the fetch outside any database transaction, and repeat both checks in the insertion transaction.
A request that fails the first check MUST NOT reach the network.

#### Scenario: Holder without Add permission
- **WHEN** a holder without AddArticles on the whole collection submits a URL for fetching
- **THEN** the request is denied and no fetch occurs

#### Scenario: Quota exhausted before fetching
- **WHEN** the collection quota is already reached
- **THEN** the request is rejected and no fetch occurs

### Requirement: Revocation during a fetch
Revocation of the presented grant or one of its ancestors that commits while a fetch is in progress SHALL prevent the final insert.

#### Scenario: Grant revoked during the fetch
- **WHEN** the presented grant is revoked after the first check and before the insertion transaction
- **THEN** the insertion transaction denies access and stores no article

### Requirement: Manual title skips fetching
When the holder supplies a title, adding an article SHALL NOT fetch metadata.

#### Scenario: Add with a title
- **WHEN** a holder adds an article with a URL and a valid title
- **THEN** the article is stored with that title and no outbound request is made

### Requirement: Fetch failure does not prevent bookmarking
A fetch failure SHALL leave the holder able to add the article, either by entering a title manually or with a title derived from the URL's hostname.

#### Scenario: Unreachable site
- **WHEN** fetching metadata for a submitted URL fails
- **THEN** the holder can still add the article through manual entry or the hostname fallback

### Requirement: Release gate for automatic fetching
Automatic fetching SHALL be enabled only after tests against the real resolver and connector show that forbidden destinations are rejected, including through redirects and changed DNS answers.
Otherwise the application MUST retain manual bookmarking only.
Tests against a fake HTTP handler do not satisfy this gate.

#### Scenario: Connector tests do not demonstrate rejection
- **WHEN** the connector tests cannot demonstrate that forbidden destinations are rejected
- **THEN** automatic fetching is not shipped and manual adding remains available
