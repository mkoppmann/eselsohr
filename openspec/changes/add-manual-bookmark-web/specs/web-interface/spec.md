## Purpose

Defines the browser boundary of Eselsohr: capability navigation with one page per bearer, escaped server-rendered HTML forms without JavaScript, safe methods and redirects, explicit bearer handling, response headers, link construction, and limits on where bearer URLs appear.

## ADDED Requirements

### Requirement: Escaped server-rendered HTML without scripts
The web interface SHALL render HTML on the server, escaping all article, title, URL, and other stored or submitted text.
Pages MUST work with plain HTML forms and MUST NOT include JavaScript, including third-party scripts.

#### Scenario: Title containing markup
- **WHEN** an article whose title contains `<script>` and quote characters is listed
- **THEN** the page shows the title as text and contains no script element

#### Scenario: Use without scripting
- **WHEN** a holder lists, adds, and edits articles in a browser with JavaScript disabled
- **THEN** every flow completes with plain forms

### Requirement: One page per bearer
Each bearer URL SHALL have exactly one page, derived from the presented grant's own permissions and scope.
The page MUST offer only the forms that grant permits, and every link and form target MUST be rendered by the server, so a holder never constructs a URL.

#### Scenario: Add-only holder
- **WHEN** a holder whose grant has only AddArticles on the whole collection opens its bearer URL
- **THEN** the page shows the add form and no article, count, or other form

#### Scenario: Owner
- **WHEN** the owner opens the owner URL
- **THEN** the page lists every article with its edit and deletion forms and shows the add form

#### Scenario: Grant without usable permissions
- **WHEN** a holder whose grant permits no action available on the page opens its bearer URL
- **THEN** the page shows no article and no form

### Requirement: No operation without a capability
Every request SHALL present a well-formed bearer in its path before any operation runs, except requests for the public deletion notice.
Any other request MUST receive the invalid-link response, whatever its method or path.

#### Scenario: Path without a bearer
- **WHEN** a request names a path that does not start with a bearer segment and is not the deletion notice
- **THEN** it receives the invalid-link response and no collection is opened

#### Scenario: Unknown action under a valid bearer
- **WHEN** a request appends an unknown segment to a valid bearer URL
- **THEN** it receives the invalid-link response and nothing changes

### Requirement: Bearer URL form
A bearer URL SHALL be the configured public origin followed by `/b/` and the bearer.
Each page and form target MUST have exactly one path form; a trailing slash or extra segment receives the invalid-link response instead of a redirect.
The only query parameter interpreted is the server-generated `added` notice flag, and other query parameters are ignored.

#### Scenario: Trailing slash
- **WHEN** a request appends a slash to a valid bearer URL
- **THEN** it receives the invalid-link response and no `Location` header contains the bearer

#### Scenario: Appended tracking parameter
- **WHEN** a holder opens a valid bearer URL with an unrelated query parameter appended
- **THEN** the page is the same as without the parameter

### Requirement: Read-only GET and HEAD requests
GET and HEAD requests SHALL NOT change stored state.
A HEAD request MUST receive the status and headers of the matching GET request without a body.

#### Scenario: Prefetching a page
- **WHEN** a browser or proxy repeats any GET or HEAD request of the interface
- **THEN** no article, grant, or collection changes

#### Scenario: HEAD request for a page
- **WHEN** a client sends HEAD for a bearer URL
- **THEN** it receives the status and headers of the GET response and no body

### Requirement: Explicit POST mutations
Every mutation SHALL use an explicit POST route for that operation.
The interface MUST NOT honour method override parameters such as `_method`.

#### Scenario: Method override attempt
- **WHEN** a POST request carries a `_method` field naming another method
- **THEN** the request is handled as the POST route it targets, or rejected, and never as the named method

### Requirement: Server-generated redirects
After a mutation, the interface SHALL redirect to a location that the server generates from the operation and the grant's scope.
It MUST NOT accept a redirect target, such as `goto`, from the request.
An addition redirects to the bearer page with the `added` notice flag, edits redirect to the bearer page, and a deletion that removed the holder's own grant redirects to the public deletion notice.

#### Scenario: Submitted redirect target
- **WHEN** a mutation request includes a field naming an external or arbitrary redirect target
- **THEN** the response does not redirect to that target

#### Scenario: Holder deletes its own article
- **WHEN** a holder whose grant is scoped to one article deletes that article
- **THEN** the response redirects to the deletion notice, which needs no bearer

### Requirement: Canonical public origin
Absolute links SHALL be constructed from the configured canonical public origin, and links within pages SHALL be root-relative.
The interface MUST NOT derive link origins from the request's Host header or forwarded headers.

#### Scenario: Spoofed Host header
- **WHEN** a request arrives with a Host or forwarded header naming another origin
- **THEN** no link or redirect in the response names that origin

### Requirement: Explicit bearer credentials
Requests SHALL carry their bearer explicitly in the URL they request.
The interface MUST NOT set or accept ambient authentication cookies.
Authorization MUST depend only on the presented bearer and current stored state, not on the Origin, Referer, or Fetch Metadata headers.

#### Scenario: No credential cookies
- **WHEN** a holder uses any page of the interface
- **THEN** no response sets a cookie, and authorization depends only on the presented bearer

#### Scenario: Form POST with a null Origin
- **WHEN** a browser submits a form of the interface with `Origin: null`, as browsers do under the no-referrer policy
- **THEN** the request is authorized by its bearer alone

### Requirement: Response security headers
Every response SHALL disable caching with no-store, send no referrer, disable content-type sniffing, ask crawlers not to index or follow it, and carry a content security policy.
The policy MUST allow no scripts, no base URL override, and no framing, and MUST restrict form actions to the same origin.
This includes the server's own responses to malformed requests and uncaught exceptions.

#### Scenario: Inspect response headers
- **WHEN** any page or error response is requested
- **THEN** it carries no-store, no-referrer, nosniff, `X-Robots-Tag: noindex, nofollow`, and the content security policy above

#### Scenario: Malformed HTTP request
- **WHEN** a client sends a request with a malformed request line
- **THEN** the error response carries the same headers

### Requirement: Request body limit
Request bodies larger than 160 KiB SHALL be rejected before their form fields are decoded.
The rejection MUST change no stored state.
The limit admits every submission whose title and URL are within their validation limits.

#### Scenario: Oversized body
- **WHEN** a holder submits a form with a body larger than 160 KiB
- **THEN** the request is rejected and nothing changes

#### Scenario: Longest valid input
- **WHEN** a holder submits a title and URL at their input limits, made of four-byte characters
- **THEN** the body is accepted for validation

### Requirement: Bearer URLs kept out of logs and errors
Bearer URLs and secrets MUST NOT appear in application logs, analytics, or exception messages, including on error paths.
The application SHALL keep no request access log, and its operator messages on standard error MUST NOT contain request lines or paths.
Proxy access logs need the same redaction contract, which the operator configures; browser history and copies made by the holder remain outside the application's control.

#### Scenario: Failing request with a bearer
- **WHEN** a request with a valid or malformed bearer fails with a storage fault or an unexpected exception
- **THEN** neither the logs nor the response contain the bearer

#### Scenario: Malformed request line with a bearer
- **WHEN** a client sends a malformed request line containing a bearer URL
- **THEN** the operator messages do not contain the bearer

### Requirement: HTTPS for deployed bearer URLs
Bearer URLs for a deployed instance SHALL use HTTPS.
The application MUST refuse to start with a plain HTTP public origin unless its host is loopback.
TLS termination and HSTS belong to the reverse proxy, which the operator configures; the application trusts no forwarded header.

#### Scenario: Deployed instance
- **WHEN** the operator configures a public origin with plain HTTP and a non-loopback host
- **THEN** the application does not start

#### Scenario: Local development
- **WHEN** a developer configures `http://localhost:6979` as the public origin
- **THEN** the application starts and shows HTTP links to that origin

### Requirement: Invalid locators reveal nothing
A request with a malformed, oversized, unknown, expired, or revoked bearer MUST NOT create a file and MUST NOT give a useful oracle for whether the named collection exists.

#### Scenario: Malformed bearer
- **WHEN** a request presents a malformed or oversized bearer
- **THEN** it is rejected before any collection is opened and no file is created

#### Scenario: Unknown collection, unknown secret, and expired grant
- **WHEN** one request names a collection that does not exist, another presents a wrong secret for an existing collection, and a third presents an expired or revoked bearer
- **THEN** all three receive the same response
