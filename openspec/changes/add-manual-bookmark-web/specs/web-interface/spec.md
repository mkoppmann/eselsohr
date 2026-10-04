## Purpose

Defines the browser boundary of Eselsohr: escaped server-rendered HTML forms without JavaScript, safe methods and redirects, explicit bearer handling, response headers, link construction, and limits on where bearer URLs appear.

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

### Requirement: Read-only GET requests
GET requests SHALL NOT change stored state.

#### Scenario: Prefetching a page
- **WHEN** a browser or proxy repeats any GET request of the interface
- **THEN** no article, grant, or collection changes

### Requirement: Explicit POST mutations
Every mutation SHALL use an explicit POST route for that operation.
The interface MUST NOT honour method override parameters such as `_method`.

#### Scenario: Method override attempt
- **WHEN** a POST request carries a `_method` field naming another method
- **THEN** the request is handled as the POST route it targets, or rejected, and never as the named method

### Requirement: Server-generated redirects
After a mutation, the interface SHALL redirect only to a same-application location that the server generates from the operation and its result.
It MUST NOT accept a redirect target, such as `goto`, from the request.

#### Scenario: Submitted redirect target
- **WHEN** a mutation request includes a field naming an external or arbitrary redirect target
- **THEN** the response does not redirect to that target

### Requirement: Canonical public origin
Absolute links SHALL be constructed from the configured canonical public origin.
The interface MUST NOT derive link origins from the request's Host header or forwarded headers.

#### Scenario: Spoofed Host header
- **WHEN** a request arrives with a Host or forwarded header naming another origin
- **THEN** every absolute link in the response uses the configured public origin

### Requirement: Explicit bearer credentials
Requests SHALL carry their bearer explicitly in the URL or form they submit.
The interface MUST NOT set or accept ambient authentication cookies.
Origin checks may add defense in depth but are not the authorization mechanism.

#### Scenario: No credential cookies
- **WHEN** a holder uses any page of the interface
- **THEN** no response sets a cookie, and authorization depends only on the presented bearer

### Requirement: Response security headers
Every response SHALL disable caching with no-store, send no referrer, disable content-type sniffing, and carry a content security policy.
The policy MUST allow no scripts, no base URL override, and no framing, and MUST restrict form actions to the same origin.

#### Scenario: Inspect response headers
- **WHEN** any page or error response is requested
- **THEN** it carries no-store, no-referrer, nosniff, and the content security policy above

### Requirement: Bearer URLs kept out of logs and errors
Bearer URLs and secrets MUST NOT appear in application logs, analytics, or exception messages, including on error paths.
Proxy access logs need the same redaction contract, which the operator configures.
Browser history and copies made by the holder remain outside the application's control.

#### Scenario: Failing request with a bearer
- **WHEN** a request with a valid or malformed bearer fails with a storage fault or an unexpected exception
- **THEN** neither the logs nor the response contain the bearer

### Requirement: HTTPS for deployed bearer URLs
Bearer URLs for a deployed instance SHALL use HTTPS.
Plain HTTP is acceptable only for local development on a loopback origin.

#### Scenario: Deployed instance
- **WHEN** Eselsohr is deployed with a non-local public origin
- **THEN** every bearer URL it shows uses HTTPS

#### Scenario: Local development
- **WHEN** a developer runs Eselsohr on localhost
- **THEN** HTTP links to that localhost origin are permitted

### Requirement: Invalid locators reveal nothing
A request with a malformed, oversized, or unknown bearer MUST NOT create a file and MUST NOT give a useful oracle for whether the named collection exists.

#### Scenario: Malformed bearer
- **WHEN** a request presents a malformed or oversized bearer
- **THEN** it is rejected before any collection is opened and no file is created

#### Scenario: Unknown collection and unknown secret
- **WHEN** one request names a collection that does not exist and another presents a wrong secret for an existing collection
- **THEN** both receive the same response
