# validated-values Specification

## Purpose
Validated identifiers, titles, and web URLs that the rest of Eselsohr 2 can rely on without revalidating, with total parsers, explicit rendering, and no unchecked construction.

## Requirements

### Requirement: Distinct identifier kinds
Collection, article, and capability identifiers SHALL be distinct types that cannot be substituted for one another or coerced between kinds.
An identifier MUST NOT be treated as evidence of existence or authority.

#### Scenario: Mixing identifier kinds fails to compile
- **WHEN** an external consumer passes an article identifier where a collection identifier is expected, or coerces one kind into another
- **THEN** compilation fails

#### Scenario: An identifier grants nothing
- **WHEN** a request names a valid collection or article identifier without a valid bearer
- **THEN** the identifier alone supplies no access

### Requirement: Canonical UUIDv4 identifiers
Identifier parsers SHALL accept exactly 36-character lowercase, hyphenated UUIDv4 text with the standard variant.
They MUST reject other UUID versions, the nil UUID, mixed or upper case, and surrounding whitespace.

#### Scenario: Accept a canonical identifier
- **WHEN** the parser receives lowercase hyphenated UUIDv4 text with the standard variant
- **THEN** it returns the identifier, and rendering returns the same text

#### Scenario: Reject a noncanonical identifier
- **WHEN** the parser receives upper-case text, a version 1 UUID, the nil UUID, or text with surrounding whitespace
- **THEN** it returns an input error

### Requirement: No unvalidated construction
Identifier, title, and URL values SHALL be abstract, with total constructors returning either a value or a small input error.
No public constructor, `Read`, `IsString`, or generic reconstruction instance MAY bypass validation.
Input errors MUST NOT contain the submitted text.

#### Scenario: Construction requires the parser
- **WHEN** an external consumer tries to use a constructor, `Read`, a string literal, or generic reconstruction to build a title, URL, or identifier
- **THEN** compilation fails

#### Scenario: Errors omit the input
- **WHEN** validation rejects submitted text
- **THEN** the error value contains no part of that text

### Requirement: Title normalization and bounds
Title validation SHALL accept at most 4,096 input code points, collapse each run of Unicode White_Space to one ASCII space, and trim both ends.
The result MUST contain 1–500 Unicode code points and no other control characters.
Other characters keep their case and normalization form, and title equality compares the normalized value.

#### Scenario: Collapse whitespace
- **WHEN** a title contains leading, trailing, and repeated Unicode whitespace
- **THEN** the stored title has single ASCII spaces between words and none at either end

#### Scenario: Reject an empty or oversized title
- **WHEN** a title is empty after trimming, has more than 500 code points after normalization, or has more than 4,096 input code points
- **THEN** validation returns an input error

#### Scenario: Reject control characters
- **WHEN** a title contains a control character other than whitespace
- **THEN** validation returns an input error

### Requirement: Web URL syntax
URL validation SHALL accept only absolute HTTP and HTTPS URLs with a nonempty host and without userinfo, literal whitespace, controls, or backslashes.
Ports MUST be omitted or numerically equal to the scheme's default, 80 for HTTP and 443 for HTTPS, and rendering removes explicit default ports.
Input is limited to 8,192 code points and the rendered ASCII URL to 8,192 characters.

#### Scenario: Remove a default port
- **WHEN** validation receives `https://example.org:443/a`
- **THEN** the URL renders as `https://example.org/a`

#### Scenario: Reject unsupported URLs
- **WHEN** validation receives a relative URL, another scheme, userinfo, a nondefault port, a backslash, or an oversized URL
- **THEN** it returns an input error

### Requirement: Internationalized and literal hosts
Domain hosts SHALL pass strict IDNA2008 checks with case and NFC mappings and render as lowercase punycode, keeping any trailing dot.
Validation MUST reject emoji domains, malformed labels, invalid UTF-8 in encoded hosts, and DNS presentation escapes.
IPv6 literals use bracketed address syntax; IPvFuture and zone identifiers are rejected.

#### Scenario: Render an internationalized domain
- **WHEN** validation receives a URL whose host contains upper-case non-ASCII letters
- **THEN** the host renders as lowercase punycode

#### Scenario: Reject invalid hosts
- **WHEN** a host is an emoji domain, contains a malformed label or DNS presentation escape, or is an IPv6 literal with a zone identifier
- **THEN** validation returns an input error

### Requirement: URL component encoding
Unicode in paths, queries, and fragments SHALL become UTF-8 percent encoding.
Existing escapes, path separators, query order, and fragments MUST be preserved.

#### Scenario: Encode a Unicode path
- **WHEN** a URL path contains non-ASCII characters
- **THEN** the rendered URL contains their UTF-8 percent encoding and keeps existing escapes unchanged

### Requirement: URL syntax is not destination safety
A validated URL SHALL establish only syntax, not a safe network destination.
Local and private hosts are accepted without DNS lookup or address classification.
URLs MUST be rendered explicitly; they have no derived text representation.

#### Scenario: Accept a private host
- **WHEN** validation receives `http://127.0.0.1/`
- **THEN** it accepts the URL, and any later fetch must apply its own destination checks
