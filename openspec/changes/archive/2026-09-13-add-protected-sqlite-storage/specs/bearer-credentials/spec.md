## Purpose

The canonical bearer credential carried in sharing links, the collection-bound digest stored in place of the secret, and the lookup of a grant by that digest.

## ADDED Requirements

### Requirement: Canonical bearer format
A bearer SHALL have the form `v2.<collection-id>.<secret>`, where the collection identifier is its 16 network-order UUIDv4 bytes encoded in 22 characters and the secret is 32 bytes encoded in 43 characters, for 69 characters in total.
Both fields MUST use canonical unpadded base64url with zero unused bits.

#### Scenario: Round trip a bearer
- **WHEN** a canonical bearer is parsed and rendered
- **THEN** rendering returns the same 69 characters

#### Scenario: Reject a noncanonical encoding
- **WHEN** a bearer has padding, whitespace, an unsupported version, nonzero unused bits, or an identifier that is not a UUIDv4
- **THEN** parsing returns InvalidBearer

### Requirement: Bounded parsing without disclosure
Parsing SHALL check the total length before splitting or decoding, and its error MUST NOT contain the submitted text.
It accepts every 32-byte secret value, so parsing cannot establish that a secret has enough entropy.

#### Scenario: Oversized input
- **WHEN** parsing receives a very long string
- **THEN** it returns InvalidBearer after the length check

### Requirement: Collection-bound digest
The stored digest SHALL be exactly `SHA256(ASCII("eselsohr:bearer:v2") || NUL || uuid16 || secret32)`, binding the secret to its collection.
The database keeps an independent public grant identifier that is not part of this preimage.
A stored digest MUST NOT be usable as a sharing link, and one database copy does not reveal reusable secrets or authority for other collections.

#### Scenario: The same secret in two collections
- **WHEN** the same secret bytes are combined with two different collection identifiers
- **THEN** their digests differ

### Requirement: Bearer values stay explicit
A parsed bearer SHALL keep the canonical credential, validated collection identifier, and derived digest behind a private constructor.
It MUST have no text, read, generic, or SQL representation; callers render it explicitly.

#### Scenario: Logging a bearer by accident
- **WHEN** source review checks the bearer module's exports and instances
- **THEN** it finds no constructor export and no derived text, read, generic, or SQL instance

### Requirement: Digest lookup
Lookup SHALL require an active transaction owned by the caller and recheck collection identity before schema version in that transaction.
It MUST find the grant by digest, loading at most two matching grant rows and nine permission rows, and return the decoded grant fields, digest, and optional petname, including expired grants.
An unknown digest returns absence; duplicate matches and malformed rows are corruption.
Lookup starts, commits, and rolls back no transaction.

#### Scenario: Lookup outside a transaction
- **WHEN** lookup runs on a connection without an active transaction
- **THEN** it returns TransactionRequired

#### Scenario: A revoked grant in a new transaction
- **WHEN** a grant was deleted and committed before a new transaction looks up its bearer
- **THEN** lookup returns absence

### Requirement: Lookup errors
Lookup errors SHALL be TransactionRequired, CorruptData, LookupBusy, and LookupFault, containing no submitted or stored data.
Busy and Locked results MUST be reported as contention, unsupported schema versions and other SQL failures as faults, and unexpected exceptions and cancellation propagate.

#### Scenario: Contention during lookup
- **WHEN** SQLite reports Busy during lookup
- **THEN** lookup returns LookupBusy

### Requirement: Lookup is not authorization
A lookup result SHALL establish only that a grant with that digest exists in the current transaction.
Ancestry, expiration, fresh time, operation policy, and sealed requests MUST be checked by the protected operation that uses it, and retaining a lookup result does not keep authority current.

#### Scenario: An expired grant
- **WHEN** lookup finds an expired grant
- **THEN** it returns the grant, and the protected operation denies access
