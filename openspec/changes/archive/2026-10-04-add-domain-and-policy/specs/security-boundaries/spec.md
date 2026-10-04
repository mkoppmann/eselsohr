## Purpose

The threat model, trust assumptions, and enforcement limits that bound every Eselsohr 2 security claim, so that intended properties are not mistaken for guarantees.

## ADDED Requirements

### Requirement: Capabilities bind collection, scope, and validity
A holder SHALL act only on its grant's collection and scope, using its allowed operations while the grant and all its ancestors remain valid, and delegation is limited by depth.
A public collection identifier locates a file; only a valid bearer secret MUST authorize access to it.

#### Scenario: A collection identifier without a secret
- **WHEN** a request knows a collection identifier but presents no valid bearer secret for it
- **THEN** it receives no access to that collection

### Requirement: Indirect authority is explicit
Use and delegation rights SHALL share one permission set, so a holder who can mint a Read capability is described as able to obtain Read authority.
Delegate MUST allow issuing only a subset of the parent's rights and never creates a right absent from the parent.
Copying the parent's own bearer URL remains possible without Delegate; delegation controls newly issued authority, not copying.

#### Scenario: Issuance without a right
- **WHEN** a grant with Delegate but without ChangeTitles requests a child holding ChangeTitles
- **THEN** issuance is rejected

### Requirement: Threats addressed
The design SHALL treat malicious requests, stolen URLs, leaked logs or referrers, malformed or compromised databases, races, and server-side request forgery as threats.
It protects article contents, bearer secrets, grant relationships, and each collection's integrity and availability.
Authorization and protected state changes MUST use a consistent transaction view; an earlier check cannot justify a later mutation after revocation.

#### Scenario: A check before revocation
- **WHEN** an operation was authorized in an earlier transaction and the grant is revoked before the mutation runs
- **THEN** the earlier check does not authorize the mutation

### Requirement: Trust assumptions are stated
The design SHALL trust the operator, the operating system, the host clock, the process's trusted handlers, the cryptographic implementation, and an adequately maintained SQLite and TLS stack.
Expiration compares deadlines with the host clock, so setting the clock back revives expired grants and setting it forward expires them early.
These assumptions MUST NOT be presented as properties established by Haskell types.

#### Scenario: A compromised process
- **WHEN** arbitrary malicious code runs with the application's process privileges
- **THEN** no type, private constructor, or effect signature is claimed to contain it

#### Scenario: A clock set back
- **WHEN** the host clock is set to a time before an expired grant's deadline
- **THEN** that grant authorizes again until the clock passes its deadline, and no expiration claim covers this case

### Requirement: Types and effects are not a sandbox
Distinct types and private constructors SHALL be described as preventing some misuse by cooperating Haskell code only.
Effects restrict the authority an interface exposes but MUST NOT be claimed to contain malicious code with the process's privileges.
Separate collection files limit some storage mistakes, not access by a process that can open every file.

#### Scenario: Whole data directory compromise
- **WHEN** an attacker controls the process or the whole data directory
- **THEN** every accessible collection is treated as at risk

### Requirement: Stolen bearer URLs are authority
Possession of a bearer URL SHALL be treated as authority until its expiration or revocation.
No account identity distinguishes a thief from the intended holder, and revocation MUST NOT be claimed to retract data already delivered.

#### Scenario: A copied link
- **WHEN** someone other than the intended holder presents a valid, unexpired, unrevoked bearer URL
- **THEN** they receive the grant's authority

### Requirement: Database copies and compromise
A read-only copy of one database SHALL be treated as disclosing its article data and grant metadata, but not reusable bearer secrets or authority for other collections.
Article URLs and text might themselves contain secrets.
A writer of a database MUST be assumed able to insert a chosen secret hash, so integrity is not promised against that writer.

#### Scenario: A writable database copy
- **WHEN** an attacker can write one collection database
- **THEN** that collection's integrity and authorization are considered lost, while malformed state is still rejected on decoding

### Requirement: Revocation limits are stated
Security claims SHALL NOT state that revocation retracts copied bearer URLs or data already delivered, or that deleting a collection erases its backups.
Preventing rollback after a backup restore MUST NOT be claimed without external trusted state, which the design does not supply.

#### Scenario: A URL copied before revocation
- **WHEN** a bearer URL was used to download articles before its grant was revoked
- **THEN** revocation stops later use but cannot recall the delivered data

### Requirement: Identifiers and equality
Distinct identifier types SHALL prevent accidental substitution without authenticating a bearer.
Article and grant equality MUST compare every field, validated-chain equality also compares the check time and grant order, and identity checks compare identifiers explicitly.

#### Scenario: Equal identifiers with different grants
- **WHEN** two grant records share an identifier but differ in permissions
- **THEN** they are not equal
