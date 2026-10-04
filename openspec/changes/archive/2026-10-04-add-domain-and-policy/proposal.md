# Proposal

## Why

Small validated values and a complete, auditable operation policy must exist before they are encoded in SQL or effects.
The legacy authorization compares permission records by derived ordering and checks permissions separately from possession, collection, expiration, and ancestry, so its laws cannot simply be ported.
This change covers milestone M3.

## What Changes

- Validated identifiers, titles, and web URLs with total parsers and explicit rendering.
- The article record with pure, field-preserving transitions and no storage metadata.
- The eight-permission vocabulary as actual sets, collection-bound scopes, and explicit inclusion predicates.
- Local attenuation for one parent-child edge and bounded validation of a complete grant chain.
- The pure operation policy for reading, adding, editing, deleting, issuing, inspecting, revoking, and owner deletion.
- The security model: threats, trust assumptions, and the limits of each enforcement mechanism.

## Capabilities

### New Capabilities

- `validated-values`: identifiers, titles, and URLs that are valid by construction.
- `article-model`: article fields, equality, and pure transitions.
- `capability-grants`: permissions, scopes, grant fields, attenuation, and ancestry validation.
- `operation-policy`: the decision table for named operations.
- `security-boundaries`: threat model, trust assumptions, and enforcement limits.

### Modified Capabilities

None.

## Non-goals

- Bearer authentication, cryptography, SQL, HTTP, or generic authorized-action wrappers.
- Random identifier or secret generation, public-address validation, persistence, or a generic validation framework.
- Revision metadata, conflict handling, database limits, repository classes, forms, or fetching.
- Porting the old runtime, its App/TestApp architecture, or its tests.

## Dependencies and approval gates

Depends on `establish-replacement-package`.
Entry review agreed the operation table and the numeric limits each task needed.
The approved policy choices consumed here are the eight-permission vocabulary, collection-bound scopes, the four-edge depth limit, a non-expiring root with all eight permissions, strict finite child deadlines, deletion without Read, proper-descendant management including expired targets, and owner root inspection without root revocation.
The milestone's exit review and its findings are recorded in `tasks.md`.

## Impact

- `v2/src/Eselsohr/Id.hs`, `Article.hs`, `WebUrl.hs`, `Capability.hs`, and `Authorization.hs`.
- Tests in `v2/test/`: the Values, Article, Permission and Scope (`Capabilities.hs`), Attenuation, Chains, and Authorization groups.
- New library dependencies `containers`, `idna2008`, `network-uri`, `text`, `time`, and `uuid-types`, pinned in the freeze file.
