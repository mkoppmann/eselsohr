# Tasks

## 1. Validated identifiers, title, and URL

- [x] 1.1 Implement distinct UUIDv4 identifier kinds, `Title`, and `WebUrl` in `Id.hs`, `Article.hs`, and `WebUrl.hs`; verified by the Values group covering empty, oversized, and malformed inputs and rendering round trips, and by external consumer checks of ID separation and private construction with a compiling positive control.

## 2. Article state transitions

- [x] 2.1 Implement the five-field `Article` with structural equality, `newArticle`, `rename`, and `setReadState`; verified by the Article group with full-field assertions and properties for construction, equality, each transition, no-ops, idempotence, and toggling twice.

## 3. Permission and scope inclusion

- [x] 3.1 Implement the eight-permission set and collection-bound scopes with candidate-first inclusion predicates in `Capability.hs`; verified by the Permission and Scope groups comparing all 65,536 permission-set pairs with pointwise membership, 65,536 nested subset chains, and scope pairs and triples with repeated article IDs across collections.

## 4. Local child attenuation

- [x] 4.1 Implement root and child field requirements, the four-edge limit, strict deadlines, and ordered attenuation errors; verified by the Attenuation group with boundary examples at expiry equality, scope narrowing, and maximum depth, and generated parent-child pairs.

## 5. Bounded ancestry validation

- [x] 5.1 Implement `validateChain` with the list bound, requested grant and collection checks, error order, and the private `ValidChain`; verified by the Chains group with generated trees, single-invariant corruptions, missing and cyclic parents, wrong collections, exact deadlines, and infinite lists, and by external consumer checks against constructor access, coercion, record updates, `Read`, and generic reconstruction.

## 6. Operation policy and security specification

- [x] 6.1 Implement `decide` and `validateGrantStructure` in `Authorization.hs` and `Capability.hs`, and write the security model; verified by the Authorization group exhausting all 256 permission sets and 6,561 subset pairs across three actor scopes, with generated trees and explicit rejection cases.
- [x] 6.2 Run Build, Test, Version, formatting, linting, and HLS for every task above on Linux; all pass, with 132 domain and policy tests.

## 7. Exit review

- [x] 7.1 Review policy inputs, the monotonicity and attenuation laws, and the distinction between valid policy data and authentic capability possession; record findings, fix or defer each one in this change, then archive.

The review covered `Capability.hs`, `Authorization.hs`, their tests, and the SQLite `authorize` caller, which establishes possession by bearer lookup before any pure check.
Edge attenuation, the chain laws, permission monotonicity with matching target paths, collection binding, and error collapsing in the store hold as specified.
Grants scoped to an article are deleted with it, so a later article cannot inherit them.

- Fixed: the host clock was an unstated trust assumption; the security-boundaries spec now states that setting it back revives expired grants.
- Fixed: grant records carry no revocation state; the operation-policy spec now requires callers to supply only currently stored grants.
- Fixed: no test stated the attenuation law in terms of decisions; a generated-tree property now checks that every ancestor may perform the article, collection, and management operations allowed to its descendants, and fails when reversed.
- Fixed: InspectLinks and RevokeLinks are effective only with Delegate; the operation-policy spec now says so, matching the permission table test.
- No change: `decide` revalidates a `ValidChain`, so its private constructor prevents accidental rather than malicious misuse, as the spec already states; `chainCheckedAt` is used only by tests and stays as specified.
- No change: `grantDepth` duplicates the position in the chain, but validation checks it and the storage schema constrains it.
