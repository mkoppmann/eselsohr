# Design

## Context

The pure operation policy decides IssueChild, InspectLink, RevokeLink, and DeleteCollection, and structural validation allows management of expired descendants whose parent links and delegation limits remain valid.
The schema stores grants with an immutable parent ID, a nullable article scope, a finite deadline for children, an optional petname, and a 32-byte token hash.
Deleting a grant cascades to its descendants and permission rows, and a unique index allows at most one root.
Bearers use the canonical `v2.<base64url-uuid>.<base64url-secret>` format, and storage keeps only the collection-bound SHA-256 digest of the secret.
Migrations and deletion metadata are deferred, and D5 will settle the deletion lifecycle.

### Legacy evidence

The old model has `SharedRef` as a category, not an immutable parent relation; see [Capability.hs](../../../src/Lib/Domain/Capability.hs).
There is no grant tree establishing descendant ownership, inherited expiration, or cascading revocation, and UI restrictions on resharing do not supply that model.

In [Page.Shared](../../../src/Lib/Ui/Web/Page/Shared.hs), `getSharedLinks` filters capabilities by a caller-supplied category predicate.
In [ShareArticle.query](../../../src/Lib/Ui/Web/Page/ShareArticle.hs), authorization to share one article is followed by `getSharedLinks colId Cap.isSharedArticleRef`, whose predicate does not restrict the results to that article or the issuing capability.
[UnlockLink.fromDomain](../../../src/Lib/Ui/Web/Page/ViewModel/UnlockLink.hs) reconstructs usable bearer links.
This is an authority-discovery concern, not merely a presentation defect: access to another grant can defeat attenuation or the intended effect of revoking the originally presented grant.

[Accesstoken.hs](../../../src/Lib/Ui/Dto/Accesstoken.hs) encodes collection and capability UUIDs as CBOR and Base32, with no independent hash-only secret store.
Its `Show` instance emits the bearer representation.
`decodeFromBase32` checks the outer encoding, but `toReference` later uses partial `deserialise`, so a syntactically valid Base32 string is not necessarily a valid token payload.

`Page.Shared.lookupReferences` loads a collection's capabilities, resolves the supplied ID, and checks expiration.
`capStillValid` treats equality with the deadline as valid, whereas the replacement uses the approved strict `now < expiresAt` rule.
That check is separate from later reads and writes, so it does not make a previously issued witness fresh at mutation time.

`Command.createUnlockLink` in [Command.hs](../../../src/Lib/App/Command.hs) constructs a capability with `defaultArticlesRef`.
Permission to create such links therefore carries indirect article authority; describing it as only administrative authority hides what the holder can obtain.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Bearer capabilities | KEEP | Preserve possession-based sharing, but replace encoding/storage and add ancestry, bounded attenuation, revocation semantics. |
| SharedRef categories and reusable-link discovery | REPLACE | Immutable parent graph, descendant metadata only, one-time secrets. |

## Goals / Non-Goals

**Goals:**

- Real issuance, inspection, revocation, and deletion whose negative paths and races are tested with real transactions and HTTP boundaries.
- Link management that never becomes a way to discover other usable authority.

**Non-Goals:**

- Preventing holders from copying their own bearer URLs; possession is authority until expiration or revocation.
- Rollback protection against restored backups, which would need external trusted state.

## Decisions

### Owner link and everyday links

Keep a long-lived, bookmarkable owner link for management and for issuing shorter everyday links.
The root has all eight permissions and no expiration; every child has a finite deadline, with equality to that deadline already expired.
Everyday links default to seven days; the owner may explicitly choose longer lifetimes.
The seven-day default belongs to issuance and its interface, not to the attenuation validator, which accepts any finite deadline no later than the parent's.
The owner can manage descendants and inspect its own root metadata; root revocation and replacement belong to the future CLI reset, which revokes the old tree and issues a new one-time owner URL.

### Issuance

Generate a fresh secret in trusted code, validate the parent and requested child in a write transaction, insert the child's metadata, permissions, and hash, commit, and show the new URL once.
The secret has 256 bits from a CSPRNG; a password-style slow hash is unnecessary for a uniformly random secret.
Secret and ID generation stay within the trusted issuance operation rather than becoming a global Random effect.
An interrupted response does not justify storing a recoverable bearer: the holder or operator can revoke the unseen grant and issue another.
Parent and child IDs used by the interface are metadata, not secrets.
Issuance follows D3 with its own sealed request, and the policy validates the complete proposed chain, including rejection of any reused ancestor ID.

### Delegate and copying

Delegate allows issuance of a subset; it never creates a right absent from the parent.
Copying the parent's own bearer URL remains possible without Delegate.
A holder who can mint a Read capability can obtain Read authority, even if an interface calls this only management; the single permission set keeps that explicit, unlike the legacy unlock link.

### Add-only contribution

The owner can issue a link with AddArticles alone, for contributors who submit articles without reading others.
The contribution uses the protected add and add-only receipt from `add-manual-bookmark-web`, and an add-only grant cannot issue a child containing Read.

### Inspection

Link lists return only permitted target metadata, without reconstructing bearers or returning ancestor or sibling grants as reusable alternatives.
InspectLinks permits viewing grant metadata, and neither it nor RevokeLinks grants article Read or recovery of the original URL.
Petnames are optional labels stored unchanged.

### Revocation

Check descendant management authority and delete the selected subtree transactionally, relying on the schema's cascading parent references.
Apply the strict deadline rule, `now < expiresAt`; equality is expired.
An operation ordered before revocation may finish; one whose protected authorization occurs after revocation commits must fail.
Reload and check the grants for each operation, and do not change parent IDs automatically.
Revocation cannot recall bytes already delivered.

### D5: collection deletion lifecycle

D5 is an approval gate: before owner deletion, review transactional deletion with later controlled file cleanup against draining requests and deleting the file immediately.

| Option | Description | Cost |
| --- | --- | --- |
| Tombstone with controlled cleanup | An owner-only POST marks the collection deleted and removes articles and grants in one transaction, then returns a receipt without depending on the now-invalid owner URL. The tombstone file remains until controlled startup or offline cleanup can establish that no handles are active. | Needs deletion metadata and a cleanup step; file removal is delayed. |
| Drain and delete | Drain the requests that hold the collection, then delete the file immediately. | Needs a way to establish that every handle has closed before deletion. |

The tombstone approach is preferred, subject to retention and recovery expectations, which remain open.
Either way, do not unlink a live database while other requests may still hold it.
File removal and backup retention must be explained separately from logical deletion.
Deleted-file cleanup itself belongs to `add-offline-maintenance` (M9).

## Risks / Trade-offs

- [A stolen link acts as its holder until expiration or revocation] → Short default lifetimes for everyday links and descendant revocation; no account identity can distinguish a thief.
- [Revocation cannot retract downloaded data] → State the limit in the interface and operator guidance.
- [Restoring an old backup restores revoked links or a deleted collection] → Explain this limit; preventing rollback needs external trusted state that this design does not supply.
- [Link lists become an authority-discovery channel, as in the legacy application] → Return descendant metadata only and never reconstruct bearers.
- [Races between revoke, edit, delegate, and delete] → Test them with barriers between real connections and authorize each operation inside its own transaction.

## Open Questions

- D5: tombstone with later controlled cleanup or draining handles and deleting immediately; tombstone is preferred, but retention and cleanup behavior remain open and need the author's approval before any deletion task.
- How an owner who loses the owner link recovers access, beyond the CLI root reset, is an open human choice.
- What deletion and backup retention operators should expect remains open.
- Numeric limits on grants, such as the number of links per collection, remain decisions for the slice that needs them.
