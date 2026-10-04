# Tasks

These are outlined slices; before `/opsx:apply`, expand the next slice into bounded tasks as the project's task rules require.
Coordinate the Store and web interfaces on one feature branch; separate experiments must not both define the grant API.

## 1. Bounded child issuance

- [ ] 1.1 Add the protected issuance operation with its sealed request, CSPRNG secret generation, and hash-only storage in one write transaction, and a page that shows the new URL once; verified by SQLite tests of permission, scope, deadline, and depth bounds, revocation racing issuance, and storage inspection for reusable secrets, and by HTTP tests that the URL appears only in the issuance response.

## 2. Owner-issued working links

- [ ] 2.1 Let the owner issue everyday links that default to seven days and accept an explicitly longer finite lifetime; verified by Test at the default, at a chosen lifetime, and at deadline equality.

## 3. Add-only contribution

- [ ] 3.1 Let the owner issue add-only links and run the contribution flow through the existing add-only receipt; verified by HTTP tests that the contributor cannot list articles, sees no titles, counts, or details, and cannot issue a child with Read.

## 4. Descendant metadata and revocation

- [ ] 4.1 Add descendant link inspection for holders with InspectLinks, including expired descendants and the owner's root metadata; verified by tests that lists contain no bearer URLs and no ancestor, sibling, or self grants for delegated holders.
- [ ] 4.2 Add transactional subtree revocation for holders with RevokeLinks; verified by SQLite tests with barriers showing that operations authorized after revocation commits fail while earlier ones may finish, that parents never change, and that the root cannot be revoked, and by HTTP tests of denied revocation attempts.

## 5. Deletion contract

- [ ] 5.1 Settle D5 in a Plan pass comparing the tombstone and drain-and-delete lifecycles, including retention, cleanup, and any schema change, and record the author's approval in `design.md`; no deletion code before approval.

## 6. Root-only collection deletion

- [ ] 6.1 After D5 is approved, add owner-only collection deletion through an explicit POST route with a receipt independent of the owner URL; verified by race tests with real connections showing that no bearer authorizes after deletion commits and no live database is unlinked, and by HTTP tests that delegated holders with every permission are denied.

## 7. Review

- [ ] 7.1 Review M7 with end-to-end owner, delegate, and contributor flows through HTML forms, negative paths, and races with real transactions and HTTP boundaries, and the stated limits of revocation, copying, and backups; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
