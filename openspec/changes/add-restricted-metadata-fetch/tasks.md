# Tasks

These are outlined slices; before /opsx:apply, expand the next slice into bounded tasks naming scope, non-goals, likely files, completion checks, and security constraints, and settle the decisions it consumes.
Use a planning pass first and an experiment worktree; never an open-ended goal such as "make SSRF safe".

## 1. Address and redirect policy

- [ ] 1.1 Add pure URL, port, userinfo, and IPv4/IPv6 address classification, including mapped addresses, mixed DNS answers, cloud metadata addresses, and redirect targets; verified by Build and Test with exhaustive boundary and generated address cases.

## 2. Address-pinned connector

- [ ] 2.1 Add the production connector that resolves once, connects only to validated addresses, ignores environment proxies, and retains TLS hostname verification and SNI; verified by real resolver and connector tests for forbidden destinations, redirects, changed DNS answers, and mismatched certificates.

## 3. Bounded responses and parser

- [ ] 3.1 Bound total time including DNS and body reading, encoded and decoded body size, and accept only HTML and XHTML without fetching linked resources; verified by real connector tests for slow servers, oversized and compressed bodies, non-HTML responses, timeout cleanup, and cancellation.
- [ ] 3.2 Extract metadata as untrusted text and send no cookies, authorization, referrers, or bearer URLs; verified by tests that inspect the outbound requests.

## 4. Preauthorization and final reauthorization

- [ ] 4.1 Check Add permission and quota before fetching outside the transaction, and again in the insertion transaction through the sealed insert; verified by real database tests showing that unauthorized requests make no fetch and that barrier-coordinated revocation during a fetch prevents the insert.

## 5. Failure and manual fallback integration

- [ ] 5.1 Integrate fetching into the add form so that a supplied title skips fetching and a failure allows manual entry or the hostname fallback, with bounded fetch concurrency; verified by HTTP tests of both paths and of a concurrent burst.

## 6. Exit review

- [ ] 6.1 Review the real forbidden-destination, redirect, rebinding, size and decompression, timeout, and cancellation tests before enabling automatic fetching, and keep manual bookmarking if any fails; verified by Build, Test, Quality, HLS, Specs, and Diff checks.
