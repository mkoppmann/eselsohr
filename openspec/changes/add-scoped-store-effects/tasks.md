# Tasks

These are outlined slices; before `/opsx:apply`, expand the next slice into bounded tasks as the project's task rules require.

## 1. Interface comparison

- [ ] 1.1 Compare candidate interfaces for one named operation in a focused Plan pass and select one, recording the decision in `design.md`; verified by reviewing the candidate signatures for readability, handler scope, and exception and cancellation behavior.

## 2. Smallest semantic interface

- [ ] 2.1 Define the selected Store facet and its small result types for one named operation, with no connection, path, or unrestricted IO in its interface; verified by Build, Test, Quality, HLS, and source review of its exports.

## 3. SQLite handler and runtime binding

- [ ] 3.1 Add a SQLite handler bound to one bearer and collection that calls the private named operations, and a runtime step that alone opens the collection named by a parsed locator; verified by Test with two real temporary databases and repeated article IDs, revocation and expiry between calls, translation of storage faults, and rollback and release after exceptions and cancellation.

## 4. Deterministic test handler

- [ ] 4.1 Add a deterministic test handler that replaces external behavior without duplicating application logic; verified by Test running the handler against the same contract cases as the SQLite handler.

## 5. Common application function

- [ ] 5.1 Write one application function using only the selected facet and run it with both handlers; verified by Test with production and test handlers.
- [ ] 5.2 Add authority and export tests, including external consumer checks that the application-facing interface exposes no connection, opening function, SQL execution, or data path; verified by Test and a compiling positive control.

## 6. Review

- [ ] 6.1 Review M5: signatures, handler scope, and exception and cancellation behavior; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
