# Tasks

Slices 2 and 3 are expanded; before applying slices 4 and 5, expand them into bounded tasks as the project's task rules require.

## 1. Interface comparison

- [x] 1.1 Compare candidate interfaces for one named operation and select one, recording the decision in `design.md`; verified by the author's review of the candidate signatures, handler scope, and exception and cancellation behavior on 2026-10-04, with no commands applicable.

## 2. Adapter prerequisites

- [ ] 2.1 Make the named protected operations take the trusted clock.
  Goal: `readArticles` and `changeTitle` take an `IO UTCTime` argument instead of calling `getCurrentTime`; first because the binding and its expiry tests need it.
  Scope: the two named operations and the Articles module's Haddock; the `readWithClock` and `writeWithClock` test helpers call the named operations instead of recomposing the runners.
  Non-goals: changes to the runners, sealed requests, authorization, or tests that compose custom transaction actions.
  Files: `v2/persistence/Eselsohr/Store/SQLite/Articles.hs`, `v2/test-sqlite/ReadTests.hs`, `v2/test-sqlite/TitleTests.hs`.
  Security: the clock is still sampled inside the transaction, after it begins, and the existing masking-state tests keep passing.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
- [ ] 2.2 Report a missing collection file as `Missing`.
  Goal: `withExistingCollection` returns a new `Missing` opening error when the collection path does not exist, so the handler can report it as a denial.
  Scope: only the collection path's status check distinguishes a nonexistent path; symlinks, non-regular files, other filesystem errors, and `mkCollectionDirectory` keep returning `Unavailable`.
  Non-goals: changes to `withExistingFile`, identity checks, or connection settings.
  Files: `v2/persistence/Eselsohr/Store/SQLite/Open.hs`, `v2/test-sqlite/Main.hs`.
  Depends on: nothing; independent of 2.1.
  Security: opening still creates no file, and `Missing` carries no path.
  Checks: Test that a missing file returns `Missing` and creates no file, that a missing directory, a symlink, and a non-regular file return `Unavailable`; Build, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.

## 3. Application-facing interface

- [ ] 3.1 Pin `effectful-core`.
  Goal: add `effectful-core` to the public library with a version available at the pinned index state, through a deliberate freeze update.
  Scope: the library's `build-depends` and `cabal-v2.project.freeze`; no other dependency changes.
  Files: `v2/eselsohr-v2.cabal`, `cabal-v2.project.freeze`.
  Depends on: nothing; serialize with any other manifest or freeze change.
  Checks: Build, Test, and Offline after the caches are populated, and inspection of the freeze diff for unrelated version changes.
  Mode: normal Code mode on this branch; no worktree.
- [ ] 3.2 Define the read and write effects and their result types.
  Goal: a public module with `ArticleSelection`, `Revision`, `Revised`, `StoreFault`, `ReadFailure`, `EditFailure`, the `ArticleReader` and `ArticleWriter` effects, and their send functions, as selected in `design.md`.
  Scope: types, effect declarations, send functions, and the module's Haddock overview.
  Non-goals: handlers, runners, `Binding`, or any import of `sqlite-internal`.
  Files: a new module under `v2/src/Eselsohr/`, `v2/eselsohr-v2.cabal`.
  Depends on: 3.1.
  Security: the module exports no connection, path, clock, `IOE` requirement, or binding constructor, and failure values carry no data.
  Checks: Build, Test, Quality, HLS, and Diff checks, and source review of the exports.
  Mode: normal Code mode on this branch; no worktree.

## 4. SQLite handler and runtime binding

- [ ] 4.1 Add `Binding`, the reader and writer runners, and the result translation in `design.md`, settling whether the runtime module is a private library or part of the executable; verified by Test with two real temporary databases and repeated article IDs, a missing collection returning a denial, revocation and expiry between calls through test clocks, translation of each adapter result, and rollback and closing after exceptions and cancellation.

## 5. Common application function

- [ ] 5.1 Write one application function that reads an article and changes its title with the returned revision, using only `ArticleReader` and `ArticleWriter`, and run it through the SQLite handler with a test binding; verified by Test.
- [ ] 5.2 Add authority and export tests, including external consumer checks that the application-facing module exposes no connection, opening function, SQL execution, data path, clock, or binding constructor, and a compiling read-only positive control; verified by Test.

## 6. Review

- [ ] 6.1 Review M5: signatures, binding scope, and exception and cancellation behavior; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
