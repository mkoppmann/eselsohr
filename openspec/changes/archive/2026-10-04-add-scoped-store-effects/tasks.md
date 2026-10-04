# Tasks

All slices are expanded.

## 1. Interface comparison

- [x] 1.1 Compare candidate interfaces for one named operation and select one, recording the decision in `design.md`; verified by the author's review of the candidate signatures, handler scope, and exception and cancellation behavior on 2026-10-04, with no commands applicable.

## 2. Adapter prerequisites

- [x] 2.1 Make the named protected operations take the trusted clock.
  Goal: `readArticles` and `changeTitle` take an `IO UTCTime` argument instead of calling `getCurrentTime`; first because the binding and its expiry tests need it.
  Scope: the two named operations and the Articles module's Haddock; the `readWithClock` and `writeWithClock` test helpers call the named operations instead of recomposing the runners.
  Non-goals: changes to the runners, sealed requests, authorization, or tests that compose custom transaction actions.
  Files: `v2/persistence/Eselsohr/Store/SQLite/Articles.hs`, `v2/test-sqlite/ReadTests.hs`, `v2/test-sqlite/TitleTests.hs`.
  Security: the clock is still sampled inside the transaction, after it begins, and the existing masking-state tests keep passing.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test passed 133 and 166 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 28 files; `git diff --check` passed.
- [x] 2.2 Report a missing collection file as `Missing`.
  Goal: `withExistingCollection` returns a new `Missing` opening error when the collection path does not exist, so the handler can report it as a denial.
  Scope: only the collection path's status check distinguishes a nonexistent path; symlinks, non-regular files, other filesystem errors, and `mkCollectionDirectory` keep returning `Unavailable`.
  Non-goals: changes to `withExistingFile`, identity checks, or connection settings.
  Files: `v2/persistence/Eselsohr/Store/SQLite/Open.hs`, `v2/test-sqlite/Main.hs`.
  Depends on: nothing; independent of 2.1.
  Security: opening still creates no file, and `Missing` carries no path.
  Checks: Test that a missing file returns `Missing` and creates no file, that a missing directory, a symlink, and a non-regular file return `Unavailable`; Build, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test passed 133 and 166 tests, including the updated missing-file case and the existing missing-directory, symlink, directory, and FIFO cases; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 28 files; `git diff --check` passed.
  Unverified: a collection file removed between the status check and opening, which returns `Unavailable`; a collections directory removed after resolution, which returns `Missing`.

## 3. Application-facing interface

- [x] 3.1 Pin `effectful-core`.
  Goal: add `effectful-core` to the public library with a version available at the pinned index state, through a deliberate freeze update.
  Scope: the library's `build-depends` and `cabal-v2.project.freeze`; no other dependency changes.
  Files: `v2/eselsohr-v2.cabal`, `cabal-v2.project.freeze`.
  Depends on: nothing; serialize with any other manifest or freeze change.
  Checks: Build, Test, and Offline after the caches are populated, and inspection of the freeze diff for unrelated version changes.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: the solver selected `effectful-core` 2.7.1.1 at the pinned index state; Build passed after downloading from Hackage; `cabal freeze` only added `effectful-core`, `base-orphans`, `monad-control`, `strict-mutable-base`, `transformers-base`, `transformers-compat`, and `unliftio-core` with their flags, changing no existing pin; Test passed 133 and 166 tests; Offline Build and Test passed; Version printed `eselsohr-v2 0.1.0.0`; `git diff --check` passed.
- [x] 3.2 Define the read and write effects and their result types.
  Goal: a public module with `ArticleSelection`, `Revision`, `Revised`, `StoreFault`, `ReadFailure`, `EditFailure`, the `ArticleReader` and `ArticleWriter` effects, and their send functions, as selected in `design.md`.
  Scope: types, effect declarations, send functions, and the module's Haddock overview.
  Non-goals: handlers, runners, `Binding`, or any import of `sqlite-internal`.
  Files: a new module under `v2/src/Eselsohr/`, `v2/eselsohr-v2.cabal`.
  Depends on: 3.1.
  Security: the module exports no connection, path, clock, `IOE` requirement, or binding constructor, and failure values carry no data.
  Checks: Build, Test, Quality, HLS, and Diff checks, and source review of the exports.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: `Eselsohr.Store` builds without warnings; the Haddock example type-checks in a scratch module; Test passed 133 and 166 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 29 files; `git diff --check` passed; source review found no connection, path, clock, `IOE`, or binding in the exports, and only nullary failure constructors.
  Unverified: no test exercises the effects yet; tasks 5.1 and 5.2 add them.

## 4. SQLite handler and runtime binding

- [x] 4.1 Add the private runtime library with `Binding` and the result translation.
  Goal: `Eselsohr.Store.Runtime` in a private `store-runtime` library, with `Binding` (collections directory, bearer, clock) and pure functions translating opening errors, `StoreError`, and `StoredArticle` into the application-facing results; first because the runners are built on them.
  Scope: the library stanza, `Binding`, the translation table in `design.md`, the module's Haddock, and unit tests of every row of the table.
  Non-goals: runners, opening collections, or changes to `sqlite-internal`.
  Files: `v2/eselsohr-v2.cabal`, `v2/runtime/Eselsohr/Store/Runtime.hs`, a new test module in `v2/test-sqlite/`.
  Depends on: 2.1, 2.2, and 3.2.
  Security: `Missing` and `StoreDenied` become denials; every other opening or storage failure becomes a fault, never a denial.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed without GHC warnings; Test passed 133 and 169 tests, including every row of the opening and operation translation tables and revision conversion; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 31 files after mapping `v2/runtime` in `hie.yaml`; `git diff --check` passed.
  Unverified: HLS on Linux with the new `hie.yaml` mapping.
- [x] 4.2 Add the reader and writer runners.
  Goal: `runArticleReader` and `runArticleWriter` handle each operation by opening the bearer's collection, running the named protected operation with the bound clock, closing the connection, and translating the result.
  Scope: the runners, the scope built from the bound collection, and tests with real temporary databases: two collections with repeated article IDs, a missing collection returning a denial without creating a file, a symlinked collection file returning a fault, a missing ChangeTitles permission, revocation and expiry between calls through an advancing clock, a corrupt row, a held write lock returning `Busy`, a collection file replaced between operations, and a clock that observes an active transaction on every sample.
  Non-goals: connection reuse, retries, or new protected operations.
  Files: `v2/runtime/Eselsohr/Store/Runtime.hs`, the 4.1 test module.
  Depends on: 4.1.
  Security: no handler holds a connection between operations, and the runners take the collection only from the bearer.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed without GHC warnings; Test passed 133 and 179 tests, including the ten runner cases in `RuntimeTests`; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 31 files; `git diff --check` passed.
  Unverified: revocation is tested by deleting the bearer's parent, which also deletes the bearer's own grant through the schema's cascade, as in the adapter tests; the clock-sampling check infers an active transaction from another connection being locked out.
- [x] 4.3 Test cleanup after exceptions and cancellation through the runners.
  Goal: show that the runners add no exception handling of their own and that the adapter's brackets still apply through `Eff`.
  Scope: tests where the bound clock throws, and where a title change is cancelled inside its write transaction while it holds the write lock; afterwards the title and revision are unchanged, another connection can take an exclusive lock, and the exception or cancellation propagates unchanged.
  Non-goals: changes to the adapter's brackets, or test hooks in production code to reach a point after the update; the adapter's TitleTests cover cancellation after an update in the same transaction runner.
  Files: the 4.1 test module.
  Depends on: 4.2.
  Security: no exception becomes a result value.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed without GHC warnings; Test passed 133 and 181 tests, including both cleanup cases in `RuntimeTests`; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 31 files; `git diff --check` passed.
  Unverified: connection closure is inferred from the released lock and the opening tests, not observed directly.

## 5. Common application function

- [x] 5.1 Run an application-shaped function through the production handler.
  Goal: a function constrained only by `ArticleReader` and `ArticleWriter` that reads an article and changes its title with the returned revision, run with a test binding.
  Scope: the function and its test in the SQLite suite, checking the returned article and the single revision increment.
  Non-goals: application functions in the public library; M6 adds those for its routes.
  Files: the 4.1 test module or a new module in `v2/test-sqlite/`.
  Depends on: 4.2.
  Security: the function's type permits no `IOE` and no other effects.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: `retitleArticle` in `RuntimeTests` has only `ArticleReader` and `ArticleWriter` constraints and runs through both runners with a fixed-clock binding; Build passed without GHC warnings; Test passed 133 and 183 tests, including the returned article at revision 1, the stored revision 1, and a missing article stopping before the title change; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 31 files; `git diff --check` passed.
- [x] 5.2 Add the read-only and export checks.
  Goal: a compiling read-only positive control and an external consumer of the application-facing module.
  Scope: in the SQLite suite, a function whose type mentions only `ArticleReader`, run with only `runArticleReader`; in `eselsohr-v2-test`, which depends on neither `sqlite-internal` nor `store-runtime`, a test that uses `Eselsohr.Store` and its result types; and a review that the public library's dependencies include no SQLite, directory, or file-path package.
  Non-goals: automated negative compilation tests, which the spec states as a limit.
  Files: `v2/eselsohr-v2.cabal`, a new module in `v2/test/`, the SQLite test module.
  Depends on: 5.1.
  Security: the consumer reaches no connection, opening function, SQL, data path, clock, or binding constructor.
  Checks: Build, Test, Quality, HLS, and Diff checks.
  Mode: normal Code mode on this branch; no worktree.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: `readOnlyRoute` in `RuntimeTests` has the effect list `[ArticleReader, IOE]` and runs with only `runArticleReader` under an owner bearer; `StoreInterface` in `eselsohr-v2-test`, which depends on the public library but not on `sqlite-internal` or `store-runtime`, compiles a read-only function and checks revisions and failure rendering; the public library depends on `base`, `bytestring`, `containers`, `effectful-core`, `idna2008`, `network-uri`, `text`, `time`, and `uuid-types`, with no SQLite, directory, or file-path package; Build passed without GHC warnings; Test passed 135 and 184 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 32 files; `git diff --check` passed.
  Unverified: no negative compilation test shows that a write fails to compile under the read-only route, as the spec states.

## 6. Review

- [x] 6.1 Review M5: signatures, binding scope, and exception and cancellation behavior; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
  Reviewed on 2026-10-04: application signatures name only `ArticleReader` and `ArticleWriter`; `Eselsohr.Store` imports only `effectful-core` and public modules; bindings are constructible only from the private runtime library, and the runners take the collection only from the bearer; the runtime and Store modules contain no `catch`, `try`, or `mask`, so exceptions and cancellation rely on the adapter's brackets; the review corrected the runtime Haddock, which had claimed that a retained binding holds no authority, although it holds the bearer.
  Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed without GHC warnings; Test passed 135 and 184 tests; Version printed `eselsohr-v2 0.1.0.0`; Offline Build and Test passed; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 32 files; `openspec validate --all --strict` passed; `git diff --check` passed and every untracked file was inspected.
  Unverified: every check on x86-64 Linux; interactive editor behavior; cancellation after an update through the runners, covered only by the adapter's transaction tests; direct observation of connection closure; negative compilation of writes under a read-only route.
