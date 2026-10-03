# Eselsohr 2 execution roadmap

The [design](eselsohr-2.md) is the current direction, not an instruction to implement the complete rewrite.
Assign one task at a time.
Each milestone ends with review; later findings may change the next task or the design.
The [investigation](investigation.md) describes existing behavior and the current build baseline.
Keep these documents and the [security model](../security-model.md) current rather than appending a log of superseded plans or tool observations.

Current state: M1 and tasks 2.1, 3.1–3.6, and 4.2–4.6 are complete, covering local checks, the pure domain/policy model, restricted SQLite opening, schema/codecs, bearer parsing/digest lookup, and protected article reads and title changes.
Local development is the current priority; GitHub CI tasks 2.2 and 2.3 are deferred.
D1, D2, D3, and D4 are approved.
Other protected mutations, the application-facing Store API, runtime, and web behavior remain unimplemented.
Article values and transitions are independent of persistence; the private adapter keeps revision metadata outside Article and checks revisions in protected title changes.
M3 is ready for exit review; M4's protected-operation API strategy and SQLite distribution decision are settled.
Completing one task does not authorize later tasks or resolve their approval gates.

## Approval gates

| Decision | Resolution or current recommendation | Status and timing |
| --- | --- | --- |
| D1: replacement starting point | Approved: temporary independent `v2/` package, no old `Lib` imports or compatibility adapters, with a later root cutover. | Settled for 1.2. |
| D2: first usable version | Approved: manual URL/title entry first. Automatic metadata fetching remains intended functionality but does not block first delivery; investigate the restricted fetcher separately. | Settled for the first usable version. |
| D3: protected-operation API | Approved: named operations create private requests containing their complete inputs after checking the bearer, collection, and target. Each request can execute only in that transaction. No generic authorized-action framework or dispatcher. | Tasks 4.5–4.6 implement private reads and title changes using the [sealed requests and transaction types](eselsohr-2.md#protected-read). The application-facing API remains deferred. |
| D4: SQLite distribution | Approved: use the engine bundled with `direct-sqlite`, with `systemlib` disabled, to simplify deployment. | Settled for M4; restricted opening is implemented in 4.2. |
| D5: deletion lifecycle | Transactional tombstone with later controlled cleanup, or drain handles and delete the file immediately. Prefer tombstone/cleanup, subject to retention and recovery expectations. | Before owner deletion in M7. Basic article storage does not depend on this choice. |

The eight-permission vocabulary and collection-bound scopes are approved and implemented in task 3.3.
The four-edge depth limit, non-expiring root with all eight permissions, and strict finite child deadlines are approved and implemented in task 3.4.
Task 3.5 validates the supplied grant and its ancestors against the requested grant ID, collection, and time, returning the checked grants and check time in ValidChain.
Task 3.6 implements the approved [operation table](../security-model.md#operation-policy), including deletion without Read, proper-descendant management of expired targets, and owner root inspection without root revocation.
Size, timeout, and quota numbers in the design are proposals to settle in their consuming tasks.
Do not reopen agreed single-set permissions, add-only access, bounded recursive delegation, hash-only bearer storage, operator creation, descendant management, owner deletion, offline migrations, or the choice to use effectful without new evidence.

## Task contract and verification vocabulary

Prefer changes comparable to an hour of focused implementation or a few hundred meaningful lines.
Split a task if its diff becomes difficult to review; do not inflate the task to finish the milestone.
Each task below identifies its goal, sequence reason, scope, non-goals, likely files, dependencies, completion checks, security constraints, execution mode, and branch/worktree suitability.

Use normal Code mode for a settled task.
Use another Plan-mode pass for an unresolved interface or security/operational contract.
Use `/goal` only for a substantial fixed task whose allowed changes and completion tests are explicit; never use it for the whole roadmap or an open-ended security claim.

The independent package is verified on Linux with GHC 9.10.3 and Cabal 3.16.1.0.
Run these commands from the repository root:

```sh
cabal build --project-file=cabal-v2.project all
cabal test --project-file=cabal-v2.project all --test-show-details=direct
cabal run --project-file=cabal-v2.project eselsohr-v2 -- --version
```

Below, **Build**, **Test**, and **Version** refer to those commands.
Test means the complete replacement suite, plus focused groups where useful; verify a focused filter actually selects tests.
Do not report an empty suite as evidence.
The old root package's failing solver is not a new task regression, but do not claim the whole repository is green while only the independent project builds.

The root [HLS configuration](../../hie.yaml) selects the replacement components and excludes source outside `v2/`.
With HLS 2.14.0.0 and GHC 9.10.3, this command typechecks every v2 Haskell source on Linux:

```sh
rg --files v2 -g '*.hs' -0 | xargs -0 haskell-language-server-wrapper -j 2
```

Editors must launch HLS with `--lsp`.
LSP initialization and clean shutdown are verified through the local editor launcher; interactive editor behavior and macOS remain unverified.

**Diff checks** means `git status --short --untracked-files=all`, `git diff --check`, and inspection of every changed/new file.
`git diff` does not show untracked contents; inspect those explicitly.
For staged work, also inspect `git diff --cached --check` and `git diff --cached --stat`.
Documentation links and implementation-status claims are part of review.

Use the available Haskell environment without installing host tools or changing global configuration merely to get a check past a blocker.
The contributor workflow must remain independent of a particular machine's container.
For the current tool versions and versioned formatter/linter executable names, see the investigation.
Haskell checks are unnecessary for docs-only edits; environment version checks and offline solver diagnostics are separate evidence.

## M1: persistent context and an independent build

Purpose: give future sessions a reliable task map and one genuinely testable replacement target.
Non-goals: application behavior, legacy fixes, persistence, web serving, effects, deployment, or broad tool installation.
Exit review: documentation accuracy, package boundary, compiler choice, and reproducible dependency solution.
Tasks 1.1–1.3 are complete.

### 1.1 Documentation foundation

* Goal and sequence: preserve the current decisions, evidence, and task contracts before code diverges; these documents currently provide that foundation.
* Scope: short project map, concrete design, current-system investigation, and this roadmap.
  Clearly separate implemented behavior, approved decisions, recommendations, and unresolved choices.
* Non-goals: code/configuration changes, new README, speculative ADRs, a vision document, a session diary, or an authoritative implemented-security claim.
* Files: `AGENTS.md`, the three documents in `docs/design/`.
* Depends on: review of the documentation content, not approval of D1–D5.
* Done when: a fresh session can locate the evidence, explain the important decisions, and identify the next bounded task without reconstructing the chat.
* Verify: Diff checks; read all documents; resolve source/document links; check findings against source and distinguish solver results from executed tests.
* Security: do not promote intended properties to guarantees, include secrets, or conceal the limits of file isolation and typed interfaces.
* Mode: normal Code for documentation only.
  Branch: a small documentation commit; a separate worktree offers little benefit.
  Never push.

### 1.2 Minimal replacement package

* Status: implemented and verified on Linux.
  Build, Test, and Version pass; the version assertion remains part of the current suite.
  Missing/unsupported CLI arguments fail with usage on stderr.
  The solved plan contains the replacement and its dependencies, with no legacy package.
* Goal and sequence: establish a green seed independently of the legacy compiler-bound package, so later domain and adapter tasks have a test home.
* Scope: explicit `cabal-v2.project`, one `v2/eselsohr-v2.cabal`, a tiny library, an executable supporting `--version`, and a real assertion-based smoke test.
  Uses GHC 9.10.3/Cabal 3.16.1.0, GHC2024, and `base ^>=4.20.2.0`.
  Task 3.1 adds value and test dependencies to this package.
* Non-goals: listening socket, dummy web application, domain features, effects, database files, old-module imports, compatibility adapters, or legacy bounds fixes.
* Files: project/package manifests, minimal `v2/app`, `v2/src`, `v2/test` files, and a copy of the repository license inside the package.
* Depends on: 1.1 and the approved D1/D2 choices.
* Done when: the independent build/test/version commands succeed and the solved project contains no legacy `eselsohr` package.
  A deliberately broken assertion must fail before restoring it; the seed must not just exit successfully.
* Verify: Build, Test, Version, inspection of the solved package plan, Diff checks.
* Security: the seed creates no collection, reads no bearer secret, opens no network listener, and conveys no false impression of a deployable application.
* Mode: normal Code.
  Branch: foundation branch; separate worktree useful only if examining the old build concurrently.

### 1.3 Repeatable package resolution

* Status: implemented with GHC 9.10.3/Cabal 3.16.1.0 on Linux.
  Repeated freezes are byte-identical.
  Build/Test and their offline equivalents pass, including offline compilation in a fresh project build directory.
  Resolved components and dependency versions match.
* Goal and sequence: stabilize the seed's dependency solution before concurrent work and CI start depending on it.
* Scope: explicit index state and the independent project's freeze file; ignore its machine-local project file; document only commands actually verified.
  Confirm the custom project uses its own freeze/local filenames.
* Non-goals: resolving future SQLite/effect/web dependencies, a compiler matrix, Nix, global installs, blanket `allow-newer`, or an extra build wrapper.
* Files: `cabal-v2.project`, its freeze file, `.gitignore`, and concise command updates in the project notes.
* Depends on: 1.2.
  Serialize manifest changes with other branches at merge time.
* Done when: repeated resolution uses the intended fixed inputs; Build/Test pass with the freeze file, including offline reuse once dependencies are cached.
* Verify: deliberate `cabal freeze --project-file=cabal-v2.project`, Build, Test, and the offline equivalents after cache population; inspect package plans/diff.
* Security: pinning improves repeatability, not dependency trust.
  Keep real dependency bounds and distinguish cache success from a clean build.
* Mode: normal Code.
  Branch: same foundation branch; concurrent worktree edits to the same manifests are counterproductive.

Keep the index state in `cabal-v2.project` aligned with the generated `cabal-v2.project.freeze`.
Regenerate the freeze file only for an intentional resolution update.
To change existing pins, adjust the intended dependency bounds or project index state and remove the old freeze file before regenerating it; review the resulting diff and run Build/Test.
Verified commands:

```sh
cabal freeze --project-file=cabal-v2.project
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

Offline checks use the installed compiler and existing package-index/dependency caches; a fresh project build directory does not establish an empty-cache bootstrap.

Cabal 3.16.1.0 uses `cabal-v2.project.freeze` and `cabal-v2.project.local` for this custom project, matching its [project-file documentation](https://cabal.readthedocs.io/en/3.16/cabal-project-description-file.html#cmdoption-project-file).
The local file is written by `cabal configure` and overrides project/freeze settings.
It and its backup are ignored; build/test results assume no local overrides.
The root project's companion files do not configure this project.

## M2: fast developer feedback

Purpose: get useful native checks before the replacement grows.
Non-goals: release binaries, containers, Haddock, Nix redesign, broad compatibility, or weakening scanners to achieve a green badge.
Exit review: local checks exercise the intended package; deferred CI work still requires cache keys that reflect actual inputs and observed runner results.
The local build, test, formatting, linting, and HLS checks are sufficient to begin M3 without waiting for CI.

### 2.1 Formatting and linting for the new package

* Status: implemented and verified on Linux with Fourmolu 0.20.1.0 and HLint 3.10.
  Both checks cover every v2 Haskell source and use the existing formatter style and HLint defaults.
* Goal and sequence: establish small, consistent diffs before many new modules.
* Scope: use the existing Fourmolu style and HLint defaults with these tested versions against only the new source.
  No additional formatter or linter configuration is needed.
* Non-goals: reformatting legacy code, style redesign, universal lint suppressions, a wrapper script, or adding unrelated tools.
* Files: v2 Haskell sources and concise verified command documentation.
* Depends on: 1.2; can proceed alongside 1.3 if manifest changes are coordinated.
* Done when: formatting and lint checks cover every new Haskell source, pass, and correctly fail on a temporary intentional violation before restoring it.
* Verify: the quality commands below, Build, Test, Version, and Diff checks.
* Security: no broad suppressions hiding partial/unsafe code; formatting and lint checks do not establish correct behavior or enforce access restrictions.
* Mode: normal Code.
  Branch: independent quality branch; worktree useful during parallel domain work if shared formatter settings remain unchanged.

Run the quality checks from the repository root:

```sh
rg --files v2 -g '*.hs' -0 | xargs -0 fourmolu-0.20.1.0 --mode check
hlint-3.10 v2
```

The formatter loads the root `fourmolu.yaml`; HLint uses its default hints without additional settings or suppressions.
Unversioned `fourmolu` and `hlint` commands are also suitable if their `--version` output matches these tested versions.
These commands cover the replacement source, not generated files or legacy code.

### 2.2 Native Linux CI for the replacement

* Status: deferred; local development is the current priority.
* Goal and sequence: continuously verify the working native path established by M1.
* Scope: a focused replacement workflow with pinned tool setup, explicit project selection, parallel quality/build work, relevant caching, and cancellation.
* Non-goals: Docker, Haddock, release artifacts, Nix, broad compiler matrix, branch-protection administration, or disabling the existing checks.
* Files: a new v2 workflow under `.github/workflows/` and necessary command notes.
* Depends on: 1.3 and 2.1.
* Done when: an actual runner builds and tests the right package, executes the quality checks, and shows useful cache reuse without trusting a stale build.
  Record cold/warm durations only after observing them.
* Verify: Build, Test, quality commands, workflow inspection, and authorized GitHub run results after the author pushes.
  A local YAML review is not a runner pass.
* Security: read-only default token permissions, no publication credentials in PR jobs, dependency inputs in cache keys, no misleading zero-test success.
* Mode: normal Code.
  Branch: independent CI branch; useful worktree alongside M3.

### 2.3 Scope expensive existing jobs

* Status: deferred with task 2.2.
* Goal and sequence: remove redundant legacy work from v2-only feedback after the new workflow demonstrates that it provides the required coverage.
* Scope: review triggers/paths in existing build and scanning workflows so unrelated v2-only changes do not rebuild the legacy application/image.
* Non-goals: dropping security checks, legacy dependency repairs, Docker rewrite, or changing repository branch-protection settings.
* Files: `.github/workflows/ci.yml`, `code-scanning.yml`, and scope notes as needed.
* Depends on: 2.2 demonstrated green on an actual runner, not just merged YAML.
* Done when: v2-only, legacy-only, shared-configuration, and packaging changes each trigger appropriate checks, with no required-check dead end or security gap.
* Verify: inspect the trigger matrix and actual runs after author-controlled pushes; re-run relevant quality/build commands when shared configuration changes.
* Security: scanner coverage remains for deployed artifacts; required check names/path filters must not silently permit unchecked merges.
* Mode: normal Code.
  Branch: separate small CI-scoping change after 2.2 review; no independent worktree needed unless other CI work is active.

### 2.4 Verify the native macOS path

* Status: verification remains pending; GitHub workflow work is deferred with the other CI work.
* Goal and sequence: prove the promised second platform before native assumptions become embedded in the architecture.
* Scope: a manual/scheduled macOS seed check and minimal verified setup guidance.
* Non-goals: Nix, signed releases, broad matrices, or claiming that future SQLite and fetcher dependencies have already been validated on macOS.
* Files: a native platform workflow and the relevant setup notes.
* Depends on: 1.3; independent of the other M2 tasks once commands are settled.
* Done when: an actual macOS environment passes Build, Test, and Version using native tools and no Nix.
  If unavailable, report pending verification explicitly.
* Verify: actual macOS command/runner results and Diff checks, not Linux emulation.
* Security: platform-specific path/library behavior stays an explicit test concern; a seed build does not establish storage or TLS security.
* Mode: normal Code.
  Branch/worktree: independent platform branch is useful.

## M3: pure domain and authorization laws

Purpose: establish small values and a complete auditable policy before encoding them in SQL or effects.
Non-goals: bearer authentication, cryptography, SQL, HTTP, generic authorized-action wrappers, or porting the old runtime/tests.
Entry review: agree the operation table and numeric limits needed by each task.
Exit review: policy inputs, monotonicity/attenuation laws, and the distinction between valid policy data and authentic capability possession.

Paths below are relative to `v2/src/Eselsohr` and `v2/test`.
The Values, Article, Permission, Scope, Attenuation, Chains, and Authorization test groups are implemented.

### 3.1 Validated identifiers, title, and URL

* Status: implemented and verified on Linux.
  The [validation contract](eselsohr-2.md#validated-values) defines UUIDv4 text, title normalization and bounds, and Unicode HTTP(S) URL syntax.
  Build, Test, Version, formatting, linting, HLS, and offline build/test pass.
  External consumer compilation checks confirm ID separation and private construction, with a compiling positive control.
* Goal and sequence: give subsequent domain/policy work shared small value types.
* Scope: distinct ID kinds, abstract Title and syntactic WebUrl, total parsers, rendering, and explicit validation boundaries.
  Validation rules are settled in the linked contract.
* Non-goals: random ID/secret generation, public-address validation, persistence, policy records, a generic validation framework, or phantom-type tricks.
* Files: `Id.hs`, `Article.hs`, `WebUrl.hs`, and the `Values` test group.
* Depends on: 1.2; coordinate any dependency additions with 1.3 before merging.
* Done when: constructors reject invalid inputs; parsers are total; valid round-trips hold; signatures prevent mixing ID kinds.
* Verify: Build, Test with a Values group covering empty/oversized/malformed inputs and rendering round-trips; inspect constructors, instances, and roles.
* Security: an ID is not a capability and a WebUrl is not a safe network target.
  No partial parsing or constructor/Read/coercion escape defeats the stated invariant.
* Mode: focused Plan if validation choices remain; otherwise normal Code.
  Branch: domain branch; a worktree is useful alongside CI.

### 3.2 Article state transitions

* Status: implemented and verified on Linux.
  The [transition contract](eselsohr-2.md#article-transitions) defines the five domain fields, structural equality, total transitions, no-ops, and idempotence.
  Build, Test, Version, formatting, linting, and HLS pass.
* Goal and sequence: define the state changes that protected persistence will apply.
* Scope: Article with title/URL/state/creation/ID, pure changes, structural equality, no-ops, and idempotence.
* Non-goals: revision metadata, conflict handling, database limits, authorization witnesses on pure transformations, repository classes, HTTP forms, fetching, SQL, or unrelated article features.
* Files: `Article.hs` and Article tests.
* Depends on: 3.1.
  No-op behavior is settled in the linked contract.
* Done when: transformations preserve ID/creation and value invariants without knowledge of storage or concurrency metadata.
* Verify: Build, Test; full-field assertions and properties for construction, equality, each transition, no-ops, idempotence, and toggling twice.
* Security: no invalid state is manufactured; structural equality must not let tests pass merely because the ID stayed the same.
  Pure changes grant no storage authority.
* Mode: normal Code once specified.
  Branch: same domain branch; coordinate edits to Article with 3.1 rather than having competing writers.

### 3.3 Permission and scope inclusion

* Status: implemented and verified on Linux.
  The [inclusion contract](eselsohr-2.md#permission-and-scope-inclusion) defines the eight permissions, collection-bound scopes, and candidate-first subset predicates.
  Build, Test, Version, formatting, linting, and HLS pass.
  Tests check all 65,536 permission-set pairs and 65,536 nested subset chains, plus scope pairs/triples with repeated article IDs across collections.
* Goal and sequence: replace ambiguous permission ordering with mathematical subset semantics before writing delegation or operation checks.
* Scope: the agreed permission vocabulary as a Set, WholeCollection/OneArticle scope, and explicit collection/target-aware inclusion functions.
* Non-goals: expiry, ancestor traversal, bearer secrets, generic witnesses, SQL, or deciding an operation merely because it appears in the UI.
* Files: `Capability.hs` and permission/scope tests.
* Depends on: 3.1's IDs and the approved finite permission vocabulary; not 3.2.
* Done when: permission sets obey reflexivity, transitivity, antisymmetry, and pointwise inclusion; scope cannot cross collection/article targets.
* Verify: Build, Test with Permission/Scope groups; compare implementation to explicit membership logic over all pairs, plus representative distinct IDs.
* Security: never use derived ordering of aggregate records/sets for attenuation.
  Reducing one permission cannot mask adding another.
* Mode: normal Code.
  Branch/worktree: independent permission branch after the shared ID interface is agreed.

### 3.4 Local child attenuation

* Status: implemented and verified on Linux.
  The [attenuation contract](eselsohr-2.md#local-child-attenuation) defines root and child field requirements, the four-edge limit, strict deadlines, and ordered failures for a supplied parent-child pair.
  Build, Test, Version, formatting, linting, and HLS pass.
* Goal and sequence: establish a single delegation edge before reasoning about a tree.
* Scope: child rights/scope/deadline/depth checks, root and child field requirements, matching parent IDs, and rejection of roots supplied as children.
* Non-goals: SQL, stored-tree traversal, revocation execution, random tokens, issuing links, or spreading depth/expiry constants through unrelated modules.
* Files: `Capability.hs` and attenuation tests.
* Depends on: 3.3 and the approved deadline/depth rules in the linked contract.
* Done when: a valid child never has greater effective authority; expired or depth-exhausted parents cannot delegate; finite child deadlines are enforced.
* Verify: Build, Test; properties for generated parent/child pairs and boundary examples at expiry equality, scope narrowing, and maximum depth.
* Security: `Delegate` controls issuance, not copying an existing bearer URL.
  A complete permission set cannot manufacture owner-only deletion authority.
  Callers can construct grant records without validation; checking one parent-child pair does not validate all ancestors or authenticate a bearer, and the store must prevent changing persisted parent IDs.
* Mode: normal Code after policy choices.
  Branch: stacked on 3.3; separate worktree only useful for a competing policy experiment, not duplicate implementation.

### 3.5 Bounded ancestry validation

* Status: implemented and verified on Linux.
  The [ancestry contract](eselsohr-2.md#bounded-ancestry-validation) defines the list bound, requested grant and collection checks, complete parent path, error order, and ValidChain result with a private constructor.
  Build, Test, Version, formatting, linting, and HLS pass.
  External consumer compilation checks confirm private construction and the absence of unchecked reconstruction or record updates, with a compiling positive control.
* Goal and sequence: extend the parent-child delegation checks to the full grant chain supplied by the caller.
* Scope: bounded pure validation of root fields, missing parents, cycles, collection mismatches, every delegation edge, and ancestor expiry.
* Non-goals: database queries, long-lived authorization caches, cleanup workers, token possession checks, or repairing an invalid chain automatically.
* Files: `Capability.hs` and `Chains.hs` tests.
* Depends on: 3.4.
* Done when: validation terminates within the bound and fails closed for every invalid ancestor/edge; a valid descendant cannot outlive its chain.
* Verify: Build, Test; generated valid trees then single-invariant corruptions, missing/cyclic parents, wrong collection, and exact deadline cases.
* Security: ValidChain records checks on the supplied grants and time; it does not authenticate a bearer or detect later storage changes.
  Protected operations must reload and validate the grants inside their transactions before creating sealed requests.
* Mode: normal Code.
  Branch: close to 3.4; no separate worktree needed unless comparing interfaces before the implementation is selected.

### 3.6 Operation policy and security specification

* Status: implemented and verified on Linux; M3 is ready for exit review.
  The [security model](../security-model.md) defines the approved operation table, actor revalidation, target ancestry, and the boundary between policy data and authentic authority.
  Authorization tests exhaust all 256 permission sets and 6,561 subset pairs across three actor scopes, with generated trees and explicit rejection cases.
  Structural validation shares the bounded chain checks while allowing expired management targets; it returns the grants in their supplied order.
* Goal and sequence: centralize the complete decision table before persistent mutation or web routes depend on scattered permission checks.
* Scope: pure decisions for read, add, edit, delete, issue, inspect, revoke, and owner deletion; durable laws extracted from the old tests; current security specification consistent with the approved policy.
* Non-goals: Authorized wrappers, effect declarations, SQL, UI, crypto, or a large characterization suite coupled to the old App/TestApp architecture.
* Files: `Authorization.hs`, shared checks in `Capability.hs`, policy tests, and `docs/security-model.md`; overlapping material moves out of the design.
* Depends on: 3.2–3.5 and the approved operation table, including DeleteArticles alone for article deletion and proper-descendant management.
* Done when: the operation table and tests agree; removing rights never enables an operation; cross-target/branch actions fail; add-only grants do not gain Read.
* Verify: Build, Test; exhaustive rights combinations and generated scope/chain cases, with table-to-test coverage and explicit fail-closed cases.
* Security: possession, decision, state transition, and persistence remain distinct.
  Tests of pure records do not establish authentic remote authority.
* Mode: Plan for unresolved policy, then normal Code.
  Branch: independent policy review after prerequisites merge, before fixing the Store API.

## Later milestones: expand before assignment

These are bounded outlines, not execution prompts.
Before each milestone, specify the next small tasks using the contract above and settle only the decisions those tasks consume.
Do not begin implementing a whole outline from its title.

### M4: SQLite storage and protected article operations

Purpose: establish trustworthy file selection and one protected read/write path.
Depends on M1 and the relevant M3 policy.
D3 selects exact-payload sealing behind named operations; tasks 4.5–4.6 implement the private read and title change together in Articles, with hidden helpers for transactions and stored grant validation.
D4 selects bundled SQLite; no further distribution comparison is required.

Task 4.2 completes restricted existing-only opening and linked-engine checks.
Task 4.3 completes the schema, atomic initialization/version checks, and validated codecs with private article revision metadata.
Task 4.4 completes canonical bearer parsing and collection-bound digest lookup inside caller-owned transactions.
Task 4.5 completes one protected read with transactional ancestry and current-authority checks, an exact sealed scope, and revision metadata in its results.
Task 4.6 completes one protected title change with revision checks and edit/revoke race tests.
M4's exit review findings are addressed.
The protected Store API must supply revision metadata alongside Article, with storage representation and bounds owned by the adapter.
Revision zero on insertion and malformed or out-of-range metadata are tested in 4.3.
Test rejection of older and newer expected revisions even for no-ops, unchanged revisions for matching no-ops, and one increment per committed change.
Check exhaustion without wrapping or partial writes, and authorization before a revision comparison in the same protected transaction.
The protected read and title mutation must require private operation-specific sealed requests in their persistence helpers, with targets and payloads taken from the seal.
Review private component visibility, constructor exports, exact payload signatures, quantified runners, and nominal mode and transaction roles in the source.
Runtime tests must verify the selected collection and bearer, grants reloaded in each transaction, and use of the exact request inputs; the submitted revision still needs to be compared with storage.
Keep raw SQL and directory authority inside the adapter.
No full CRUD API, web, pool, effects framework, or elaborate migration history.
Review with two real temporary databases, repeated object IDs, missing/wrong files, malformed rows, rollback, and concurrent requests.
Use Plan for the bounded M4 implementation plan, then normal Code per approved slice.
An independent SQLite worktree is useful.

#### 4.1 SQLite distribution

D4 is settled: use the SQLite engine bundled with `direct-sqlite`, with `systemlib` disabled.
Task 4.2 pins `direct-sqlite` 2.3.29 and `sqlite-simple` 0.4.19.0 with bundled SQLite 3.45.0.
Deployment requires no separate SQLite shared library.
SQLite fixes are delivered through dependency updates and application rebuilds.
The opening adapter is implemented in task 4.2, the schema/codecs in 4.3, and protected read and title-change transactions in 4.5–4.6.

#### 4.2 Bundled SQLite integration and restricted collection opening

* Status: implemented and verified on Linux with the pinned bundled engine.
  Build, Test, Version, offline build/test, formatting, linting, and HLS pass.
  Opening tests cover file selection, identity metadata, and connection lifetime; task 4.3 extends the SQLite suite.
* Goal and sequence: establish the production opening primitive before adding storage and protected operations.
* Scope: private Cabal library `sqlite-internal`, [opening adapter](../../v2/persistence/Eselsohr/Store/SQLite/Open.hs), and a dedicated [integration suite](../../v2/test-sqlite/Main.hs).
  Cabal specification 3.0 enables private component visibility; the public library remains SQLite-independent.
  The existing dependency pins and Hackage index state are preserved while adding the SQLite solution.
  `mkCollectionDirectory` resolves an existing canonical absolute directory; `withExistingCollection` derives the filename from a validated collection ID.
  Non-regular files and symlinks are rejected; the underlying helper opens without CREATE or URI interpretation, using ReadWrite, PrivateCache, and NoFollow.
  A real identity table must contain exactly one integer singleton 1 and the matching UUIDv4 as a 16-byte BLOB in network byte order.
  Metadata query conversion failures return InvalidMetadata, including invalid UTF-8 in either column; regression tests check rejection without file changes, connection cleanup, and callback exception propagation.
  Foreign keys ON, trusted schema OFF, and busy_timeout 0 are set and read back before metadata validation.
  Contention fails immediately; callback exceptions and cancellation propagate after cleanup.
* Non-goals: schema initialization/versioning, article/grant storage, bearer authentication, sealed requests, protected transactions, migrations, CLI, runtime, web, or further SQLite distribution experiments.
* Files: the package/project/freeze, `hie.yaml`, `v2/persistence/`, `v2/test-sqlite/`, and current-state documentation.
* Depends on: D4 and the existing validated collection ID type.
  D3 enforcement and D5 deletion metadata are deferred to their consuming slices.
* Verify: Build, Test, Version, offline build/test, formatting, linting, HLS, and Diff checks.
  The integration suite reports SQLite version, source ID, and compile options and requires SQLite 3.45.0.
  The solved plan has `direct-sqlite` 2.3.29 with `systemlib: false`; Linux `ldd` of the SQLite test executable lists no SQLite shared library.

```sh
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
eselsohr_sqlite_test_binary=$(cabal list-bin --project-file=cabal-v2.project test:eselsohr-v2-sqlite-test)
ldd "$eselsohr_sqlite_test_binary"
```

* Security: stored identity is not bearer authentication.
  The collections directory and its writers are trusted; private packaging and opening flags do not sandbox hostile filesystem replacement or arbitrary IO.
  ReadWrite may fall back to read-only; success does not guarantee later writes.
  Callback code must finalize statements and must not close or retain the connection.
  Closed rejected fixture bytes and directory entries remain unchanged; hot-journal recovery is outside that claim.
* Limits: verification uses Linux and populated dependency caches, not an empty-cache bootstrap, macOS, legacy root builds, or GitHub CI.
* Mode: normal Code for this approved slice.
  Commit locally and stop; the next M4 task needs its own bounded plan and approval.

#### 4.3 Initial schema and validated storage codecs

* Status: implemented and verified on Linux with bundled SQLite 3.45.0.
  Schema and codec tests cover the storage contract; task 4.4 extends the SQLite suite.
  Build, Test, Version, offline build/test, formatting, linting, HLS, and Diff checks pass.
* Goal and sequence: establish the storage format and checked conversions before bearer lookup and protected operations consume persisted state.
* Scope: private [Schema](../../v2/persistence/Eselsohr/Store/SQLite/Schema.hs) and [Codec](../../v2/persistence/Eselsohr/Store/SQLite/Codec.hs) modules in `sqlite-internal`.
  The initializer requires an already-open empty database at version zero with no active transaction, enabled foreign keys, and supported journaling with OFF rejected.
  It atomically installs the four STRICT tables, root uniqueness index, supplied collection identity, and schema version 1, without creating a file or root grant.
  Its transaction boundary handles body and commit failures, including SQLite automatic rollback, and propagates unexpected exceptions after cleanup.
  `withExistingCollection` checks identity before version and accepts only version 1 without initializing or migrating the file.
  Private ArticleRow, GrantRow, and PermissionRow DTOs have mechanical FromRow/ToRow instances and separate checked conversions to domain values and private metadata DTOs.
  IDs use UUIDv4 network bytes; titles and URLs must already be canonical; state and permission strings are explicit.
  Timestamps use exact signed 64-bit Unix microseconds, rejecting finer precision, leap seconds, and overflow.
  Article revisions are Integer metadata bounded to zero through the signed 64-bit maximum before narrowing; the schema defaults new revisions to zero.
  Grant codecs check digest length, root and child fields, and at most eight distinct permission rows while allowing expired grants and preserving optional petnames unchanged.
  The adapter replaces row conversion errors with CodecError values containing no submitted or stored values; unexpected and SQL exceptions remain distinct.
* Non-goals: bearer parsing/hashing, authentication, protected operations or seals, a public metadata DTO, revision conflict/increment logic, CLI/file creation, migrations, runtime, web, or dependency upgrades.
* Files: the private adapter, its test modules, component dependencies, and current-state documentation.
  Domain sources, dependency versions, and the Hackage index state are unchanged.
* Depends on: 4.2 and the validated domain values; the approved D3 API contract is consumed by later protected-operation slices.
* Verify: Build, Test, Version, formatting, linting, HLS, and Diff checks, plus:

```sh
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

  [Schema tests](../../v2/test-sqlite/SchemaTests.hs) cover initialization, version refusal without callbacks, nonempty and active-transaction refusal, constraints, cascades, and rollback after controlled failures, SQL exceptions, unexpected exceptions, and MVar-coordinated cancellation.
  Real SQLite FULL, BEGIN Busy, and COMMIT Busy failures leave no partial schema/version or retained transaction; retry succeeds after removing the cause.
  [Codec tests](../../v2/test-sqlite/CodecTests.hs) cover golden representations, bounded generated round trips, timestamp/revision extrema, malformed values and SQL types, invalid UTF-8, all 256 child permission subsets, bounded oversized lists, and independent files with repeated article IDs.
* Security: grant decoding validates stored fields but does not authenticate a bearer or load ancestors.
  Scope uses the collection ID checked by the caller; protected operations must check the full grant chain, target existence where required, and permission for the operation inside their transaction.
  A version match does not attest every schema object or make SQLite a malicious-file sandbox.
  The initializer's caller must leave the opening callback on failure so the handle closes.
* Limits: Linux and populated dependency caches are verified; macOS, empty-cache bootstrap, legacy root builds, GitHub CI, interactive editor behavior, and crash/power-loss durability are not verified by this slice.
* Mode: normal Code for this approved slice.
  Commit locally and stop; the next M4 task needs its own bounded plan and approval.

#### 4.4 Canonical bearer parsing and digest lookup

* Status: implemented and verified on Linux with bundled SQLite 3.45.0.
  Bearer parser and digest tests cover this slice; task 4.5 extends the SQLite suite.
  Build, Test, Version, offline build/test, formatting, linting, HLS, and Diff checks pass.
* Goal and sequence: establish bearer representation and stored digest resolution before protected operations validate current authority.
* Scope: private [Bearer](../../v2/persistence/Eselsohr/Store/SQLite/Bearer.hs) module with `parseBearer`, `renderBearer`, `bearerCollection`, and `lookupBearer`.
  The approved [format and hash preimage](eselsohr-2.md#bearer-format) use separate canonical base64url fields for the UUID and 32-byte secret, totaling 69 characters.
  Bearer has a private constructor, explicit accessors, and no Show, Read, Generic, record-update fields, or SQL instances.
  Parsing checks the length, encoding, and UUID, accepts every secret byte pattern, and returns `InvalidBearer` without including the submitted text.
  Lookup requires an active caller-owned transaction and shares the opener's identity-before-version checks.
  Bound BLOB digest lookup limits grant matches to two and permission rows to nine, using the existing private row DTOs and checked conversions.
  Unknown digests return absence; matches return StoredGrant with validated fields and permissions, including expired grants.
  Malformed metadata/rows and duplicate matches are corruption; Busy/Locked are contention, unsupported versions and other SQL failures are faults, and unexpected exceptions propagate.
* Non-goals: transaction lifecycle management, ancestry or current-authority checks, clocks, operation policy, seals, protected reads/writes, issuance/random generation, file creation, migrations, runtime, HTTP, or public API additions.
* Files: the private adapter and tests, package/project/freeze, and current-state documentation.
  Pin `base64-bytestring` 1.2.1.0 and `cryptohash-sha256` 0.11.102.1 with its bundled C implementation enabled, preserving every existing version, the Hackage index state, and bundled SQLite flags.
  These dependencies remain within the private component and tests that consume them directly; domain sources remain unchanged.
* Depends on: 4.2 opening, 4.3 schema/codecs, and the approved encoding and dependency choices.
* Verify: Build, Test, Version, formatting, linting, HLS, and Diff checks, plus:

```sh
cabal freeze --project-file=cabal-v2.project
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

  [Bearer tests](../../v2/test-sqlite/BearerTests.hs) cover canonical encoding, malformed versions/fields/UUIDs, unused bits, oversized input, all secret byte values, 500 generated round trips, and independent SHA-256 goldens.
  Real temporary databases cover separate collection identities with repeated grant IDs and secrets, copied digests, expired grants, missing and duplicate matches, malformed rows/permissions, transaction ownership, identity/version rechecks, rollback, contention, and committed deletion seen by a new transaction.
* Security: hash lookup does not check ancestors, expiration, or permission for an operation; reusing its result would miss later revocation or storage changes.
  Secret entropy belongs to future issuance; parser tests cannot establish it.
  Lookup starts no transaction and makes no commit or rollback decision for its trusted caller.
* Limits: Linux and populated dependency caches are verified; macOS, empty-cache bootstrap, legacy root builds, GitHub CI, interactive editor behavior, and crash/power-loss durability are not verified by this slice.
* Mode: normal Code for this approved slice.
  Commit locally and stop; the next M4 task needs its own bounded plan and approval.

#### 4.5 Private protected article read

* Status: implemented and verified on Linux with bundled SQLite 3.45.0.
  Its 26 read cases pass within the complete SQLite suite; task 4.6 records the current totals.
  Build, Test, Version, offline build/test, formatting, linting, HLS, and Diff checks pass.
* Goal and sequence: complete one protected read before adding title mutation or the application-facing Store interface.
* Scope: the named readArticles operation in [Articles](../../v2/persistence/Eselsohr/Store/SQLite/Articles.hs), supported by the hidden Internal.Transaction and Internal.Authority modules inside `sqlite-internal`.
  ReadTx and ReadRequest have private constructors and nominal transaction parameters; as explained in the [protected read contract](eselsohr-2.md#protected-read), these types prevent executing a request in a different transaction, even through `coerce`.
  The runner fixes the connection, bearer, and clock for the transaction.
  The named operation supplies `getCurrentTime` and immediately checks and selects the exact requested scope.
  Articles exposes no IO lifting, context replacement, individual action runner, or replacement payload for selection.
  It rejects active transactions without changing them, verifies required connection settings and supported journaling, and uses BEGIN DEFERRED without configuration changes or retries.
  Bearer lookup rechecks identity/version; bounded parent lookup loads at most five grants with two-match and nine-permission limits, detects cycles, and checks the terminal root is the sole root.
  A fresh time sample after ancestry loading feeds both `validateChain` and `decide` before the request is sealed.
  Selection restricts OneArticle reads in SQL and validates every returned row and revision, rejecting duplicate article IDs before commit.
  Results use the existing private StoredArticle DTO, with empty lists for authorized absence, no pagination, and unspecified ordering.
  StoreError distinguishes denial, contention, corruption, storage faults, and existing transactions without including submitted or stored values.
  Failures roll back when needed, and unexpected exceptions or cancellation propagate after cleanup.
* Non-goals: mutations, revision conflict/increment logic, issuance, migrations, application-facing APIs, effects/runtime, web, dependency changes, or adopting new journal settings.
* Files: private adapter and codecs, SQLite tests, package module declarations, and current-state documentation.
  Domain sources, dependency versions, Hackage index state, and bundled SQLite flags remain unchanged.
* Depends on: 4.2–4.4, pure chain/policy validation, D3, and the approved scoped-transaction and private-read choices.
* Verify: Build, Test, Version, formatting, linting, HLS, and Diff checks, plus:

```sh
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

  [Read tests](../../v2/test-sqlite/ReadTests.hs) cover root/delegated/add-only authority, exact scopes, two collections with repeated IDs, strict expiry, maximum ancestry and traversal bounds, malformed storage, and absence.
  Lifetime tests cover unchanged caller transactions, rejected settings, contention, exceptions, cancellation, controlled COMMIT Busy and automatic rollback, and MVar-coordinated read/update/revocation ordering.
* Security: authorization and article selection read the same database state in one transaction; a new read after revocation commits denies access.
  Wrapping a request or action to hide its transaction type does not allow the API to execute it in another transaction.
  Trusted clocks and exclusive connection use remain interpreter responsibilities; callers must leave the opening callback on failure.
* Limits: Linux and populated dependency caches are verified; macOS, empty-cache bootstrap, legacy root builds, GitHub CI, interactive editor behavior, and crash/power-loss durability are not verified by this slice.
* Mode: normal Code for this approved slice.
  Title mutation and the shared transaction modes are implemented in task 4.6.

#### 4.6 Private protected title change

* Status: implemented and verified on Linux with bundled SQLite 3.45.0.
  All 132 domain/policy tests and 163 SQLite tests pass, including 32 title-change cases.
  Build, Test, Version, offline build/test, formatting, linting, HLS (28 files), and Diff checks pass.
* Goal: complete M4's one protected write path with exact-payload sealing, revision checks, and edit/revoke ordering.
* Scope: the named changeTitle operation, its sealed request, and persistence helper alongside reads in [Articles](../../v2/persistence/Eselsohr/Store/SQLite/Articles.hs), using the same hidden Internal.Transaction and Internal.Authority modules.
  ReadTx and WriteTx share transaction cleanup and bounded grant loading, with their types preventing writes through the read runner or execution in another transaction.
  Request constructors remain private to Articles; transaction constructors remain private to Internal.Transaction, whose context access and IO lifting are available only within the adapter library.
  Both operations return StoreError without including submitted or stored values; writes add NotFound, InvalidRevision, Conflict, and RevisionExhausted categories.
  The write runner rejects an existing transaction without changing it, requires existing DELETE journaling, sets and verifies synchronous EXTRA and secure_delete ON, and uses BEGIN IMMEDIATE without retries or mode conversion.
  Inside the transaction, it rechecks the journal mode and opener settings, reloads identity/version, bearer and ancestry, checks the sole root, samples fresh time, and requires ReadArticles plus ChangeTitles for the sealed target.
  The operation-specific request seals the exact article ID, validated title, and Integer expected revision.
  After authorization, the helper validates revision bounds, loads at most two matching article rows, compares the expected and stored revisions, and applies pure rename.
  Older/newer revisions conflict even for apparent no-ops; matching no-ops issue no UPDATE, including at the maximum revision.
  Real changes increment once, and exhaustion fails without writing.
  A private ToRow update DTO binds only the title, new revision, article ID, and current revision; an unexpected affected-row count is corruption and rolls back.
  Success returns the private StoredArticle only after commit; later failures and cancellation roll back earlier writes.
* Non-goals: other mutations, issuance or revocation APIs, schema/dependency changes, public Store interfaces, effects/runtime, HTTP, file creation, migrations, pooling, and M4 exit review.
  Revocation fixtures use trusted SQL to exercise ordering without implementing a revocation API.
* Depends on: 4.2–4.5, pure rename and operation policy, D3, D4, and the approved shared typed modes and write settings.
* Verify: Build, Test, Version, formatting, linting, HLS, and Diff checks, plus:

```sh
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

  [Title tests](../../v2/test-sqlite/TitleTests.hs) cover owner/delegated authority, permission combinations and no-ops, exact payloads, two files with repeated IDs, fresh expiration and ancestor checks, malformed storage, and revision extrema.
  Lifetime tests cover required settings, rejected journal modes, caller transactions, exceptions and cancellation after an UPDATE, affected-row checks, automatic rollback, COMMIT Busy, competing edits, and ancestor revocation in both transaction orderings.
* Security: authorization, revision checks, and mutation share one immediate transaction.
  A sealed request records the expected revision but does not establish that it matches storage or prevent reuse; repeating a request that changed the article conflicts and rolls back the transaction.
  An edit ordered before revocation may finish; new edits after committed revocation deny access.
  Trusted clocks and exclusive connection use remain interpreter responsibilities; callers must leave the opening callback on failure.
* Limits: Linux and populated dependency caches are verified; macOS, empty-cache bootstrap, legacy root builds, GitHub CI, interactive editor behavior, secure erasure, and crash/power-loss durability remain unverified.
* Mode: normal Code for this approved slice.
  Commit locally and stop before the separately approved M4 exit review or M5.

### M5: scoped effects and runtime

Purpose: run the same small application function with production and test handlers, without global Store authority or `IOE` in application logic.
Depends on the selected protected-store API, not the whole web application.

Separate slices: smallest semantic effect/interface; SQLite handler and runtime binding; deterministic test handler; one common application function and its authority/export tests.
Start from one named operation, give it only the effects it uses, and keep authorization inside the Store handler's transaction, following the [preferred direction](eselsohr-2.md#preferred-direction).
Do not recreate App/TestApp, a universal environment, or effects for every helper, clock read, random value, and log message.
Review signatures, handler scope, and exception/cancellation behavior.
Use a focused Plan comparison, then normal Code.
Competing interface worktrees can help, but select one before expanding coverage.

### M6: manual-bookmark web flow

Purpose: a small usable vertical slice with secure web defaults from its first endpoint.
Depends on M4/M5 and D2; not on the restricted fetcher or complete sharing.

Separate slices: validated config/local listener; operator CLI creation and one-time owner output; protected list/manual add; title/read-state edits with stale forms; response and leakage tests.
Escaping, explicit bearer handling, safe methods/redirects, and headers belong to those endpoints, not a late hardening pass.
No JavaScript, automatic fetching, full management UI, deployment, or legacy removal.
Review a real owner/manual-add/edit flow.
Normal Code per feature; HTML work can branch independently after view inputs are stable.

### M7: sharing and owner lifecycle

Purpose: make delegated access and its limits observable in real software.
Depends on the transaction/web foundations and approved pure policy; D5 gates deletion.

Separate slices: bounded child issuance and one-time URL display; owner-issued seven-day working links; add-only contribution/receipt; descendant metadata and revocation; root-only collection deletion.
Test negative paths and races with real transactions and HTTP boundaries.
No accounts, reusable-token recovery, fetcher dependency, or claims that deletion removes backups.
Use Plan for the deletion/cleanup contract, then normal Code.
Coordinate Store and web interfaces on one feature branch; separate experiments must not both define the grant API.

### M8: restricted metadata retrieval

Purpose: add metadata without broadening application code to arbitrary network IO.
Its experiment can start after M1; integration needs M5/M6, not completion of M7.

Separate slices: address/redirect policy; actual address-pinned connector with TLS hostname checks; bounded responses/parser; preauthorization/final reauthorization; failure/manual fallback integration.
No crawler, body archive, production bypass flag, or claim based solely on a fake HTTP handler.
Review real forbidden-destination, redirect, rebinding, size/decompression, timeout, and cancellation tests before enabling automatic fetching.
Use Plan first and normal Code in an experiment worktree.
Do not use an unbounded `/goal` such as "make SSRF safe"; manual bookmarking remains usable if the experiment fails.

### M9: offline maintenance and recovery

Purpose: make persisted data maintainable without undermining collection boundaries.
Depends on stable schema, grant/deletion semantics, and a reviewed maintenance lock.

Separate slices: explicit version checks and one transactional migration; consistent backup/restore; article-only export; root reset; deleted-file cleanup.
Test failure recovery, unknown/future versions, another collection remaining unchanged, and restored revocation semantics.
No central admin database, automatic request migrations, backup fleet, or admin UI.
Use Plan for exclusion and recovery contracts, then normal Code.
A `/goal` may suit a fixed, approved backup/migration failure matrix once APIs, allowed fixes, and finish criteria exist.
Maintenance tests can use a separate worktree after schema interfaces settle.

### M10: root cutover and removal

Purpose: leave one comprehensible application that builds and tests with the root Cabal project.
Depends on acceptance of the intended replacement feature/security baseline, not merely on a seed that builds.

Separate changes: move the replacement to the root without behavior changes; remove now-unused legacy code; prune dependencies, scripts, and workflows; update or remove documentation that no longer describes the code.
Do not mix feature work or opportunistic upgrades into cutover.
Verify `cabal test` and `cabal run eselsohr` from the repository root (subject to the accepted executable name), quality checks, and the accepted end-to-end flows.
Normal Code on a dedicated cutover branch.
A mechanically large deletion is acceptable if ownership and lack of consumers are clear.
Avoid parallel branches changing paths during cutover.

### M11: native deployment and operational checks

Purpose: validate a deliberately small deployment and recovery procedure.
Depends on the runnable replacement and maintenance behavior; documentation must describe actual dependencies, not a hoped-for static binary.

Separate slices: config contract; systemd/filesystem permissions; reverse-proxy TLS and log redaction; graceful shutdown; install/upgrade/restore rehearsal.
No Kubernetes, mandatory container/Nix setup, automatic publication, or pushing.
Review actual checks for writable-state boundaries, no bearer logging, restart, busy shutdown, and backup restoration.
Normal Code per slice; Plan where an operator choice remains.
A tightly specified authorized deployment rehearsal may use `/goal`, but cannot silently expand into external production changes.
Deployment configuration can branch independently after runtime contracts settle.

Optional Nix, container, release, and wider compatibility tasks follow the compiler/SQLite/deployment choices they consume.
A small Nix flake must build the same package; a container packages the application rather than defining the canonical development path.
Expand these tasks only when needed.

## Parallel work and review boundaries

```text
Documentation -> seed + dependency solution
                    |-> native CI / quality / macOS
                    |-> pure domain and policy ---------+
                    |-> SQLite opening -----------------+-> Store -> Effects -> Web
                    |-> PublicWeb experiment -------------------------------> Fetch integration

Web + policy -> sharing/lifecycle -> maintenance -> accepted root cutover
Runnable application + maintenance contract -> deployment checks
```

The arrows show interface dependencies, not a requirement to finish every task on the left before doing any useful work on the right.
For example, maintenance design can begin once the schema/locking contract is stable, and deployment configuration can be drafted before root cutover.

Sensible independent worktrees include SQLite opening, PublicWeb, CI/DX, and pure-domain work.
Agree shared ID types and interfaces, and coordinate manifest changes.
Multiple writing sessions in one checkout are not independent branches.
Do not delegate agent work or create competing implementations without an explicit task requesting it.

## Session handoff and the next task

Use one coherent change per working session.
Keep debugging and review fixes for that change in the same session; start fresh for a different task or an independent review.
A session needs the current project map/design, the exact task card, relevant approval decisions, and acceptance criteria, not the full conversation.

Task completion should report the diff scope, commands and actual results, unverified cases, and any remaining approval gate.
Update documents when current behavior or decisions change; do not add a running history of sessions.
Commit only when asked, and never push.
The author reviews and pushes.

M3 is ready for exit review of policy inputs, attenuation laws, and the distinction between policy data and authentic capability possession.
D1/D2/D3/D4 are settled, M1 and tasks 2.1, 3.1–3.6, and 4.2–4.6 are complete, and CI tasks 2.2 and 2.3 are deferred.
M4's exit review findings are addressed; M5 needs its own bounded task plan and approval.
D5 remains pending.

The machine-specific execution environment belongs in the session's task input, not contributor instructions.
A fresh local session should be told to use the existing Haskell toolchain environment, verify its path/versions, and avoid host installations or global changes.
The repository's native contributor path remains Linux/macOS with Cabal and optional Nix.
