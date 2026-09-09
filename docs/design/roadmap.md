# Eselsohr 2 execution roadmap

The [design](eselsohr-2.md) is the current direction, not an instruction to implement the complete rewrite.
Assign one task at a time.
Each milestone ends with review; later findings may change the next task or the design.
The [investigation](investigation.md) describes existing behavior and the current build baseline.
Keep all three documents current rather than appending a log of superseded plans or tool observations.

Current state: M1 and task 2.1's formatting and linting checks are complete.
Local development is the current priority; GitHub CI tasks 2.2 and 2.3 are deferred.
D1 and D2 are approved.
No replacement domain, persistence, runtime, or web implementation exists.
The next task is 3.1; it remains pending approval.
Completing one task does not authorize later tasks or resolve their approval gates.

## Approval gates

| Decision | Resolution or current recommendation | Status and timing |
| --- | --- | --- |
| D1: replacement starting point | Approved: temporary independent `v2/` package, no old `Lib` imports or compatibility adapters, with a later root cutover. | Settled for 1.2. |
| D2: first usable version | Approved: manual URL/title entry first. Automatic metadata fetching remains intended functionality but does not block first delivery; investigate the restricted fetcher separately. | Settled for the first usable version. |
| D3: protected-operation API | Named transactional operations with private checks, or a closed action/opaque authorized-action mechanism. Compare one read and one title change; named operations are the simpler candidate. | Before fixing the store/runtime interfaces in M4/M5, not before pure domain work or a file-opening experiment. |
| D4: SQLite distribution | System SQLite or a maintained pinned bundled engine. Prefer system SQLite if both native platforms and the selected bindings validate it. | Before the persistence dependency/packaging choice in M4. Check the linked library, not merely the CLI version. |
| D5: deletion lifecycle | Transactional tombstone with later controlled cleanup, or drain handles and delete the file immediately. Prefer tombstone/cleanup, subject to retention and recovery expectations. | Before owner deletion in M7. Basic article storage does not depend on this choice. |

The complete permission/operation table needs approval before M3 policy work.
Depth, size, timeout, and quota numbers in the design are proposals to settle in their consuming tasks.
Do not reopen agreed single-set permissions, add-only access, bounded recursive delegation, hash-only bearer storage, operator creation, descendant management, owner deletion, offline migrations, or the choice to use effectful without new evidence.

## Task contract and verification vocabulary

Prefer changes comparable to an hour of focused implementation or a few hundred meaningful lines.
Split a task if its diff becomes difficult to review; do not inflate the task to finish the milestone.
Each task below identifies its goal, sequence reason, scope, non-goals, likely files, dependencies, completion checks, security constraints, execution mode, and branch/worktree suitability.

Use normal Code mode for a settled task.
Use another Plan-mode pass for an unresolved interface or security/operational contract.
Use `/goal` only for a substantial fixed task whose allowed changes and completion tests are explicit; never use it for the whole roadmap or an open-ended security claim.

The independent seed is verified on Linux with GHC 9.10.3 and Cabal 3.16.1.0.
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
With HLS 2.14.0.0 and GHC 9.10.3, this command typechecks all three files on Linux:

```sh
haskell-language-server-wrapper -j 2 v2/src/Eselsohr/Version.hs v2/app/Main.hs v2/test/Main.hs
```

Editors must launch HLS with `--lsp`.
LSP initialization and clean shutdown are verified through the local editor launcher; interactive editor behavior and macOS remain unverified.

**Diff checks** means `git status --short --untracked-files=all`, `git diff --check`, and inspection of every changed/new file.
Ordinary diff does not show untracked contents; inspect those explicitly.
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
  Build, Test, and Version pass; the smoke test fails on a version mismatch.
  Missing/unsupported CLI arguments fail with usage on stderr.
  The solved plan contains only the replacement and installed GHC packages.
* Goal and sequence: establish a green seed independently of the legacy compiler-bound package, so later domain and adapter tasks have a test home.
* Scope: explicit `cabal-v2.project`, one `v2/eselsohr-v2.cabal`, a tiny library, an executable supporting `--version`, and a real assertion-based smoke test.
  Uses GHC 9.10.3/Cabal 3.16.1.0, GHC2024, and `base ^>=4.20.2.0`.
  No third-party dependency or additional toolchain is needed for the seed.
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
  Both checks cover all three v2 Haskell sources, reject intentional violations, and pass again after restoration.
* Goal and sequence: establish small, consistent diffs before many new modules.
* Scope: use the existing Fourmolu style and HLint defaults with these tested versions against only the new source.
  No additional formatter or linter configuration is needed.
* Non-goals: reformatting legacy code, style redesign, universal lint suppressions, a wrapper script, or adding unrelated tools.
* Files: v2 Haskell sources and concise verified command documentation.
* Depends on: 1.2; can proceed alongside 1.3 if manifest changes are coordinated.
* Done when: formatting and lint checks cover every new Haskell source, pass, and correctly fail on a temporary intentional violation before restoring it.
* Verify: the quality commands below, Build, Test, Version, and Diff checks.
* Security: no broad suppressions hiding partial/unsafe code; quality checks are not proof of correctness or confinement.
* Mode: normal Code.
  Branch: independent quality branch; worktree useful during parallel domain work if shared formatter settings remain unchanged.

Run the quality checks from the repository root:

```sh
rg --files v2/app v2/src v2/test -g '*.hs' -0 | xargs -0 fourmolu-0.20.1.0 --mode check
hlint-3.10 v2/app v2/src v2/test
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
Test group names are descriptive targets to establish, not names already present in the repository.

### 3.1 Validated identifiers, title, and URL

* Goal and sequence: give subsequent domain/policy work shared small value types.
* Scope: distinct ID kinds, abstract Title and syntactic WebUrl, total parsers, rendering, and explicit validation boundaries.
  Settle title length/whitespace and URL syntax rules in this task before implementing their constructors.
* Non-goals: random ID/secret generation, public-address validation, persistence, policy records, a generic validation framework, or phantom-type tricks.
* Files: `Id.hs`, `Article.hs` or a small URL module if warranted, value tests.
* Depends on: 1.2; coordinate any dependency additions with 1.3 before merging.
* Done when: invalid inputs cannot construct ordinary validated values; parsers are total; valid round-trips hold; ordinary signatures cannot mix ID kinds.
* Verify: Build, Test with a Values group covering empty/oversized/malformed inputs and rendering round-trips; inspect constructors, instances, and roles.
* Security: an ID is not a capability and a WebUrl is not a safe network target.
  No partial parsing or constructor/Read/coercion escape defeats the stated invariant.
* Mode: focused Plan if validation choices remain; otherwise normal Code.
  Branch: domain branch; a worktree is useful alongside CI.

### 3.2 Article state transitions

* Goal and sequence: define the state changes that protected persistence will apply.
* Scope: Article with title/URL/state/creation/ID, pure changes, structural equality, and a revision/conflict model with explicit no-op and overflow behavior.
* Non-goals: authorization witnesses on pure transformations, repository classes, HTTP forms, fetching, SQL, or unrelated article features.
* Files: `Article.hs` and Article tests.
* Depends on: 3.1.
  Resolve no-op/revision details in the task specification.
* Done when: transformations preserve ID/creation and value invariants; expected state changes and stale-revision handling have unambiguous results.
* Verify: Build, Test; full-field assertions and properties for each transition, no-op, revision mismatch, and representable revision bounds.
* Security: no invalid state is manufactured; structural equality must not let tests pass merely because the ID stayed the same.
  Pure changes grant no storage authority.
* Mode: normal Code once specified.
  Branch: same domain branch; coordinate edits to Article with 3.1 rather than having competing writers.

### 3.3 Permission and scope inclusion

* Goal and sequence: replace ambiguous permission ordering with mathematical subset semantics before writing delegation or operation checks.
* Scope: the agreed permission vocabulary as a Set, WholeCollection/OneArticle scope, and explicit collection/target-aware inclusion functions.
* Non-goals: expiry, ancestor traversal, bearer secrets, generic witnesses, SQL, or deciding an operation merely because it appears in the UI.
* Files: `Capability.hs` and permission/scope tests.
* Depends on: 3.1's IDs and approval of the finite permission vocabulary; not 3.2.
* Done when: exhaustive permission-set pairs obey reflexivity, transitivity, antisymmetry, and pointwise inclusion; scope cannot cross collection/article targets.
* Verify: Build, Test with Permission/Scope groups; compare implementation to explicit membership logic over all pairs, plus representative distinct IDs.
* Security: never use derived ordering of aggregate records/sets for attenuation.
  Reducing one permission cannot mask adding another.
* Mode: normal Code.
  Branch/worktree: independent permission branch after the shared ID interface is agreed.

### 3.4 Local child attenuation

* Goal and sequence: establish a single delegation edge before reasoning about a tree.
* Scope: child rights/scope/deadline/depth checks, explicit root/non-root shape, immutable parent relation, and nondelegable owner status.
* Non-goals: SQL, stored-tree traversal, revocation execution, random tokens, issuing links, or spreading depth/expiry constants through unrelated modules.
* Files: `Capability.hs` and attenuation tests.
* Depends on: 3.3 and approved deadline/depth rules; the design's four-edge limit is only a candidate until this task's input contract settles it.
* Done when: a valid child never has greater effective authority; expired or depth-exhausted parents cannot delegate; finite child deadlines are enforced.
* Verify: Build, Test; properties for generated parent/child pairs and boundary examples at expiry equality, scope narrowing, and maximum depth.
* Security: `Delegate` controls issuance, not copying an existing bearer URL.
  A complete permission set cannot manufacture owner-only deletion authority.
* Mode: normal Code after policy choices.
  Branch: stacked on 3.3; separate worktree only useful for a competing policy experiment, not duplicate implementation.

### 3.5 Bounded ancestry validation

* Goal and sequence: lift local attenuation to a full grant chain without introducing persistence or trusting arbitrary snapshots.
* Scope: bounded pure validation of root shape, missing parents, cycles, collection mismatches, every delegation edge, and ancestor expiry.
* Non-goals: database queries, long-lived authorization caches, cleanup workers, token possession checks, or repairing an invalid chain automatically.
* Files: `Capability.hs` or a small `Authorization.hs` boundary and chain tests.
* Depends on: 3.4.
* Done when: validation terminates within the bound and fails closed for every invalid ancestor/edge; a valid descendant cannot outlive its chain.
* Verify: Build, Test; generated valid trees then single-invariant corruptions, missing/cyclic parents, wrong collection, and exact deadline cases.
* Security: a validated snapshot is policy input, not bearer authenticity or proof that authority remains valid in a later transaction.
* Mode: normal Code.
  Branch: close to 3.4; no separate worktree needed unless comparing interfaces before the implementation is selected.

### 3.6 Operation policy and security specification

* Goal and sequence: centralize the complete decision table before persistent mutation or web routes depend on scattered permission checks.
* Scope: pure decisions for read, add, edit, delete, issue, inspect, revoke, and owner deletion; durable laws extracted from the old tests; current security specification consistent with the approved policy.
* Non-goals: Authorized wrappers, effect declarations, SQL, UI, crypto, or a large characterization suite coupled to the old App/TestApp architecture.
* Files: `Authorization.hs`, policy tests, and `docs/security-model.md` once the approved rules warrant it; move overlapping material out of the design.
* Depends on: 3.2–3.5 and approval of the full operation table, including article deletion's Read conjunction and descendant-management behavior.
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

### M4: collection-bound SQLite kernel

Purpose: establish trustworthy file selection and one protected read/write path.
Depends on M1, the relevant M3 policy, and D3/D4 before freezing their interfaces.
An opening-only prototype can run earlier without those false dependencies.

Separate slices: restricted existing-only opening and linked-engine checks; schema/constraints and validated decoders; token parser/hash lookup; one protected read; one title mutation with revision and revoke-race tests.
Keep raw SQL and directory authority inside the adapter.
No full CRUD API, web, pool, effects framework, or elaborate migration history.
Review with two real temporary databases, repeated object IDs, missing/wrong files, malformed rows, rollback, and concurrent requests.
Use Plan for D3/D4, then normal Code per slice.
An independent SQLite worktree is useful.

### M5: scoped effects and runtime

Purpose: run the same small application function with production and test handlers, without global Store authority or `IOE` in ordinary logic.
Depends on the selected protected-store API, not the whole web application.

Separate slices: smallest semantic effect/interface; SQLite handler and runtime binding; deterministic test handler; one common application function and its authority/export tests.
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

Purpose: leave one comprehensible application and ordinary root Cabal commands.
Depends on acceptance of the intended replacement feature/security baseline, not merely on a seed that builds.

Separate changes: move the replacement to the root without behavior changes; remove now-unused legacy code; prune dependencies, scripts, and workflows; update or remove documentation that no longer describes the code.
Do not mix feature work or opportunistic upgrades into cutover.
Verify ordinary `cabal test` and `cabal run eselsohr` (subject to the accepted executable name), quality checks, and the accepted end-to-end flows.
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
                    |-> SQLite opening experiment ------+-> Store -> Effects -> Web
                    |-> PublicWeb experiment -------------------------------> Fetch integration

Web + policy -> sharing/lifecycle -> maintenance -> accepted root cutover
Runnable application + maintenance contract -> deployment checks
```

The arrows show interface dependencies, not a requirement to finish every task on the left before doing any useful work on the right.
For example, maintenance design can begin once the schema/locking contract is stable, and deployment configuration can be drafted before root cutover.

For D3, use `experiment/named-operations` and `experiment/sealed-actions` only if comparing implementations will resolve a real uncertainty.
Give both the same protected read/title-change requirements, wrong-target cases, and revocation race semantics.
Compare exposed authority, omission opportunities, target/payload binding, freshness, trusted-code size, and readability.
Keep one implementation and useful tests, not two application frameworks.

Other sensible independent worktrees are SQLite opening, PublicWeb, CI/DX, and pure-domain work.
Agree shared ID/interface shapes and coordinate manifest changes.
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

The next task is 3.1, validated identifiers, title, and URL.
D1/D2 are settled, M1 and task 2.1 are complete, and CI tasks 2.2 and 2.3 are deferred.
Task 3.1 needs approval before work starts; settle its validation rules before implementation.

The machine-specific execution environment belongs in the session's task input, not contributor instructions.
A fresh local session should be told to use the existing Haskell toolchain environment, verify its path/versions, and avoid host installations or global changes.
The repository's native contributor path remains Linux/macOS with Cabal and optional Nix.
