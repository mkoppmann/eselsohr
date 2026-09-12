# Working on Eselsohr

Eselsohr is a self-hostable bookmark manager and an experiment in capability-based security.
Eselsohr 2 should stay small enough for one developer to understand the complete application.

## Project map

Read [README.md](README.md) for the existing application and the author's prose.
Read [the Eselsohr 2 design](docs/design/eselsohr-2.md) for agreed decisions, proposals, approval gates, and security requirements.
Read [the security model](docs/security-model.md) for the approved operation policy and its enforcement limits.
Use [the investigation](docs/design/investigation.md) for existing behavior and evidence, and [the roadmap](docs/design/roadmap.md) for bounded tasks and acceptance checks.
The design distinguishes the replacement's implemented pure model and policy from its intended application behavior.
It does not describe the legacy application's behavior.

The existing code is evidence of requirements and experiments, not an architecture that must be preserved.
There is no legacy data or URL compatibility requirement for the redesign.

## Working scope

Implement the requested task, not the whole roadmap.
Keep changes reviewable.
Stop at unresolved architectural decisions that affect the task.
Preserve unrelated changes.
Do not add speculative documentation or abstractions.
Preserve persistence ignorance: domain values and transitions must not depend on revision metadata, storage formats, or database limits.
Keep docs current: distinguish implemented behavior from current proposals.
Replace or remove obsolete information; Git history records past states.
Update the design and roadmap when decisions or implementation status change.
Use concise, concrete prose in the style of the README and existing comments.
Name the data and checks directly, and explain technical terms where first needed.
Begin each Haskell module with a Haddock block after any language pragmas and before the module declaration, using `Module` and `Description` fields followed by an API overview.
Describe the module's current types, entry points, semantics, and caller contracts in explanatory prose, with linked identifiers and examples where useful.
Keep the overview proportional to the module's complexity; avoid task-style instructions, a repeated export list, or descriptions of planned behavior.
Use section dividers for distinct concepts within a module, following the style in Capability.hs and matching export and body headings where applicable.
Leave focused modules unsectioned, and keep small helpers beside the code they support.
Keep substantive decisions in pure functions and sequence effects in short, named steps (Pure Core, Imperative Shell).
Use explicit Either matches where a result determines whether another effect may run, and keep resource cleanup and exception handling visible.
Prefer short function composition such as `f . g $ argument` over nested parentheses or repeated `$` when it makes the expression easier to follow.
Keep parentheses and named arguments where they clarify grouping, precedence, or data flow; do not force point-free definitions.
Separate logical phases within larger functions with a single blank line, keeping related bindings and checks together.
Use the same grouping in larger tests to distinguish setup, execution, and assertions without separating every statement.
Write one sentence per line in documentation, source code comments, and other prose to keep Git diffs focused.
Do not hard-wrap sentences to a fixed column width.
Commit only when requested.
Never push; the author reviews and pushes changes.

## Security intent

Keep pure transformations pure and external authority explicit and narrow.
Delegation must not increase permissions, target scope, or lifetime.
An invalid, expired, or revoked ancestor must deny descendant access.
Bind authorization to the collection, object, operation, and current state.
Add-only access must not reveal other articles or mint read authority.
Whole-collection deletion is owner-only and cannot be delegated.
Do not log bearer tokens or make stored hashes recoverable sharing links.
Public-web fetching must not grant arbitrary network access to application code.
Effects and private constructors are not a sandbox for a compromised process.
Separate collection files are not operating-system isolation.
These are redesign requirements, not guarantees of the old implementation.

## Commands and current status

The legacy package is `eselsohr`; its executable is `eselsohr-exe`.
Its root `cabal build` and `cabal test` commands are not a passing baseline; see the investigation for the compiler mismatch.

The independent `v2/` package is `eselsohr-v2`.
Its library, executable, and tests use GHC2024.
The shared Cabal `language` stanza enables OverloadedStrings for all v2 components; keep extensions used only by particular modules in their source pragmas.
It contains a version command, validated values, pure article transitions, grant validation, operation policy, and a private SQLite adapter with opening, initial schema installation, validated row codecs, bearer parsing/digest lookup, and protected article reads and title changes.
Other protected mutations, the application-facing Store API, and the web application remain unimplemented.
The private adapter keeps article revision metadata outside the domain model; the public protected Store API remains deferred.
These commands pass on Linux with GHC 9.10.3 and Cabal 3.16.1.0:

```sh
cabal build --project-file=cabal-v2.project all
cabal test --project-file=cabal-v2.project all --test-show-details=direct
cabal run --project-file=cabal-v2.project eselsohr-v2 -- --version
```

The root `hie.yaml` selects `cabal-v2.project` for HLS and excludes source outside `v2/`.
HLS 2.14.0.0 loads all five v2 components with GHC 9.10.3 on Linux; see the roadmap for the check command.

Fourmolu 0.20.1.0 and HLint 3.10 check every v2 Haskell source and pass on Linux.
See roadmap task 2.1 for the commands; use the existing formatter style and HLint defaults.

D1 (independent package), D2 (manual bookmarking first), D3 (exact-payload sealing behind named operations), and D4 (bundled SQLite) are approved.
M1 and tasks 2.1, 3.1–3.6, and 4.2–4.6 are complete; M3 is ready for exit review.
Local development is the current priority; GitHub CI tasks 2.2 and 2.3 are deferred.
The eight-permission vocabulary and collection-bound scopes are approved.
Delegation is limited to four edges; the root has all eight permissions and no expiry, and children have finite deadlines no later than their parent's.
Ancestry validation checks the supplied grant and its ancestors against the requested grant ID, collection, and time.
The operation policy rechecks expiration and target permissions; it allows management of expired descendants whose parent links and delegation limits remain valid.
Protected reads and title changes use sealed requests: values with private constructors that store the exact operation inputs and can execute only in the transaction that checked them.
Pure transformations require no sealed request; revision comparisons happen in the persistence helper after authorization.
Use the SQLite engine bundled with `direct-sqlite`, with `systemlib` disabled, to simplify deployment.
The private `sqlite-internal` component opens existing collection files and validates their stored identity and schema version 1.
Its source root is `v2/persistence`; `SQLite.Internal` names the hidden helper modules, while the component's exposed modules form its interface for sibling components.
Bearer has a private constructor, parses the canonical 69-character `v2.<base64url-uuid>.<base64url-secret>` format, and derives a SHA-256 hash from the collection ID and secret.
Lookup requires an active transaction, checks collection identity and version, and finds the grant and permissions by that hash; ancestor, expiration, and operation checks belong to the protected operations.
Articles owns `readArticles`, `changeTitle`, their sealed requests, and article SQL; the hidden helpers manage transactions and load and validate stored grants.
`readArticles` requires Read permission, and `changeTitle` requires Read plus ChangeTitles for the requested article, using grants loaded inside each transaction.
Title changes check that the submitted revision is in range and matches storage; matching no-ops perform no UPDATE, changed titles increment once, and exhaustion fails without writing.
See the [protected read](docs/design/eselsohr-2.md#protected-read) and [title change](docs/design/eselsohr-2.md#protected-title-change) for transaction types, constructor restrictions, and failure handling.
Writes require DELETE journaling and set and verify synchronous EXTRA and secure_delete ON without converting journal modes.
Initialization installs the schema and identity atomically into an already-open empty database, without creating a file or root grant.
Private row DTOs have SQL instances and separate checked conversions; timestamps use exact signed 64-bit Unix microseconds, and revisions range from zero through the signed 64-bit maximum.
Task 4.2 pins `direct-sqlite` 2.3.29 and `sqlite-simple` 0.4.19.0 with bundled SQLite 3.45.0.
M4's exit review findings are addressed; the application-facing Store API remains deferred to M5 and D5 remains pending.
Dependency versions and the Hackage index state are pinned.
See roadmap task 1.3 for deliberate freeze updates and verified offline commands; machine-local overrides stay untracked.
The contributor workflow should support Linux and macOS without requiring Nix or a particular developer's container setup.

For documentation-only changes, inspect new files and relative links, run `git status --short` and `git diff --check`, and check untracked files explicitly.
`git diff` does not show their contents.
Do not change dependencies or install tooling to verify documentation.
