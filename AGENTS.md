# Working on Eselsohr

Eselsohr is a self-hostable bookmark manager and an experiment in capability-based security.
Eselsohr 2 should stay small enough for one developer to understand the complete application.

## Project map

Read [README.md](README.md) for the existing application and the author's prose.
Eselsohr 2 is specified with [OpenSpec](https://openspec.dev/):

* [openspec/specs/](openspec/specs/) holds the current requirements of the replacement: implemented, verified behavior and its limits.
* [openspec/changes/](openspec/changes/) holds one change per milestone, with its proposal, design decisions, delta specs, and tasks.
  Active changes describe in-progress or planned work; [openspec/changes/archive/](openspec/changes/archive/) keeps the completed ones and the reasoning behind their decisions.
* [openspec/config.yaml](openspec/config.yaml) holds the project context, approved decisions, verification commands, and the rules for writing each artifact.

The specs and changes describe Eselsohr 2, not the legacy application.
Legacy behavior appears as evidence in the design of the change it motivates.
The existing code is evidence of requirements and experiments, not an architecture that must be preserved.
There is no legacy data or URL compatibility requirement for the redesign.

Use `openspec list` and `openspec list --specs` to see changes and capabilities, and `openspec show <name>` to read one.
In Claude Code, the `/opsx:explore`, `/opsx:propose`, `/opsx:apply`, `/opsx:update`, `/opsx:sync`, and `/opsx:archive` commands drive the workflow; in Codex, the matching `$openspec-*` skills do.
The generated files in `.claude/skills/`, `.claude/commands/opsx/`, and `.agents/skills/` come from `openspec init`; regenerate them with `openspec update` after upgrading OpenSpec instead of editing them.

## Working scope

Implement the requested change, not the whole sequence of milestones.
Before applying an outlined change, expand its next slice into bounded tasks and settle the decisions that slice consumes.
Keep changes reviewable.
Stop at unresolved architectural decisions that affect the task.
Preserve unrelated changes.
Do not add speculative documentation or abstractions.
Preserve persistence ignorance: domain values and transitions must not depend on revision metadata, storage formats, or database limits.
Keep specs current: they state implemented behavior, and active changes state proposals.
Replace or remove obsolete information; Git history and archived changes record past states.
Update the specs, change artifacts, and this file when decisions or implementation status change.
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
Write one sentence per line in documentation, specs, source code comments, and other prose to keep Git diffs focused.
Do not hard-wrap sentences to a fixed column width.
Commit only when requested.
Never push; the author reviews and pushes changes.

## Sessions and parallel work

Completing one task does not authorize later tasks or resolve their approval gates.
Use normal Code mode for a settled task and a Plan-mode pass for an unresolved interface or security or operational contract.
Use `/goal` only for a substantial fixed task whose allowed changes and completion tests are explicit; never for a whole change or an open-ended security claim.
Use one session per coherent change, keep its debugging and review fixes in that session, and start fresh for a different change or an independent review.
A session needs the project map, the change and its next tasks, the relevant decisions, and the completion checks, not the full earlier conversation.
Report the diff scope, commands and actual results, unverified cases, and any remaining approval gate.

The machine-specific execution environment belongs in the session's task input, not in these instructions.
Use the existing Haskell toolchain, verify its paths and versions, and do not install host tools or change global configuration merely to get a check past a blocker.

Independent worktrees suit separate concerns such as storage, PublicWeb, CI and developer experience, and pure-domain work.
Agree shared identifier types and interfaces first, and serialize manifest and freeze-file changes at merge time.
Multiple writing sessions in one checkout are not independent branches.
Do not delegate agent work or create competing implementations without a task that explicitly requests it.

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
The [security-boundaries spec](openspec/specs/security-boundaries/spec.md) states the threat model and the limits of each enforcement mechanism.

## Commands and current status

The legacy package is `eselsohr`; its executable is `eselsohr-exe`.
Its root `cabal build` and `cabal test` commands are not a passing baseline; GHC 9.10.3 does not satisfy its `base` bound.

The independent `v2/` package is `eselsohr-v2`.
Its library, executable, and tests use GHC2024.
The shared Cabal `language` stanza enables OverloadedStrings for all v2 components; keep extensions used only by particular modules in their source pragmas.
These commands pass on x86-64 Linux and Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0:

```sh
cabal build --project-file=cabal-v2.project all
cabal test --project-file=cabal-v2.project all --test-show-details=direct
cabal run --project-file=cabal-v2.project eselsohr-v2 -- --version
```

The [development-workflow spec](openspec/specs/development-workflow/spec.md) and `openspec/config.yaml` list the offline, formatting, linting, and HLS checks, and the archived `add-local-quality-checks` change explains the macOS setup.
Dependency versions and the Hackage index state are pinned; machine-local overrides stay untracked.
The contributor workflow should support Linux and macOS without requiring Nix or a particular developer's container setup.

M1, the local checks of M2, and M3 and M4 are implemented: validated values, article transitions, grant validation, operation policy, and a private SQLite adapter with protected article reads and title changes.
`add-scoped-store-effects` (M5) is the next milestone; the GitHub CI work in `add-native-ci` is deferred while local development is the priority.
D1–D4 are approved; D5 (deletion lifecycle) remains open in `add-link-sharing-and-owner-lifecycle`.

For documentation-only changes, run `openspec validate --all --strict`, inspect new files and relative links, run `git status --short` and `git diff --check`, and check untracked files explicitly.
`git diff` does not show their contents.
Do not change dependencies or install tooling to verify documentation.
