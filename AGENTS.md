# Working on Eselsohr

Eselsohr is a self-hostable bookmark manager and an experiment in capability-based security.
Eselsohr 2 should stay small enough for one developer to understand the complete application.

## Project map

Read [README.md](README.md) for the existing application and the author's prose.
Read [the Eselsohr 2 design](docs/design/eselsohr-2.md) for agreed decisions, proposals, approval gates, and security requirements.
Use [the investigation](docs/design/investigation.md) for existing behavior and evidence, and [the roadmap](docs/design/roadmap.md) for bounded tasks and acceptance checks.
The design describes the replacement's intended behavior; only its build seed is implemented.
It does not describe the legacy application's behavior.

The existing code is evidence of requirements and experiments, not an architecture that must be preserved.
There is no legacy data or URL compatibility requirement for the redesign.

## Working scope

Implement the requested task, not the whole roadmap.
Keep changes reviewable.
Stop at unresolved architectural decisions that affect the task.
Preserve unrelated changes.
Do not add speculative documentation or abstractions.
Keep docs current: distinguish implemented behavior from current proposals.
Replace or remove obsolete information; Git history records past states.
Update the design and roadmap when decisions or implementation status change.
Use concise, concrete prose in the style of the README and existing comments.
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
It contains only a version command and an assertion-based smoke test, not bookmark functionality.
These commands pass on Linux with GHC 9.10.3 and Cabal 3.16.1.0:

```sh
cabal build --project-file=cabal-v2.project all
cabal test --project-file=cabal-v2.project all --test-show-details=direct
cabal run --project-file=cabal-v2.project eselsohr-v2 -- --version
```

The root `hie.yaml` selects `cabal-v2.project` for HLS and excludes source outside `v2/`.
HLS 2.14.0.0 loads all three v2 components with GHC 9.10.3 on Linux; see the roadmap for the check command.

Fourmolu 0.20.1.0 and HLint 3.10 check every v2 Haskell source and pass on Linux.
See roadmap task 2.1 for the commands; use the existing formatter style and HLint defaults.

D1 (independent package) and D2 (manual bookmarking first) are approved.
M1 and task 2.1 are complete.
Local development is the current priority; GitHub CI tasks 2.2 and 2.3 are deferred.
The next task is 3.1 and needs approval before work starts; D3–D5 remain pending.
Dependency versions and the Hackage index state are pinned.
See roadmap task 1.3 for deliberate freeze updates and verified offline commands; machine-local overrides stay untracked.
The contributor workflow should support Linux and macOS without requiring Nix or a particular developer's container setup.

For documentation-only changes, inspect new files and relative links, run `git status --short` and `git diff --check`, and check untracked files explicitly.
Ordinary `git diff` does not show their contents.
Do not change dependencies or install tooling to verify documentation.
