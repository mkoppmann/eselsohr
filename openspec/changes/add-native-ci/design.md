# Design

## Context

The replacement builds and tests on x86-64 Linux and Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0, and its formatting, linting, and HLS checks pass on both platforms.
Those checks currently run only locally.
They cover the current domain, policy, and SQLite work; SQLite integration uses the bundled engine and needs no separate system SQLite development package.
On macOS, the local verification also passed from empty Cabal caches, and the SQLite test executable links no SQLite library.

The legacy [CI workflow](../../../.github/workflows/ci.yml) runs on every push.
Its costly structure is observable, but actual GitHub run timings were not retrieved:

- The build waits for HLint before starting.
- It updates and freezes dependencies before restoring the cache, so the cache key follows whatever the index offers that day rather than committed pins.
- Its cache key includes `${{ matrix.ghc }}` although the workflow has no compiler matrix.
- It generates Haddock, a static install, and artifacts on every push.
- It targets the legacy package, whose GHC 9.8 constraint does not solve with the replacement's toolchain.

The legacy [code scanning workflow](../../../.github/workflows/code-scanning.yml) also rebuilds the Docker image for Trivy on its configured pull request, main, and scheduled events.

## Goals / Non-Goals

**Goals:**

- Fast native feedback for the replacement on every push and pull request, with honest cache inputs.
- No redundant legacy work for replacement-only changes, without losing scanner coverage of deployed artifacts.
- A recurring check of the promised second platform.

**Non-Goals:**

- Promising a feedback duration; a few-minute warm build is a target, not a measured prediction.
- Treating a Nix build as evidence for the native path.

## Decisions

### A separate workflow for the replacement

Add a new workflow for `v2/` rather than reworking the legacy `ci.yml` first.
The legacy workflows stay in place until the new one has demonstrated the required coverage on an actual runner; only then are their triggers narrowed.
This keeps each step reviewable and avoids a window in which neither package is checked.

### Parallel quality and build jobs

Run Build and Test in one job and the Fourmolu and HLint checks in parallel jobs, using the commands and tool versions from the Verification vocabulary.
The legacy workflow's build waits for HLint, which delays the more expensive feedback without making it more trustworthy.
A run must fail if the suite selects no tests, so an empty suite never reads as success.

### Committed pins and honest cache keys

The project file and its freeze file pin the Hackage index state and the complete dependency solution.
CI uses those pins and does not run `cabal update` followed by `cabal freeze` to select new versions on each run; pins change only through a deliberate update.
Cache keys include the operating system, architecture, compiler, Cabal version, and dependency inputs, and omit inputs the workflow does not have.
Observed cache reuse must not come from trusting a stale build.

### Least privilege and cancellation

Default token permissions are read-only, and pull request jobs carry no publication credentials.
A newer push cancels superseded runs for the same branch or pull request, since their results no longer describe the code under review.

### Expensive work on separate events

Keep Haddock, release artifacts and static linking, packaging, optional Nix checks, and wider compiler or platform matrices on suitable main, scheduled, or tag jobs.
Those jobs are outside this change; they follow the compiler, SQLite, and deployment choices they consume.
An optional Nix check must build the same package; do not leave the native path untested because Nix happens to build.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| CI | REPLACE | Native fast feedback, honest cache inputs, expensive work on separate events. |

### Scoping the legacy jobs

After the replacement workflow is green on an actual runner, review the triggers and paths of `ci.yml` and `code-scanning.yml`.
Build a trigger matrix for v2-only, legacy-only, shared-configuration, and packaging changes, and check each against actual runs.
Scanner coverage remains for deployed artifacts, and required check names and path filters must not silently permit unchecked merges.
Repository branch-protection settings stay with the author.

### macOS workflow

Run Build, Test, and Version manually or on a schedule on an actual macOS runner with native GHCup tools and no Nix.
The local setup notes from `add-local-quality-checks` describe the tool installation; the workflow must not depend on a particular developer's machine paths or container.
Platform-specific path and library behavior stays an explicit test concern.

## Risks / Trade-offs

- [A local YAML review looks like a pass] → Count only authorized GitHub run results after the author pushes.
- [A cache restores a build that no longer matches its inputs] → Key caches by every input listed above and observe cold and warm runs before recording any duration.
- [Path filters silently permit unchecked merges or leave a required check pending] → Inspect the trigger matrix and actual runs for each change category before relying on the filters.
- [Narrowing legacy triggers drops scanner coverage] → Keep scanning for every change that affects deployed artifacts, and change triggers only after the replacement workflow is green.
- [Pinning improves repeatability, not dependency trust] → Review pin updates deliberately.

## Open Questions

- Which platforms and compiler versions the project supports beyond the verified pair is a human choice, to be settled by what CI proves affordable.
