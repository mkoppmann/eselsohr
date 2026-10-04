# Design

## Context

The legacy [eselsohr.cabal](../../../eselsohr.cabal) targets GHC 9.8 with `base ^>=4.19.1.0`, and its executable is `eselsohr-exe`.
With the available GHC 9.10.3, the root `cabal build` and `cabal test` dry runs exit 1 with Cabal-7107 because the base constraint excludes the installed `base` 4.20.2.0; the intended GHC 9.8 build is unverified.
The replacement is the independent `eselsohr-v2` package in `v2/`, selected by `cabal-v2.project` and pinned by `cabal-v2.project.freeze`.
The root [hie.yaml](../../../hie.yaml) maps the v2 components through `cabal-v2.project` and gives source outside `v2/` a `none` cradle.
The legacy package also brings scripts such as [bin/format](../../../bin/format) and [bin/run](../../../bin/run), and [CI](../../../.github/workflows/ci.yml) and [code scanning](../../../.github/workflows/code-scanning.yml) workflows that build it.

## Goals / Non-Goals

**Goals:**

- One application whose root build, tests, and quality checks pass.
- Reviewable steps whose diffs show either a move or a removal, not both mixed with behavior changes.

**Non-Goals:**

- Preserving legacy code as reference material in the tree; Git history keeps it.

## Decisions

### Separate changes in a fixed order

Cut over in separate changes: move the replacement to the root without behavior changes; remove now-unused legacy code; prune dependencies, scripts, and workflows; update or remove documentation that no longer describes the code.
The move keeps the existing dependency pins and Hackage index state, so the solved plan differs only by the removed legacy package.
Do not mix feature work or opportunistic upgrades into cutover.

### A dedicated cutover branch

Use normal Code on a dedicated cutover branch.
Avoid parallel branches changing paths during cutover, because path moves conflict with almost every other change.
A mechanically large deletion is acceptable if ownership and lack of consumers are clear.

### Specs and documentation follow the code

Every artifact that names `v2/`, `cabal-v2.project`, its freeze file, or `eselsohr-v2` changes with the move.
This includes the remaining `development-workflow` requirements for the version command, pinned resolution, formatting and linting, and editor integration, the verification vocabulary in `openspec/config.yaml`, `AGENTS.md`, and the README.
The delta spec rewrites only the package requirement until the cutover starts.
A MODIFIED requirement replaces the whole requirement when it is archived, so the other blocks are written from the main spec as it stands then, after earlier changes such as `add-native-ci` and once the executable name is accepted.
Keep the README's concise, understated prose, not its stale assertions.

### Executable name

Verification uses `cabal run eselsohr` from the repository root, subject to the accepted executable name.
The specs, documentation, and later deployment name the executable, so the move uses the accepted name in all of them.

## Risks / Trade-offs

- [Concurrent branches change paths during the move] → Avoid parallel branches during cutover and land the move first.
- [A removed file still has a consumer] → Delete only code whose ownership and lack of consumers are clear; Build, Test, Quality, and HLS run after each step.
- [Pruned workflows leave the replacement unchecked] → Keep or replace the checks the replacement needs before removing legacy jobs.
- [Stale v2 paths survive in specs or documentation] → Search for `v2/`, `cabal-v2.project`, and `eselsohr-v2` as part of the documentation step.

## Open Questions

- The accepted executable and package names.
- Whether the legacy Dockerfile and Nix flake are removed at cutover or replaced in `add-native-deployment`; their workarounds should go only after a small replacement is demonstrated.
