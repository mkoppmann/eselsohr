# Proposal

## Why

While the replacement lives in `v2/` beside the legacy package, the repository contains two applications and its root `cabal build` and `cabal test` fail.
This change covers milestone M10: leave one comprehensible application that builds and tests with the root Cabal project.

## What Changes

- Move the replacement to the repository root without behavior changes.
- **BREAKING**: remove the now-unused legacy code, including the `eselsohr-exe` executable and its compressed JSON collection format; no legacy data or URL compatibility is required.
- Prune dependencies, scripts, and workflows that only the legacy package used.
- Update or remove documentation and specs that no longer describe the code, including those that mention `v2/` paths, `cabal-v2.project`, or `eselsohr-v2`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `development-workflow`: the replacement builds, tests, and runs from the root Cabal project instead of the independent `cabal-v2.project`.

## Non-goals

- Feature work or opportunistic dependency upgrades mixed into the cutover.
- Changes to application behavior, security properties, or the dependency solution beyond removing legacy-only packages.
- Native deployment, optional Nix, and containers, which `add-native-deployment` covers.

## Dependencies and approval gates

Depends on acceptance of the intended replacement feature and security baseline, not merely on a seed that builds.
The accepted baseline follows `add-link-sharing-and-owner-lifecycle` (M7) and `add-offline-maintenance` (M9).
This change completes D1, which started the replacement as an independent package with a later root cutover.
The executable name remains open; verification uses `eselsohr`, subject to the accepted name.

## Impact

- The `v2/` package, its project and freeze files, and `hie.yaml` move to root equivalents with the same pins.
- Legacy `src/`, `app/`, `test/`, `eselsohr.cabal`, and legacy-only scripts and workflows are removed.
- The verification vocabulary in `openspec/config.yaml`, the `development-workflow` requirements, `AGENTS.md`, and the README change to root commands.
