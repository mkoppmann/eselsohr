# Proposal

## Why

The legacy package targets GHC 9.8 and does not solve with the available GHC 9.10.3 toolchain, so it offers no testable starting point for the rewrite.
Future sessions also need a reliable map of the decisions, evidence, and bounded tasks before the code diverges from the legacy application.

## What Changes

- Record the current decisions, legacy evidence, and task contracts as project documentation.
  The `adopt-openspec` change later moves this material into OpenSpec specs and changes.
- Add the independent `cabal-v2.project` and the `eselsohr-v2` package in `v2/`, with a tiny library, an executable supporting `--version`, and an assertion-based test suite.
- Pin the Hackage index state and the complete dependency solution in a freeze file, and ignore the machine-local project file.

## Capabilities

### New Capabilities

- `development-workflow`: how contributors build, test, and check the replacement natively with pinned dependencies.

### Modified Capabilities

None.

## Non-goals

- Application behavior, legacy fixes, persistence, web serving, effects, deployment, or broad tool installation.
- A listening socket, dummy web application, database files, old-module imports, or compatibility adapters.
- Resolving future SQLite, effect, or web dependencies, a compiler matrix, Nix, global installs, blanket `allow-newer`, or a build wrapper.

## Dependencies and approval gates

This change consumes D1 (independent package) and D2 (manual bookmarking first).
It does not settle D3–D5.

## Impact

- New: `cabal-v2.project`, `cabal-v2.project.freeze`, `v2/eselsohr-v2.cabal`, `v2/LICENSE`, and minimal `v2/app`, `v2/src`, and `v2/test` files.
- Changed: `.gitignore` ignores `cabal-v2.project.local` and its backup.
- The legacy package and its build remain unchanged.
