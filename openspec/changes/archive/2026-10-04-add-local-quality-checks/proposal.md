# Proposal

## Why

The replacement needs fast, consistent local feedback before many new modules arrive, and the promised second platform must be proven before native assumptions become embedded in the architecture.
This change covers the local part of milestone M2; the GitHub workflows stay deferred in `add-native-ci` while local development is the priority.

## What Changes

- Check every v2 Haskell source with Fourmolu 0.20.1.0 and HLint 3.10, using the existing root `fourmolu.yaml` and HLint defaults.
- Configure HLS through the root `hie.yaml` for the replacement components only.
- Verify Build, Test, Version, offline build and test, quality checks, and HLS on Apple Silicon macOS with native GHCup tools, and record the setup steps.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `development-workflow`: adds formatting and linting, editor integration, and native macOS verification.

## Non-goals

- Reformatting legacy code, style redesign, universal lint suppressions, a wrapper script, or unrelated tools.
- Release binaries, containers, Haddock, Nix redesign, signed releases, or broad compiler and platform matrices.
- GitHub workflows, which `add-native-ci` covers.

## Dependencies and approval gates

Depends on `establish-replacement-package`.
No approval gate is consumed.

## Impact

- `hie.yaml` selects `cabal-v2.project`; source outside `v2/` receives no HLS project diagnostics.
- v2 Haskell sources follow the formatter and linter.
- No dependency, freeze, or global configuration changes.
