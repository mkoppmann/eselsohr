# Design

## Context

The legacy CI runs HLint over `app/`, `src/`, and `test/` and checks Fourmolu formatting; see [ci.yml](../../../../.github/workflows/ci.yml).
[bin/format](../../../../bin/format) still invokes Brittany through Nix and is not the canonical formatter.
[bin/run](../../../../bin/run) invokes Nix and ghcid; it is convenience machinery, not proof that Cabal needs Nix.

## Goals / Non-Goals

**Goals:**

- Small, consistent diffs and editor diagnostics for every replacement source.
- Evidence that the native path works on Apple Silicon macOS, not only Linux.

**Non-Goals:**

- Claiming that future SQLite and fetcher dependencies are validated on macOS before they exist, or that a seed build establishes storage or TLS security.

## Decisions

### Existing formatter style and default hints

The formatter loads the root [fourmolu.yaml](../../../../fourmolu.yaml); HLint uses its default hints without additional settings or suppressions.
No additional configuration earns its place yet.
The checks cover the replacement source, not generated files or legacy code.

On x86-64 Linux the tools are callable as `fourmolu-0.20.1.0` and `hlint-3.10`, because their unversioned aliases are not set there.
Unversioned `fourmolu` and `hlint` commands are also suitable when their `--version` output matches the tested versions.

### Explicit HLS cradle

The root [hie.yaml](../../../../hie.yaml) uses a multi cradle: `v2/` maps the two libraries, the executable, and both test directories to their Cabal components through `cabal-v2.project`, and the rest of the repository uses a `none` cradle.
This follows the supported [explicit Cabal configuration](https://github.com/haskell/hie-bios#cabal).
Without `--lsp`, HLS runs its command-line setup tester, so editor launchers must pass it.

### macOS setup

Install the Xcode Command Line Tools for the C compiler, then GHC 9.10.3, Cabal 3.16.1.0, and optionally HLS 2.14.0.0 with GHCup.
Put GHCup's `bin` directory on `PATH` and run `cabal update` once before Build.
Fourmolu and HLint need incompatible `ghc-lib-parser` versions, so install them with separate commands outside the project:

```sh
cabal install --ignore-project fourmolu-0.20.1.0
cabal install --ignore-project hlint-3.10
```

Cabal installs the executables under its configured `installdir`, `~/.local/bin` by default.

## Risks / Trade-offs

- [Formatting and linting do not establish correct behavior or enforce access restrictions] → They only keep diffs small and catch common mistakes; no broad suppressions may hide partial or unsafe code.
- [Apple's linker warns that `-U` is redundant with `-undefined dynamic_lookup` when linking executables] → The warning comes from GHC's link flags and does not affect the result.
- [Interactive editor behavior is unverified] → LSP initialization and clean shutdown pass through a local Linux editor launcher; interactive use is not claimed.
