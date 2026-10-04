# development-workflow Specification

## Purpose
Defines how contributors build, test, and check the Eselsohr 2 replacement natively on Linux and macOS, with pinned dependencies and without requiring Nix or a particular developer's container setup.

## Requirements

### Requirement: Independent replacement package
The replacement SHALL build and test as the `eselsohr-v2` package selected by `cabal-v2.project`, independently of the legacy root package.
It MUST NOT import the legacy `Lib` tree, and its solved plan MUST NOT contain the legacy `eselsohr` package.
Build and Test pass with GHC 9.10.3 and Cabal 3.16.1.0.
The legacy root `cabal build` and `cabal test` remain failing and are not evidence about the replacement.

#### Scenario: Build and test the replacement
- **WHEN** a contributor runs `cabal build --project-file=cabal-v2.project all` and `cabal test --project-file=cabal-v2.project all --test-show-details=direct` from the repository root
- **THEN** both commands succeed with GHC 9.10.3 and Cabal 3.16.1.0

#### Scenario: Solved plan excludes the legacy package
- **WHEN** the independent project's package plan is inspected
- **THEN** it contains the replacement components and their pinned dependencies, and no legacy `eselsohr` package

### Requirement: Version command
The `eselsohr-v2` executable SHALL print its version for `--version` and MUST reject missing or unsupported arguments with exit status 1 and usage on stderr.
The test suite retains an assertion on the version output.

#### Scenario: Print the version
- **WHEN** a contributor runs `cabal run --project-file=cabal-v2.project eselsohr-v2 -- --version`
- **THEN** the executable prints `eselsohr-v2 0.1.0.0` and exits successfully

#### Scenario: Reject an unsupported argument
- **WHEN** the executable runs without arguments or with an unsupported argument
- **THEN** it prints usage on stderr and exits with status 1

### Requirement: Pinned dependency resolution
The independent project SHALL pin the Hackage index state in `cabal-v2.project` and the complete dependency solution in `cabal-v2.project.freeze`.
The freeze file changes only through a deliberate update.
The machine-local `cabal-v2.project.local` and its backup MUST stay untracked, and verification results assume no local overrides.
Pinning establishes repeatability, not trust in the dependencies.

#### Scenario: Repeated resolution is stable
- **WHEN** a contributor runs `cabal freeze --project-file=cabal-v2.project` without changing bounds or the index state
- **THEN** the regenerated freeze file is byte-identical to the committed one

#### Scenario: Offline reuse of populated caches
- **WHEN** Build and Test run with `--offline` after the dependency caches are populated
- **THEN** they succeed and resolve the same components and dependency versions as the online run

#### Scenario: Local overrides stay out of the repository
- **WHEN** `cabal configure` writes `cabal-v2.project.local`
- **THEN** Git ignores the file and its backup

### Requirement: Formatting and linting
Every Haskell source in `v2/` SHALL pass Fourmolu 0.20.1.0 in check mode with the root `fourmolu.yaml` and HLint 3.10 with its default hints.
The checks MUST NOT rely on broad suppressions, and they cover neither generated files nor legacy code.

#### Scenario: Check the replacement sources
- **WHEN** a contributor runs `rg --files v2 -g '*.hs' -0 | xargs -0 fourmolu-0.20.1.0 --mode check` and `hlint-3.10 v2` from the repository root, or the unversioned `fourmolu` and `hlint` when their `--version` matches
- **THEN** both commands succeed

#### Scenario: A violation fails the check
- **WHEN** a v2 source contains a formatting or lint violation
- **THEN** the corresponding check fails

### Requirement: Editor integration
The root `hie.yaml` SHALL select `cabal-v2.project` and map both v2 libraries, the executable, and both test directories to their Cabal components.
Source outside `v2/` MUST receive no HLS project diagnostics.
HLS 2.14.0.0 with GHC 9.10.3 typechecks every v2 source on Linux and macOS; interactive editor behavior is unverified.

#### Scenario: Typecheck every replacement source
- **WHEN** a contributor runs `rg --files v2 -g '*.hs' -0 | xargs -0 haskell-language-server-wrapper -j 2`
- **THEN** HLS reports every v2 file worked and zero failures

#### Scenario: Editors launch HLS as a language server
- **WHEN** an editor launches HLS with `--lsp`
- **THEN** LSP initialization and shutdown complete cleanly

### Requirement: Native macOS verification
The native contributor path SHALL work on Apple Silicon macOS with the Xcode Command Line Tools and GHCup-installed GHC 9.10.3, Cabal 3.16.1.0, and HLS 2.14.0.0, without Nix.
Build, Test, Version, the offline checks, formatting, linting, and HLS pass there from empty Cabal caches.
This is not a wider platform support matrix.

#### Scenario: Fresh macOS checkout
- **WHEN** a contributor on Apple Silicon macOS installs the native tools, runs `cabal update`, and runs the verification commands from empty caches
- **THEN** every command succeeds and the solved plan matches the freeze file

#### Scenario: Bundled SQLite on macOS
- **WHEN** `otool -L` inspects the SQLite test executable
- **THEN** it lists no SQLite shared library

### Requirement: Specifications and changes in OpenSpec
Current behavior SHALL be specified in `openspec/specs/`, and planned or in-progress work SHALL be described by changes in `openspec/changes/`, one per milestone.
Specs MUST describe only implemented, verified behavior, and every spec and change passes `openspec validate --all --strict`.
Documentation-only changes also pass the Diff checks, with untracked files inspected explicitly.

#### Scenario: Validate the specifications
- **WHEN** a contributor runs `openspec validate --all --strict` from the repository root
- **THEN** every spec and active change is valid

#### Scenario: Complete a milestone
- **WHEN** a change's tasks and exit review are complete
- **THEN** it is archived, and its delta specs become part of the main specs
