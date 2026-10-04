## Purpose

Defines how contributors build, test, and check the Eselsohr 2 replacement natively on Linux and macOS, with pinned dependencies and without requiring Nix or a particular developer's container setup.

## ADDED Requirements

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
