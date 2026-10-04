## ADDED Requirements

### Requirement: Native Linux continuous integration
Pushes and pull requests SHALL run Build and Test natively on Linux for the `eselsohr-v2` project selected by `cabal-v2.project`, using the pinned GHC and Cabal versions.
The formatting and linting checks MUST run in parallel with Build and Test rather than gating them.
A run MUST NOT report success when the test suite selects no tests.
A local review of the workflow file is not a runner pass.

#### Scenario: Pull request changes replacement source
- **WHEN** a pull request changes a file in `v2/`
- **THEN** a runner builds and tests the replacement package and runs Fourmolu and HLint over every v2 Haskell source
- **AND** a failure in any of these checks fails the run

#### Scenario: Build does not wait for linting
- **WHEN** the workflow starts
- **THEN** the build and test job and the quality jobs start without waiting for each other

### Requirement: Least-privilege workflow permissions
The replacement workflows SHALL use read-only default token permissions.
Pull request jobs MUST NOT receive publication credentials.

#### Scenario: Inspect workflow permissions
- **WHEN** the workflow definitions are inspected
- **THEN** the default token permissions are read-only and no pull request job references a publication secret

### Requirement: Cancellation of superseded runs
A newer push to the same branch or pull request SHALL cancel the replacement workflow's in-progress run for the older commit.

#### Scenario: Push twice in quick succession
- **WHEN** a second commit is pushed to a branch while the first commit's run is in progress
- **THEN** the first run is cancelled and the second run proceeds

### Requirement: Cache keys reflect build inputs
CI caches SHALL be keyed by the operating system, architecture, compiler version, Cabal version, and dependency inputs, including `cabal-v2.project` and `cabal-v2.project.freeze`.
CI MUST use the committed dependency pins rather than updating or freezing dependencies before restoring a cache.
A key MUST NOT name inputs, such as a compiler matrix, that the workflow does not have.

#### Scenario: Dependency pins change
- **WHEN** a commit changes the freeze file or the project file
- **THEN** the run uses a different cache key and does not reuse a build produced from the old pins

#### Scenario: Warm run
- **WHEN** a run follows another with unchanged inputs on the same platform
- **THEN** it restores the cached dependencies and still builds and tests the current source

### Requirement: Scoped legacy jobs
The legacy build and scanning workflows SHALL NOT rebuild the legacy application or its image for changes confined to the replacement.
Legacy-only, shared-configuration, and packaging changes MUST still trigger the checks they need, and scanner coverage MUST remain for deployed artifacts.
Required check names and path filters MUST NOT leave a required check pending forever or let a merge pass without the relevant checks.

#### Scenario: Replacement-only change
- **WHEN** a change touches only files in `v2/`
- **THEN** the replacement workflow runs and the legacy application and Docker image are not rebuilt

#### Scenario: Packaging change
- **WHEN** a change touches the `Dockerfile` or other packaging inputs
- **THEN** the image scanning job runs

#### Scenario: Shared configuration change
- **WHEN** a change touches configuration shared by both packages, such as `fourmolu.yaml`
- **THEN** the checks of every package that uses it run

### Requirement: Native macOS continuous verification
A manual or scheduled workflow SHALL run Build, Test, and Version on an actual macOS runner with native GHCup tools and without Nix.
Linux emulation is not evidence for macOS.
A passing seed build does not establish storage or TLS security on that platform.

#### Scenario: Run the macOS workflow
- **WHEN** the macOS workflow is started manually or by its schedule
- **THEN** Build, Test, and Version run natively on macOS and the run fails if any of them fails
