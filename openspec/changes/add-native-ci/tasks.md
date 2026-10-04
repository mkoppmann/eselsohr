# Tasks

These are outlined slices; before `/opsx:apply`, expand the next slice into bounded tasks as the project's task rules require.

## 1. Native Linux workflow

- [ ] 1.1 Add a replacement workflow under `.github/workflows/` with pinned GHC and Cabal setup, explicit `cabal-v2.project` selection, a Build and Test job, and parallel Fourmolu and HLint jobs; verified by Build, Test, and Quality locally, workflow inspection, and an authorized GitHub run after the author pushes showing that the runner builds and tests the right package and runs the quality checks.
- [ ] 1.2 Set read-only default token permissions, keep publication credentials out of pull request jobs, and cancel superseded runs; verified by workflow inspection and an actual run cancelled by a newer push.
- [ ] 1.3 Cache dependencies with keys for the operating system, architecture, compiler, Cabal version, and dependency inputs, using the committed pins; verified by observed cold and warm runs showing useful cache reuse without a stale build, recording durations only after observing them.

## 2. Scoped legacy jobs

- [ ] 2.1 After task 1.1 is green on an actual runner, narrow the triggers and paths of `ci.yml` and `code-scanning.yml` so that v2-only changes do not rebuild the legacy application or image; verified by a trigger matrix for v2-only, legacy-only, shared-configuration, and packaging changes checked against actual runs after author-controlled pushes.
- [ ] 2.2 Confirm that scanner coverage remains for deployed artifacts and that required check names and path filters leave no dead end or unchecked merge; verified by inspecting the matrix and actual runs, and by re-running the relevant Build and Quality commands when shared configuration changes.

## 3. Native macOS workflow

- [ ] 3.1 Add a manual or scheduled macOS workflow running Build, Test, and Version with native GHCup tools and no Nix; verified by an actual macOS runner result, not Linux emulation, and Diff checks, reporting the verification as pending if no runner is available.

## 4. Review

- [ ] 4.1 Review M2's CI part: local checks exercise the intended package, cache keys reflect actual inputs, and every claim rests on observed runner results; record unverified platforms and cases.
