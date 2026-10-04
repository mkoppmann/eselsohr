# Proposal

## Why

The replacement's native path is verified only by local runs, so nothing checks it continuously as branches change.
The legacy CI rebuilds the legacy application, its documentation, and its Docker image for work that only touches `v2/`, and its cache keys do not reflect the actual build inputs.
This change covers the deferred GitHub part of milestone M2: tasks 2.2 and 2.3 and the workflow of task 2.4.
It is deferred while local development is the priority; the local build, test, formatting, linting, and HLS checks from `add-local-quality-checks` are sufficient to continue without it.

## What Changes

- Add a focused native Linux workflow for the replacement with pinned tool setup, explicit project selection, Build and Test running in parallel with the quality checks, relevant caching, and cancellation of superseded runs.
- Give the workflow read-only default token permissions and no publication credentials in pull request jobs.
- After that workflow is green on an actual runner, scope the triggers and paths of the existing `ci.yml` and `code-scanning.yml` so that v2-only changes do not rebuild the legacy application or image, without dropping scanner coverage.
- Add a manual or scheduled native macOS workflow that runs Build, Test, and Version without Nix.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `development-workflow`: adds continuous integration of the replacement on Linux and macOS and the scoping of the legacy jobs.

## Non-goals

- Docker, Haddock, release artifacts, static linking, packaging, Nix, or a broad compiler and platform matrix in pull request and push feedback.
- Branch-protection administration or changes to repository settings.
- Disabling the existing checks, dropping security scanners, or weakening them to achieve a green badge.
- Legacy dependency repairs or a Docker rewrite.
- Signed releases, or claiming that future SQLite and fetcher dependencies are validated on macOS.

## Dependencies and approval gates

Depends on `establish-replacement-package` (pinned resolution) and `add-local-quality-checks` (the quality commands and the local macOS verification).
Scoping the legacy jobs depends on the new workflow being demonstrated green on an actual runner, not merely merged.
No approval gate is consumed.
The supported platform matrix remains an open human choice, to be settled by what CI proves affordable.
Runner results require the author to push; agents never push.

## Impact

- New workflows under `.github/workflows/` for native Linux and macOS checks of the replacement.
- Changed triggers and paths in `.github/workflows/ci.yml` and `.github/workflows/code-scanning.yml`.
- Concise command notes where the workflows need them.
- No source, dependency, or freeze changes.
