# Tasks

## 1. Tooling

- [x] 1.1 Install OpenSpec 1.14.0 with Homebrew, disable telemetry, and run `openspec init --tools claude,codex`; verified by `openspec --version` and by reading the generated skills and commands.
- [x] 1.2 Write `openspec/config.yaml` with the project context, rules, and apply and archive guidance; verified by `openspec instructions` printing the rules without warnings.

## 2. Completed milestones

- [x] 2.1 Write and archive `establish-replacement-package` and `add-local-quality-checks`; verified by `openspec validate --strict` before archiving and by the resulting `development-workflow` spec.
- [x] 2.2 Write `add-domain-and-policy` with its exit review open, and sync its five specs into `openspec/specs/`; verified by comparing the main specs with an archive of a scratch copy.
- [x] 2.3 Write and archive `add-protected-sqlite-storage`; verified by `openspec validate --strict` and the resulting storage, bearer, protected-operation, and security specs.
- [x] 2.4 Rename the archive directories to the dates of the commits that completed each milestone, and fix the relative links for their archived location.

## 3. Pending milestones

- [x] 3.1 Write outlined changes for the deferred CI work and milestones M5–M11, with their approved requirements, decisions, open questions, legacy evidence, and unchecked slices; verified by `openspec validate --strict` for each.

## 4. Agent instructions and old documents

- [x] 4.1 Rewrite `AGENTS.md` for the OpenSpec workflow keeping it agent-neutral; verified by reading it.
- [x] 4.2 Delete the old design, roadmap, investigation, and security-model documents after checking that each section has a new home or a stated reason for removal.

## 5. Verification

- [x] 5.1 Run `openspec validate --all --strict`, check every relative link under `openspec/` and `AGENTS.md`, search for references to the removed documents, and run the Diff checks.
