# Design

## Context

OpenSpec separates current requirements in `openspec/specs/` from proposed work in `openspec/changes/`, and moves completed changes to `openspec/changes/archive/` while merging their delta specs.
The previous documents mixed both: implemented contracts, approved decisions, proposals, task cards, and legacy evidence.

## Goals / Non-Goals

**Goals:**

- One place for current behavior and one for each milestone's purpose, decisions, and tasks.
- The same workflow for Claude Code and Codex, with project rules applied to every artifact.

**Non-Goals:**

- Preserving the old documents' structure or task numbering beyond each change's own task list.

## Decisions

### Tooling

OpenSpec 1.14.0 is installed through Homebrew; contributors who prefer npm can install `@fission-ai/openspec` instead.
`openspec init --tools claude,codex` generated the core profile (propose, explore, apply, update, sync, archive) as skills and commands for Claude Code and skills for Codex.
The generated files are committed and left unedited, because `openspec update` regenerates them after an upgrade; project guidance lives in `openspec/config.yaml` instead.
Telemetry is disabled with `openspec config set telemetry.enabled false`, and `OPENSPEC_TELEMETRY=0` disables it for a single command.
Rules for every artifact go in the config context, because 1.14.0 only accepts rules keyed by an artifact name.

### One change per milestone

Each milestone becomes one change, so its proposal states the purpose and non-goals, its design keeps the decisions and alternatives, and its tasks carry the former task cards.
Completed milestones were reconstructed as changes and archived in order with `openspec archive`, which built the main specs from their deltas.
Their archive directories were then renamed to the dates of the commits that completed them.
M2 is split: its completed local checks form `add-local-quality-checks`, and its deferred GitHub workflows form `add-native-ci`.
M3 stays active because its exit review is pending; its specs were synced into the main specs, so it must be archived with `--skip-specs`.
The remaining milestones are outlined changes whose tasks must be expanded before applying them.

### Specs hold only implemented behavior

Specs state what the implementation does and what tests establish, with limits, and avoid type signatures and library choices.
Signatures, schema SQL, and alternatives tables stay in the design of the change that introduced them.
Proposed but unimplemented requirements live in the delta specs of the active change that will implement them.

### Legacy evidence

Each legacy finding moved into the design of the change it motivates, citing the legacy source.
Findings about the build baseline, CI, and Nix went into `establish-replacement-package`, `add-local-quality-checks`, `add-native-ci`, `cut-over-to-root-package`, and `add-native-deployment`.
The investigation's overall lesson stays true: possession differs from a constructible policy record, authority discovery can amplify access, fresh authorization belongs with the protected operation, URL parsing is not network confinement, and tests must exercise the production boundary they claim to protect.

### Agent instructions

`AGENTS.md` keeps the working rules, style, security intent, and commands, and describes the OpenSpec workflow instead of linking the old documents.
`AGENTS.md` is the only instruction file: the project uses several models and agents, so it relies on that agent-neutral convention rather than tool-specific files such as `CLAUDE.md`.

## Risks / Trade-offs

- [Archived changes for completed milestones are reconstructions] → They are written from the previous documents and Git history, not from contemporaneous OpenSpec use; Git keeps the original documents.
- [Links in a change move when it is archived] → Links in active changes are relative to `openspec/changes/<name>/`; archiving a change requires adjusting them by one directory level.
- [Generated agent files can drift from the CLI version] → Run `openspec update` after upgrading OpenSpec and review the diff.
