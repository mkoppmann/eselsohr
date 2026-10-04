# Proposal

## Why

Four hand-maintained documents held the design, roadmap, legacy investigation, and security model, and `AGENTS.md` repeated much of their status.
Keeping implemented behavior, approved decisions, and planned work consistent across them took growing effort, and Claude Code and Codex had no shared workflow for proposing, applying, and archiving work.

## What Changes

- Install OpenSpec through Homebrew and generate its skills and commands for Claude Code (`.claude/skills/`, `.claude/commands/opsx/`) and Codex (`.agents/skills/`).
- Add `openspec/config.yaml` with the project context, approved decisions, constraints, verification vocabulary, and per-artifact rules.
- Express every roadmap milestone as a change: M1, the local part of M2, and M4 as archived changes; M3 as an active change awaiting its exit review, with its specs synced; and the remaining milestones as outlined active changes.
- Move implemented, verified behavior into specs, and each legacy finding into the design of the change it motivates.
- **BREAKING** for contributors: delete `docs/design/eselsohr-2.md`, `docs/design/roadmap.md`, `docs/design/investigation.md`, and `docs/security-model.md`.
- Point `AGENTS.md` at OpenSpec as the agent-neutral instructions for every coding agent.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `development-workflow`: adds the requirement that specs and planned work live in OpenSpec and validate strictly.

## Non-goals

- Changing any Haskell source, dependency, or verification command.
- Settling D5 or any other open decision, or expanding outlined milestones into bounded tasks.

## Dependencies and approval gates

None; the author chose one change per milestone, retroactive changes for completed milestones, an active M3 change for its pending exit review, Homebrew installation, and disabled telemetry.

## Impact

- New: `openspec/`, `.claude/skills/openspec-*`, `.claude/commands/opsx/`, and `.agents/skills/openspec-*`.
- Changed: `AGENTS.md`.
- Removed: `docs/`.
