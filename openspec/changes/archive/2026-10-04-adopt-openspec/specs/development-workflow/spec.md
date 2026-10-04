## ADDED Requirements

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
