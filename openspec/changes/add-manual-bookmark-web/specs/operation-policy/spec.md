## ADDED Requirements

### Requirement: Describing the acting grant
Describing the acting grant to its holder SHALL require only a valid acting chain at the decision time, and no permission.
Like every decision, success returns no data; the protected operation limits what a description reveals.

#### Scenario: A grant without permissions
- **WHEN** a valid grant with no permissions requests its own description
- **THEN** the decision allows it

#### Scenario: An expired grant
- **WHEN** a grant requests its own description at or after its expiration
- **THEN** the decision returns ActorChainRejected
