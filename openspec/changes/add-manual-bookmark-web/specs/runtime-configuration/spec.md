## Purpose

Defines how Eselsohr reads and validates its configuration: one mechanism, explicit failure on malformed values, and no side effects on the rest of the process environment.

## ADDED Requirements

### Requirement: One configuration mechanism
The application SHALL read its configuration through one mechanism.

#### Scenario: Configure the application
- **WHEN** an operator configures the data directory, listener, public origin, and log level
- **THEN** every value is supplied through the same mechanism

### Requirement: Malformed explicit values fail
When an explicitly supplied configuration value is malformed, the application SHALL fail to start.
It MUST NOT silently fall back to a default for that value.

#### Scenario: Malformed public origin
- **WHEN** the operator supplies a public origin that is not a valid origin
- **THEN** the application does not start

### Requirement: Process environment preserved
Reading configuration MUST NOT unset or change environment variables that do not belong to Eselsohr.

#### Scenario: Unrelated variables
- **WHEN** the application starts with unrelated environment variables set
- **THEN** those variables remain unchanged in its process

### Requirement: Files and directories kept distinct
Configured directory paths SHALL be validated as directories and configured file paths as files, without rewriting one kind into the other.

#### Scenario: Data directory
- **WHEN** the configured data directory does not exist or names a regular file
- **THEN** the application does not start
