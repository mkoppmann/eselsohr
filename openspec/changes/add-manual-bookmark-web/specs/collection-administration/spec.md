## Purpose

Defines how an operator creates a collection and receives its owner URL, and keeps collection creation out of the web interface.

## ADDED Requirements

### Requirement: Operator collection creation
An operator command SHALL create a new collection file in the configured `collections` directory, with its identity, the current schema, and one root grant.
The root grant MUST have all eight permissions, whole-collection scope, and no expiration.
The command MUST NOT create the data or collections directory.

#### Scenario: Create a collection
- **WHEN** the operator runs the collection creation command
- **THEN** a new collection file exists in the `collections` directory with its identity, schema version, and exactly one root grant with all eight permissions and no expiration

#### Scenario: Missing collections directory
- **WHEN** the operator runs the command with a data directory that has no `collections` directory
- **THEN** the command fails and creates no directory or file

### Requirement: Fresh owner secret
The owner bearer's secret SHALL consist of 256 bits from the operating system's cryptographically secure random generator, generated in trusted creation code.
The collection identifier SHALL come from the same generator.
Parsing or round-trip tests cannot establish this entropy.

#### Scenario: Two collections
- **WHEN** the operator creates two collections
- **THEN** their owner bearers have independently generated secrets

### Requirement: One-time owner URL
The creation command SHALL show the owner URL once, formed as the configured public origin, `/b/`, and the owner bearer.
Storage MUST keep only the secret's hash, and no later command or page can recover the URL.

#### Scenario: Owner URL shown at creation
- **WHEN** the operator creates a collection with the public origin `https://example.org`
- **THEN** the command prints the owner URL, starting with `https://example.org/b/`, once

#### Scenario: Inspect stored data
- **WHEN** the new collection's database is inspected
- **THEN** it contains the bearer's hash and no reusable bearer secret

### Requirement: Operator-only creation
Collection creation SHALL be available only to the operator command.
The web interface MUST NOT offer a way to create a collection.

#### Scenario: Web client attempts creation
- **WHEN** a web client sends any request to the interface
- **THEN** no new collection file is created
