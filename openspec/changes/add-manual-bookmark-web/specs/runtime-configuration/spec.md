## Purpose

Defines how Eselsohr reads and validates its configuration: environment variables as the one mechanism, explicit failure on missing or malformed values, and no side effects on the rest of the process environment.

## ADDED Requirements

### Requirement: Environment variables as the one mechanism
The server and the collection creation command SHALL read their configuration only from the environment variables `ESELSOHR_DATA_DIR`, `ESELSOHR_LISTEN`, and `ESELSOHR_PUBLIC_ORIGIN`.
No configuration file, command-line option, or log level setting exists.

#### Scenario: Configure the application
- **WHEN** an operator configures the data directory, listener, and public origin
- **THEN** both commands read every value from the same variables

### Requirement: Required values and defaults
`ESELSOHR_DATA_DIR` and `ESELSOHR_PUBLIC_ORIGIN` SHALL be required.
`ESELSOHR_LISTEN` SHALL default to `127.0.0.1:6979` when unset.

#### Scenario: Missing public origin
- **WHEN** a command starts without `ESELSOHR_PUBLIC_ORIGIN`
- **THEN** it does not start

#### Scenario: Default listener
- **WHEN** the server starts without `ESELSOHR_LISTEN`
- **THEN** it listens only on `127.0.0.1` port 6979

### Requirement: Malformed explicit values fail
When a configuration value is set but malformed, the command SHALL fail before doing anything else.
It MUST NOT silently fall back to a default for that value.

#### Scenario: Malformed public origin
- **WHEN** the operator supplies a public origin that is not a valid origin
- **THEN** the command does not start

#### Scenario: Malformed listener
- **WHEN** the operator supplies a listener without a valid host and port
- **THEN** the server does not start

### Requirement: Listener format
`ESELSOHR_LISTEN` SHALL have the form `host:port`, where the host is a name, a dotted IPv4 address, or an IPv6 address enclosed in brackets.
A host name MUST be resolved when the server starts, and a name that cannot be resolved stops the server.

#### Scenario: IPv6 loopback
- **WHEN** the operator sets `ESELSOHR_LISTEN` to `[::1]:6979`
- **THEN** the server listens on the IPv6 loopback address at port 6979

#### Scenario: Unresolvable host name
- **WHEN** the operator sets `ESELSOHR_LISTEN` to a name that does not resolve
- **THEN** the server does not start

### Requirement: Public origin format
`ESELSOHR_PUBLIC_ORIGIN` SHALL consist exactly of a lowercase scheme `http` or `https`, a lowercase ASCII host name or IP literal, and an optional port, without a trailing slash, path, query, fragment, or user information.
The value is not normalized, and the scheme `http` MUST be accepted only for the loopback hosts `localhost`, `127.0.0.0/8`, and `::1`.

#### Scenario: Origin with a path
- **WHEN** the operator supplies `https://example.org/eselsohr` as the public origin
- **THEN** the command does not start

#### Scenario: Trailing slash
- **WHEN** the operator supplies `https://example.org/` as the public origin
- **THEN** the command does not start, and its message names the variable

#### Scenario: Plain HTTP for a public host
- **WHEN** the operator supplies `http://example.org` as the public origin
- **THEN** the command does not start

### Requirement: Process environment preserved
Reading configuration MUST NOT unset or change environment variables that do not belong to Eselsohr.

#### Scenario: Unrelated variables
- **WHEN** the application starts with unrelated environment variables set
- **THEN** those variables remain unchanged in its process

### Requirement: Existing data directories
`ESELSOHR_DATA_DIR` SHALL name an existing directory that contains an existing `collections` directory, where collection files are stored.
The commands MUST NOT create either directory, and a path naming a file instead of a directory MUST be rejected without rewriting it.

#### Scenario: Data directory
- **WHEN** the configured data directory does not exist or names a regular file
- **THEN** the command does not start

#### Scenario: Missing collections directory
- **WHEN** the configured data directory exists but contains no `collections` directory
- **THEN** the command does not start and creates no directory
