## Purpose

Defines how Eselsohr runs in a deployment: one unprivileged process behind a TLS reverse proxy with private state, honest runtime requirements, and optional packaging that uses the same executable.

## ADDED Requirements

### Requirement: One non-root process per data directory
A deployment SHALL run the application as one process under a non-root user, and MUST NOT run more than one application process on the same data directory.

#### Scenario: Service starts
- **WHEN** the operator starts the documented service
- **THEN** exactly one application process runs for the data directory, under an unprivileged user

### Requirement: Loopback listener behind a TLS proxy
The application SHALL listen on a loopback address in a deployment, and a configured reverse proxy MUST terminate TLS for public clients.

#### Scenario: Remote client connects
- **WHEN** a remote client connects to the deployment
- **THEN** it reaches the application only through the TLS reverse proxy, and the application's listener is not reachable from other hosts

### Requirement: HTTPS for deployed bearer URLs
Deployed bearer URLs SHALL use HTTPS.
Plain HTTP MUST be limited to localhost development.

#### Scenario: Deployment issues a link
- **WHEN** a deployed instance shows a newly issued bearer URL
- **THEN** the URL uses the HTTPS public origin

### Requirement: Private writable state
The application's writable state SHALL be confined to one state directory that other users cannot read or write.
The application MUST NOT need write access elsewhere in the filesystem.

#### Scenario: Files created by the application
- **WHEN** the application creates or changes collection files
- **THEN** they reside in the state directory and are accessible only to the service user

#### Scenario: Write outside the state directory
- **WHEN** the deployed process attempts to write outside its state directory
- **THEN** the service configuration denies the write

### Requirement: Bounded graceful shutdown
On a stop request, the application SHALL stop accepting new requests and finish or cancel in-progress requests within a bounded time, closing its database handles before it exits.

#### Scenario: Shutdown while busy
- **WHEN** the service is stopped while requests are in progress
- **THEN** the process exits within the shutdown bound, and no transaction remains partially applied

#### Scenario: Restart
- **WHEN** the service restarts after a shutdown
- **THEN** it opens the existing collections and serves requests

### Requirement: One executable and one configuration mechanism
A deployment SHALL consist of one application executable configured through one configuration mechanism.

#### Scenario: Install the application
- **WHEN** the operator installs the application by following the documentation
- **THEN** they install one executable and supply its settings through a single documented mechanism

### Requirement: Documented runtime requirements
The deployment documentation SHALL state the actual runtime libraries and CA-certificate requirements of the executable.
It MUST NOT claim a dependency-free binary.

#### Scenario: Compare documentation with the executable
- **WHEN** the dynamic dependencies of the release executable are listed on a supported platform
- **THEN** every listed library and the CA-certificate requirement appear in the documentation

### Requirement: Proxy access log redaction
Reverse-proxy access logs SHALL follow the same bearer redaction contract as the application's logs and MUST NOT record bearer tokens.

#### Scenario: Request with a bearer URL
- **WHEN** a client requests a page through the documented proxy configuration with a bearer in its URL or form
- **THEN** the proxy access log contains no part of that bearer

### Requirement: Rehearsed install, upgrade, and restore
The documented install, upgrade, and restore procedures SHALL be rehearsed on a test deployment, and the documentation MUST describe the procedure that was rehearsed.

#### Scenario: Restore rehearsal
- **WHEN** the operator follows the documented restore procedure on a test deployment
- **THEN** the restored collection serves requests, and the guidance states that restoring can bring back revoked authority

### Requirement: Optional container
An optional container image SHALL package the same executable as the native build and MUST NOT define the canonical development path.

#### Scenario: Run the container
- **WHEN** the operator runs the optional container image
- **THEN** it runs the same executable with the same configuration mechanism as a native deployment

### Requirement: Optional Nix flake
An optional Nix flake SHALL build the same package as the native Cabal path and MUST NOT rely on blanket broken-package overrides.

#### Scenario: Build with Nix
- **WHEN** a contributor builds the package through the optional flake on a supported Linux or macOS architecture
- **THEN** it builds the same package without marking broken packages as usable globally
