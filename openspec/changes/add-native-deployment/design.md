# Design

## Context

The legacy [README](../../../README.md) says Eselsohr is distributed as a single binary without other dependencies, yet it also requires a static resources directory.
The legacy [Dockerfile](../../../Dockerfile) copies shared libraries and static assets next to the executable.
The README's deployment path `dist` also differs from the actual [contrib](../../../contrib/systemd/eselsohr.service) directory.
Preserve the README's understated writing style, not these stale assertions.

The legacy systemd unit shows useful security intent: a private state directory, a restrictive umask, and privilege restrictions such as a read-only system, no new privileges, and an empty capability bounding set.
Its compatibility with the replacement still needs testing.
The legacy [server](../../../src/Lib/Ui/Server.hs) trusts a forwarded client-IP header without a configured trusted-proxy boundary, and HTTPS and HSTS behavior needs an explicit proxy contract; see [AddHsts](../../../src/Network/Wai/Middleware/AddHsts.hs).
[Config](../../../src/Config.hs) silently falls back for several malformed values, clears the entire process environment, and confuses file and directory paths.

[flake.nix](../../../flake.nix) has only x86_64 Linux and Darwin outputs, an implicit nixpkgs input, global `allowBroken`, a project jailbreak, and a base32 override combining jailbreak, disabled tests, and unmarking broken.
It includes flake-compat and [default.nix](../../../default.nix) for older Nix entry points.
No Nix evaluation was performed, so the flake is not validated reproducibility.

Under D4 the replacement compiles SQLite into the executable; Linux `ldd` and macOS `otool -L` of the SQLite test executable list no SQLite shared library.
Collections live in the data directory as `data/collections/<collection-id>.sqlite`, and initial deployment supports one application process per data directory.

## Goals / Non-Goals

**Goals:**

- A small deployment whose writable state, logging, restart, shutdown, and restore behavior are checked, not assumed.
- Documentation that names the actual runtime requirements.

**Non-Goals:**

- Choosing configuration names or formats; `add-manual-bookmark-web` proposes `ESELSOHR_DATA_DIR`, `ESELSOHR_LISTEN`, `ESELSOHR_PUBLIC_ORIGIN`, and `ESELSOHR_LOG_LEVEL`.
- Application-level bearer redaction, which the web interface owns; this change applies the same contract to the proxy.

## Decisions

### One executable, private state, and a reverse proxy

Prefer one executable, one configuration mechanism, a private data directory, and a reverse proxy where appropriate.
Deployment starts with one non-root process, a loopback listener behind a configured TLS proxy, private writable state, and bounded graceful shutdown.
Construct absolute links from the configured canonical public origin, not from untrusted Host or forwarded headers.
HTTPS is mandatory for deployed bearer URLs; localhost development is distinct.

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Deployment | SIMPLIFY | Retain non-root/private-state intent, validate one executable and explicit operational dependencies. |

### Actual runtime requirements

State the actual runtime libraries and CA-certificate requirements instead of claiming a dependency-free binary.
The restricted fetcher's TLS verification needs trusted CA certificates.
Minimizing assets matters here: embedding small CSS may remove the runtime static directory, with a matching CSP policy.

### Service definition and filesystem permissions

Keep the legacy unit's intent, a private state directory, restrictive umask, and privilege restrictions, and test each setting against the replacement rather than copying it.
The data directory and its writers are trusted; the application is not a sandbox against hostile filesystem replacement.

### Proxy logs

Do not put bearer URLs in logs, analytics, exception messages, or outbound metadata requests.
Proxy access logs need the same redaction contract.

### Graceful shutdown

Shutdown is bounded and lets asynchronous cancellation unwind bracketed resources, so transactions roll back and connections close.

### Optional container and Nix

Optional Nix, container, release, and wider compatibility tasks follow the compiler, SQLite, and deployment choices they consume, and are expanded only when needed.
An optional container packages the same executable; it does not define the canonical development path.
A small optional Nix flake should build the same package and support relevant Linux and macOS architectures without blanket broken-package overrides; no heavy framework is justified yet.

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| DX | Native Cabal/GHCup; optional small Nix flake; Nix-first/haskell.nix | Native path is canonical. Optional Nix must exercise the same package, not maintain a second architecture or mask broken bounds. |

Remove the legacy flake's workarounds only after demonstrating a small replacement.

## Risks / Trade-offs

- [Documentation promises a static binary that the build does not produce] → List the actual dynamic libraries and CA-certificate requirements from the shipped executable.
- [Proxy access logs record bearer URLs] → Apply the application's redaction contract to the documented proxy configuration and check the logs.
- [Legacy systemd hardening may not suit the replacement] → Test each setting against the replacement before adopting it.
- [Optional Nix masks broken bounds] → Build the same package without blanket overrides, and keep the native path tested independently.
- [Restoring a backup restores revoked authority] → The restore guidance states it, following `add-offline-maintenance`.
- [A rehearsal expands into production changes] → A tightly specified authorized rehearsal may use `/goal`, but cannot silently expand into external production changes.

## Open Questions

- Exact configuration formats and defaults, settled with `add-manual-bookmark-web`.
- Which reverse proxy the documentation covers, and the bound for graceful shutdown.
- Whether embedding CSS removes the runtime static directory.
- Which Linux and macOS architectures the optional flake supports, and which platforms CI can affordably prove.
