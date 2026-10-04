# Proposal

## Why

The legacy README promises a single binary without other dependencies, yet the same instructions require a static directory and the Dockerfile copies shared libraries and static assets.
Bearer URLs carry all authority, so deployment details such as TLS, proxy logs, and file permissions are part of the security model.
This change covers milestone M11 and the optional packaging that follows it: validate a deliberately small deployment and recovery procedure.

## What Changes

- Run one non-root application process per data directory, listening on loopback behind a configured TLS reverse proxy, with a private writable state directory and bounded graceful shutdown.
- Keep one executable and one configuration mechanism.
- Document the actual runtime libraries and CA-certificate requirements instead of claiming a dependency-free binary.
- Require HTTPS for deployed bearer URLs and apply the bearer redaction contract to proxy access logs.
- Rehearse install, upgrade, and restore with the documented procedure.
- Optionally package the same executable in a container, and optionally build the same package with a small Nix flake without blanket broken-package overrides.

## Capabilities

### New Capabilities

- `deployment`: how the application runs in production, what it requires from its host and proxy, and how optional packaging relates to the native build.

### Modified Capabilities

None.

## Non-goals

- Kubernetes, a mandatory container or Nix setup, automatic publication, or pushing.
- A static or dependency-free binary as a goal in itself.
- External production changes beyond an authorized rehearsal.
- Defining configuration names and formats, which `add-manual-bookmark-web` proposes.

## Dependencies and approval gates

Depends on the runnable replacement from `add-manual-bookmark-web` (M6) and `add-scoped-store-effects` (M5), and on the maintenance behavior of `add-offline-maintenance` (M9).
Deployment configuration can be drafted before `cut-over-to-root-package` (M10); optional Nix, container, and release work follows the compiler, SQLite, and deployment choices it consumes.
This change consumes D4: the bundled SQLite engine removes the need for a separate SQLite library at deployment.
Operator choices that remain open are listed in design.md.

## Impact

- New deployment documentation, a service definition, and reverse-proxy guidance, replacing the stale README deployment section and the legacy [systemd unit](../../../contrib/systemd/eselsohr.service).
- Optional container and Nix files that replace the legacy [Dockerfile](../../../Dockerfile), [flake.nix](../../../flake.nix), and [default.nix](../../../default.nix) once a replacement is demonstrated.
- Application changes are limited to the operator-facing configuration contract and graceful shutdown.
