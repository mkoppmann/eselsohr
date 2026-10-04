# Tasks

These are outlined slices; before /opsx:apply, expand the next slice into bounded tasks naming scope, non-goals, likely files, completion checks, and security constraints, and settle the decisions it consumes.
Use a planning pass where an operator choice remains.

## 1. Configuration contract

- [ ] 1.1 Settle the operator-facing configuration contract with one mechanism, building on the names proposed by `add-manual-bookmark-web`; verified by tests that malformed values fail at startup and by the documented example starting the application.

## 2. Service definition and filesystem permissions

- [ ] 2.1 Add a systemd service definition with a non-root user, private state directory, restrictive umask, and tested privilege restrictions; verified by actual checks that the process can write only its state directory and that other users cannot read it.

## 3. Reverse-proxy TLS and log redaction

- [ ] 3.1 Document a loopback listener behind a TLS reverse proxy with the configured public origin and bearer redaction in access logs; verified by requests through the proxy whose access logs contain no bearer.

## 4. Graceful shutdown

- [ ] 4.1 Add bounded graceful shutdown that cancels in-progress work and closes database handles; verified by stopping the service while busy, checking that it exits within the bound without partial transactions, and restarting it.

## 5. Install, upgrade, and restore rehearsal

- [ ] 5.1 Document install, upgrade, and restore with the actual runtime libraries and CA-certificate requirements; verified by rehearsing each procedure on a test deployment and comparing the documented libraries with the executable's dynamic dependencies.

## 6. Optional packaging

- [ ] 6.1 When needed, add a container that packages the same executable; verified by running the image with the native configuration.
- [ ] 6.2 When needed, replace the legacy flake with a small one that builds the same package without blanket broken-package overrides; verified by a Nix build on each supported architecture, alongside the unchanged native Build and Test.

## 7. Exit review

- [ ] 7.1 Review actual checks for writable-state boundaries, no bearer logging, restart, busy shutdown, and backup restoration; verified by the recorded rehearsal results, Specs, and Diff checks.
