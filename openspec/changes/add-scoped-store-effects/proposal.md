# Proposal

## Why

The private SQLite adapter implements protected article reads and title changes, but only trusted code holding a database connection can call them.
Application code needs an interface that runs a named operation for one presented capability without receiving a connection, raw SQL, or the data directory.
The legacy App and TestApp monads gave ordinary page queries far broader storage and IO authority than their operations needed, and a renamed interface over the same broad handlers would not fix that.
This change covers milestone M5: run the same small application function with production and test handlers, without global Store authority or unrestricted IO in application logic.

## What Changes

- Define the smallest semantic Store interface for one named operation, starting from the existing protected read or title change.
- Add a SQLite handler bound to one presented bearer and its collection, and a runtime step that alone turns a parsed collection locator into an open connection.
- Add a deterministic test handler that replaces external behavior without a second application hierarchy.
- Run one common application function under both handlers, with tests of its authority and of the interface's exports.
- Report expected failures as small result types and keep storage faults, unexpected exceptions, and cancellation distinct from them.

## Capabilities

### New Capabilities

- `application-store`: the application-facing protected Store interface, bound to one presented capability and collection.

### Modified Capabilities

None.

## Non-goals

- Recreating App/TestApp, a universal environment, or effects for every helper, clock read, random value, and log message.
- A generic authorized-action framework, dispatcher, or type-level registry of operations, effects, or permissions.
- A full CRUD API, further protected mutations, issuance, revocation, or collection deletion.
- The web application, the operator CLI, PublicWeb, connection pooling, or migrations.

## Dependencies and approval gates

Depends on the protected article operations of `add-protected-sqlite-storage` (M4), specified in [protected-article-operations](../../specs/protected-article-operations/spec.md), [bearer-credentials](../../specs/bearer-credentials/spec.md), and [collection-storage](../../specs/collection-storage/spec.md), not on the whole web application.
Consumes D3 by exposing its existing private sealed requests through a bound handler, and the agreed use of `effectful`.
The exact effect interfaces remain undecided; a focused Plan comparison settles them before implementation, as the first task group records.
D5 is not consumed.

## Impact

- New application-facing Store and runtime modules in `v2/`, and a SQLite handler using the private `sqlite-internal` component.
- A deterministic test handler and tests for the common application function, handler scope, exports, and cleanup.
- New dependencies on `effectful-core`, pinned through a deliberate freeze update.
