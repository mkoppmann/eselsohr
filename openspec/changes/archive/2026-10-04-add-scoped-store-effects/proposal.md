# Proposal

## Why

The private SQLite adapter implements protected article reads and title changes, but only trusted code holding a database connection can call them.
Application code needs an interface that runs these operations for one presented bearer without receiving a connection, raw SQL, the data directory, or a clock.
The legacy App and TestApp monads gave ordinary page queries far broader storage and IO authority than their operations needed, and a renamed interface over the same broad handlers would not fix that.
This change covers milestone M5: application functions read and change articles through effects whose handler is bound to one bearer, without global Store authority or unrestricted IO in application logic.

## What Changes

- Separate read and write effects for articles, so code given only the reader cannot change anything, with small result types that keep denials, expected edit failures, and storage faults apart.
- A binding of one presented bearer, the collections directory, and the trusted clock, constructed only by the runtime and tests; each operation opens the collection the bearer names, runs one transaction, and closes it.
- The named protected operations take the trusted clock as an argument instead of reading the host clock themselves.
- Opening reports a missing collection file as Missing, and the Store reports it as a denial identical to an unknown secret.
- Tests run application functions through the production SQLite handler with a test clock and a temporary collections directory, without a separate test handler.
- One application function that reads an article and changes its title, with authority and export tests.

## Capabilities

### New Capabilities

- `application-store`: the application-facing read and write interfaces, bound to one presented bearer and its collection.

### Modified Capabilities

- `collection-storage`: opening distinguishes a missing collection file (Missing) from other failures (Unavailable).
- `development-workflow`: `hie.yaml` maps every v2 library, including the new private runtime library.

## Non-goals

- Recreating App/TestApp, a universal environment, or effects for every helper, clock read, random value, and log message.
- A separate test handler; a scripted stub for forcing faults is left to the web tests of M6.
- A generic authorized-action framework, dispatcher, or type-level registry of operations, effects, or permissions.
- A full CRUD API, further protected mutations, issuance, revocation, or collection deletion.
- The web application, the operator CLI, PublicWeb, connection pooling, or migrations.

## Dependencies and approval gates

Depends on the protected article operations of `add-protected-sqlite-storage` (M4), specified in [protected-article-operations](../../../specs/protected-article-operations/spec.md), [bearer-credentials](../../../specs/bearer-credentials/spec.md), and [collection-storage](../../../specs/collection-storage/spec.md), not on the whole web application.
Consumes D3 by calling its existing private sealed requests from the handler, and the agreed use of `effectful`.
The effect interfaces, binding, and result types are settled in [design.md](design.md).
D5 is not consumed.

## Impact

- Small changes to the private `sqlite-internal` component: a clock argument for the named operations and the Missing opening result.
- A new application-facing module in the public `v2/` library with the read and write effects and their result types, and a private `store-runtime` library with the binding and its SQLite handlers.
- Tests for the common application function, binding scope, exports, and cleanup, using real temporary databases.
- A new dependency on `effectful-core`, pinned through a deliberate freeze update.
