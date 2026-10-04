# Tasks

Slice 1 is expanded into bounded tasks; the later slices are outlined, and before `/opsx:apply` reaches one, expand it into bounded tasks as the project's task rules require.
The decisions they consume are settled in `design.md`.
Every task runs in normal Code mode in the main checkout, in order; none needs a separate branch or worktree, because each builds on the previous one and 1.1 changes the freeze file.
Each task ends with Diff checks and a report of the commands run, their results, and the unverified cases.

## 1. Validated configuration and local listener

Until task 3.2 connects the Store, every bearer route answers with the invalid-link page, so this slice serves only fixed pages.

- [ ] 1.1 Add the web dependencies and the component skeleton.
  - Goal: pin `wai`, `warp`, `http-types`, and `lucid` and create the private library `web` and the test suite `eselsohr-v2-web-test`; it comes first because every later task builds on them, and freeze changes must be serialized.
  - Scope: `build-depends` and components in `v2/eselsohr-v2.cabal`, a regenerated `cabal-v2.project.freeze`, a `web/` module with its Haddock header, and a `test-web/Main.hs` with one smoke test; the executable depends on `web`.
  - Non-goals: `crypton`, which task 2.1 adds; any route, page, or configuration code.
  - Likely files: `v2/eselsohr-v2.cabal`, `cabal-v2.project.freeze`, `v2/web/Eselsohr/Web/Server.hs`, `v2/test-web/Main.hs`.
  - Dependencies: Hackage access for the solver and downloads at the pinned index state; no host tool installation.
  - Completion checks: Build; Test, with the smoke test selected and passing; Offline; the freeze diff adds about the 40 packages of the earlier solve and changes no existing pin, with any difference recorded; `web` lists neither `sqlite-internal`, `direct-sqlite`, nor `sqlite-simple`; Quality; HLS.
  - Security constraints: no change to the index state or to existing pins, and no network access beyond Hackage.
- [ ] 1.2 Parse configuration values purely.
  - Goal: `Eselsohr.Config` turns the three variable values into a validated configuration or an error naming the variable; it precedes loading because loading only supplies the values.
  - Scope: the listener's `host:port` with a name, dotted IPv4, or bracketed IPv6 host and a port from 1 through 65535; the exact public origin with its loopback rule for `http`; required values and the listener default; error values that name the variable and the problem.
  - Non-goals: reading the environment, checking directories, or resolving names.
  - Likely files: `v2/web/Eselsohr/Config.hs`, `v2/test-web/ConfigTests.hs`.
  - Dependencies: 1.1.
  - Completion checks: Build; Test with a pattern selecting the configuration tests, covering missing and malformed values, IPv6 brackets, ports 0 and 65536, uppercase letters, a trailing slash, a path, user information, query and fragment, and loopback and non-loopback hosts with each scheme; Quality; HLS.
  - Security constraints: parse totally without exceptions; error messages may repeat configuration text, because configuration holds no bearer.
- [ ] 1.3 Load configuration from the environment and the filesystem.
  - Goal: a loading step reads the variables through a supplied lookup function and checks that the data directory and its `collections` directory exist; it gives the server and `create-collection` one entry point.
  - Scope: the lookup function, production use of `lookupEnv`, and directory checks that create nothing and reject regular files.
  - Non-goals: opening collections, which `mkCollectionDirectory` does in later slices, and resolving the listener name, which task 1.6 leaves to Warp.
  - Likely files: `v2/web/Eselsohr/Config.hs`, `v2/test-web/ConfigTests.hs`.
  - Dependencies: 1.2.
  - Completion checks: Build; Test with temporary directories for a missing data directory, a regular file, and a missing `collections` directory, checking that no directory appears, and with the real environment compared before and after loading to show unrelated variables unchanged; Quality; HLS.
  - Security constraints: never unset or set environment variables; never rewrite a path between file and directory forms.
- [ ] 1.4 Add the route function and the fixed pages.
  - Goal: `Eselsohr.Web.Route` maps a method and path segments to the routes of the design, and `Eselsohr.Web.Html` renders the invalid-link page, the deletion notice, and the error pages; the server in 1.5 needs both.
  - Scope: a pure, total route function in which HEAD matches like GET, a trailing slash or extra segment matches nothing, and the bearer stays raw text; Lucid pages with fixed text and no request data.
  - Non-goals: parsing bearers, which needs `store-runtime` in task 2.2; the bearer page; reading query parameters.
  - Likely files: `v2/web/Eselsohr/Web/Route.hs`, `v2/web/Eselsohr/Web/Html.hs`, `v2/test-web/RouteTests.hs`.
  - Dependencies: 1.1.
  - Completion checks: Build; Test with a pattern selecting the route and page tests, including a property that generated paths outside the route table match nothing and that every route accepts only its listed methods, and checks that no page contains a script element; Quality; HLS.
  - Security constraints: no page renders request data; the route table admits only bearer routes and the deletion notice.
- [ ] 1.5 Serve the fixed pages with the response headers and Warp settings.
  - Goal: `Eselsohr.Web.Server` builds the WAI application and the Warp settings, so that every response carries the headers of the design; the `serve` command in 1.6 only starts it.
  - Scope: a middleware adding no-store, no-referrer, nosniff, `X-Robots-Tag: noindex, nofollow`, and the CSP; HEAD answers without a body; a POST whose `Content-Length` exceeds 160 KiB answers 413 before routing; a limited body reader for later routes; Warp settings for the listener, an exception-response hook rendering the error pages with the same headers, and an exception logger writing a fixed category per exception to a supplied sink.
  - Non-goals: bearer pages, form decoding, and the Store.
  - Likely files: `v2/web/Eselsohr/Web/Server.hs`, `v2/test-web/ServerTests.hs`, `v2/test-web/RawHttp.hs`.
  - Dependencies: 1.4.
  - Completion checks: Build; Test with a pattern selecting the server tests, running under `testWithApplicationSettings` with the raw-socket helper and checking every header on the deletion notice, the invalid-link page, a 413 response, a 400 response to a malformed request line, and a 500 response from an application that throws, HEAD without a body, the limited reader on chunked bodies below, at, and above the limit, and that captured operator messages never contain a bearer-shaped path sent in a malformed request line; Quality; HLS.
  - Security constraints: no response sets a cookie; no log line contains a request line, path, or header; nothing trusts Host or forwarded headers.
- [ ] 1.6 Add the `serve` command.
  - Goal: the executable accepts `serve` and `--version`, loads the configuration, and starts the listener; this completes slice 1 as a running server.
  - Scope: argument matching in `Main`; configuration errors on standard error with a failing exit status; one startup message naming the listener and the public origin.
  - Non-goals: `create-collection`, which task 2.1 adds; signal handling or graceful shutdown beyond Warp's defaults.
  - Likely files: `v2/app/Main.hs`, `v2/web/Eselsohr/Web/Server.hs`.
  - Dependencies: 1.3 and 1.5.
  - Completion checks: Build; Test; Version; a manual run with a temporary data directory and `curl -sI` against the deletion notice and an invalid link, recording the headers, or recording that the sandbox forbids local binding; a manual run with an unresolvable listener name and with a malformed origin, recording the failure messages; Quality; HLS.
  - Security constraints: the startup message contains no bearer, and a configuration error exits before the listener binds.

## 2. Operator collection creation and the web boundary

- [ ] 2.1 Add the `create-collection` command that creates a collection file under `<data>/collections/` with its identity, schema, and root grant, generates the identifier and owner secret with `crypton`, stores only the hash, and prints `<public origin>/b/<bearer>` once; verified by Test against real temporary directories, inspection of the stored data for reusable secrets, and Version.
- [ ] 2.2 Keep `sqlite-internal`, `direct-sqlite`, `sqlite-simple`, and the creation code out of the web component, with `store-runtime` providing the bearer and directory types the entry step needs, and handlers without `IOE`; verified by review of the component's `build-depends` and handler signatures, a property test that requests without a well-formed bearer receive the invalid-link response, and HTTP tests showing that no request creates a file.

## 3. Own-grant description, bearer page, and manual add

- [ ] 3.1 Add the describe-own-grant policy operation, its protected read with a sealed request, and the read-only grant interface with its runner; verified by policy tests including the monotonicity properties, SQLite tests that the result has permissions, scope, and expiration but no petname, depth, or ancestors, and denial of unknown, expired, and revoked bearers.
- [ ] 3.2 Add the bearer page as a pure function of the grant description and the articles of its scope, with escaped rendering; verified by unit tests over the page table of the design and HTTP tests with whole-collection, single-article, add-only, and delete-only bearers, two collection files with repeated article IDs, and malformed, oversized, unknown, expired, and revoked bearers that create no files and receive the same response.
- [ ] 3.3 Add the UNIQUE index on `article(url)` to schema version 1 and the protected add operation with its sealed request, inserting with `ON CONFLICT(url) DO NOTHING`, and the add form with the `added` notice; verified by schema tests of the index, SQLite tests of authorization inside the write transaction, revision zero, a repeated URL leaving one unchanged article with the same result, and rollback, and HTTP tests that receipts disclose no titles, counts, details, or read authority and are identical for new and stored URLs.

## 4. Title and read-state edits

- [ ] 4.1 Add the title edit forms over the existing protected title change, with explicit POST routes and server-generated redirects; verified by HTTP tests of missing Read, stale forms, matching no-ops, `_method` and `goto` fields, and cross-object attempts.
- [ ] 4.2 Add the protected read-state operation with its sealed request and forms; verified by SQLite tests of Read plus ChangeReadState, revision comparison and increment, exhaustion, and rollback after an update, and by the same HTTP cases as task 4.1.

## 5. Article deletion

- [ ] 5.1 Add the protected delete operation with its sealed request, no revision, and the same result for a missing article, an explicit POST route, and the redirect to the bearer page or, when the presented grant was scoped to the deleted article, to the deletion notice; verified by SQLite tests of DeleteArticles inside the write transaction, the cascade to scoped grants, and rollback, and HTTP tests of deletion without Read, missing DeleteArticles, out-of-scope and cross-collection attempts, already deleted articles, `_method` and `goto` fields, and responses that disclose no article contents.

## 6. Response and leakage tests

- [ ] 6.1 Test that bearer URLs stay out of logs and exception messages on every error path, including malformed request lines, that absolute links use the configured public origin despite spoofed Host and forwarded headers, that form POSTs with `Origin: null` succeed, and that no response sets a cookie; verified by Test with captured logs.

## 7. Review

- [ ] 7.1 Review a real owner, manual-add, edit, and delete flow end to end with HTML forms and JavaScript disabled; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
- [ ] 7.2 When syncing the specs, update the Purpose sections of `application-store` and `protected-article-operations`, which name only reads and title changes, and the M6 status in `AGENTS.md`.
