# Tasks

Slices 1 and 2 are expanded into bounded tasks; the later slices are outlined, and before `/opsx:apply` reaches one, expand it into bounded tasks as the project's task rules require.
The decisions they consume are settled in `design.md`.
Every task runs in normal Code mode in the main checkout, in order; none needs a separate branch or worktree, because each builds on the previous one and 1.1 changes the freeze file.
Each task ends with Diff checks and a report of the commands run, their results, and the unverified cases.

## 1. Validated configuration and local listener

Until task 3.2 connects the Store, every bearer route answers with the invalid-link page, so this slice serves only fixed pages.

- [x] 1.1 Add the web dependencies and the component skeleton.
  - Goal: pin `wai`, `warp`, `http-types`, and `lucid` and create the private library `web` and the test suite `eselsohr-v2-web-test`; it comes first because every later task builds on them, and freeze changes must be serialized.
  - Scope: `build-depends` and components in `v2/eselsohr-v2.cabal`, a regenerated `cabal-v2.project.freeze`, a `web/` module with its Haddock header, and a `test-web/Main.hs` with one smoke test; the executable depends on `web`.
  - Non-goals: `crypton`, which task 2.1 adds; any route, page, or configuration code.
  - Likely files: `v2/eselsohr-v2.cabal`, `cabal-v2.project.freeze`, `v2/web/Eselsohr/Web/Server.hs`, `v2/test-web/Main.hs`.
  - Dependencies: Hackage access for the solver and downloads at the pinned index state; no host tool installation.
  - Completion checks: Build; Test, with the smoke test selected and passing; Offline; the freeze diff adds about the 40 packages of the earlier solve and changes no existing pin, with any difference recorded; `web` lists neither `sqlite-internal`, `direct-sqlite`, nor `sqlite-simple`; Quality; HLS.
  - Security constraints: no change to the index state or to existing pins, and no network access beyond Hackage.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: the freeze diff adds exactly the 40 packages of the earlier solve and removes or changes no pin; Build passed; Test passed 1, 184, and 135 tests, including the selected smoke test; Build and Test with `--offline` passed, and a repeated offline `cabal freeze` was byte-identical; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 34 files after `hie.yaml` mapped `v2/web` and `v2/test-web` to their components; `git diff --check` passed.
  - Unverified: x86-64 Linux; the skeleton module holds the fixed security header list that task 1.5 applies.
- [x] 1.2 Parse configuration values purely.
  - Goal: `Eselsohr.Config` turns the three variable values into a validated configuration or an error naming the variable; it precedes loading because loading only supplies the values.
  - Scope: the listener's `host:port` with a name, dotted IPv4, or bracketed IPv6 host and a port from 1 through 65535; the exact public origin with its loopback rule for `http`; required values and the listener default; error values that name the variable and the problem.
  - Non-goals: reading the environment, checking directories, or resolving names.
  - Likely files: `v2/web/Eselsohr/Config.hs`, `v2/test-web/ConfigTests.hs`.
  - Dependencies: 1.1.
  - Completion checks: Build; Test with a pattern selecting the configuration tests, covering missing and malformed values, IPv6 brackets, ports 0 and 65536, uppercase letters, a trailing slash, a path, user information, query and fragment, and loopback and non-loopback hosts with each scheme; Quality; HLS.
  - Security constraints: parse totally without exceptions; error messages may repeat configuration text, because configuration holds no bearer.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test with `-p Configuration` selected and passed 13 tests, and the full Test passed 135, 14, and 184 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 36 files; `git diff --check` passed.
  - Unverified: x86-64 Linux; error messages repeat no configured text, which the design allowed but did not require; loopback detection accepts only the spellings `localhost`, `127.` addresses, and `[::1]`, so other spellings of the IPv6 loopback count as non-loopback.
- [x] 1.3 Load configuration from the environment and the filesystem.
  - Goal: a loading step reads the variables through a supplied lookup function and checks that the data directory and its `collections` directory exist; it gives the server and `create-collection` one entry point.
  - Scope: the lookup function, production use of `lookupEnv`, and directory checks that create nothing and reject regular files.
  - Non-goals: opening collections, which `mkCollectionDirectory` does in later slices, and resolving the listener name, which task 1.6 leaves to Warp.
  - Likely files: `v2/web/Eselsohr/Config.hs`, `v2/test-web/ConfigTests.hs`.
  - Dependencies: 1.2.
  - Completion checks: Build; Test with temporary directories for a missing data directory, a regular file, and a missing `collections` directory, checking that no directory appears, and with the real environment compared before and after loading to show unrelated variables unchanged; Quality; HLS.
  - Security constraints: never unset or set environment variables; never rewrite a path between file and directory forms.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test with `-p Loading` selected and passed 7 tests, and the full Test passed 135, 21, and 184 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 36 files; `git diff --check` passed.
  - Unverified: x86-64 Linux; symlinked data and collections directories are accepted because `doesDirectoryExist` follows links, and `mkCollectionDirectory` resolves them later; a directory removed after loading is detected only when a collection is opened.
- [x] 1.4 Add the route function and the fixed pages.
  - Goal: `Eselsohr.Web.Route` maps a method and path segments to the routes of the design, and `Eselsohr.Web.Html` renders the invalid-link page, the deletion notice, and the error pages; the server in 1.5 needs both.
  - Scope: a pure, total route function in which HEAD matches like GET, a trailing slash or extra segment matches nothing, and the bearer stays raw text; Lucid pages with fixed text and no request data.
  - Non-goals: parsing bearers, which needs `store-runtime` in task 2.2; the bearer page; reading query parameters.
  - Likely files: `v2/web/Eselsohr/Web/Route.hs`, `v2/web/Eselsohr/Web/Html.hs`, `v2/test-web/RouteTests.hs`.
  - Dependencies: 1.1.
  - Completion checks: Build; Test with a pattern selecting the route and page tests, including a property that generated paths outside the route table match nothing and that every route accepts only its listed methods, and checks that no page contains a script element; Quality; HLS.
  - Security constraints: no page renders request data; the route table admits only bearer routes and the deletion notice.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test with `-p Routes` selected and passed 8 tests, including the table property with at least 5% matching requests checked by `checkCoverage`, and the full Test passed 135, 29, and 184 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 39 files; `git diff --check` passed.
  - Unverified: x86-64 Linux; the pages are checked for scripts, styles, a doctype, and a charset, not rendered in a browser.
- [x] 1.5 Serve the fixed pages with the response headers and Warp settings.
  - Goal: `Eselsohr.Web.Server` builds the WAI application and the Warp settings, so that every response carries the headers of the design; the `serve` command in 1.6 only starts it.
  - Scope: a middleware adding no-store, no-referrer, nosniff, `X-Robots-Tag: noindex, nofollow`, and the CSP; HEAD answers without a body; a POST whose `Content-Length` exceeds 160 KiB answers 413 before routing; a limited body reader for later routes; Warp settings for the listener, an exception-response hook rendering the error pages with the same headers, and an exception logger writing a fixed category per exception to a supplied sink.
  - Non-goals: bearer pages, form decoding, and the Store.
  - Likely files: `v2/web/Eselsohr/Web/Server.hs`, `v2/test-web/ServerTests.hs`, `v2/test-web/RawHttp.hs`.
  - Dependencies: 1.4.
  - Completion checks: Build; Test with a pattern selecting the server tests, running under `testWithApplicationSettings` with the raw-socket helper and checking every header on the deletion notice, the invalid-link page, a 413 response, a 400 response to a malformed request line, and a 500 response from an application that throws, HEAD without a body, the limited reader on chunked bodies below, at, and above the limit, and that captured operator messages never contain a bearer-shaped path sent in a malformed request line; Quality; HLS.
  - Security constraints: no response sets a cookie; no log line contains a request line, path, or header; nothing trusts Host or forwarded headers.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0, with local binding allowed in the sandbox: Build passed; Test with `-p Server` selected and passed 10 tests, and the full Test passed 135, 38, and 184 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 41 files; `git diff --check` passed.
  - Deviation from the listed checks: `warp-3.4.15` closes the connection without any reply when it cannot split the request line (`BadFirstLine`), so no 400 exists for that case; the test checks that such a line with a bearer-shaped path gets no reply and no log, and a 400 with every header is checked for an oversized header block (`OverLargeHeader`), which Warp answers through the exception-response hook.
  - Unverified: x86-64 Linux; the declared-length check at exactly the limit is tested by calling the application directly, because Warp drops a connection whose large body the application leaves unread; Warp's own 413 and 431 cases (`PayloadTooLarge`, `RequestHeaderFieldsTooLarge`) are mapped but not triggered by any test.
- [x] 1.6 Add the `serve` command.
  - Goal: the executable accepts `serve` and `--version`, loads the configuration, and starts the listener; this completes slice 1 as a running server.
  - Scope: argument matching in `Main`; configuration errors on standard error with a failing exit status; one startup message naming the listener and the public origin.
  - Non-goals: `create-collection`, which task 2.1 adds; signal handling or graceful shutdown beyond Warp's defaults.
  - Likely files: `v2/app/Main.hs`, `v2/web/Eselsohr/Web/Server.hs`.
  - Dependencies: 1.3 and 1.5.
  - Completion checks: Build; Test; Version; a manual run with a temporary data directory and `curl -sI` against the deletion notice and an invalid link, recording the headers, or recording that the sandbox forbids local binding; a manual run with an unresolvable listener name and with a malformed origin, recording the failure messages; Quality; HLS.
  - Security constraints: the startup message contains no bearer, and a configuration error exits before the listener binds.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0, with local binding allowed in the sandbox: Build passed; Test passed 135, 184, and 38 tests; Version printed `eselsohr-v2 0.1.0.0`; a manual `serve` with a temporary data directory answered `curl -sI` with 200 for `/deleted` and 404 for a bearer-shaped `/b/` path, both with every security header, wrote only the startup message to standard error, and created no collection file; manual runs with an unresolvable listener name, a trailing-slash origin, a plain-HTTP public origin, and a data directory without `collections` each printed one message naming the problem and exited with status 1; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 41 files; `git diff --check` passed.
  - The executable is built with `-threaded`, because Warp's timeout manager raises an `ErrorCall` on every request under the non-threaded runtime; the HTTP tests did not catch this, because the test suites already used `-threaded`. Adding the flag did not relink the existing binary, so the executable's build directory under `dist-newstyle` had to be removed once.
  - Unverified: x86-64 Linux; an IPv6 listener and a listener name that resolves to several addresses; behaviour on SIGTERM beyond Warp's defaults.

## 2. Operator collection creation and the web boundary

- [x] 2.1 Add the bearer and schema building blocks for creation.
  - Goal: the bearer module builds a bearer from a collection identifier and a 32-byte secret, and the schema module offers its installation as a step without its own transaction; creation in 2.2 needs both, and they change existing modules, so they come first.
  - Scope: a bearer constructor that shares the hashing and rendering of `parseBearer`, and the stored hash it exposes to `sqlite-internal`; `initializeSchema` rewritten over the new installation step with unchanged behavior.
  - Non-goals: creating files, randomness, or root grants; any change to bearer parsing or lookup.
  - Likely files: `v2/persistence/Eselsohr/Store/SQLite/Bearer.hs`, `v2/persistence/Eselsohr/Store/SQLite/Schema.hs`, `v2/test-sqlite/BearerTests.hs`, `v2/test-sqlite/SchemaTests.hs`.
  - Dependencies: none in this slice.
  - Completion checks: Build; Test, with the existing schema tests unchanged and passing and a round-trip test that a constructed bearer renders to text that `parseBearer` accepts with the same hash, and that a secret of another length is rejected; Quality; HLS.
  - Security constraints: the constructor accepts exactly 32 secret bytes; no new export reveals a secret from a hash; the installation step refuses to run outside a transaction.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test with a pattern selecting the new cases passed 4 tests, including 500 generated round trips and the golden token and digest built from parts, and the full Test passed 135, 38, and 188 tests with every existing schema test unchanged; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 41 files; `git diff --check` passed.
  - Unverified: x86-64 Linux.
- [x] 2.2 Add the creation module.
  - Goal: `Eselsohr.Store.SQLite.Create` creates a collection with its identity, schema, and root grant and returns the owner bearer; the command in 2.3 only calls it.
  - Scope: random collection identifier, root grant identifier, and secret from `crypton`; exclusive creation of `<collection-id>.sqlite` with mode 0600; one immediate transaction for schema, identity, and a root grant with all eight permissions, whole-collection scope, no expiration, depth zero, and no petname; deletion of the file after any failure; a variant taking explicit identifiers and secret for tests; `crypton` added to `sqlite-internal`.
  - Non-goals: the command, configuration, printing, and the web component.
  - Likely files: `v2/persistence/Eselsohr/Store/SQLite/Create.hs`, `v2/eselsohr-v2.cabal`, `v2/test-sqlite/CreateTests.hs`, `v2/test-sqlite/Main.hs`.
  - Dependencies: 2.1.
  - Completion checks: Build; the freeze file is unchanged, because `crypton` is already pinned; Test with a pattern selecting the creation tests, on real temporary directories, checking the file mode, the identity and schema version, exactly one root grant with all eight permissions and no expiration, that the database bytes contain neither the raw secret nor its encoded form, that the returned bearer reads an empty collection through the production runners, that two creations differ in identifiers and secrets, that an existing file at the path is left unchanged, and that a failure inside the transaction leaves no file; Quality; HLS.
  - Security constraints: never follow or replace an existing path; store only the hash; error values carry no secret or path.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; the freeze file was unchanged, and an offline `cabal freeze` reproduced it byte for byte; Test with `-p "Collection creation"` selected and passed 7 tests, and the full Test passed 135, 38, and 195 tests; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 43 files; `git diff --check` passed.
  - Supporting changes: `Codec` exports `fromCapabilityBytes`, `Open` exports `collectionPath`, and the SQLite suite's entry module hides the latter so its own path oracle stays independent.
  - Unverified: x86-64 Linux; cancellation inside the creation transaction, whose cleanup relies on `onException` but is not exercised; a crash mid-transaction, which can leave the file and a hot journal; a failure to remove the file, which is ignored and keeps the original error; whether each invalid root grant in the failure test is rejected by the codec or by a SQL constraint, both of which happen inside the transaction.
- [x] 2.3 Add the `create-collection` command.
  - Goal: the executable creates a collection from the configuration and prints the owner URL once; this gives an operator the first usable link.
  - Scope: argument matching for `create-collection`; loading the configuration; resolving the collections directory; calling `createCollection`; printing `<public origin>/b/<bearer>` on standard output with text saying that it grants full access and cannot be shown again; errors on standard error with a failing exit status; the executable depends on `sqlite-internal`.
  - Non-goals: any web route, other management commands, or recovering a lost URL.
  - Likely files: `v2/app/Main.hs`, `v2/eselsohr-v2.cabal`.
  - Dependencies: 1.3 and 2.2.
  - Completion checks: Build; Test; Version; a manual run twice against a temporary data directory, recording that each prints one URL with the configured origin and that two files with mode 0600 appear; a manual run with a missing `collections` directory, recording the message and that nothing was created; Quality; HLS.
  - Security constraints: the URL appears once on standard output and nowhere else, including standard error and any log.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0: Build passed; Test passed 135, 38, and 195 tests; Version printed `eselsohr-v2 0.1.0.0`; two manual runs against a temporary data directory each printed the explanation and one `https://example.org/b/` URL on standard output, wrote nothing to standard error, and exited with status 0, leaving two different files with mode `-rw-------`; a manual run with a data directory lacking `collections` printed the configuration message, exited with status 1, and created nothing; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 43 files; `git diff --check` passed.
  - Unverified: x86-64 Linux; the `CollectionExists`, `CreateUnavailable`, and `CreateFault` messages, which the creation tests reach through the module but no manual run printed.
- [x] 2.4 Connect the web component to the runtime boundary.
  - Goal: the web component turns presented bearer text into a binding only through `store-runtime`, so that slice 3 can serve bearer pages without the web component reaching storage directly.
  - Scope: `Eselsohr.Store.Runtime` exports an opaque collections directory, its resolver, and `bindPresented`, which returns a binding with the host clock or nothing for text that is not a well-formed bearer; the web component depends on `store-runtime` and not on `sqlite-internal`, `direct-sqlite`, or `sqlite-simple`; `serve` resolves the collections directory at startup and stops when it cannot; bearer routes call `bindPresented` and still answer with the invalid-link page.
  - Non-goals: Store calls, bearer pages, and handlers, which arrive in slice 3.
  - Likely files: `v2/runtime/Eselsohr/Store/Runtime.hs`, `v2/web/Eselsohr/Web/Server.hs`, `v2/app/Main.hs`, `v2/eselsohr-v2.cabal`, `v2/test-web/ServerTests.hs`, `v2/test-sqlite/RuntimeTests.hs`.
  - Dependencies: 1.6 and 2.2.
  - Completion checks: Build; Test with a property that bearer routes with generated malformed bearer segments receive the invalid-link response and create no file in a real collections directory, and a runtime test that `bindPresented` accepts a created owner bearer and rejects malformed and oversized text; review of the web component's `build-depends` and imports, recording that no web module imports a `sqlite-internal` module; Quality; HLS.
  - Security constraints: the web component never names a collection file or opens a connection; a malformed bearer is rejected before any file is opened.
  - Verified on Apple Silicon macOS with GHC 9.10.3 and Cabal 3.16.1.0, with local binding allowed in the sandbox: Build passed; the freeze file was unchanged and reproduced by an offline `cabal freeze`; Test with `-p "Bearer boundary"` selected and passed 2 web tests, including 100 generated texts, and `-p "Presented bearers"` selected and passed 3 runtime tests, and the full Test passed 135, 198, and 40 tests; `rg` found no `Eselsohr.Store.SQLite` or `Database.SQLite` import in `v2/web`, whose only storage import is `Eselsohr.Store.Runtime`, and its `build-depends` list neither `sqlite-internal`, `direct-sqlite`, nor `sqlite-simple`; a manual `serve` after `create-collection` answered 404 for the owner URL and a malformed bearer and added no file; fourmolu 0.20.1.0 check and HLint 3.10 reported no changes or hints; HLS loaded all 43 files; `git diff --check` passed.
  - The re-exported `CollectionDirectory` of `store-runtime` serves as the opaque collections directory; the executable now depends on `store-runtime` too and resolves the directory through it for both commands.
  - Unverified: x86-64 Linux; because bearer routes make no Store call yet, the tests cannot show that a well-formed bearer is bound before a file is opened, which slice 3 exercises.

## 3. Own-grant description, bearer page, and manual add

- [ ] 3.1 Add the describe-own-grant policy operation, its protected read with a sealed request, and the read-only grant interface with its runner; verified by policy tests including the monotonicity properties, SQLite tests that the result has permissions, scope, and expiration but no petname, depth, or ancestors, and denial of unknown, expired, and revoked bearers.
- [ ] 3.2 Add the bearer page as a pure function of the grant description and the articles of its scope, with escaped rendering and handlers whose effects include no `IOE`; verified by review of the handler signatures, unit tests over the page table of the design, and HTTP tests with whole-collection, single-article, add-only, and delete-only bearers, two collection files with repeated article IDs, and malformed, oversized, unknown, expired, and revoked bearers that create no files and receive the same response.
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
