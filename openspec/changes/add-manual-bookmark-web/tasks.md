# Tasks

These are outlined slices; before `/opsx:apply`, expand the next slice into bounded tasks as the project's task rules require.

## 1. Validated configuration and local listener

- [ ] 1.1 Settle the configuration names, formats, and defaults, then read them through one mechanism that fails on malformed explicit values, preserves unrelated environment variables, and keeps file and directory paths distinct; verified by Build, Test with malformed, missing, and unrelated-variable cases, Quality, and HLS.
- [ ] 1.2 Start a local listener with the response headers, escaping, and leakage rules from the first route; verified by HTTP tests on an ephemeral port with scoped server lifetimes checking every header on page and error responses.

## 2. Operator collection creation

- [ ] 2.1 Add the operator command that creates a collection file with its identity, schema, and root grant, generates the owner secret with a CSPRNG, stores only its hash, and prints the owner URL once; verified by Test against real temporary directories, inspection of the stored data for reusable secrets, and Version.
- [ ] 2.2 Confirm that no web route creates a collection; verified by HTTP tests showing that no request creates a file.

## 3. Protected list and manual add

- [ ] 3.1 Add the protected list page through the M5 Store interface with escaped rendering; verified by HTTP tests with whole-collection, single-article, and add-only bearers, two collection files with repeated article IDs, and malformed, oversized, and unknown bearers that create no files and receive the same response.
- [ ] 3.2 Add the protected add operation with its sealed request and the manual add form, storing at most one article per URL and returning an add-only receipt; verified by SQLite tests of authorization inside the write transaction, revision zero, a repeated URL leaving one unchanged article, and rollback, and HTTP tests that receipts disclose no titles, counts, details, or read authority and are identical for new and stored URLs.

## 4. Title and read-state edits

- [ ] 4.1 Add the title edit form over the existing protected title change, with explicit POST routes and server-generated redirects; verified by HTTP tests of missing Read, stale forms, matching no-ops, `_method` and `goto` fields, and cross-object attempts.
- [ ] 4.2 Add the protected read-state operation with its sealed request and form; verified by SQLite tests of Read plus ChangeReadState, revision comparison and increment, exhaustion, and rollback after an update, and by the same HTTP cases as task 4.1.

## 5. Article deletion

- [ ] 5.1 Add the protected delete operation with its sealed request, an explicit POST route, and a server-generated redirect; verified by SQLite tests of DeleteArticles inside the write transaction and rollback, and HTTP tests of deletion without Read, missing DeleteArticles, out-of-scope and cross-collection attempts, `_method` and `goto` fields, and responses that disclose no article contents.

## 6. Response and leakage tests

- [ ] 6.1 Test that bearer URLs stay out of logs and exception messages on every error path, that absolute links use the configured public origin despite spoofed Host and forwarded headers, and that no response sets a cookie; verified by Test with captured logs.

## 7. Review

- [ ] 7.1 Review a real owner, manual-add, edit, and delete flow end to end with HTML forms and JavaScript disabled; run Build, Test, Version, Offline, Quality, HLS, and Diff checks, and record unverified platforms and cases.
