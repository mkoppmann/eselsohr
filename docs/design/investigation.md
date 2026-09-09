# Existing-system investigation

This describes the existing implementation and the findings behind the [Eselsohr 2 design](eselsohr-2.md), not a complete security audit.
Findings are based on source inspection; no exploit was run.
The native build baseline is reported separately below.
The [roadmap](roadmap.md) distinguishes completed foundation tasks from pending redesign work.

Keep findings and references current as implementation changes.
Remove this document when it no longer describes the code; Git history preserves past states.

## Product and intentional experiments

The application stores article URLs, titles, creation times, and a read/unread state.
Users can list articles, edit titles, mark them read or unread, delete them, and share access to a collection or article.
Collection overview links manage everyday access links.
Capabilities can have a petname and expiration.
There are no user accounts.
HTML is rendered on the server using Servant and Lucid; forms work without JavaScript.
URL fetching extracts the first `h1`, not an article archive.
See [routes](../../src/Lib/Ui/Web/Route.hs), [commands](../../src/Lib/App/Command.hs), and [the scraper](../../src/Lib/Infra/Adapter/Scraper.hs).

The [README](../../README.md) explicitly treats capability-based security as an experiment.
Haskell's pure functions, explicit effects, separate collection resources, and authorization witnesses are deliberate teaching ideas.
Clean/Hexagonal/Three Layer Cake examples influenced the directory structure.
That organization does not establish the security claims.
In particular, a publicly accessible operation does not gain protection merely by being expressed as a capability, and a narrow port name does not restrict a broad implementation.

Preserve the small product, explicit sharing, simple HTML, meaningful domain values, and attention to operational hardening.
Do not preserve a technique merely because it was one of the experiments.

## Permissions and authorization

[Capability.hs](../../src/Lib/Domain/Capability.hs) defines `OverviewPerms`, `ArticlesPerms`, and `ArticlePerms` as records of `Maybe` singleton values.
They derive `Ord`.
`createSharedOverviewRef`, `createSharedArticlesRef`, and the article-to-article case of `createSharedArticleRef` use `ogPerms >= newPerms`.
This compares records lexicographically, not by permission inclusion.

For example, an article-list parent with Read and Share but no Add compares greater than a child without Read but with Add: the first field decides the comparison.
Giving up an earlier permission can therefore mask acquiring a later one.
This is a concrete defect in those attenuation helpers.
The list-to-article case instead checks individual Boolean pairs and does implement pointwise presence inclusion.
It must not be grouped with the faulty cases.

[Authorization.hs](../../src/Lib/Domain/Authorization.hs) hides the constructors of permission witnesses such as `ChangeTitlePerm`.
Some contain an article ID, which downstream functions extract rather than accepting a second ID.
This is a useful compile-time connection between an operation and its target.
The local alias `Authorized a` is only `Either AppErrorType a`; it is not a generic sealed authorized-action representation.

The witnesses do not prove possession of an authentic bearer token.
Their policy inputs (`Capability`, `ObjectReference`, permission records) have public constructors.
Witnesses do not bind a collection, expiration, ancestry, current database state, or transaction.
An article ID in a witness is useful, but it is not the complete authorization context.

[ArticleList.hs](../../src/Lib/Domain/ArticleList.hs) illustrates both sides: `changeArticleTitle` consumes the witness's ID, while `addArticle` ignores its permission value.
`fromMap` and `toMap` expose the underlying representation.
The [repository interface](../../src/Lib/Domain/Repo/ArticleList.hs) accepts a separate collection ID and arbitrary article-list transformations.
These APIs do not make bypassing policy impossible throughout the application.

The existing `canChangeArticleTitle` tests the write permission alone.
[EditArticle.query](../../src/Lib/Ui/Web/Page/EditArticle.hs) likewise checks that write permission and then loads the article title, without a separate Read check.
Eselsohr 2's agreed Read-plus-write rule is an explicit redesign requirement, not a guarantee established by the current policy or editing page.

`Command.createUnlockLink` constructs a capability with `defaultArticlesRef`.
Permission to create such links therefore carries indirect article authority.
That is not inherently contrary to object capabilities, but describing it as only administrative authority hides what the holder can obtain.

## Sharing, discovery, and token handling

The old model has `SharedRef` as a category, not an immutable parent relation.
There is no grant tree establishing descendant ownership, inherited expiration, or cascading revocation.
UI restrictions on resharing do not supply that model.

In [Page.Shared](../../src/Lib/Ui/Web/Page/Shared.hs), `getSharedLinks` filters capabilities by a caller-supplied category predicate.
In [ShareArticle.query](../../src/Lib/Ui/Web/Page/ShareArticle.hs), authorization to share one article is followed by `getSharedLinks colId Cap.isSharedArticleRef`.
The predicate does not restrict the results to that article or issuing capability.
[UnlockLink.fromDomain](../../src/Lib/Ui/Web/Page/ViewModel/UnlockLink.hs) reconstructs usable bearer links.
This is an authority-discovery concern, not merely a presentation defect: access to another grant can defeat attenuation or the intended effect of revoking the originally presented grant.

[Accesstoken.hs](../../src/Lib/Ui/Dto/Accesstoken.hs) encodes collection and capability UUIDs as CBOR and Base32.
There is no independent hash-only secret store.
Its `Show` instance emits the bearer representation.
`decodeFromBase32` checks the outer encoding, but `toReference` later uses partial `deserialise`.
A syntactically valid Base32 string is not necessarily a valid token payload.

`Page.Shared.lookupReferences` loads a collection's capabilities, resolves the supplied ID, and checks expiration.
`capStillValid` treats equality with the deadline as valid; the redesign proposes a strict `now < expiresAt` rule.
The check is separate from later reads and writes.
It does not make a previously issued witness fresh at mutation time.

## Effect boundaries and fetching

[App](../../src/Lib/Infra/Monad.hs) wraps `ReaderT AppEnv IO`, implements the port/repository classes, and derives `MonadIO` and `MonadUnliftIO`.
[TestApp](../../test/Test/Infra/Monad.hs) duplicates much of this structure.
Classes in [Port.hs](../../src/Lib/App/Port.hs) express useful intent, but the actual constraints matter more than the class names.

For example, `Page.Shared.WithQuery` includes `WithFile`, which includes the data-directory environment and `MonadUnliftIO`.
Ordinary page queries therefore have much broader storage/IO authority than their semantic operation requires.
Renaming these classes to effects without narrowing their handlers would not correct that boundary.

`Command.createArticle` calls the scraper before `canCreateArticles`.
An unauthorized add attempt can therefore reach the network-fetch path.
[Uri.hs](../../src/Lib/Domain/Uri.hs) checks schemes, ports, literal addresses, and the name `localhost`, but does not bind a DNS result to the actual socket.
It also exports its constructor and `unfilteredUri`; the [controller](../../src/Lib/Ui/Web/Controller/ArticleList.hs) uses unfiltered parsing in Dev and Test modes.
The validation even permits a missing scheme.

The production scraper calls Scalpel's ordinary `scrapeURL` with `MonadIO`.
The inspected code does not establish redirect revalidation, DNS-rebinding protection, public-address socket pinning, or application-specific response limits.
Literal-address validation tests cannot establish those properties.

## Persistence and concurrency

[File.hs](../../src/Lib/Infra/Persistence/File.hs) stores each collection as `.json.gz`, using Aeson, gzip, and a durable atomic whole-file write.
This is not SQLite.
Loading materializes the collection; malformed data can reach `error`.
The module exposes arbitrary collection lookup through the shared data path.

[Queue.hs](../../src/Lib/Infra/Persistence/Queue.hs) uses a `TQueue` of commands and per-collection worker queues.
`commit` waits on a `TMVar`; `storeUpdate` fills it only after the action succeeds.
Exceptions have no corresponding failure result in that handshake.
The [persistence server](../../src/Lib/Infra/Persistence/Server.hs) also runs expiration cleanup.
These workers solve real whole-file update coordination, but do not provide cross-process locking or authorization-and-write atomicity.

SQLite transactions can replace the write-serialization purpose.
A connection registry or shutdown counter could later need synchronization, but that is not a reason to carry forward the queue or require STM in the initial design.

## Domain, errors, web, and configuration

[Article](../../src/Lib/Domain/Article.hs) already has a small `Unread | Read` ADT and pure title/state transformations.
Keep those ideas.
[NonEmptyText](../../src/Lib/Domain/NonEmptyText.hs) hides its constructor and rejects empty text, though not whitespace-only text.
Its error argument couples validation to the general application error.
[Id](../../src/Lib/Domain/Id.hs) uses a phantom parameter to distinguish kinds of IDs in ordinary signatures; the exported constructor and phantom role are not an unforgeability boundary.
`Eq Article` and `Eq Capability` compare IDs only, which can hide field changes in tests that intend structural equality.

[Domain.Error](../../src/Lib/Domain/Error.hs) mixes expected validation and authorization failures, storage/internal failures, and redirects.
[Infra.Error](../../src/Lib/Infra/Error.hs) and `App` add exception-based transport.
The comments acknowledge redirects as a workaround, not domain failures.
Separate these concerns without introducing dozens of trivial errors.

The [server](../../src/Lib/Ui/Server.hs) sets no-store, no-referrer, nosniff, and a restrictive CSP.
Lucid provides escaped rendering.
These are worthwhile choices.
The same server applies POST method override and trusts a forwarded client-IP header without a configured trusted-proxy boundary.
Controllers use unconstrained form `goto` values for redirects.
HTTPS/HSTS behavior also needs an explicit proxy contract; see [AddHsts](../../src/Network/Wai/Middleware/AddHsts.hs).

[Config](../../src/Config.hs) silently falls back for several malformed values.
`clearEnv` unsets the entire process environment, not just Eselsohr variables.
`sanitizePath` adds a trailing directory separator and is also used for TLS certificate/key paths.
Replace this behavior with explicit validation and separate file/directory handling.

The README's single-binary/no-other-dependencies claim conflicts with its static directory instructions and the [Dockerfile](../../Dockerfile), which copies shared libraries and static assets.
Its deployment path `dist` also differs from [contrib](../../contrib/systemd/eselsohr.service).
Preserve the understated writing style, not these stale assertions.
The systemd unit's private state directory, restrictive umask, and privilege restrictions show useful security intent; their compatibility with a replacement still needs testing.

## Tests worth extracting, not porting wholesale

[Authorization tests](../../test/Test/Domain/Authorization.hs) contain 815 lines of mostly example combinations.
Preserve their behavioral questions, then use exhaustive permission-set checks and generated scope/ancestry cases.
Do not preserve a faulty attenuation comparison as expected behavior.

The fake [article repository](../../test/Test/Infra/Repo/ArticleList.hs), [capability repository](../../test/Test/Infra/Repo/CapabilityList.hs), and [collection repository](../../test/Test/Infra/Repo/Collection.hs) share one `IORef CollectionPm` and ignore collection IDs in key operations.
They cannot prove collection isolation.
Some property generators use real random IO rather than explicit shrinkable inputs; see [token properties](../../test/Test/Ui/Web/Dto/Accesstoken.hs).
ID-only equality also weakens naive persistence round-trip assertions.

[HTTP test setup](../../test/Test/Ui/Web/Controller/Shared.hs) brackets its data directory but starts asynchronous servers without retaining and cancelling their handles.
It uses fixed port 6980 and bypasses production URI validation.
The suite therefore does not establish the production fetcher's restrictions.
Replacement integration tests need scoped lifetimes, ephemeral ports where a real listener is necessary, multiple collection files, and actual handlers.

## Build, CI, and Nix baseline

[eselsohr.cabal](../../eselsohr.cabal) targets GHC 9.8 with `base ^>=4.19.1.0`.
[cabal.project](../../cabal.project) is an ordinary Cabal project; native Cabal does not intrinsically require Nix.
The available verified toolchain is GHC 9.10.3 (`base` 4.20.2.0), Cabal 3.16.1.0, Fourmolu 0.20.1.0, HLint 3.10, and HLS 2.14.0.0, including its GHC 9.10.3 executable.
Fourmolu and HLint are callable as `fourmolu-0.20.1.0` and `hlint-3.10`; their unversioned aliases are not currently set.
Verification covers the Linux seed, not a wider platform support matrix.

`cabal build --offline --dry-run --builddir=<temporary-build-dir> all` and `cabal test --offline --dry-run --builddir=<temporary-test-dir> all` both exit 1 with Cabal-7107: the package's base constraint excludes the installed 4.20.2.0.
Neither reaches compilation or test execution.
The intended GHC 9.8 build is unverified.
Legacy bounds, legacy dependencies, and global tool configuration were left unchanged.
The temporary-directory placeholders are deliberately not reusable machine paths.

The independent [replacement seed](../../v2/eselsohr-v2.cabal) builds and tests on Linux with GHC 9.10.3, `base` 4.20.2.0, and Cabal 3.16.1.0.
Its version command prints `eselsohr-v2 0.1.0.0`; missing or unsupported arguments exit 1 with usage on stderr.
The smoke test executes one explicit assertion and fails on a version mismatch.
The solved package plan contains the three replacement components and installed GHC packages, with no legacy `eselsohr` package.
See the [roadmap](roadmap.md#task-contract-and-verification-vocabulary) for the verified commands.
This is only a build seed; macOS verification remains pending.

The root [HLS configuration](../../hie.yaml) selects `cabal-v2.project` and maps the v2 library, executable, and test directories to their Cabal components.
Source outside `v2/` uses a `none` cradle and receives no HLS project diagnostics.
This follows the supported [explicit Cabal configuration](https://github.com/haskell/hie-bios#cabal).
HLS 2.14.0.0 with GHC 9.10.3 typechecks all three v2 source files on Linux.
LSP initialization and clean shutdown also pass through the local editor launcher with `--lsp`.
Interactive editor behavior and macOS project loading remain unverified.
Editor launchers must pass `--lsp`; without it HLS runs its command-line setup tester.

The [independent project](../../cabal-v2.project) pins the cached Hackage index state `2026-09-09T02:42:09Z`.
Its [freeze file](../../cabal-v2.project.freeze) pins only `base` and GHC's bundled packages.
Normal and offline build/test runs resolve the same components and dependency versions; repeated freezes are byte-identical.
Offline compilation and testing also pass in a fresh project build directory, using the existing compiler and package-index cache.
This does not establish an empty-cache bootstrap.
Cabal 3.16.1.0 reads `cabal-v2.project.freeze` and `cabal-v2.project.local`; local settings override project/freeze settings.
Build/test results assume no local overrides.

Fourmolu 0.20.1.0 and HLint 3.10 pass over all three v2 Haskell sources using the existing [formatter configuration](../../fourmolu.yaml) and HLint defaults.
Both checks reject intentional violations and pass again after restoration.
See [roadmap task 2.1](roadmap.md#21-formatting-and-linting-for-the-new-package) for the commands.

Legacy quality commands are evidenced by [CI](../../.github/workflows/ci.yml): HLint over `app/`, `src/`, `test/`, and Fourmolu checking.
[bin/format](../../bin/format) still invokes Brittany through Nix and is not the canonical formatter.
[bin/run](../../bin/run) invokes Nix and ghcid; it is convenience machinery, not proof that Cabal needs Nix.

CI's costly structure is observable, but actual GitHub run timings were not retrieved.
The push workflow waits for HLint before building, updates/freezes dependencies before cache restoration, includes `${{ matrix.ghc }}` in a cache key despite no compiler matrix, and generates Haddock, a static install, and artifacts on every push.
[Code scanning](../../.github/workflows/code-scanning.yml) also rebuilds the Docker image for Trivy on its configured PR/main/scheduled events.
A few-minute warm build is a target, not a measured prediction.

[flake.nix](../../flake.nix) has only x86_64 Linux/Darwin outputs, an implicit nixpkgs input, global `allowBroken`, a project jailbreak, and a base32 override combining jailbreak, disabled tests, and unmarking broken.
It includes flake-compat and [default.nix](../../default.nix) for older Nix entry points.
No Nix evaluation was performed.
Remove workarounds only after demonstrating a small replacement; do not describe this flake as validated reproducibility.

## What to carry forward

The main lessons are about boundaries, not libraries: possession is different from a constructible policy record; authority discovery can amplify access; fresh authorization belongs with the protected operation; URL parsing is not network confinement; and tests must exercise the production boundary they claim to protect.
The design's classification table applies these findings to the replacement.
No old-source security finding above has been fixed by this documentation handoff.
