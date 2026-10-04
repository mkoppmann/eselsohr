# Design

## Context

[eselsohr.cabal](../../../../eselsohr.cabal) targets GHC 9.8 with `base ^>=4.19.1.0`.
[cabal.project](../../../../cabal.project) is an ordinary Cabal project; native Cabal does not intrinsically require Nix.
The available toolchain is GHC 9.10.3 (`base` 4.20.2.0) and Cabal 3.16.1.0.
`cabal build --offline --dry-run --builddir=<temporary-build-dir> all` and the corresponding `cabal test` both exit 1 with Cabal-7107: the legacy base constraint excludes the installed 4.20.2.0.
Neither reaches compilation or test execution, and the intended GHC 9.8 build remains unverified.
Legacy bounds, legacy dependencies, and global tool configuration were left unchanged.

The legacy application stores article URLs, titles, creation times, and a read or unread state.
Users can list articles, edit titles, mark them read or unread, delete them, and share access to a collection or article; collection overview links manage everyday access links, and capabilities can have a petname and expiration.
There are no user accounts; see the legacy [routes](../../../../src/Lib/Ui/Web/Route.hs) and [commands](../../../../src/Lib/App/Command.hs).
The legacy findings recorded in this and later changes come from source inspection, not a complete security audit; no exploit was run, and none of them was fixed in the legacy code.

The legacy code uses MTL-style interfaces and compressed JSON collection files.
Its existing behavior, comments, and tests remain useful evidence, but its module layers need not survive.
No running instances, stored data, or old capability URLs require compatibility.

## Goals / Non-Goals

**Goals:**

- A green replacement target that is independent of the legacy compiler constraint.
- A dependency solution that later tasks and branches can rely on.

**Non-Goals:**

- Repairing the legacy build, or claiming the whole repository is green while only the independent project builds.

## Decisions

### D1: start with an independent `v2/` package

The replacement starts as a temporary independent package selected by `cabal-v2.project`, with no imports from the old `Lib` tree and no compatibility adapters.
A later change cuts it over to the root package.
Fixing the legacy bounds first would spend effort on code the rewrite replaces, and an in-place refactor would carry the old layering forward.
The package uses GHC 2024 and `base ^>=4.20.2.0`; a shared Cabal `language` stanza enables OverloadedStrings for every component, and modules keep other extensions in their own pragmas.

### Concept-oriented modules

Prefer modules named after concepts, such as Article, Capability, Authorization, Store, Fetch, and Web.
Retain separation where it protects an invariant, not merely to reproduce the old Domain/Application/Infrastructure hierarchy.
Avoid generic Repository, DTO, Controller, or Service layers, and keep helper types close to their concept.
This is the intended layout, not a scaffold to create in advance; add each module when a change needs it.

```text
app/Main.hs
src/Eselsohr/
  Id.hs
  Article.hs
  WebUrl.hs
  Capability.hs
  Authorization.hs
  Store.hs
  Store/SQLite.hs
  Store/Migrate.hs
  Fetch.hs
  Fetch/Http.hs
  Web.hs
  Web/Routes.hs
  Web/Html.hs
  Cli.hs
  Config.hs
  Runtime.hs
test/
```

Article owns validated article values and pure transitions.
Capability owns permission, scope, and grant values and attenuation; Authorization owns the operation policy.
Store exposes operations such as reading articles and changing titles, while Store.SQLite owns connections, row decoding, and transaction checks.
Store.Migrate is CLI-only authority, not an operation available to web application code.
Fetch is the narrow metadata interface; Fetch.Http owns the network handler.
Web translates HTTP inputs and results, Routes owns typed links, and Html renders already-authorized view data.
Runtime alone composes directory, network, clock, and process authority; Config validates its inputs.

### Native contributor path

| Choice | Alternatives | Recommendation and cost |
| --- | --- | --- |
| DX | Native Cabal/GHCup; optional small Nix flake; Nix-first/haskell.nix | Native path is canonical. Optional Nix must exercise the same package, not maintain a second architecture or mask broken bounds. |

A developer's container setup and machine paths must not become project requirements.
A wrapper, Makefile, or Justfile is unnecessary until it shortens a real workflow.

### Pinned resolution

`cabal-v2.project` pins the Hackage index state `2026-09-09T02:42:09Z`, and `cabal-v2.project.freeze` pins the complete solution; keep the two aligned.
Serialize manifest changes with other branches at merge time.
Cabal 3.16.1.0 uses `cabal-v2.project.freeze` and `cabal-v2.project.local` for this custom project, matching its [project-file documentation](https://cabal.readthedocs.io/en/3.16/cabal-project-description-file.html#cmdoption-project-file).
The local file is written by `cabal configure` and overrides project and freeze settings, so it and its backup are ignored; results assume no local overrides.
The root project's companion files do not configure this project.

Regenerate the freeze file only for an intentional resolution update.
To change existing pins, adjust the intended dependency bounds or project index state and remove the old freeze file before regenerating it; review the resulting diff and run Build and Test.

```sh
cabal freeze --project-file=cabal-v2.project
cabal build --project-file=cabal-v2.project --offline all
cabal test --project-file=cabal-v2.project --offline all --test-show-details=direct
```

Offline checks use the installed compiler and existing package-index and dependency caches; a fresh project build directory does not establish an empty-cache bootstrap.

### Classification of legacy mechanisms

| Existing mechanism | Classification | Reason |
| --- | --- | --- |
| Layered module tree | REPLACE | Organize by concepts and concrete trusted boundaries, without controller/DTO/repository duplication by default. |
| Article/read-state/domain values | SIMPLIFY | Keep pure transitions, typed IDs, and non-empty values; tighten constructors/decoders and use structural equality. |
| Permission records and derived ordering | REPLACE | Actual sets with explicit subset semantics; derive ordering only for set elements. |
| Permission-specific witnesses | ADAPT | D3 uses sealed requests at the protected persistence boundary to keep the checked inputs together within one transaction. Pure transformations require no such request. |
| Bearer capabilities | KEEP | Preserve possession-based sharing, but replace encoding/storage and add ancestry, bounded attenuation, revocation semantics. |
| SharedRef categories and reusable-link discovery | REPLACE | Immutable parent graph, descendant metadata only, one-time secrets. |
| MTL port/runtime scheme | REPLACE | Selected effectful handlers must narrow authority rather than merely change syntax. |
| App/TestApp duplication | REMOVE | Compose production and test handlers around the same application functions. |
| Compressed aggregate persistence | REPLACE | Per-collection SQLite, explicit SQL, constraints, and transactions. |
| STM persistence queue | REMOVE | Its write-serialization purpose moves to SQLite; keep no educational queue without a remaining need. |
| General errors and redirects | SIMPLIFY | Small expected result types; boundary faults and HTTP control flow kept separate. |
| Configuration | REPLACE | Fail on malformed explicit values; no global environment clearing or file/directory confusion. |
| Scraper/network authority | REPLACE | Restricted PublicWeb handler with actual connector tests. |
| Servant | KEEP | Typed routes and link construction remain useful; assess complexity in a real slice. |
| Lucid HTML | KEEP | Escaped server rendering and plain forms fit the product. |
| No-JavaScript frontend | KEEP | No demonstrated problem currently justifies script authority or a build pipeline. |
| Test suite | REPLACE | Extract durable laws and behavior; remove repeated permutations and isolation-blind fakes. |
| Nix | SIMPLIFY | Optional small flake; remove existing overrides only after validation. |
| CI | REPLACE | Native fast feedback, honest cache inputs, expensive work on separate events. |
| Deployment | SIMPLIFY | Retain non-root/private-state intent, validate one executable and explicit operational dependencies. |

The [README](../../../../README.md) treats capability-based security as an experiment.
Pure functions, explicit effects, separate collection resources, and authorization witnesses were deliberate teaching ideas, and Clean/Hexagonal/Three Layer Cake examples influenced the directory structure.
That organization does not establish the security claims: a publicly accessible operation does not gain protection merely by being expressed as a capability, and a narrow port name does not restrict a broad implementation.
Preserve the small product, explicit sharing, simple HTML, meaningful domain values, and attention to operational hardening, not a technique merely because it was one of the experiments.

## Risks / Trade-offs

- [Pinning improves repeatability, not dependency trust] → Keep real dependency bounds and review pin updates deliberately.
- [Populated caches hide bootstrap failures] → Distinguish cache success from a clean build when reporting results.
- [Two projects in one repository] → The legacy root failure is not a regression of this package; never report the repository as green from the independent project alone.
