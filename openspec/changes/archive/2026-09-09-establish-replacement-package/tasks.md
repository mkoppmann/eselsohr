# Tasks

## 1. Documentation foundation

- [x] 1.1 Record the decisions, legacy evidence, and task contracts in a short project map, design, investigation, and roadmap; verified by reading every document, resolving its links, and checking findings against source and solver results.
  The `adopt-openspec` change replaced these documents with OpenSpec specs and changes.

## 2. Minimal replacement package

- [x] 2.1 Add `cabal-v2.project`, `v2/eselsohr-v2.cabal`, and a copy of the repository license in `v2/`, targeting GHC 9.10.3, Cabal 3.16.1.0, GHC2024, and `base ^>=4.20.2.0`; verified by inspecting the solved plan, which contains no legacy `eselsohr` package.
- [x] 2.2 Add a tiny library and an executable supporting `--version`, with usage on stderr for missing or unsupported arguments; verified by Version printing `eselsohr-v2 0.1.0.0`.
- [x] 2.3 Add an assertion-based smoke test; verified by Build and Test passing, and by a deliberately broken assertion failing before it was restored.

## 3. Repeatable package resolution

- [x] 3.1 Pin the index state in `cabal-v2.project` and generate `cabal-v2.project.freeze`; verified by repeated freezes producing byte-identical files.
- [x] 3.2 Ignore `cabal-v2.project.local` and its backup; verified with `git status`.
- [x] 3.3 Run Build and Test normally and offline, including offline compilation in a fresh project build directory; all pass on Linux with the same resolved components and dependency versions.
