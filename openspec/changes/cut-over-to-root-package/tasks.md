# Tasks

These are outlined slices; before /opsx:apply, expand the next slice into bounded tasks naming scope, non-goals, likely files, completion checks, and security constraints, and settle the decisions it consumes.
Work on a dedicated cutover branch and avoid parallel branches that change paths.

## 1. Move without behavior changes

- [ ] 1.1 Move the replacement package, its project and freeze files, and the HLS configuration to the root with the accepted executable name and unchanged pins; verified by root build, test, and `cabal run eselsohr -- --version`, Quality, HLS, and a solved plan whose dependency versions match the previous freeze file.

## 2. Remove legacy code

- [ ] 2.1 Remove the legacy `src/`, `app/`, `test/`, and `eselsohr.cabal` once no consumer remains; verified by root build and test, Quality, HLS, and a search showing no remaining reference to the legacy `Lib` tree.

## 3. Prune dependencies, scripts, and workflows

- [ ] 3.1 Remove dependencies, scripts, and workflows used only by the legacy package, keeping the checks the replacement needs; verified by root build and test and by inspecting each removed file's consumers.

## 4. Update documentation and specs

- [ ] 4.1 Update `AGENTS.md`, the README, and the verification vocabulary in `openspec/config.yaml`, and remove documentation that no longer describes the code; verified by a search for `v2/`, `cabal-v2.project`, and `eselsohr-v2`, and Diff checks.
- [ ] 4.2 Extend this change's `development-workflow` delta from the main spec as it stands at cutover: MODIFIED requirements for `Version command`, `Pinned dependency resolution`, `Formatting and linting`, `Editor integration`, and any requirement archived since, such as those from `add-native-ci`, that names `v2/`, `cabal-v2.project`, or `eselsohr-v2`, and a RENAMED entry for `Independent replacement package`; verified by `openspec show cut-over-to-root-package`, the same search over the delta, and the Specs check.

## 5. Exit review

- [ ] 5.1 Review the cutover against the accepted end-to-end flows; verified by root `cabal test` and `cabal run eselsohr`, Quality, HLS, Specs, and Diff checks.
