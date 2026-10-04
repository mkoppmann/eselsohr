# Tasks

## 1. Formatting and linting

- [x] 1.1 Format and lint every v2 Haskell source with Fourmolu 0.20.1.0 and HLint 3.10 using the existing style and default hints; verified by both checks passing on Linux and macOS and failing on a temporary intentional violation before it was restored.

## 2. Editor integration

- [x] 2.1 Configure the root `hie.yaml` to select `cabal-v2.project` for the v2 components and a `none` cradle elsewhere; verified by the HLS batch check finishing with all files worked and zero failures, and by LSP initialization and clean shutdown with `--lsp` through the local Linux editor launcher.

## 3. Native macOS verification

- [x] 3.1 Run Build, Test, Version, offline Build and Test, formatting, linting, and HLS (28 files) on an Apple Silicon Mac with macOS 27.0.1, Xcode Command Line Tools, and native GHCup tools, starting from empty Cabal caches; all pass.
- [x] 3.2 Confirm the solved plan matches the freeze file and that `otool -L` of the SQLite test executable lists no SQLite library.
- [x] 3.3 Record the macOS setup steps and the separate Fourmolu and HLint installs; verified by following them on the same machine.
