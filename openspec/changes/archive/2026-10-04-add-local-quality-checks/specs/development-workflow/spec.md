## ADDED Requirements

### Requirement: Formatting and linting
Every Haskell source in `v2/` SHALL pass Fourmolu 0.20.1.0 in check mode with the root `fourmolu.yaml` and HLint 3.10 with its default hints.
The checks MUST NOT rely on broad suppressions, and they cover neither generated files nor legacy code.

#### Scenario: Check the replacement sources
- **WHEN** a contributor runs `rg --files v2 -g '*.hs' -0 | xargs -0 fourmolu-0.20.1.0 --mode check` and `hlint-3.10 v2` from the repository root, or the unversioned `fourmolu` and `hlint` when their `--version` matches
- **THEN** both commands succeed

#### Scenario: A violation fails the check
- **WHEN** a v2 source contains a formatting or lint violation
- **THEN** the corresponding check fails

### Requirement: Editor integration
The root `hie.yaml` SHALL select `cabal-v2.project` and map both v2 libraries, the executable, and both test directories to their Cabal components.
Source outside `v2/` MUST receive no HLS project diagnostics.
HLS 2.14.0.0 with GHC 9.10.3 typechecks every v2 source on Linux and macOS; interactive editor behavior is unverified.

#### Scenario: Typecheck every replacement source
- **WHEN** a contributor runs `rg --files v2 -g '*.hs' -0 | xargs -0 haskell-language-server-wrapper -j 2`
- **THEN** HLS reports every v2 file worked and zero failures

#### Scenario: Editors launch HLS as a language server
- **WHEN** an editor launches HLS with `--lsp`
- **THEN** LSP initialization and shutdown complete cleanly

### Requirement: Native macOS verification
The native contributor path SHALL work on Apple Silicon macOS with the Xcode Command Line Tools and GHCup-installed GHC 9.10.3, Cabal 3.16.1.0, and HLS 2.14.0.0, without Nix.
Build, Test, Version, the offline checks, formatting, linting, and HLS pass there from empty Cabal caches.
This is not a wider platform support matrix.

#### Scenario: Fresh macOS checkout
- **WHEN** a contributor on Apple Silicon macOS installs the native tools, runs `cabal update`, and runs the verification commands from empty caches
- **THEN** every command succeeds and the solved plan matches the freeze file

#### Scenario: Bundled SQLite on macOS
- **WHEN** `otool -L` inspects the SQLite test executable
- **THEN** it lists no SQLite shared library
