## MODIFIED Requirements

### Requirement: Editor integration
The root `hie.yaml` SHALL select `cabal-v2.project` and map every v2 library, the executable, and both test directories to their Cabal components.
Source outside `v2/` MUST receive no HLS project diagnostics.
HLS 2.14.0.0 with GHC 9.10.3 typechecks every v2 source on Linux and macOS; interactive editor behavior is unverified.

#### Scenario: Typecheck every replacement source
- **WHEN** a contributor runs `rg --files v2 -g '*.hs' -0 | xargs -0 haskell-language-server-wrapper -j 2`
- **THEN** HLS reports every v2 file worked and zero failures

#### Scenario: Editors launch HLS as a language server
- **WHEN** an editor launches HLS with `--lsp`
- **THEN** LSP initialization and shutdown complete cleanly
