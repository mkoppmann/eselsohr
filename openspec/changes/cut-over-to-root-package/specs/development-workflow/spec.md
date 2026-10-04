## MODIFIED Requirements

### Requirement: Independent replacement package
The replacement SHALL build, test, and run as the only package of the root Cabal project.
No legacy code remains: the repository MUST NOT contain the legacy `Lib` tree, and the solved plan MUST NOT contain legacy components or dependencies used only by them.
Build and Test pass from the repository root with GHC 9.10.3 and Cabal 3.16.1.0.
The root `cabal build` and `cabal test` are the passing baseline.

#### Scenario: Build and test the replacement
- **WHEN** a contributor runs `cabal build all` and `cabal test all --test-show-details=direct` from the repository root
- **THEN** both commands succeed with GHC 9.10.3 and Cabal 3.16.1.0

#### Scenario: Run the replacement from the root
- **WHEN** a contributor runs `cabal run eselsohr -- --version` from the repository root, using the accepted executable name
- **THEN** the replacement executable prints its version and exits successfully

#### Scenario: Solved plan excludes the legacy package
- **WHEN** the root project's package plan is inspected
- **THEN** it contains the replacement components and their pinned dependencies, and no legacy component or legacy-only dependency
