{
  description = "Eselsohr’s description";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));
        hp = pkgs.haskell.packages.ghc98.override {
          overrides = hself: hsuper: {
            base32 = jailbreakUnbreak hsuper.base32;
          };
        };
        project = returnShellEnv:
          pkgs.haskell.lib.doJailbreak
            (hp.developPackage {
              inherit returnShellEnv;
              name = "eselsohr";
              root = ./.;
              withHoogle = true;
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
              };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv (with hp;
                [
                  # Specify your build/dev dependencies here.
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  pkgs.nixpkgs-fmt
                  pkgs.zlib
                ]);
            });
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
