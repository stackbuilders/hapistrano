{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      stacklock2nix,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            stacklock2nix.overlay
            (final: prev: {
              hapistrano-stacklock = final.stacklock2nix {
                stackYaml = ./stack.yaml;
                baseHaskellPkgSet = final.haskell.packages.ghc984;
                all-cabal-hashes = final.fetchFromGitHub {
                  owner = "commercialhaskell";
                  repo = "all-cabal-hashes";
                  rev = "299918adb3205b2dfe960bcdc79a9b1b300b11e6";
                  sha256 =
                    if final.stdenv.isLinux then
                      "sha256-9nkHnZusYNDntpH9LrLTamY9BimRWfeX2m99lAuMCMI="
                    else
                      "sha256-Z0UJ78I3O8kWduNOqz7jASnR5XB8mwBDP0fVvjJoqOg=";
                };
                additionalHaskellPkgSetOverrides = hfinal: hprev: {
                  hapistrano = final.haskell.lib.compose.overrideCabal (drv: {
                    testToolDepends = drv.testToolDepends ++ [
                      final.git
                      final.zsh
                    ];
                  }) hprev.hapistrano;
                };
              };
              hapistrano = final.hapistrano-stacklock.pkgSet.hapistrano;
            })
          ];
        }
      );
    in
    {
      packages = forAllSystems (system: {
        default = nixpkgsFor.${system}.hapistrano;
      });
      overlays.default = final: prev: {
        hapistrano = self.packages.${prev.system}.default;
      };
    };
}
