{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      stacklock2nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
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
                  sha256 = "sha256-Z0UJ78I3O8kWduNOqz7jASnR5XB8mwBDP0fVvjJoqOg=";
                };
                additionalHaskellPkgSetOverrides = hfinal: hprev: {
                  hapistrano = final.haskell.lib.compose.overrideCabal (drv: {
                    testToolDepends = drv.testToolDepends ++ [
                      pkgs.git
                      pkgs.zsh
                    ];
                  }) hprev.hapistrano;
                };
              };
              hapistrano = final.hapistrano-stacklock.pkgSet.hapistrano;
            })
          ];
        };
      in
      {
        packages.default = pkgs.hapistrano;
      }
    )
    // {
      overlays.default = final: prev: {
        hapistrano = self.packages.${prev.system}.default;
      };
    };
}
