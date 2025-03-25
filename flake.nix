{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs =
    {
      nixpkgs,
      stacklock2nix,
      ...
    }:
    let
      pkgs = import nixpkgs {
        system = "aarch64-darwin";
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
            };
            hapistrano = final.hapistrano-stacklock.pkgSet.hapistrano;
          })
        ];
      };
    in
    {
      packages.aarch64-darwin.default = pkgs.hapistrano;
    };
}
