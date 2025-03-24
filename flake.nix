{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs =
    {
      nixpkgs,
      stacklock2nix,
    }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [
          stacklock2nix.overlay
          (final: prev: {
            hapistrano-stacklock = final.stacklock2nix {
              stackYaml = ./stack.yaml;
              baseHaskellPkgSet = final.haskell.packages.ghc984;
            };
          })
        ];
      };
    in
    {
      packages.x86_64-linux = pkgs;
    };
}
