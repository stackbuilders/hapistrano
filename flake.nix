{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  nixConfig = {
    allow-import-from-derivation = true;
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  outputs = inputs@{ self, flake-utils, haskellNix, nixpkgs }:
    # https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            hapistrano =
              final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc8107";
              };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hapistrano.flake { };
      in
      flake // {
        packages.default = flake.packages."hapistrano:exe:hap";
      });
}
