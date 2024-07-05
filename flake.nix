{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  nixConfig = {
    allow-import-from-derivation = "true";
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  outputs = inputs@{ self, flake-utils, haskellNix, nixpkgs }:
    # https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            haskellNix.overlay
            (final: prev: {
              hapistrano = final.haskell-nix.cabalProject' {
                src = final.haskell-nix.haskellLib.cleanGit {
                  name = "hapistrano";
                  src = ./.;
                };
                compiler-nix-name = "ghc8107";
              };
            })
          ];
        };
        flake = pkgs.hapistrano.flake { };
      in
      flake // rec {
        apps = {
          test = {
            type = "app";
            program = "${packages.test}/bin/test";
          };
        };
        legacyPackages = pkgs;
        packages = {
          default = flake.packages."hapistrano:exe:hap";
          test = flake.packages."hapistrano:test:test".overrideAttrs (_: {
            postFixup = ''
              wrapProgram $out/bin/test \
                --set PATH ${pkgs.lib.makeBinPath [
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.findutils
                  pkgs.git
                  pkgs.zsh
                ]}
            '';
          });
        };
      });
}


