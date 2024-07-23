{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  nixConfig = {
    allow-import-from-derivation = "true";
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
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
              hapistrano-ghc8107 = final.haskell-nix.cabalProject' {
                src = final.haskell-nix.haskellLib.cleanGit {
                  name = "hapistrano";
                  src = ./.;
                };
                compiler-nix-name = "ghc8107";
              };
              hapistrano-ghc902 = final.haskell-nix.cabalProject' {
                src = final.haskell-nix.haskellLib.cleanGit {
                  name = "hapistrano";
                  src = ./.;
                };
                compiler-nix-name = "ghc902";
              };
            })
          ];
        };
        flake-ghc8107 = pkgs.hapistrano-ghc8107.flake { };
        flake-ghc902 = pkgs.hapistrano-ghc902.flake { };
      in
      flake-ghc8107 // flake-ghc902 // rec {
        apps = {
          test-ghc8107 = {
            type = "app";
            program = "${packages.test-ghc8107}/bin/test";
          };
          test-ghc902 = {
            type = "app";
            program = "${packages.test-ghc902}/bin/test";
          };
        };
        legacyPackages = pkgs;
        packages = {
          default = flake-ghc8107.packages."hapistrano:exe:hap";
          test-ghc8107 = flake-ghc8107.packages."hapistrano:test:test".overrideAttrs (_: {
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
          test-ghc902 = flake-ghc902.packages."hapistrano:test:test".overrideAttrs (_: {
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
