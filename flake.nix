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
              hapistrano-ghc966 = final.haskell-nix.cabalProject' {
                src = final.haskell-nix.haskellLib.cleanGit {
                  name = "hapistrano";
                  src = ./.;
                };
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                compiler-nix-name = "ghc966";
              };
            })
          ];
        };
        flake-ghc966 = pkgs.hapistrano-ghc966.flake { };
      in rec {
        apps = {
          test-ghc966 = {
            type = "app";
            program = "${packages.test-ghc966}/bin/test";
          };
        };
        packages = {
          default = flake-ghc966.packages."hapistrano:exe:hap";
          test-ghc966 = flake-ghc966.packages."hapistrano:test:test".overrideAttrs (_: {
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
        devShells = {
          default = devShells.ghc966;
          ghc966 = flake-ghc966.devShells.default;
        };
      });
}
