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
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            haskellNix.overlay
            (final: prev:
              let
                mkCabalProject = compiler-nix-name: final.haskell-nix.cabalProject' {
                  inherit compiler-nix-name;
                  src = final.haskell-nix.haskellLib.cleanGit {
                    name = "hapistrano";
                    src = ./.;
                  };
                };
              in
              {
                hapistrano-ghc8107 = mkCabalProject "ghc8107";
                hapistrano-ghc902 = mkCabalProject "ghc924";
              })
          ];
        };
        flake-ghc8107 = pkgs.hapistrano-ghc8107.flake { };
        flake-ghc902 = pkgs.hapistrano-ghc902.flake { };
        overrideTestPackage = flake: flake.packages."hapistrano:test:test".overrideAttrs (_: {
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
      in
      rec {
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
        devShells = {
          default = devShells.ghc902;
          ghc8107 = flake-ghc8107.devShells.default;
          ghc902 = flake-ghc902.devShells.default;
        };
        packages = {
          default = packages.hap-ghc902;
          hap-ghc8107 = flake-ghc8107.packages."hapistrano:exe:hap";
          hap-ghc902 = flake-ghc902.packages."hapistrano:exe:hap";
          test-ghc8107 = overrideTestPackage flake-ghc8107;
          test-ghc902 = overrideTestPackage flake-ghc902;
        };
      });
}


