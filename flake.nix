{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix/nix-tools-0.2.2";
    nixpkgs-2205.follows = "haskellNix/nixpkgs-2205";
    nixpkgs-unstable.follows = "haskellNix/nixpkgs-unstable";
  };

  nixConfig = {
    allow-import-from-derivation = "true";
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  outputs = inputs@{ self, flake-utils, haskellNix, nixpkgs-2205, nixpkgs-unstable }:
    # https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html
    flake-utils.lib.eachDefaultSystem (system:
      let
        mkCabalProject = final: compiler-nix-name: final.haskell-nix.cabalProject' {
          inherit compiler-nix-name;
          src = final.haskell-nix.haskellLib.cleanGit {
            name = "hapistrano";
            src = ./.;
          };
        };
        pkgs-2205 = import nixpkgs-2205 {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            haskellNix.overlay
            (final: prev: {
              hapistrano = mkCabalProject final "ghc8107";
            })
          ];
        };
        pkgs-unstable = import nixpkgs-unstable {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            haskellNix.overlay
            (final: prev: {
              hapistrano = mkCabalProject final "ghc902";
            })
          ];
        };
        flake-ghc8107 = pkgs-2205.hapistrano.flake { };
        flake-ghc902 = pkgs-unstable.hapistrano.flake { };
        overrideTestPackage = flake: flake.packages."hapistrano:test:test".overrideAttrs (_: {
          postFixup = ''
            wrapProgram $out/bin/test \
              --set PATH ${pkgs-unstable.lib.makeBinPath [
                pkgs-unstable.bash
                pkgs-unstable.coreutils
                pkgs-unstable.findutils
                pkgs-unstable.git
                pkgs-unstable.zsh
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


