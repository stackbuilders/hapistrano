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
    let
      buildWithGhc = ghcVersion: system:
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
                  compiler-nix-name = ghcVersion;
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
        };
    in
    flake-utils.lib.eachDefaultSystem (buildWithGhc "ghc902") // 
    flake-utils.lib.eachDefaultSystem (buildWithGhc "ghc8107");
}
