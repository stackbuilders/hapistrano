{
  inputs = {
    devenv.url = "github:cachix/devenv";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  };

  # nixConfig = {
  #   extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
  #   extra-substituters = "https://devenv.cachix.org";
  # };

  outputs = inputs@{ self, devenv, flake-utils, nixpkgs }:
    # let
    #   supportedSystems = [
    #   "x86_64-darwin"
    #   "x86_64-linux"
    # ];
    # in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        mkHaskellShell = ghcPackage: pkgs.mkShell {
          buildInputs = [
            ghcPackage
            pkgs.cabal-install
            pkgs.zsh
          ];
        };
      in
      {
        devShells = rec {
          ghc810 = mkHaskellShell pkgs.haskell.compiler.ghc810;
          ghc90 = mkHaskellShell pkgs.haskell.compiler.ghc90;
          default = ghc90;
        };
      });

  # # --- Flake Local Nix Configuration ----------------------------
  # nixConfig = {
  #   # This sets the flake to use the IOG nix cache.
  #   # Nix should ask for permission before using it,
  #   # but remove it here if you do not want it to.
  #   extra-substituters = [ "https://cache.iog.io" ];
  #   extra-trusted-public-keys = [
  #     "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  #   ];
  #   allow-import-from-derivation = "true";
  # };
}
