{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, haskellNix, nixpkgs }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs {
          system = if system == "aarch64-darwin" then "x86_64-darwin" else system;
          inherit overlays;
          inherit (haskellNix) config;
        };
      in
      {
        devShells = rec {
          ghc810 = import ./shell.nix { inherit pkgs; ghcVersion = "8.10"; };
          ghc90 = import ./shell.nix { inherit pkgs; ghcVersion = "9.0"; };
          default = ghc90;
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}
