{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-22.11-small";
    # https://github.com/NixOS/nixpkgs/pull/149942
    # nixpkgs.url = "github:nixos/nixpkgs?rev=e9d090fcd60d3005d2c33db8ac03d8e40ec392b3";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { localSystem = system; };
      in
      rec {
        devShells = {
          ghc810 = import ./shell.nix { inherit pkgs; ghcVersion = "8.10"; };
          ghc90 = import ./shell.nix { inherit pkgs; ghcVersion = "9.0"; };
          default = devShells.ghc90;
        };
      });
}
