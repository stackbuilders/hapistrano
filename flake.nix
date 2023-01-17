{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-22.11-small";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        devShells = {
          ghc810 = import ./shell.nix { inherit pkgs; version = "810"; };
          ghc90 = import ./shell.nix { inherit pkgs; version = "90"; };
          default = devShells.ghc90;
        };
      });
}
