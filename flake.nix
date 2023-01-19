{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "nixpkgs/nixos-22.11-small";
  };

  outputs = { self, flake-utils, haskell, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskell.overlay ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell) config; };
      in
      rec {
        devShells = {
          ghc810 = import ./shell.nix { inherit pkgs haskell; ghcVersion = "8.10"; };
          ghc90 = import ./shell.nix { inherit pkgs haskell; ghcVersion = "9.0"; };
          default = devShells.ghc90;
        };
      });
}
