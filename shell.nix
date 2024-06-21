{ pkgs, ghcVersion ? "9.0" }:

let
  supportedGhcVersions = {
    "8.10" = pkgs.haskell.ghc8107;
    "9.0" = pkgs.haskell.ghc901;
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.zsh
    supportedGhcVersions.${ghcVersion}
  ];
}
