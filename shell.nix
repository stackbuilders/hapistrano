{ pkgs, version ? "90" }:

let
  supportedGhcVersions = {
    "810" = pkgs.haskell.compiler.ghc810;
    "90" = pkgs.haskell.compiler.ghc90;
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    supportedGhcVersions.${version}
  ];
}
