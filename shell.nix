{ pkgs, ghcVersion ? "9.0" }:

let
  haskellCompiler =
    if pkgs.stdenv.isDarwin
    then pkgs.haskell-nix.compiler
    else pkgs.haskell.compiler;
  supportedGhcVersions = {
    "8.10" = haskellCompiler.ghc8107;
    "9.0" = haskellCompiler.ghc902;
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.zsh
    supportedGhcVersions.${ghcVersion}
  ];
}
