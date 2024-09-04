{ pkgs, lib, config, inputs, ... }:

{
  packages = [ pkgs.git ];

  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc98;

  enterTest = ''
    cabal test
  '';
}
