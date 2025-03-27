{ pkgs, inputs, ... }:

let
  pkgs-unstable = import inputs.nixpkgs-unstable {
    system = pkgs.stdenv.system;
  };
in
{
  packages = [
    pkgs-unstable.haskell.compiler.ghc984
  ];

  scripts.stack.exec = ''
    ${pkgs.stack}/bin/stack --system-ghc "$@"
  '';
}
