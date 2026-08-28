{ pkgs, lib, ... }:

{
  packages =
    [
      pkgs.git
      pkgs.haskell.compiler.ghc984
      pkgs.stack
      pkgs.zsh
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      pkgs.gmp
    ];
}
