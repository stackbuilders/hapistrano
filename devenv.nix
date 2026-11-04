{ pkgs, lib, ... }:

{
  packages =
    [
      pkgs.git
      pkgs.stack
      pkgs.zsh
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      pkgs.gmp
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      pkgs.haskell.compiler.ghc984
    ];
}
