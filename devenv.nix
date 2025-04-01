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
    ];
}
