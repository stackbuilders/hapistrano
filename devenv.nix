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
    # Use GHC 9.8.4 from Nix so its libffi matches the macOS development shell.
    ++ lib.optionals pkgs.stdenv.isDarwin [
      pkgs.haskell.compiler.ghc984
    ];
}
