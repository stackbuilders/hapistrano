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

  enterShell = lib.optionalString pkgs.stdenv.isDarwin ''
    # Let Stack's downloaded GHC use the host Darwin toolchain/libs instead of
    # inheriting Nix stdenv linker flags that can pull in incompatible libffi.
    unset NIX_CFLAGS_COMPILE NIX_LDFLAGS
    export CC=/usr/bin/clang
    export CXX=/usr/bin/clang++
    export LD=/usr/bin/ld
  '';
}
