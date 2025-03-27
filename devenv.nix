{ pkgs, ... }:

{
  packages = [
    pkgs.git
    pkgs.stack
    pkgs.zsh
  ];
}
