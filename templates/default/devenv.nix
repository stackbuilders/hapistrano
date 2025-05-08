{ pkgs, ... }:

{
  packages = [ pkgs.hapistrano ];

  cachix = {
    enable = true;
    pull = [ "stackbuilders" ];
  };
}
