{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      stacklock2nix,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      unstestedSystems = [
        "aarch64-linux"
        "x86_64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            stacklock2nix.overlay
            (import ./nix/overlay.nix)
          ];
        }
      );
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          default =
            pkgs.lib.warnIf (builtins.any (x: x == system) unstestedSystems)
              "'${system}' is not tested as part of the CI workflow; please report any issues you encounter while dealing with it."
              pkgs.hapistrano;
        }
      );
      overlays.default = final: prev: {
        hapistrano = self.packages.${prev.system}.default;
      };
      templates.default = {
        description = "Install Hapistrano using devenv";
        path = ./templates/default;
      };
    };
}
