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
      packages = forAllSystems (system: {
        default = nixpkgsFor.${system}.hapistrano;
      });
      overlays.default = final: prev: {
        hapistrano = self.packages.${prev.system}.default;
      };
    };
}
