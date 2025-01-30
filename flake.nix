{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    systems,
    ...
  } @ inputs: let
    forEachSystem = nixpkgs.lib.genAttrs (import systems);
  in {
    packages = forEachSystem (system: {
      devenv-up = self.devShells.${system}.default.config.procfileScript;
      devenv-test = self.devShells.${system}.default.config.test;
    });

    devShells =
      forEachSystem
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = devenv.lib.mkShell {
          inherit inputs pkgs;
          modules = [
            {
              # Need to set GHC_NO_UNICODE as mentioned here:
              # https://gitlab.haskell.org/ghc/ghc/-/commit/74132c2b0992849f83ef87c8a56ac3975738e767#af1d9e2647379d23697be69f8b7fc568ed0294c5_1825_1825
              env.LC_ALL = "en_US.UTF-8";
              env.GHC_CHARENC = "GHC_NO_UNICODE";

              languages.haskell.enable = true;
              languages.haskell.package = pkgs.haskell.compiler.ghc96;

              # https://devenv.sh/reference/options/
              packages = with pkgs; [
                pre-commit
                zsh
              ];

              enterShell = ''
                pre-commit install
              '';

              enterTest = ''
                cabal update
                cabal test
              '';
            }
          ];
        };
      });
  };
}
