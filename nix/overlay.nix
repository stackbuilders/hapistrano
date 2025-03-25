final: prev: {
  hapistrano-stacklock = final.stacklock2nix {
    stackYaml = ../stack.yaml;
    baseHaskellPkgSet = final.haskell.packages.ghc984;
    all-cabal-hashes = final.fetchFromGitHub {
      owner = "commercialhaskell";
      repo = "all-cabal-hashes";
      rev = "299918adb3205b2dfe960bcdc79a9b1b300b11e6";
      sha256 =
        if final.stdenv.isLinux then
          "sha256-9nkHnZusYNDntpH9LrLTamY9BimRWfeX2m99lAuMCMI="
        else
          "sha256-Z0UJ78I3O8kWduNOqz7jASnR5XB8mwBDP0fVvjJoqOg=";
    };
    additionalHaskellPkgSetOverrides = hfinal: hprev: {
      hapistrano = final.haskell.lib.compose.overrideCabal (drv: {
        testToolDepends = drv.testToolDepends ++ [
          final.git
          final.zsh
        ];
      }) hprev.hapistrano;
    };
  };
  hapistrano = final.hapistrano-stacklock.pkgSet.hapistrano;
}
