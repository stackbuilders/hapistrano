{ mkDerivation, aeson, ansi-terminal, async, base, directory
, filepath, formatting, gitrev, hspec, hspec-discover, mtl
, optparse-applicative, path, path-io, process, QuickCheck
, silently, stdenv, stm, temporary, time, transformers
, typed-process, yaml, git, zlib, zsh
}:
mkDerivation {
  pname = "hapistrano";
  version = "0.4.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base filepath formatting gitrev mtl path
    process stm time transformers typed-process
  ];
  executableHaskellDepends = [
    aeson async base formatting gitrev optparse-applicative path
    path-io stm yaml
  ];
  testHaskellDepends = [
    base directory filepath hspec mtl path path-io process QuickCheck
    silently temporary
  ];
  testSystemDepends = [ git zlib zsh ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/stackbuilders/hapistrano";
  description = "A deployment library for Haskell applications";
  license = stdenv.lib.licenses.mit;
}
