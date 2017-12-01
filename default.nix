{ mkDerivation, base, dhall, directory, filepath, hinotify, HUnit
, lens, lucid, matrix-bot-api, monad-loops, optparse-applicative
, optparse-text, plpd-utils, stdenv, stm, test-framework
, test-framework-hunit, test-framework-th, text, text-format
}:
mkDerivation {
  pname = "matrix-dirwatch";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dhall directory filepath hinotify lens lucid matrix-bot-api
    monad-loops optparse-applicative optparse-text plpd-utils stm text
    text-format
  ];
  executableHaskellDepends = [ base lens matrix-bot-api text ];
  testHaskellDepends = [
    base filepath HUnit lens matrix-bot-api plpd-utils test-framework
    test-framework-hunit test-framework-th text
  ];
  doHaddock = false;
  homepage = "https://gitlab.plapadoo.de/plapadoo/matrix-dirwatch";
  description = "Watch directory changes and post them to a Matrix channel";
  license = stdenv.lib.licenses.bsd3;
}
