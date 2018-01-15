{ mkDerivation, aeson, base, Cabal, containers, lens, MissingH
, pretty-show, QuickCheck, sdtpl, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "pack-it-forms-msgfmt";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lens MissingH sdtpl text time
  ];
  testHaskellDepends = [
    base Cabal containers lens MissingH pretty-show QuickCheck tasty
    tasty-hunit tasty-quickcheck text time
  ];
  description = "Pack-It-Forms Message File Handling";
  license = stdenv.lib.licenses.asl20;
}
