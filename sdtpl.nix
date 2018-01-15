{ mkDerivation, base, containers, fgl, graphviz, mtl, parsec
, stdenv, template-haskell, text, transformers
}:
mkDerivation {
  pname = "sdtpl";
  version = "0.1.0.0";
  src = ../sdtpl;
  libraryHaskellDepends = [
    base containers fgl graphviz mtl parsec template-haskell text
    transformers
  ];
  description = "The String Decision Tree Processing Language";
  license = stdenv.lib.licenses.asl20;
}
