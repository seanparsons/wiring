{ mkDerivation, base, hspec, mtl, QuickCheck, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "wiring";
  version = "0.5.0";
  src = ./..;
  libraryHaskellDepends = [ base mtl template-haskell transformers ];
  testHaskellDepends = [
    base hspec mtl QuickCheck template-haskell transformers
  ];
  homepage = "http://github.com/seanparsons/wiring/";
  description = "Wiring, promotion and demotion of types";
  license = stdenv.lib.licenses.bsd3;
}
