{ mkDerivation, aeson, base, containers, HUnit, mtl, scotty, stdenv
, text, Unique
}:
mkDerivation {
  pname = "SkillchainCalculatorProject";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base containers Unique ];
  executableHaskellDepends = [
    aeson base containers mtl scotty text Unique
  ];
  testHaskellDepends = [ base containers HUnit ];
  doHaddock = false;
  description = "Calculate skillchains";
  license = stdenv.lib.licenses.bsd3;
}
