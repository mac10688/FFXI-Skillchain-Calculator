{ mkDerivation, aeson, base, containers, HUnit, mtl, parallel, safe
, scotty, stdenv, text, Unique, wai-cors, wai-extra
}:
mkDerivation {
  pname = "SkillchainCalculatorProject";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers parallel safe Unique
  ];
  executableHaskellDepends = [
    aeson base containers mtl safe scotty text Unique wai-cors
    wai-extra
  ];
  testHaskellDepends = [ base containers HUnit safe ];
  doHaddock = false;
  description = "Calculate skillchains";
  license = stdenv.lib.licenses.bsd3;
}
