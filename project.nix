{ mkDerivation, aeson, base, brick, containers, HUnit, mtl
, parallel, safe, scotty, stdenv, text, Unique, vty, wai-cors
, wai-extra
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
    aeson base brick containers mtl parallel safe scotty text Unique
    vty wai-cors wai-extra
  ];
  testHaskellDepends = [ base containers HUnit safe ];
  doHaddock = false;
  description = "Calculate skillchains";
  license = stdenv.lib.licenses.bsd3;
}
