{ mkDerivation, base, containers, HUnit, stdenv, Unique }:
mkDerivation {
  pname = "SkillchainCalculatorProject";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base containers Unique ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base containers HUnit ];
  doHaddock = false;
  description = "Calculate skillchains";
  license = stdenv.lib.licenses.bsd3;
}
