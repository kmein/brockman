{ mkDerivation, async, base, bytestring, fetchFromGitHub, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "kirk";
  version = "1.0.1";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "kirk";
    rev = "9b591cd17b631a8fceab7af94760493d375ee303";
    sha256 = "174b2jsnhs3z5kqnqv9fl3k7s94whcd6zzl0blwq1lh2rxp88skf";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring network optparse-applicative text
  ];
  executableHaskellDepends = [
    async base network optparse-applicative text
  ];
  license = stdenv.lib.licenses.mit;
}
