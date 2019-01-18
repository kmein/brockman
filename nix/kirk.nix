{ mkDerivation, async, base, bytestring, fetchFromGitHub, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "kirk";
  version = "1.0.1";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "kirk";
    rev = "602646765fcfc4aa7b161c9d12f44d2b7c424a60";
    sha256 = "1zf25vszhjb0jv8cn1kbsbra95glw41jlbv8z6zpm1glk283bv5h";
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
