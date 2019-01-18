{ mkDerivation, async, base, bytestring, fetchFromGitHub, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "kirk";
  version = "1.0.1";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "kirk";
    sha256 = "16x4zry1939dbhxmki27wq34pkhmvf8nwfax2fr18mnyyclmh635";
    rev = "c64b22acee08a6545a72d3254d08c88c6d4a5038";
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
