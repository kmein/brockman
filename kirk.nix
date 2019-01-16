{ mkDerivation, async, base, bytestring, fetchgit, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "kirk";
  version = "1.0.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/kirk/";
    sha256 = "1acsmmc485c54axpy9bd0320j18hs261vl1vdxns4n04sxzqd7k0";
    rev = "cdf3cb373af8f9b03a9487a63eb32e0226913589";
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
