{ mkDerivation, aeson, async, base, bloomfilter, bytestring
, conduit, feed, hslogger, irc-conduit, microlens, network
, optparse-applicative, stdenv, stm, text, wreq
}:
mkDerivation {
  pname = "brockman";
  version = "1.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bloomfilter bytestring conduit feed hslogger
    irc-conduit microlens network optparse-applicative stm text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
