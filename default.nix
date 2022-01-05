{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hashable, hslogger, html-entity, http-client, irc-conduit, lens
, lib, lrucaching, network, optparse-applicative, random, safe
, text, time, timerep, unordered-containers, wreq
}:
mkDerivation {
  pname = "brockman";
  version = "4.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive conduit
    containers directory feed filepath hashable hslogger html-entity
    http-client irc-conduit lens lrucaching network
    optparse-applicative random safe text time timerep
    unordered-containers wreq
  ];
  license = lib.licenses.mit;
}
