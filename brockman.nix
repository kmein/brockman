{ mkDerivation, aeson, async, base, bloomfilter, bytestring, feed
, kirk, microlens, safe, stdenv, stm, text, wreq
}:
mkDerivation {
  pname = "brockman";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bloomfilter bytestring feed kirk microlens safe
    stm text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
