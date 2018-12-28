{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  kirk = { mkDerivation, async, base, bytestring, fetchgit, network
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
    };

  f = { mkDerivation, aeson, base, bloomfilter, bytestring, feed
      , irc-client, irc-conduit, microlens, mtl, stdenv, stm, text, wreq, kirk
      }:
      mkDerivation {
        pname = "brockman";
        version = "1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bloomfilter bytestring feed irc-client irc-conduit
          microlens mtl stm text wreq kirk
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    kirk = haskellPackages.callPackage kirk {};
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
