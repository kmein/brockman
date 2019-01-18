{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bloomfilter, bytestring
      , feed, kirk, microlens, safe, stdenv, stm, text, wreq
      }:
      mkDerivation {
        pname = "brockman";
        version = "1.0.2";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson async base bloomfilter bytestring feed kirk microlens safe
          stm text wreq
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {
    kirk = haskellPackages.callPackage nix/kirk.nix {};
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
