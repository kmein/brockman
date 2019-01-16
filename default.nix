{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./brockman.nix {
  kirk = nixpkgs.pkgs.haskellPackages.callPackage ./kirk.nix {};
}
