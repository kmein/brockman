{
  description = "Brockman package, module and test VM.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-generators }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    package = pkgs.haskellPackages.callPackage ./default.nix {};
  in {
    packages.${system} = {
      brockman = package;

      vm = nixos-generators.nixosGenerate {
        inherit pkgs;
        modules = [ self.nixosModule (import nix/vm.nix) ];
        format = "vm-nogui";
      };
    };

    defaultPackage.${system} = self.packages.${system}.brockman;

    apps.${system} = {
      generate = {
        type = "app";
        program = toString (pkgs.writeScript "generate" "${pkgs.cabal2nix}/bin/cabal2nix . > default.nix");
      };
      check-formatting = {
        type = "app";
        program = toString (pkgs.writeScript "check-formatting" ''${pkgs.findutils}/bin/find src/ -type f -exec ${pkgs.ormolu}/bin/ormolu --mode check '{}' \;'');
      };
      check-configs = {
        type = "app";
        program = toString (pkgs.writeScript "check-configs" ''
          set -e
          for config in ./config/*.json; do
            echo === checking "$config"
            ${self.defaultPackage.${system}}/bin/brockman --check "$config"
          done
        '');
      };
    };

    devShell.${system} = package.env.overrideAttrs (old: old // {
      buildInputs = [ pkgs.cabal-install ];
    });

    nixosModule = { config, lib, pkgs, ... }: import nix/module.nix {
      inherit package config lib pkgs;
    };
  };
}
