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
    supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
  in {
    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.callPackage ./default.nix {};
    in {
      default = package;
      brockman = package;

      vm = nixos-generators.nixosGenerate {
        inherit pkgs;
        modules = [ self.nixosModule (import nix/vm.nix) ];
        format = "vm-nogui";
      };
    });

    apps = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      generate = {
        type = "app";
        program = toString (pkgs.writeScript "generate" "${pkgs.cabal2nix}/bin/cabal2nix . > default.nix");
      };
      fix-formatting = {
        type = "app";
        program = toString (pkgs.writeScript "fix-formatting" ''${pkgs.findutils}/bin/find src/ -type f -exec ${pkgs.ormolu}/bin/ormolu --mode inplace '{}' \;'');
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
    });

    devShell = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      package = pkgs.haskellPackages.callPackage ./default.nix {};
    in package.env.overrideAttrs (old: old // {
      buildInputs = [ pkgs.cabal-install ];
    }));

    nixosModule = { config, lib, pkgs, ... }: import nix/module.nix {
      package = pkgs.haskellPackages.callPackage ./default.nix {};
      inherit config lib pkgs;
    };
  };
}
