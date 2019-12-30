{ pkgs, lib, config, ... }:
with lib;
let
  package = pkgs.haskellPackages.callPackage ../shell.nix {};

  cfg = config.services.brockman;
in {
  options.services.brockman = {
    enable = mkEnableOption "brockman";
    package = mkOption { type = types.package; default = package; };
    config = mkOption { type = types.attrs; };
  };

  config = mkIf cfg.enable {
    users.extraUsers.brockman.isNormalUser = false;

    systemd.services.brockman = {
      description = "RSS to IRC broadcaster";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${cfg.package}/bin/brockman ${pkgs.writeText "brockman.json" (builtins.toJSON cfg.config)}
        '';
        User = config.users.extraUsers.brockman.name;
        PrivateTemp = true;
        RuntimeDirectory = "brockman";
        WorkingDirectory = "%t/brockman";
      };
    };
  };
}
