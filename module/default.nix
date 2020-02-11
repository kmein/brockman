{ pkgs, lib, config, ... }:
with lib;
let
  package = pkgs.callPackage ../. {};

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
      after = [ "network.target" ];
      serviceConfig = {
        Restart = "always";
        ExecStart = ''
          ${cfg.package}/bin/brockman ${pkgs.writeText "brockman.json" (builtins.toJSON cfg.config)}
        '';
        User = config.users.extraUsers.brockman.name;
        PrivateTmp = true;
        RuntimeDirectory = "brockman";
        WorkingDirectory = "%t/brockman";
      };
    };
  };
}
