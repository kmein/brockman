{ pkgs, lib, config, package ? pkgs.callPackage ../. {}, ... }:
with lib;
let
  cfg = config.services.brockman;
in {
  options.services.brockman = {
    enable = mkEnableOption "brockman";
    package = mkOption { type = types.package; default = package; };
    config = mkOption { type = types.attrs; };
  };

  config = mkIf cfg.enable {
    systemd.services.brockman = {
      description = "RSS to IRC broadcaster";

      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];

      serviceConfig = {
        Type = "simple";
        Restart = "always";
        ExecStart = ''
          ${cfg.package}/bin/brockman ${pkgs.writeText "brockman.json" (builtins.toJSON cfg.config)}
        '';

        WorkingDirectory = "~";
        RuntimeDirectory = "brockman";

        DynamicUser = true;
        NoNewPrivileges = true;

        ProtectProc = "invisible";
        ProtectSystem = "strict";
        ProtectHome = "tmpfs";
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        MemoryDenyWriteExecute = true;
      };
    };
  };
}
