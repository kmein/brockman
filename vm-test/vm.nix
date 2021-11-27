{ config, lib, pkgs, ... }:
{
  imports = [ ../module ];

  services.brockman = {
    enable = true;
    config = builtins.fromJSON (builtins.readFile ../config/example.json);
  };

  systemd.services.brockman.environment.BROCKMAN_LOG_LEVEL = "DEBUG";

  services.getty.autologinUser = "root";
  programs.bash.promptInit = ''
    ${pkgs.tmux}/bin/tmux new-session \
      '${pkgs.epic5}/bin/epic5 -c "#news" listener localhost' \; \
      split-window -v -p 60 \
      'journalctl -fu brockman.service' \;
  '';

  services.charybdis = {
    enable = true;
    config = ''
      serverinfo {
        name = "test.irc.r";
        sid = "1as";
        description = "miep!";
        network_name = "irc.r";
        hub = yes;

        vhost = "0.0.0.0";
        vhost6 = "::";

        #ssl_private_key = "etc/ssl.key";
        #ssl_cert = "etc/ssl.cert";
        #ssl_dh_params = "etc/dh.pem";
        #ssld_count = 1;

        default_max_clients = 10000;
        #nicklen = 30;
      };

      listen {
        defer_accept = yes;

        /* If you want to listen on a specific IP only, specify host.
         * host definitions apply only to the following port line.
         */
        host = "0.0.0.0";
        port = 6667;
        sslport = 6697;

        /* Listen on IPv6 (if you used host= above). */
        host = "::";
        port = 6667;
        sslport = 9999;
      };

      class "users" {
        ping_time = 10 seconds;
        number_per_ident = 10;
        number_per_ip = 2048;
        number_per_ip_global = 4096;
        cidr_ipv4_bitlen = 24;
        cidr_ipv6_bitlen = 64;
        number_per_cidr = 65536;
        max_number = 3000;
        sendq = 1 megabyte;
      };

      exempt {
        ip = "127.0.0.1";
      };

      auth {
        user = "*@*";
        class = "users";
        flags = kline_exempt, exceed_limit, flood_exempt;
      };

      channel {
        use_invex = yes;
        use_except = yes;
        use_forward = yes;
        use_knock = yes;
        knock_delay = 5 minutes;
        knock_delay_channel = 1 minute;
        max_chans_per_user = 15;
        max_bans = 100;
        max_bans_large = 500;
        default_split_user_count = 0;
        default_split_server_count = 0;
        no_create_on_split = no;
        no_join_on_split = no;
        burst_topicwho = yes;
        kick_on_split_riding = no;
        only_ascii_channels = no;
        resv_forcepart = yes;
        channel_target_change = yes;
        disable_local_channels = no;
      };
      general {
        default_floodcount = 1000;
        disable_auth = yes;
        throttle_duration = 1;
        throttle_count = 1000;
      };
    '';
  };
}
