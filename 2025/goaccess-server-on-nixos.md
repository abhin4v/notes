---
date: 2025-04-20
tags: nix self-hosting programming
---

# Running a Goaccess Server on NixOS

[Goaccess](https://goaccess.io/) is an open source real-time web log analyzer. We can use it to parse a server access log file, such as of [Nginx](https://nginx.org/), and see the analysis report in a terminal in real-time. However, Goaccess also comes with an HTTP server built into it that can serve the same real-time report over HTTP ([demo](https://rt.goaccess.io/)).

<details markdown="1">
<summary>So I wrote a NixOS service module for Goaccess server.</summary>

```nix
{ config, lib, pkgs, ... }:
let
  serviceName = "goaccess";
  cfg = config.services."${serviceName}";
  nginxCfg = config.services.nginx;
  userName = cfg.userName;
  types = lib.types;
in
{
  options.services."${serviceName}" = {
    enable = lib.mkEnableOption "${serviceName} service";

    package = lib.mkOption {
      type = types.package;
      default = pkgs.goaccess;
      description = "The Goaccess package.";
    };

    userName = lib.mkOption {
      type = types.nonEmptyStr;
      default = serviceName;
      description = "The username to use for running the Goaccess service.";
    };

    dataDir = lib.mkOption {
      type = types.path;
      default = "/var/lib/${userName}";
      description = "The directory in which the Goaccess data is saved.";
    };

    dataRetentionDays = lib.mkOption {
      type = types.int;
      default = 7;
      description = "The number of days for which the Goaccess server retains the report data.";
    };

    reportDir = lib.mkOption {
      type = types.path;
      default = "/var/www/${userName}";
      description = "The directory in which the Goaccess report file is saved.";
    };

    host = lib.mkOption {
      type = types.nonEmptyStr;
      default = "127.0.0.1";
      description = "The host to run the Goaccess server on.";
    };

    port = lib.mkOption {
      type = types.port;
      default = 7890;
      description = "The port to run the Goaccess server on.";
    };

    logFilePath = lib.mkOption {
      type = types.path;
      description = "The full path to the log file to analyze.";
    };

    logFileFormat = lib.mkOption {
      type = types.enum [
        "COMBINED"
        "VCOMBINED"
        "COMMON"
        "VCOMMON"
        "W3C"
        "SQUID"
        "CLOUDFRONT"
        "CLOUDSTORAGE"
        "AWSELB"
        "AWSS3"
        "AWSALB"
        "CADDY"
        "TRAEFIKCLF"
      ];
      description = "The format of the log file to analyze.";
    };

    reportTitle = lib.mkOption {
      type = types.nullOr types.nonEmptyStr;
      default = null;
      description = "The title of the report webpage.";
    };

    enableNginx = lib.mkEnableOption ''
      Nginx as the reverse proxy for the Goaccess server. If enabled, an Nginx virtual host will
      be created for access to the Goaccess server'';

    nginxEnableSSL = lib.mkEnableOption "SSL for the Nginx reverse proxy";

    serverHost = lib.mkOption {
      type = types.nonEmptyStr;
      description = "The full public domain of the Goaccess server.";
    };

    serverPath = lib.mkOption {
      type = types.nonEmptyStr;
      default = "";
      description = "The path component URL of the Goaccess server. Must be an empty string or end with '/'.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.serverPath == "" || lib.strings.hasSuffix "/" cfg.serverPath;
        message = "The serverPath option is neither an empty string, nor ends with '/'.";
      }
    ];

    users.users.${userName} = {
      isSystemUser = true;
      group = userName;
      home = cfg.dataDir;
      createHome = true;
    };
    users.groups.${userName} = { };
    users.users."${nginxCfg.user}" = lib.mkIf cfg.enableNginx {
      extraGroups = [ userName ];
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.reportDir}/ 750 ${userName} ${userName}"
      "Z ${cfg.reportDir} 750 ${userName} ${userName}"
    ];

    systemd.services."${serviceName}" = {
      enable = true;
      description = "${serviceName} real-time dashboard service";
      restartIfChanged = true;
      restartTriggers = [ pkgs.goaccess ];
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        RestartSec = "5s";
        User = userName;
        Group = userName;
        WorkingDirectory = cfg.dataDir;
        Type = "simple";
        ExecStart = ''
          ${cfg.package}/bin/goaccess --log-file=${cfg.logFilePath} --log-format=${cfg.logFileFormat} \
            --anonymize-ip --persist --restore --db-path=${cfg.dataDir} --keep-last=${toString cfg.dataRetentionDays} \
            --all-static-files --real-time-html \
            ${if cfg.reportTitle != null then "--html-report-title=\"${cfg.reportTitle}\"" else ""} \
            --output=${cfg.reportDir}/index.html --addr=127.0.0.1 --port=${toString cfg.port} \
            --ws-url=wss://${cfg.serverHost}:443/${cfg.serverPath}ws --origin=https://${cfg.serverHost}'';

        AmbientCapabilities = [ ];
        CapabilityBoundingSet = [
          "~CAP_RAWIO"
          "~CAP_MKNOD"
          "~CAP_AUDIT_CONTROL"
          "~CAP_AUDIT_READ"
          "~CAP_AUDIT_WRITE"
          "~CAP_SYS_BOOT"
          "~CAP_SYS_TIME"
          "~CAP_SYS_MODULE"
          "~CAP_SYS_PACCT"
          "~CAP_LEASE"
          "~CAP_LINUX_IMMUTABLE"
          "~CAP_IPC_LOCK"
          "~CAP_BLOCK_SUSPEND"
          "~CAP_WAKE_ALARM"
          "~CAP_SYS_TTY_CONFIG"
          "~CAP_MAC_ADMIN"
          "~CAP_MAC_OVERRIDE"
          "~CAP_NET_ADMIN"
          "~CAP_NET_BROADCAST"
          "~CAP_NET_RAW"
          "~CAP_SYS_ADMIN"
          "~CAP_SYS_PTRACE"
          "~CAP_SYSLOG"
        ];
        DevicePolicy = "closed";
        KeyringMode = "private";
        LockPersonality = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "full";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
      };
    };

    services.nginx = lib.mkIf cfg.enableNginx {
      enable = true;
      virtualHosts."${cfg.serverHost}" = {
        forceSSL = cfg.nginxEnableSSL;
        enableACME = cfg.nginxEnableSSL;
        locations = {
          "/${cfg.serverPath}" = {
            alias = "${cfg.reportDir}/";
            extraConfig = ''
              add_header Cache-Control 'private no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0';
              if_modified_since off;
              expires off;
              etag off;
            '';
          };
          "/${cfg.serverPath}ws" = {
            proxyPass = "http://127.0.0.1:${toString cfg.port}";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
}
```

<center><em>goaccess.nix</em></center>

</details>

This is how you can use it to set up a Goaccess service that analyzes an Nginx server access log, and exposes the Goaccess server over Nginx acting as a reverse proxy.

```nix
{ config }:
{
  imports = [ ./goaccess.nix ];

  config = {
    services.goaccess = {
      enable = true;
      logFilePath = "/var/log/nginx/access.log";
      logFileFormat = "COMBINED";
      enableNginx = true;
      nginxEnableSSL = true;
      serverHost = "goaccess.example.net";
    };

    # Add goaccess user to nginx group so that goaccess server can access nginx logs.
    users.users.${config.services.goaccess.userName}.extraGroups = [ config.services.nginx.group ];
  };
}
```

After deploying this, the Goaccess report will be available at <https://goaccess.example.net>.

That's all for setting up a Goaccess server on NixOS. I hope this helps someone. If you have any questions or suggestions, please feel free to leave a comment. Thanks for reading!

Like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/114371114365696704){:class="mastodon-link"}.
