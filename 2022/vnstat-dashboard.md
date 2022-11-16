---
date: 2022-11-16
tags: nix self-hosting
---

# Monitoring VPS Network Usage with vnStat and NixOS

I self-host a bunch of services on a VPS. To monitor the network usage of the VPS, I use
[`vnstat`](https://humdi.net/vnstat/), a simple network monitoring tool. After enabling the `vnstat`
service on the VPS, I can check the network usage on the terminal:

```
$ vnstat

                      rx      /      tx      /     total    /   estimated
 ens3:
       2022-10      8.07 GiB  /    4.15 GiB  /   12.21 GiB
       2022-11      5.81 GiB  /    2.64 GiB  /    8.45 GiB  /   16.33 GiB
     yesterday     74.16 MiB  /   90.43 MiB  /  164.59 MiB
         today      1.23 GiB  /   80.63 MiB  /    1.31 GiB  /    2.47 GiB

 ens4:
       2022-10     87.94 KiB  /   65.29 KiB  /  153.23 KiB
       2022-11     44.27 KiB  /   34.73 KiB  /   79.00 KiB  /     --
     yesterday      2.84 KiB  /    1.86 KiB  /    4.71 KiB
         today      2.02 KiB  /    3.57 KiB  /    5.59 KiB  /       8 KiB
```

I can also run `vnstat` with different arguments to show me the data for different time periods:

```
$ vnstat -d 5

 ens3  /  daily

          day        rx      |     tx      |    total    |   avg. rate
     ------------------------+-------------+-------------+---------------
     2022-11-12   163.62 MiB |  177.99 MiB |  341.62 MiB |   33.17 kbit/s
     2022-11-13   240.53 MiB |  104.76 MiB |  345.29 MiB |   33.52 kbit/s
     2022-11-14   215.96 MiB |  200.89 MiB |  416.85 MiB |   40.47 kbit/s
     2022-11-15    74.16 MiB |   90.43 MiB |  164.59 MiB |   15.98 kbit/s
     2022-11-16     1.23 GiB |   81.00 MiB |    1.31 GiB |  244.39 kbit/s
     ------------------------+-------------+-------------+---------------
     estimated      2.31 GiB |  152.44 MiB |    2.46 GiB |
```

However, what I want is a dashboard with this data, that I can access using a web browser instead of
logging in to the VPS. So I wrote a [NixOS](https://nixos.org/) service for it.

Here's how my dashboard looks:

![vnStat Dashboard](/files/vnstat-dashboard/dashboard.png "vnStat Dashboard")
[vnStat Dashboard](/files/vnstat-dashboard/dashboard.png "vnStat Dashboard")

`vnstat` comes with a tool `vnstati` that can be used to generate images containing the network usage data. The
NixOS service I wrote invokes `vnstati` every five minutes to dump various images in a directory, and serves
these images through an [Nginx](https://nginx.org/) web server.

Here's is the NixOS module code:

```nix
{ lib, config, pkgs, ... }:

let
  vnstatUser = "vnstatd";
  vnstatImageDir = "/var/www/vnstat";
  vnstatDashboardFile = pkgs.writeText "vnstat-dashboard.html" ''
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>vnStat dashboard</title>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width,initial-scale=1" />
    <noscript>
      <meta http-equiv="refresh" content="60" />
    </noscript>
    <style>
    body { max-width: 1341px; margin: auto; padding: 5px; }
    div { column-count: 2; column-width: 668px; column-gap: 5px; text-align: center }
    img { max-width: 100%; min-width: 320px; }
    </style>
  </head>
  <body>
    <div>
      <img src="vnstat-s.png"> <img src="vnstat-5g.png"> <img src="vnstat-hg.png">
      <img src="vnstat-h.png"> <img src="vnstat-d.png"> <img src="vnstat-t.png">
      <img src="vnstat-m.png"> <img src="vnstat-y.png">
    </div>
    <script>
      setInterval(function() {
          for (let image of document.images) {
            image.src = new URL(image.src).pathname + "?t=" + new Date().getTime();
          }
      }, 60000);
    </script>
  </body>
</html>'';

  serverName = "vnstat.abnv.me";
  serviceConfig = config.services."${serverName}";
  options = { enable = lib.mkEnableOption "${serverName} service"; };
in {
  options.services.${serverName} = options;
  config = lib.mkIf serviceConfig.enable {
    services.vnstat.enable = true;

    systemd.tmpfiles.rules = [
      "d ${vnstatImageDir} 1775 ${vnstatUser} ${vnstatUser}"
      "L+ ${vnstatImageDir}/index.html - - - - ${vnstatDashboardFile}"
      "Z ${vnstatImageDir} 755 ${vnstatUser} ${vnstatUser}"
    ];

    systemd.services."vnstat-image-gen" = {
      enable = true;
      description = "vnstat image generator service";
      startAt = "*:0/5:00";
      restartIfChanged = true;
      after = [ "vnstat.service" ];
      path = [ pkgs.vnstat ];
      serviceConfig = {
        User = vnstatUser;
        Group = vnstatUser;
        WorkingDirectory = vnstatImageDir;
        Type = "oneshot";
        AmbientCapabilities = [ ];
        CapabilityBoundingSet = [ ];
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
        RestrictAddressFamilies = [ ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
      };
      script = ''
        vnstati --style 1 -L -s -o vnstat-s.png
        vnstati --style 1 -L --fivegraph 576 218 -o vnstat-5g.png
        vnstati --style 1 -L -hg -o vnstat-hg.png
        vnstati --style 1 -L -h 24 -o vnstat-h.png
        vnstati --style 1 -L -d 30 -o vnstat-d.png
        vnstati --style 1 -L -t 10 -o vnstat-t.png
        vnstati --style 1 -L -m 12 -o vnstat-m.png
        vnstati --style 1 -L -y 5 -o vnstat-y.png
      '';
    };

    systemd.timers."vnstat-image-gen".timerConfig = {
      User = vnstatUser;
      Group = vnstatUser;
    };

    services.nginx.virtualHosts.${serverName} = {
      root = vnstatImageDir;
      extraConfig = ''
        add_header Cache-Control 'private no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0';
        if_modified_since off;
        expires off;
        etag off;
      '';
    };
  };
}
```

First, we enable the `vnstat` service. Then, we write a [Systemd](https://systemd.io/) service that
runs every five minutes. This service runs the `vnstati` command multiple times with different arguments to
generate a bunch of images. Finally, we configure Nginx to serve the directory containing the images
on a virtual host.

The directory contains an `index.html` file that contains the dashboard code. The dashboard includes
the generated images, and refreshes itself every minute. We add some extra config in Nginx so that
the images are not cached by browsers or proxies.

The way it is written here, the dashboard is publicly accessible. We can use HTTP basic authentication
to put it behind a password.

That's it for now. Happy monitoring.

[Like](https://types.pl/interact/109352440601326791?type=favourite), [repost](https://types.pl/interact/109352440601326791?type=reblog), or [reply](https://types.pl/interact/109352440601326791?type=reply) to this note on Fediverse.
