---
date: 2022-11-20
tags: nix self-hosting programming fediverse
---

# Self-hosting GoToSocial on NixOS

[GoToSocial](https://gotosocial.org/) is an [ActivityPub](https://activitypub.rocks/) server. It is a lightweight alternative to [Mastodon](https://joinmastodon.org/), and very suitable for self-hosting single user instances[^fn3]. Though it does not have all of Mastodon's features (it's getting there), it is already quite useable.

[^fn3]: GoToSocial provides only the backend server. You'll need a frontend as well. [Pinafore](https://pinafore.social/) on web, or [Tusky](https://tusky.app/) on Android work well.

I decided to self-host GoToSocial on my VPS (which runs [NixOS](https://nixos.org)), pointing to my own domain. So instead of having a fediverse address like _@abnv@mastodon.social_, I'd have an address _@abnv@soc.abnv.me_[^fn1].

[^fn1]: It is also possible to have an address like `@abnv@abnv.me` with some [extra configuration](https://docs.gotosocial.org/en/latest/installation_guide/advanced/#can-i-host-my-instance-at-fediexampleorg-but-have-just-exampleorg-in-my-username).

First, we package GoToSocial for NixOS:

```nix
{ pkgs }:

let
  pname = "gotosocial";
  version = "0.5.2";
in pkgs.stdenv.mkDerivation {
  inherit pname version;
  src = pkgs.fetchzip {
    url = "https://github.com/superseriousbusiness/${pname}/releases/download/v${version}/${pname}_${version}_linux_amd64.tar.gz";
    sha256 = "sha256-AfHXsQm0NHaqoyv7Jg6LHqzHmuahBiyAqHIBbY6rDJg=";
    stripRoot = false;
  };
  installPhase = ''
    mkdir -p "$out"/bin
    mv gotosocial $out/bin/
    mv web $out/
  '';
}
```
<center><em>nix/packages/gotosocial.nix</em></center>

Next, we write a minimal[^fn2] NixOS module to run GoToSocial:

[^fn2]: There are couple of ways we can improve this module:

    - By adding [sandboxing](https://www.digitalocean.com/community/tutorials/how-to-sandbox-processes-with-systemd-on-ubuntu-20-04) for the GoToSocial process.
    - By adding [automatic restarts](https://www.freedesktop.org/software/systemd/man/systemd.service.html#Restart=) on failures.
    - By adding a separate route in Nginx for better caching of public assets.

```nix
{ options, lib, config, pkgs, ... }:

let
  serverName = "soc.abnv.me";
  port = 9755;
  userName = "gotosocial";
  serviceName = userName;
  dataDir = "/var/lib/${userName}";
  pkg = import ./nix/packages/gotosocial.nix { inherit pkgs; };
  yaml = pkgs.formats.yaml { };
  configFile = yaml.generate "${serviceName}.yaml" {
    host = serverName;
    bind-address = "localhost";
    port = port;
    db-type = "sqlite";
    db-address = "${dataDir}/data/sqlite.db";
    web-template-base-dir = "${pkg}/web/template/";
    web-asset-base-dir = "${pkg}/web/assets/";
    accounts-registration-open = false;
    storage-local-base-path = "${dataDir}/media";
  };
  serviceConfig = config.services."${serverName}";
  options = { enable = lib.mkEnableOption "${serverName} service"; };
in {
  options.services.${serverName} = options;
  config = lib.mkIf serviceConfig.enable {
    users.users.${userName} = {
      isSystemUser = true;
      group = userName;
      createHome = true;
      home = dataDir;
    };
    users.groups.${userName} = { };

    systemd.tmpfiles.rules = [
      "d ${dataDir}/data 1770 ${userName} ${userName}"
      "d ${dataDir}/media 1770 ${userName} ${userName}"
    ];

    systemd.services.${serviceName} = {
      enable = true;
      description = "${serviceName} service";
      restartIfChanged = true;
      restartTriggers = [ configFile pkg ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = userName;
        Group = userName;
        WorkingDirectory = dataDir;
        ExecStart = "${pkg}/bin/gotosocial --config-path ${configFile} server start";
      };
    };

    services.nginx.virtualHosts.${serverName} = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${port}";
        extraConfig = ''
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection $connection_upgrade;
        '';
      };
      extraConfig = ''
        client_max_body_size 40M;
      '';
    };
  };
}
```

First, we set up a user and a group for GoToSocial, and the directories for data and uploaded media. Then, we create the config file. We are using [SQLite](https://www.sqlite.org/) as the database here[^fn4]. Next, we set up a [Systemd](https://systemd.io/) service to run the GoToSocial binary with the created config file. Finally, we set up an [Nginx](https://nginx.org/) virtual host to proxy requests to the GoToSocial process.

[^fn4]: GoToSocial also supports [PostgreSQL](https://www.postgresql.org/).

When we import and enable this module in our NixOS configuration, we have a self-hosted GoToSocial instance running. I have mine at [soc.abnv.me](https://soc.abnv.me), and you can follow me at [*@abnv@abnv.me*](https://soc.abnv.me/@abnv).

That's it for now. Happy #twittermigration.

[Like](https://types.pl/interact/109375589712954339?type=favourite), [repost](https://types.pl/interact/109375589712954339?type=reblog), or [reply](https://types.pl/interact/109375589712954339?type=reply) to this note on Fediverse.
