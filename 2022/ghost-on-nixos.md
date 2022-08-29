---
date: 2022-08-29
tags: nix self-hosting
---

# Hosting a Ghost blog on NixOS

[Ghost](https://github.com/TryGhost/Ghost) is a popular open-source and self-hostable blogging
platform. [NixOS](https://nixos.org/) is a linux distribution that is well suited for self-hosting
because it can be configured entirely declaratively, and has thousands of
[packages](https://search.nixos.org/packages) and hundreds of pre-configured
[services](https://search.nixos.org/options?channel=22.05&from=0&size=50&sort=relevance&type=packages&query=services.)
available. Unfortunately, Ghost is not one of them. So, when I wanted to host a Ghost blog on NixOS
(with MySQL as the database backend), I had to write the packaging and service configuration by myself.
I'm sharing the same in this note.

## Packaging Ghost for NixOS

Ghost comes with the `ghost-cli` tool for easy installation and setup. We use the same for packaging Ghost.

```nix
{ pkgs }:

let
  pname = "ghost";
  version = "5.8.0";
in pkgs.stdenv.mkDerivation {
  inherit pname version;
  buildInputs = with pkgs; [ nodejs yarn vips ];
  ghostCliVersion = "1.21.1";
  builder = ./builder.sh;
}
```

<center><em>ghost/default.nix</em></center>

```bash
source "$stdenv"/setup

export HOME=$(mktemp -d)
npm install --loglevel=info --logs-max=0 "ghost-cli@$ghostCliVersion"

mkdir --parents "$out"/
node_modules/ghost-cli/bin/ghost install "$version" --db=sqlite3 \
  --no-enable --no-prompt --no-stack --no-setup --no-start --dir "$out"
```

<center><em>ghost/builder.sh</em></center>

First we install `ghost-cli` from [NPM](https://www.npmjs.com/), and then use it to install Ghost.
We choose to not configure the web server, database or process manager for Ghost because we do that
ourselves for NixOS in the next section.

## Running Ghost as a service on NixOS

Next, we write the NixOS service for Ghost. The code below is commented for details.

```nix
{ options, lib, config, pkgs, ... }:
let
  # domain for the Ghost blog
  serverName = "blog.example.net";
  # port on which the Ghost service runs
  port = 1357;
  # user used to run the Ghost service
  userName = builtins.replaceStrings [ "." ] [ "_" ] serverName;
  # MySQL database used by Ghost
  dbName = userName;
  # MySQL user used by Ghost
  dbUser = userName;
  # directory used to save the blog content
  dataDir = "/var/lib/${userName}";
  # Ghost package we created in the section above
  ghost = import ./ghost { inherit pkgs; };
  # script that sets up the Ghost content directory
  setupScript = pkgs.writeScript "${serverName}-setup.sh" ''
    #! ${pkgs.stdenv.shell} -e
    chmod g+s "${dataDir}"
    [[ ! -d "${dataDir}/content" ]] && cp -r "${ghost}/content" "${dataDir}/content"
    chown -R "${userName}":"${userName}" "${dataDir}/content"
    chmod -R +w "${dataDir}/content"
    ln -f -s "/etc/${serverName}.json" "${dataDir}/config.production.json"
    [[ -d "${dataDir}/current" ]] && rm "${dataDir}/current"
    ln -f -s "${ghost}/current" "${dataDir}/current"
    [[ -d "${dataDir}/content/themes/casper" ]] && rm "${dataDir}/content/themes/casper"
    ln -f -s "${ghost}/current/content/themes/casper" "${dataDir}/content/themes/casper"
  '';

  databaseService = "mysql.service";

  serviceConfig = config.services."${serverName}";
  options = { enable = lib.mkEnableOption "${serverName} service"; };
in {
  options.services.${serverName} = options;
  config = lib.mkIf serviceConfig.enable {
    # Creates the user and group
    users.users.${userName} = {
      isSystemUser = true;
      group = userName;
      createHome = true;
      home = dataDir;
    };
    users.groups.${userName} = { };

    # Creates the Ghost config
    environment.etc."${serverName}.json".text = ''
      {
        "url": "https://${serverName}",
        "server": {
          "port": ${port},
          "host": "0.0.0.0"
        },
        "database": {
          "client": "mysql",
          "connection": {
            "host": "localhost",
            "user": "${dbUser}",
            "database": "${dbName}",
            "password": "",
            "socketPath": "/run/mysqld/mysqld.sock"
          }
        },
        "mail": {
          "transport": "sendmail"
        },
        "logging": {
          "transports": ["stdout"]
        },
        "paths": {
          "contentPath": "${dataDir}/content"
        }
      }
    '';

    # Sets up the Systemd service
    systemd.services."${serverName}" = {
      enable = true;
      description = "${serverName} ghost blog";
      restartIfChanged = true;
      restartTriggers =
        [ ghost config.environment.etc."${serverName}.json".source ];
      requires = [ databaseService ];
      after = [ databaseService ];
      path = [ pkgs.nodejs pkgs.vips ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = userName;
        Group = userName;
        WorkingDirectory = dataDir;
        # Executes the setup script before start
        ExecStartPre = setupScript;
        # Runs Ghost with node
        ExecStart = "${pkgs.nodejs}/bin/node current/index.js";
        # Sandboxes the Systemd service
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
      environment = { NODE_ENV = "production"; };
    };

    # Sets up the blog virtual host on NGINX
    services.nginx.virtualHosts.${serverName} = {
      # Sets up Lets Encrypt SSL certificates for the blog
      forceSSL = true;
      enableACME = true;
      locations."/" = { proxyPass = "http://127.0.0.1:${port}"; };
      extraConfig = ''
        charset UTF-8;

        add_header Strict-Transport-Security "max-age=2592000; includeSubDomains" always;
        add_header Referrer-Policy "strict-origin-when-cross-origin";
        add_header X-Frame-Options "SAMEORIGIN";
        add_header X-XSS-Protection "1; mode=block";
        add_header X-Content-Type-Options nosniff;
      '';
    };

    # Sets up MySQL database and user for Ghost
    services.mysql = {
      ensureDatabases = [ dbName ];
      ensureUsers = [{
        name = dbUser;
        ensurePermissions = { "${dbName}.*" = "ALL PRIVILEGES"; };
      }];
    };
  };
}
```

<center><em>blog.example.net.nix</em></center>

Now, we can import the above service into our favourite NixOS deployment tool (like
[NixOps](https://nixos.org/nixops) or [Morph](https://github.com/DBCDK/morph)), and enable the Ghost
service:

```nix
{
  webserver =
    { config, pkgs, ... }: {
      import = [ ./blog.example.net.nix ];
      deployment.targetHost = "blog.example.net";

      # Enables NGINX
      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
      };

      # Enables MySQL
      services.mysql = {
        enable = true;
        package = pkgs.mariadb;
      };

      # Enables the Ghost blog service
      services."blog.example.net".enable = true;
    };
}
```

<center><em>deployment.nix</em></center>

That's it. After deployment, we can start blogging on Ghost on NixOS.
