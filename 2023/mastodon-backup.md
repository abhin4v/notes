---
date: 2023-05-13
tags: self-hosting nix fediverse programming
---

# Automating Mastodon Backups with NixOS and Syncthing

Six months ago, a bunch of my friends and I started the Mastodon instance [fantastic.earth](https://fantastic.earth). Till now, we were kind of experimenting with it, but now that we are accepting new users, I wanted to make sure that we have a backup strategy for the Mastodon data. This post describes how I set up the backup system using [NixOS](https://nixos.org/), which we use to manage the server.

There are two kinds of data that we need to backup for Mastodon: the database and the media files. Mastodon uses [PostgreSQL](https://www.postgresql.org/) for database, and the media files are stored in a directory on the server. Fortunately, NixOS already has all the tools we need to do this, we just need to assemble them together.

The idea is to dump the Mastodon database hourly using [`pg_dump`](https://www.postgresql.org/docs/15/app-pgdump.html), encrypt it using [GnuPG](https://gnupg.org/), and then back it up to one of more remote locations using [Syncthing](https://syncthing.net/). The media files are backed up using Syncthing as well, but they are not encrypted. I chose this combination because I found it to be simpler and easier to understand that other options like [Borg Backup](https://www.borgbackup.org/), [Restic](https://restic.net/), or [Postgres WAL archiving](https://www.postgresql.org/docs/15/continuous-archiving.html).

## Setting Up the PostgreSQL Backup

Here's the Nix module that I wrote to set this up:

<details markdown="1">
<summary>The NixOS module</summary>

```nix
{ lib, config, pkgs, ... }:

let
  mastodonCfg = config.services.mastodon;
  syncthingCfg = config.services.syncthing;
  dbName = mastodonCfg.database.name;
  pgBackupDir = config.services.postgresqlBackup.location;
  backupFileLocation = "${pgBackupDir}/${dbName}.sql.gz";
  encBackupFileLocation = "${backupFileLocation}.enc";
  backupEncPassphraseFile = "/var/lib/postgresql/backup-enc-passphrase";
  backupDir = "${syncthingCfg.dataDir}/Backups";
  mastodonFilesDir = "/var/lib/mastodon/public-system";
  devices = [ ];
in
{
  services.postgresqlBackup = {
    enable = true;
    startAt = "*-*-* *:40:00";
    databases = [ dbName ];
  };

  systemd.services."postgresqlBackup-${dbName}".serviceConfig = {
    ExecStartPost = ''
      /bin/sh -c '${pkgs.gnupg}/bin/gpg -c --batch --yes \
        --passphrase-file ${backupEncPassphraseFile} \
        --output ${encBackupFileLocation} ${backupFileLocation} && \
      echo "DB dump encrypted successfully" && \
      chgrp ${syncthingCfg.group} ${encBackupFileLocation} && \
      mv ${encBackupFileLocation} ${backupDir}/ && \
      echo "DB dump moved to the backup directory"'
    '';
  };

  services.syncthing = {
    enable = true;
    devices = builtins.listToAttrs (builtins.map
      (dev: {
        name = dev;
        value = { id = builtins.readFile "${syncthingCfg.dataDir}/${dev}-id"; };
      })
      devices);
    folders = {
      "${backupDir}" =
        let folderId = "db-dump";
        in {
          id = folderId;
          label = folderId;
          devices = devices;
          rescanInterval = 300;
          type = "sendonly";
          versioning = {
            type = "simple";
            params.keep = "6";
          };
        };
      "${mastodonFilesDir}" =
        let folderId = "mastodon-files";
        in {
          id = folderId;
          label = folderId;
          devices = devices;
          rescanInterval = 60;
          type = "sendonly";
        };
    };
    extraFlags = [ "--no-upgrade" "--no-restart" ];
  };

  systemd.tmpfiles.rules = [
    "z ${syncthingCfg.dataDir} 0750 ${syncthingCfg.user} ${syncthingCfg.group}"
    "d ${backupDir} 0775 ${syncthingCfg.user} ${syncthingCfg.group}"
    "z ${mastodonFilesDir} 0770 ${mastodonCfg.user} ${mastodonCfg.group}"
    "z ${backupEncPassphraseFile} 400 postgres postgres"
  ];

  users.users = {
    postgres.extraGroups = [ syncthingCfg.group ];
    ${syncthingCfg.user}.extraGroups = [ mastodonCfg.group ];
  };
}
```
</details>

In the NixOS module above, first, we enable the [PostgreSQL Backup service](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/backup/postgresql-backup.nix) to dump the Mastodon database every hour. Next, we use Systemd's `ExecStartPost` setting to run a shell script after the database dump is taken. This script encrypts the database dump using GnuPG, and moves it to the Syncthing backup directory. The passphrase for the GnuPG encryption is stored in a file that is readable only by the postgresql user.

Next, we configure the [Syncthing service](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/syncthing.nix) to backup the Mastodon media files and the `Backup` directory containing the encrypted database dump to all the devices in the `devices` list. The directories are set to be `sendonly` so that any changes made to them on the backup devices are not synced back to the Mastodon server. The `Backup` directory is also configured to keep the last 6 versions of the database dump.

Finally, we set up file permissions and user groups so that the PostgreSQL user can write to the Syncthing backup directory, and the Syncthing user can read from the Mastodon files directory. That's all for the NixOS module.

We also put a strong password in the file `/var/lib/postgresql/backup-enc-passphrase` that is used to encrypt the database dump.

## Setting Up Syncthing

Before enabling the module we need to set up the Syncthing, the instructions for which are as follows:

1. Get the Mastodon server's Syncthing device ID by running the following commands on the server:
    ```
    sudo su syncthing -s /bin/sh
    $(ls /nix/store/*syncthing*/bin/syncthing) --device-id
    ```
1. Copy the device ID, and use it to add a remote device in the Syncthing web UI of the backup device.
    - Check the "Auto Accept" checkbox in the Sharing tab.

1. Copy the device ID of the backup device, and use it to add a remote device in the Mastodon server:
    - While still logged in as the `syncthing` user after running the previous command on the server, create a file `/var/lib/syncthing/<backup-device-name>-id`, put the backup device's device ID into it, and change the file's mode to 400:
    ```
    echo <backup-device-id> > /var/lib/syncthing/<backup-device-name>-id
    chmod 400 /var/lib/syncthing/<backup-device-name>-id
    ```
  1. Edit the Nix module above to add the `<backup-device-name>` in the line containing `devices = [ ]`.
1. Deploy the module on the Mastodon server. This will start the Syncthing service on the server.
1. In the Syncthing web UI of the backup device:
    1. accept the syncing for `db-dump` and `mastodon-files` folders,
    1. click the "Edit" button in the `db-dump` folder's section, and:
        1. change the "File Versioning" option in the "File Versioning" tab to "Simple File Versioning",
        1. change the "Keep Versions" option to "6", and
        1. change the "Folder Type" option in the "Advanced" tab to "Receive Only".
    1. click the "Edit" button in the `mastodon-files` folder's section, and:
        1. add the ignore pattern "cache/**" in the "Ignore Patterns" tab, and
        1. change the "Folder Type" option in the "Advanced" tab to "Receive Only".

After this, the Syncthing backup should be set up and working as expected.

## Restoring the Database

To restore the database, we need to decrypt the database dump, and then restore it using the `pg_restore` command:

```
gpg -d --batch --yes \
  --passphrase-file /var/lib/postgresql/backup-enc-passphrase \
  --output /tmp/mastodon.sql.gz /var/lib/syncthing/Backup/mastodon.sql.gz.enc
pg_restore --clean --if-exists --dbname mastodon /tmp/mastodon.sql.gz
```

That's all for setting up the Syncthing backup for Mastodon. I hope this helps someone. If you have any questions or suggestions, please feel free to leave a comment. Thanks for reading!

You can like, share, or comment on this post on [Mastodon](https://fantastic.earth/@abnv/110362391871164484).
