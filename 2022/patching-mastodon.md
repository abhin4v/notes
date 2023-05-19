---
date: 2022-12-20
tags: self-hosting nix fediverse programming
---

# Patching Mastodon on NixOS for Removing User Avatars and Headers

[Mastodon](https://joinmastodon.org/) is the most used software for running an [Fediverse](https://en.wikipedia.org/wiki/Fediverse) server. However, it is notorious for its high resource usage. In particular, it caches a lot of media from remote servers, which can be a problem for machines with limited storage.

[Mastodon docs](https://docs.joinmastodon.org/admin/setup/#cleanup) recommend setting up a scheduled task to remove old media. Till version 4.0, this can be used to remove remote media and preview cards. But it does not have support for removing old user avatars and headers.

A [pull request](https://github.com/mastodon/mastodon/pull/22149) just landed in Mastodon that adds support for removing old user avatars and headers. However, it is not yet released. My [instance](https://fantastic.earth) was running critically low on storage, So, I decided to patch it to add this feature. It is running on [NixOS](https://nixos.org/), so it was a simple task.

## Patching Mastodon Source

First step is to patch the Mastodon source to get the PR's changes. Fortunately, the [Mastodon Nix package](https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/mastodon/default.nix) allows us to specify the source for building Mastodon. So I created a `mastodon-source.nix` file with the following content:

```nix
{ fetchgit, fetchpatch, applyPatches }: let
  src = fetchgit {
    url = "https://github.com/mastodon/mastodon.git";
    rev = "v4.0.2";
    sha256 = "1szb11bss66yvh8750pzib3r0w1fm9h84sf5daqsnbm871hgzlw0";
  };
  remove-header-avatar-patch = fetchpatch {
    url = "https://github.com/mastodon/mastodon/pull/22149.patch";
    sha256 = "sha256-/BqyVrRAMYgx8Edn/6GBR2Ne7tjttk2Vy0J42l9CB+I=";
  };
in applyPatches {
  inherit src;
  patches = [ remove-header-avatar-patch ];
}
```

It fetches the source of Mastodon v4.0.2 from Github and the patch file for the PR. Then it applies the patch on the source.

## Configuring the Mastodon Service

Next step is to configure the Mastodon NixOS service for using the patched source:

```nix
{ lib, config, pkgs, ... }:

{
  services.mastodon = {
    enable = true;
    package = pkgs.callPackage "${pkgs.path}/pkgs/servers/mastodon" {
      srcOverride = pkgs.callPackage ./mastodon-source.nix { };
    };
    configureNginx = true;
    localDomain = "fantastic.earth";
    mediaAutoRemove = {
      enable = true;
      startAt = "hourly";
      olderThanDays = 1;
    };
  };

  systemd.services.mastodon-media-auto-remove.script =
    let cfg = config.services.mastodon;
    in lib.mkIf cfg.mediaAutoRemove.enable
      (let olderThanDays = toString cfg.mediaAutoRemove.olderThanDays;
      in "${cfg.package}/bin/tootctl media remove --prune-profiles --days=${olderThanDays}");
}
```

I configured the Mastodon service to use the patched source by passing the `package` option. I already had the `mediaAutoRemove` option configured. I appended the `mastodon-media-auto-remove` Systemd service script to invoke the right command for removing old user avatars and headers.

## Conclusion

That's it. The disk usage of my Mastodon instance is now under control:

![Disk usage over time](https://fantastic.earth/system/media_attachments/files/109/544/832/069/621/724/original/e3eab8073385853e.png "Disk usage over time")
[Disk usage over time](https://fantastic.earth/system/media_attachments/files/109/544/832/069/621/724/original/e3eab8073385853e.png)

I hope this helps someone else who is running a Mastodon instance on NixOS.

Like, share and reply to this post on the [Fediverse](https://fantastic.earth/@abnv/109546513445577883){:class="mastodon-link"}.
