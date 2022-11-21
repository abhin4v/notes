---
date: 2022-08-27
tags: nix self-hosting programming
---

# Creating NixOS custom image for Digital Ocean VPS

Recently, I decided to move my [Digital Ocean](https://www.digitalocean.com/) (DO) VPS to [NixOS](https://nixos.org/).
The first step to do so is to create a custom NixOS image because NixOS is not an OS option available
on DO. I use a MacBook so the process to create the image is a bit convoluted. So I going to record
them in this note.

We use [Niv](https://github.com/nmattia/niv) to pin the NixOS version and revision. We also use
[QEMU](https://www.qemu.org/) to spin up a NixOS virtual machine on MacOS because the NixOS image can
only be created on a NixOS machine.

First, we create a directory for our image, and set up Niv:

```shell
$ mkdir nix-image
$ cd nix-image
$ nix-shell -p niv
$ niv init
$ niv drop nixpkgs
$ niv add NixOS/nixpkgs -n nixpkgs --branch nixos-22.05
```

22.05 is the latest stable NixOS version at the time of writing this post. Feel free to substitute it with your
preferred version.

Next, we create two nix files, one for setting up the Nix shell:

```nix
let
  pkgs = import (import ./nix/sources.nix).nixpkgs { overlays = [ ]; config = { }; };
in pkgs.mkShell {
  buildInputs = with pkgs; [ curl jq qemu ];
}
```
<center><em>shell.nix</em></center>

And other for creating the image:

```nix
let
  pkgs = import (import ./nix/sources.nix).nixpkgs { overlays = [ ]; config = { }; };
  config = {
      imports =
        [ "${pkgs.path}/nixos/modules/virtualisation/digital-ocean-image.nix" ];
    };
in (pkgs.nixos config).digitalOceanImage
```

<center><em>image.nix</em></center>

Next, we create and start a NixOS virtual machine on QEMU:

```shell
$ nix-shell
$ export NIXOS=$(jq -r ".nixpkgs.branch" nix/sources.json)
$ curl -L "https://channels.nixos.org/$NIXOS/latest-nixos-minimal-x86_64-linux.iso" \
    -o "/tmp/$NIXOS-minimal-x86_64-linux.iso"
$ qemu-system-x86_64 -machine q35,accel=hvf:tcg -boot d \
    -cdrom "/tmp/$NIXOS-minimal-x86_64-linux.iso" -m 12G -cpu host -smp 12 \
    -virtfs local,path=`pwd`,mount_tag=vol0,security_model=mapped-xattr
```

Next steps are run inside the virtual machine. We wait for it to boot up, and for the command prompt
to appear. On the prompt, we run the following commands to create the NixOS custom image for
DO:

```shell
$ mkdir workdir
$ sudo mount -t 9p -o trans=virtio,version=9p2000.L vol0 workdir
$ cd workdir/
$ sudo cp `nix-build --no-out-link image.nix`/nixos.qcow2.gz .
$ shutdown now
```

That's it. If everything went well, there should be a `nixos.qcow2.gz` file in the `nix-image` directory.
We can upload this file as a [custom image](https://cloud.digitalocean.com/images/custom_images) on DO,
and use it to create our NixOS VPS.

Usually, the above code is part of the project that contains the rest of the configuration for the VPS as well.
If you are doing this as an isolated activity, you can skip Niv, and just start a Nix shell with QEMU.
