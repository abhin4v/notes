---
date: 2022-11-07
tags: nix programming
---

# TIL: How to patch Nixpkgs

Today I woke up to a [broken](https://github.com/NixOS/nixpkgs/issues/198736) `cloudflare-dyndns` package on nixpkgs. Fortunately, some good fellow already has a [PR](https://github.com/NixOS/nixpkgs/pull/198739) to fix the issue. Unfortunately, the PR has not been merged yet and I'm too impatient.

So I did what had to be done. I learned how to patch nixpkgs so that I can build a patched `cloudflare-dyndns`. It was actually quiet easy.

First, I downloaded the patch from Github:

```
curl "https://github.com/NixOS/nixpkgs/pull/198739.patch" \
  -Lo packages/nixos-nixpkgs-198739.patch
```

Then, I changed `flake.nix` outputs [from](https://github.com/abhin4v/nix-managed-macbook/blob/7fc71d2237813b48afdad75bedc1e4eb21a8b1f5/flake.nix):

```nix
outputs = inputs@{ nixpkgs, home-manager, ... }:
  let
    system = "x86_64-darwin";
    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };
  in {
    homeConfigurations.abhinav =
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = { inherit inputs; };
      };
  };
```

[to](https://github.com/abhin4v/nix-managed-macbook/blob/7303c6ec45f76a5f3ff1cc5f412006c58fe0bdb1/flake.nix):

```nix
outputs = inputs@{ nixpkgs, home-manager, ... }:
  let
    system = "x86_64-darwin";
    nixpkgs-patched =
      (import nixpkgs { inherit system; }).applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs;
        patches = [ ./packages/nixos-nixpkgs-198739.patch ];
      };
    pkgs = import nixpkgs-patched {
      inherit system;
      config = { allowUnfree = true; };
    };
  in {
    homeConfigurations.abhinav =
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = { inherit inputs; };
      };
  };
```

That's it. It built fine, and I went on to write this note.
