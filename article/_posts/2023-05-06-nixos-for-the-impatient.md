---
title: NixOS for the Impatient
summary: Using NixOS for a more tractable computing setup.
card: nixos-for-the-impatient.jpg
card_source: |
    neofetch output for my laptop.
---

[NixOS][nixos] is a Linux distribution configured using [Nix][nix]. It is
declarative, meaning that the entire system state can be defined in a single
`.nix` file; and reproducible, meaning you can have multiple computers set up
identically.

[nixos]: https://nixos.org/
[nix]: https://en.wikipedia.org/wiki/Nix_(package_manager)

If this sounds like a bullshit timesink like Arch or Gentoo: it's not. There was
a time when the idea of spending an afternoon typing cryptsetup incantations
into a terminal would have been appealing. That time is past. I don't use
distros that lack a graphical installer (running `fdisk` is frankly beneath my
dignity) or that require extensive maintenance. I'm not building my own kernel
with some bespoke set of paranoid `CFLAGS`. I just want something that works.

NixOS works. It actually works great.

This post is about how I set up NixOS. I don't know Nix best practices and
haven't the time to learn, what I know is my setup works and is easy to
understand, and that's good enough at present.

# Contents

1. [Getting Started](#started)
   1. [Installation](#install)
   1. [Post-Install](#postinstall)
   1. [Installing Packages](#packages)
   1. [Dotfiles](#dotfiles)
1. [A Single-Device Setup](#single)
1. [My Current Setup](#current)
1. [Comparison](#comparison)

# Getting Started {#started}

Head over to [downloads][downloads] and pick a graphical ISO image.

[downloads]: https://nixos.org/download.html#nixos-iso

## Installation {#install}

- installation
    1. Installer is very user-friendly
       - based on calamares
    1. Separates timezone from locale.
        1. I can set timezone to AU and locale to en_US.UTF-8 as God intended.
    1. Erase disk.
        1. With swap, for hibernation.
        1. Full disk encryption out of the box.
        1. Just type in the passphrase.
        1. By default, this is configured to have an infinite number of tries.
            1. I can't express how good this is.
            1. Debian and Ubuntu, by default, give you three tries and then the
                thing locks up and you have to hold down the power button and
                smother your PC. This makes me feel awful.
        1. Dual booting and manual partitioning are a pain. You should
           unironically just buy multiple disks.
    1. one troubleshooting note
       1. when partitioning, make sure the disk says "GPT" rather than "MBR"
       1. an MBR install is severely degraded
       1. assuming the disk is empty,
          1. this is just a matter of opening up gparted and changing the disk's partition table

## Post-Install {#postinstall}

A journey of a thousand lightyears begins with a simple step. Let's take a small
step: changing the hostname.

By default, NixOS's configuration lives in `/etc/nixos`. Eventually we will move
this to a more convenient place. There are two files here: `configuration.nix`
is the main thing we'll use, and `hardware-configuration.nix` is generated for
you by the installer.

First, open up the main configuration file:

```bash
$ sudo nano /etc/nixos/configuration.nix
```

Navigate to `networking.hostName`. The default value is `"nixos"`. Change it to
something you like:

```nix
networking.hostName = "sextant"
```

Apply the configuration with:

```bash
$ sudo nixos-rebuild switch
```

Now log out and log back in (trying to start a new terminal window after
changing hostnames might error, but logging out works). Observe your new hostname.

This is the basic pattern to configuring Nix: you make a change to your config
file, run one command, and the system is updated.

## Installing Packages {#packages}

- Installing Packages
   1. `sudo nano /etc/nixos/configuration.nix`
   1. `users.users.eudoxia`
   1. To install something system-wide, use `environment.systemPackages` instead.

## Dotfiles {#dotfiles}

- Home Manager.
   1. Go to `/etc/nixos/home.nix`
   1. Copy:

     ```
     { config, pkgs, ... }:
     let
       home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
     in
     {
       imports = [
         (import "${home-manager}/nixos")
       ];

       home-manager.users.eudoxia = {
         home.stateVersion = "22.11";
       };
     }
     ```
   1. Then open `configuration.nix`.
   1. Add `./home.nix` to `imports`.
   1. Run `sudo nixos-rebuild switch`.
   1. `the following new units were started: home-manager-eudoxia.service`.
   1. Open `home.nix`.
   1. Home directory config. `home.file` etc.

# A Single-Device Setup {#single}

# My Current Setup {#current}

- Security
- DNS
- Syncthing

# Comparison {#comparison}

My [dotfiles][df] repo is ten years old. Over the years I've tried different
schemes for managing them. What I settled on for the longest chunk of time was a
bash script to install software and then copy some source dotfiles to their
destinations. This works well enough, but for two drawbacks:

[df]: https://github.com/eudoxia0/dotfiles

1. Sometimes I'd install something by hand and forget to add it to the bootstrap script.
2. Analogously, I'd make a change to a dotfile in its target location, out of
   expedience, and forget to apply the change to the corresponding file in the
   dotfiles repo.

And so the live configuration and my dotfiles would slowly drift over time. When
I ran the bootstrap script, I had no idea whether it would overwrite some key
part of my config I'd forgotten to copy over to the repo.

I tried [GNU Stow][stow], a "symlink farm" manager, to symlink rather than copy
my dotfiles. So changes made to the live dotfiles automatically update the
repo. But symlinks are brittle: moving the dotfiles directory broke everything.

[stow]: https://www.gnu.org/software/stow/

Nix solves both problems:

1. I _can't_ install packages by hand, I can only add them to the Nix
   configuration.
2. The dotfiles that `home-manager` creates are write-protected, so I can't
   change them directly, rather, I have to change them in the dotfiles repo and
   run the script to apply the changes.

For the first time, I'm entirely satisfied with my system configuration.
