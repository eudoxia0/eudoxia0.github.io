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

<img style="margin-left: auto; margin-right: auto; width: 75%;" src="/assets/content/nixos-for-the-impatient/nix1.jpg"/>

The installer is based on [Calamares][cal] and is very user friendly. I want to
highlight two features in particular.

[cal]: https://calamares.io/

Firstly, a minor thing, timezones are separated from locales. I use
`en_US.UTF-8` everywhere, but a lot of installers give you some obscure locale
if your timezone is outside the US. Here they are correctly
separated[^timezone]:

<img style="margin-left: auto; margin-right: auto; width: 75%;" src="/assets/content/nixos-for-the-impatient/nix2.jpg"/>

Secondly, something more important: full disk encryption is completely supported
out of the box. This is crucial: full disk encryption should be the
default. Some distros like Ubuntu and Debian also support it in the installer,
while others treat this as some exotic edge case, and there might be some
incomplete wiki page that lists the dozens of commands you have to run to set up
LUKS manually.

<img style="margin-left: auto; margin-right: auto; width: 75%;" src="/assets/content/nixos-for-the-impatient/nix3.jpg"/>

For my setup I chose to wipe the whole disk (I dual-boot, but have each OS in a
separate disk, which really is the only tractable way to do it), and chose swap
plus hibernation so I can suspend the computer. Another plus here is the full
disk encryption setup, by default, gives you infinite retries if you misspell
the password. I have a long disk encryption password so this is a lifesaver:
with Ubuntu and Debian it was locked at three retries, and I'd regularly fail
that many times and have to hold down the power button to try again, which feels
like smothering the poor laptop.

Other minor things: I chose "allow unfree software" and the Pantheon desktop
since it's like a nicer version of old GNOME 2.

**Troubleshooting Note:** when you get to the "Partitions" page, make sure the
label on the upper-right hand corner reads "GPT" and not "MBR". Installing on an
MBR system gives you a severely degraded setup. Fixing this is just a matter of
opening up GParted (which comes with the installer) and changing the partition
table for the disk, assuming the disk is empty.

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

Now, trying to start a new terminal window after changing hostnames might error,
so you have to log out and log back in to observe your new hostname. For most
changes, however, they take effect immediately. How much you have to do depends
on how much you've changed: if you've updated your `.bashrc` you'll have to open
a new terminal, if you've updated your window manager config you'll have to
reboot the window manager, etc.

This is the basic pattern to configuring Nix: you make a change to your config
file, run one command, and the system is updated.

## Installing Packages {#packages}

There's two ways to install packages: system-wide and user specific. Open the
`configuration.nix` file again. System-wide packages go in `environmentSystemPackages`:

```nix
environment.systemPackages = with pkgs; [
    firefox
];
```

User-specific packages go in `users.users.$USER.packages`:

```nix
users.users.eudoxia.packages = with pkgs; [
    emacs
];
```

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

- make folder
- move files
- write script

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

# Footnotes

[^footnotes]:
    A friend etc.
