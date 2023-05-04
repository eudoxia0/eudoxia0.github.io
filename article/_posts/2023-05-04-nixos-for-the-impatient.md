---
title: NixOS for the Impatient
card: nixos-for-the-impatient.jpg
---

- NixOS
    - what it is
    - why it is useful
- Previous setup
    - dotfiles repo
        - ten years old
    - installing software
        - bash script to install things
        - problem: ad-hoc installs
    - configuring software
        - bash script to copy things
            - problem: making changes to targets, and not sources
            - dotfiles slowly drift
            - when you run the dotfile install script, you have no way of knowing
              whether you're gonna overwrite some key part of your live config
            - GNU Stow
                - pros: writing to targets updates source files in the dotfiles repo
                - cons: brittle
                    - moving the dotfiles dir breaks everything
- installation
    1. Installer is very user-friendly
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
        1. Dual booting and manual partitioning are a pain. You should unironically
        just buy multiple disks.
- Changing the Hostname
   1. `sudo nano /etc/nixos/configuration.nix`
   1. `networking.hostName = "nixos";`
   1. `sudo nixos-rebuild switch`
   1. Log out.
      1. Trying to start the terminal after switching hostnames errors.
   1. Hostname has been updated.
- Installing Packages
   1. `sudo nano /etc/nixos/configuration.nix`
   1. `users.users.eudoxia`
   1. To install something system-wide, use `environment.systemPackages` instead.
- Updating.
   1. NixOS uses channels, analogous to Debian/Ubuntu distributions/sources.
   1. To find the channel you're using: `sudo nix-channel list | grep nixos`
   1. To upgrade the current channel: `sudo nixos-rebuild switch --upgrade`
   1. You can also auto-upgrade:
      1. `system.autoUpgrade.enable = true;`
      1. `system.autoUpgrade.allowReboot = true;`
      1. This will reboot the system.
- Garbage Collection.
   1. Nix accumulates garbage.
   1. You clean it by periodically running `sudo nix-collect-garbage`.
   1. Naturally this can be automated: https://nixos.org/manual/nixos/stable/index.html#sec-nix-gc
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
- My Current Setup
    - Security
    - DNS
    - Syncthing