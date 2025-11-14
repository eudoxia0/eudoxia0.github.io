---
title: FreeBSD with Encrypted ZFS and X11 on a Laptop
summary: My installation notes.
tags: [bsd, freebsd, tutorial]
---

# Introduction

A few days ago I installed FreeBSD on my laptop with an encrypted ZFS root and a
minimal X11 setup. I documented every step and then re-did the installation to
ensure my setup was reproducible. This article is an edited version of my
installation notes, which I hope is useful to you.

## The Hardware

My laptop is an [ASUS K501][asus]. It has an NVIDIA GPU but in this guide I'll
focus on the Intel integrated graphics.

# Pre-Installation

The zeroth step: the target computer must have UEFI enabled, and the laptop
plugged it to ethernet so you don't have to deal with Wi-Fi at installation
time.

## Getting FreeBSD

1. Download the `memstick` image file.

    ~~~
    curl -o memstick.img \
    "https://download.freebsd.org/ftp/releases/amd64/amd64/ISO-IMAGES/11.2/FreeBSD-11.2-RELEASE-amd64-memstick.img"
    ~~~

2. Verify. As of 17 August 2018 the SHA256 hash of the mini-memstick image is:

    ~~~
    4b90f19c9f08cc7a2db39072e43967d90f3bf057125aa16b8c9d7c3ea8d23b39
    ~~~

    Run `sha256sum memstick.img` to compare this with the file's actual hash.

## Copying the Image to a USB Drive

1. Run `fdisk -l` to find your USB drive's device identifier. In my case it's
   `/dev/sdb`.

2. Copy the `memstick.img` file to the USB drive using this command:

    ~~~bash
    sudo dd if=memstick.img of=/dev/<your USB device id> conv=sync
    ~~~

# Installation

## First Steps

1. Press 1 to select boot multi user, wait for the blue screen to come up.

2. Choose `Install`.

3. Choose your keymap.

4. Enter a meaningful hostname.

5. You will be prompted for system components to install, I just choose the
   default.

6. Network install. On my hardware it autodetected everything.

7. Say yes to using IPv4 and DHCP, wait for DHCP lease.

8. Say yes to IPv6 and SLAAC, wait for router solicitation.

9. Set DNS to Google's

    - IPv6:
        - `2001:4860:4860::8888`
        - `2001:4860:4860::8844`
    - IPv4:
        - `8.8.8.8`
        - `8.8.4.4`

    Or whichever you want to use.

10. Choose the mirror to download packages from.

## Full Disk Encryption Setup

This part of the guide is straight from [this howto][crypt-howto].

### Partitioning

1. At the partitioning screen, select `Shell`, the option to partition by hand.

2. Destroy existing partitions:

    ~~~bash
    gpart destroy -F /dev/ada0
    ~~~

3. Create the partition table:

    ~~~bash
    gpart create -s GPT /dev/ada0
    ~~~

4. Create the EFI partition:

    ~~~bash
    gpart add -t efi -s 100M -a 1M -l EFI /dev/ada0
    ~~~

5. Create a swap partition:

    ~~~bash
    gpart add -t freebsd-swap -s 4G -a 1M -l FreeBSD-swap /dev/ada0
    ~~~

6. Create the boot partition

    ~~~bash
    gpart add -t freebsd-ufs -s 10G -a 1M -l FreeBSD-ufsboot /dev/ada0
    ~~~

7. Create the encrypted partition

    ~~~bash
    gpart add -t freebsd-zfs -a 1M -l FreeBSD-enczroot /dev/ada0
    ~~~

8. Configure the EFI partition:

    ~~~bash
    newfs_msdos -F 16 -L FreeBSD_EFI /dev/ada0p1
    mkdir /tmp/efi
    mount -t msdosfs /dev/ada0p1 /tmp/efi
    mkdir -p /tmp/efi/EFI/BOOT
    cp /boot/boot1.efi /tmp/efi/EFI/BOOT/BOOTX64.EFI
    umount /dev/ada0p1
    ~~~

9. Configure the UFS boot partition:

    ~~~bash
    newfs -L ufsboot -S 4096 /dev/ada0p3
    mkdir /tmp/ufsboot
    mount /dev/ada0p3 /tmp/ufsboot
    ~~~

(Note: we went from `ada0p1` to `ada0p3` because we dont need to do anything
with swap.)

### Configuring Encryption

1. Configuring GELI:

    ~~~bash
    kldload aesni
    mkdir -p /tmp/ufsboot/boot/geli
    dd if=/dev/random of=/tmp/ufsboot/boot/geli/ada0p4.key bs=64 count=1
    geli init -e AES-XTS -l 128 -s 4096 -b -K /tmp/ufsboot/boot/geli/ada0p4.key /dev/ada0p4
    ~~~

    This will prompt you for the disk encryption password. Choose carefully.

2. Then, run:

    ~~~bash
    cp /var/backups/ada0p4.eli /tmp/ufsboot/boot/geli/
    geli attach -k /tmp/ufsboot/boot/geli/ada0p4.key /dev/ada0p4
    ~~~

    This will ask you to enter the password you entered above.

3. Very important final step:

    ~~~bash
    geli configure -b /dev/ada0p4.eli
    ~~~

### Configuring ZFS:

1. Create the pool:

    ~~~bash
    zpool create -R /mnt \
        -O canmount=off -O mountpoint=none -O atime=off -O compression=on \
        zroot /dev/ada0p4.eli
    ~~~

    The values of the properties are described [here][zfsprops]. Briefly,
    turning `atime` off reduces disk traffic because the filesystem is not
    updating access time metadata every time the file is accessed. Setting
    `compression` to `on` turns on compression.

2. Create the boot container:

    ~~~bash
    zfs create -o canmount=off -o mountpoint=none zroot/ROOT
    ~~~

3. The default boot environment:

    ~~~bash
    zfs create -o mountpoint=/ zroot/ROOT/master
    ~~~

4. Some more things:

    ~~~bash
    zfs create -o mountpoint=/usr/jails zroot/ROOT/master/jails
    zfs create -o mountpoint=/usr/local zroot/ROOT/master/local
    zfs create -o mountpoint=/usr/ports zroot/ROOT/master/ports
    zfs create -o mountpoint=/var zroot/ROOT/master/var
    zfs create -o mountpoint=/var/log zroot/ROOT/master/log
    ~~~

    And:

    ~~~bash
    zfs create -o mountpoint=/usr/home zroot/home
    zfs create -o mountpoint=/usr/obj zroot/obj
    zfs create -o mountpoint=/usr/ports/distfiles zroot/distfiles
    zfs create -o mountpoint=/usr/src zroot/src101
    zfs create -o mountpoint=/tmp zroot/tmp
    zfs create -o mountpoint=/var/tmp zroot/vartmp
    ~~~

    Why the 101? I'm not sure. I copied that straight from the guide.

5. Finish up:

    ~~~bash
    mkdir /mnt/ufsboot
    umount /dev/ada0p3
    mount /dev/ada0p3 /mnt/ufsboot
    cd /mnt
    ln -s ufsboot/boot boot
    cd /
    ~~~

6. Edit `fstab`. Run:

    ~~~bash
    vi /tmp/bsdinstall_etc/fstab
    ~~~

    And write this:

    ~~~bash
    # Device   Mountpoint   FStype   Options   Dump   Pass#
    /dev/ada0p2.eli   none   swap   sw,ealgo=AES-XTS,keylen=128,sectorsize=4096   0   0
    /dev/ada0p3   /ufsboot   ufs   rw   1   1
    ~~~

7. Return to the installer by running `exit`.

Since this is a net installation it will start downloading the rest of the
packages it needs

## Installation Continued

1. Enter the root password.

2. Set the time.

3. Select your region.

4. Select startup services. I add `ntpd` and `powerd`.

5. Security options. Choose all of them.

6. Add new users to the system. When prompted, make your user a part of the
   `wheel` group. Otherwise, the interactive program for adding users is fairly
   self-explanatory.

7. At this stage installation is complete, but we have to write some more
   ZFS-related configuration. Select the option to exit the installation, you'll
   be prompted whether or not you want to open a shell to make any post-installation
   changes. Choose `Yes`.

## Final Configuration

1. Configure the `/boot/loader.conf` file so load your geli services:

    ~~~bash
    vi /boot/loader.conf
    ~~~

    Its contents should be:

    ~~~
    zfs_load="YES"
    aesni_load="YES"
    geom_eli_load="YES"
    geli_ada0p4_keyfile0_load="YES"
    geli_ada0p4_keyfile0_type="ada0p4:geli_keyfile0"
    geli_ada0p4_keyfile0_name="/boot/geli/ada0p4.key"
    vfs.root.mountfrom="zfs:zroot/ROOT/master"

    # Boot prompt delay
    autoboot_delay="3"

    # enable temperature sensors
    coretemp_load="YES"

    # enable AHCI on modern hardware for better performance
    ahci_load="YES"

    # enable asynchronous I/O (big performance gains with NGINX)
    aio_load="YES"

    # in-memory file system
    tmpfs_load="YES"

    # load PF firewall and the Intel ethernet driver early at boot time
    pf_load="YES"
    pflog_load="YES"
    if_igb_load="YES"
    ~~~

2. Update `/etc/rc.conf` to enable ZFS:

    ~~~bash
    vi /etc/rc.conf
    ~~~

    At the end add the line:

    ~~~
    zfs_enable="YES"
    ~~~

3. Finally, reboot. You have a working FreeBSD system.

# Post-Installation

Log in as root.

## UTF-8 Everywhere

1. Edit `/etc/profile` and add this at the end:

    ~~~
    LANG=en_US.UTF-8; export LANG
    CHARSET=UTF-8;    export CHARSET
    ~~~

2. Edit `/etc/login.conf`, and at the bottom of the default login class,
   `default:\`, change the last line to:

    ~~~
    :umask=022:\
    :charset=UTF-8:\
    :lang=en_US.UTF-8:
    ~~~

3. Rebuild the DB:

    ~~~bash
    cap_mkdb /etc/login.conf
    ~~~

## Setting up pkg

1. Run `/usr/sbin/pkg`, say yes, and wait for it to install the new `pkg`.

2. Run `pkg2ng` to upgrade the package database.

3. Finally, run `pkg update`.

## Setting up Ports

Run:

~~~bash
portsnap fetch
portsnap extract
portsnap fetch update
~~~

## X11

1. Install the required packages:

    ~~~bash
    pkg install xorg-server xf86-input-mouse xf86-input-keyboard xinit xauth xterm twm xorg-fonts
    ~~~

2. Download kernel sources:

    ~~~bash
    cd /
    fetch ftp://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.2-RELEASE/src.txz
    tar -C / -xzvf src.txz
    rm src.txz
    ~~~

3. Set up `drm-next-kmod`.

    ~~~bash
    cd /usr/ports/graphics/drm-next-kmod/
    make install clean
    ~~~

    You will be prompted for options. Leave them as-is. After the installation
    is complete, run

    ~~~bash
    pkg info -D drm-next-kmod
    ~~~

    To view the installation information.

4. Edit `/etc/rc.conf`, add this to the end:

    ~~~
    kld_list="/boot/modules/i915kms.ko"
    ~~~

5. Add your user to the `video` group:

    ~~~bash
    pw groupmod video -m <YOUR USER NAME>
    ~~~

6. Edit `~/.xinitrc` and write:

    ~~~bash
    exec twm
    ~~~

    Reboot. If this works, the screen will flash after entering your passphrase
    as the console switches drivers.

    Login as your user, not as root, and run `startx`. This should work, and by
    work I mean show you a completely black background and no cursor. On my
    setup the touchpad doesn't work, so plug it an external mouse and left-click
    anywhere on the screen to bring up the `twm` menu. This will confirm that
    `twm` is working.

## Web Browser Setup

1. Run `pkg install firefox`. This will take a while.

2. Run `dbus-uuidgen > /var/lib/dbus/machine-id` to fix something.

3. Exit root and run `firefox` as your user.

## File Manager Setup

1. As root, run `pkg install pcmanfm`

2. Exit root and run `pcmanfm` as your user to test it.

## Improving Battery Life

This part of the guide comes from [this guide][freebsd-laptop].

1. First, to view battery status, use

   ~~~bash
   $ acpiconf -i 0
   ~~~

2. Add the following lines to `/etc/rc.conf`:

    ~~~
    powerd_enable="YES"
    powerd_flags="-a hiadaptive -b adaptive"
    performance_cx_lowest="Cmax"
    economy_cx_lowest="Cmax"
    ifconfig_wlan0="WPA DHCP powersave"
    ~~~

    (You might already have `powerd_enable="YES"` from the installation step.)

3. Add the following to `/boot/loader.conf`:

    ~~~
    hw.pci.do_power_nodriver="3"
    hw.snd.latency="7"
    hint.p4tcc.0.disabled="1"
    hint.acpi_throttle.0.disabled="1"
    hint.ahcich.0.pm_level="5"
    hint.ahcich.1.pm_level="5"
    hint.ahcich.2.pm_level="5"
    hint.ahcich.3.pm_level="5"
    hint.ahcich.4.pm_level="5"
    hint.ahcich.5.pm_level="5"

    # for intel cards only
    drm.i915.enable_rc6="7"
    drm.i915.semaphores="1"
    drm.i915.intel_iommu_enabled="1"
    ~~~

4. Reboot to make sure everything is fine

## sudo

1. From root, run:

    ~~~bash
    pkg install sudo
    ~~~

    This will create a sudoers file on `/usr/local/etc/sudoers`.

2. If you want to set up passwordless sudo for your user, run `visudo` to open
the sudoers file, and add:Add:

   ~~~
   <your username here> ALL=(ALL) NOPASSWD: ALL
   ~~~

    at the end. Save and quit, and try `sudo su` on a new terminal to make sure
    it works.

## Emacs

Installing Emacs from binary packages is thankfully simple:

~~~bash
sudo pkg install emacs
~~~

Afterwards run `emacs` to ensure everything works.

Depending on your Emacs setup, you might have to install some packages for your
configuration to work. For example, I use [Inconsolata][inconsolata] as the
Emacs font, so I had to run:

~~~bash
sudo pkg install inconsolata-ttf
~~~

## Sound

1. Edit `/boot/loader.conf` and add this line:

    ~~~bash
    snd_hda_load="YES"
    ~~~

2. Edit `/etc/rc.conf` and add this line:

    ~~~bash
    sndiod_enable="YES"
    ~~~

3. Open `/dev/sndstat` to view your sound devices. In my case:

    ~~~bash
    $ cat /dev/sndstat
    Installed devices:
    pcm0: <Conexant CX20751/2 (Analog)> (play/rec) default
    pcm1: <Conexant CX20751/2 (Right Analog)> (play/rec)
    ~~~

4. The `pcm*` devices above are your audio outputs. By default on my laptop,
   sound comes out on the speaker (`pcm0` above). To change to the headphone
   jack (`pcm1`), I run:

    ~~~bash
    sysctl hw.snd.default_unit=1
    ~~~

    Any program using sound needs to be restarted for the change to take
    effect. Alternatively, I can set this in `/etc/sysctl.conf`:

    ~~~
    hw.snd.default_unit=1
    ~~~

     Replace `1` with the ID of the PCM device you wish to use by default.

5. You can check the settings of your audio devices using the `mixer` command:

    ~~~bash
    $ mixer
    Mixer vol      is currently set to  86:86
    Mixer pcm      is currently set to 100:100
    Mixer mic      is currently set to  56:56
    Mixer rec      is currently set to  93:93
    Recording source: mic
    ~~~

    And change the volume like so:

    ~~~bash
    $ mixer -s vol 50
    Setting the mixer vol from 86:86 to 50:50.
    $ mixer -s vol 80
    Setting the mixer vol from 50:50 to 80:80.
    $ mixer -s vol 100
    Setting the mixer vol from 80:80 to 100:100.
    ~~~

# Miscellaneous

## Ruby

I manage my Ruby environment using [rbenv][rbenv] to manage different versions
of Ruby, and the rbenv plugin [ruby-build][ruby-build] to install them.

~~~bash
sudo pkg install rbenv ruby-build
~~~

To see a list of available rubies:

~~~bash
rbenv install --list
~~~

Let's install a recent version of Ruby:

~~~bash
rbenv install 2.5.1
rbenv global 2.5.1
~~~

The Ruby install comes with RubyGems, so let's install the [Jekyll][jekyll] gem:

~~~bash
rbenv exec gem install jekyll
~~~

Once it's installed, you can execute it using `rbenv exec`, for example, I build
this site using:

~~~bash
rbenv exec jekyll serve --watch --future
~~~

## Mounting USB Drives

If you have a USB drive you mount from time to time (say, for backups), you can
set up a mount directory for it:

~~~bash
$ sudo mkdir /mnt/$USER
$ sudo chown $USER:$USER /mnt/$USER
~~~

Then, when the drive is detected:

~~~bash
$ sudo mount -t msdosfs -o -m=644,-M=755 /dev/da0s1 /mnt/$USER
$ # do what you need
$ umount /mnt/$USER
~~~

Replace `/dev/da0s1` with the path to your device.

## The Lisp Keyboard

I use [`xcape`][xcape] to override the shift keys so when they are used without
a modifying key they insert a parenthesis. Left shift inserts an open
parenthesis and right shift inserts a close parenthesis. Once I got used to this
I couldn't live without it. In Linux I use this in my `.xsession`:

~~~bash
xcape -e "Shift_L=parenleft;Shift_R=parenright"
~~~

In FreeBSD this doesn't work. More precisely, left shift inserts a 9 and right
shift inserts a 0. [Others have had this issue][xcape-issue]. Fortunately, a comment
in that issue provides the solution:

~~~bash
xcape -e Shift_L='Shift_L|9'
xcape -e Shift_R='Shift_R|0'
~~~

## Neofetch

~~~bash
$ sudo pkg install neofetch
~~~

Without further ado:

![A screenshot of xterm showing the output of neofetch]({{ site.post_images }}/freebsd-zfs-x11-laptop/neofetch.png)

# Acknowledgements

The following guides were crucial in writing this article:

- [FreeBSD with encrypted ZFS howto][crypt-howto]
- [FreeBSD and Intel Video Cards][intel]
- [FreeBSD on a Laptop][freebsd-laptop]

[asus]: https://www.amazon.com/gp/product/B01DT49XN8/

[crypt-howto]: https://forums.freebsd.org/threads/howto-freebsd-10-1-amd64-uefi-boot-with-encrypted-zfs-root-using-geli.51393/
[intel]: https://www.srobb.net/freebsdintel.html
[freebsd-laptop]: https://www.c0ffee.net/blog/freebsd-on-a-laptop/

[jekyll]: https://jekyllrb.com/
[rbenv]: https://github.com/rbenv/rbenv
[ruby-build]: https://github.com/rbenv/ruby-build

[inconsolata]: https://fonts.google.com/specimen/Inconsolata

[zfsprops]: https://docs.oracle.com/cd/E19253-01/819-5461/gazss/index.html

[xcape]: https://github.com/alols/xcape
[xcape-issue]: https://github.com/alols/xcape/issues/52
