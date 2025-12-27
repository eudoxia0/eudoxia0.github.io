---
title: Using the Brother DS-640 Scanner on NixOS
summary: I suffer so you don't have to.
---

You need this configuration:

```nix
# Enable SANE scanners.
hardware.sane.enable = true;

# Add yourself to the scanner and printer groups.
users.users.USERNAME.extraGroups = [ "scanner" "lp" ];

# Add support for Brother scanners.
hardware.sane.brscan5.enable = true;
```

After applying this you have to log out and in, or reboot, for the usergroup changes to apply.

Note also `brscan5`: if you use `brscan4` (as I did initially), the scanner will kind of work, but it only scans the first third or so of every page.

And if you want a GUI:

```nix
# Install GNOME Document Scanner.
home-manager.users.USERNAME.home.packages = with pkgs; [
  simple-scan
];
```

Now, make sure the scanner is there:

```
$ scanimage --list-devices
device `brother5:bus4;dev2' is a Brother DS-640 USB scanner
```

If you get `Brother *Unknown USB scanner`, you either have the wrong driver or (as I did, surprisingly) a faulty USB port. In which case move the scanner to another port. `scanimage --list-devices` should recognize the model number.

The most basic test that should work: put a page in the scanner until it locks and run:

```
$ scanimage --device="brother5:bus4;dev2" \
  --format=jpeg \
  --output-file=scan.jpg
```

This will produce a (probably not very good) scan in `scan.jpg`. Now, we can improve things using the device-specific options, which you can check with this command:

```
$ scanimage --all-options --device="brother5:bus4;dev2"
All options specific to device `brother5:bus4;dev2':
    --mode 24bit Color[Fast]|Black & White|True Gray|Gray[Error Diffusion] [24bit Color[Fast]]
        Select the scan mode
    --resolution 100|150|200|300|400|600|1200dpi [100]
        Sets the resolution of the scanned image.
    --source Automatic Document Feeder(left aligned) [Automatic Document Feeder(left aligned)]
        Selects the scan source (such as a document-feeder).
    --brightness -50..50% (in steps of 1) [inactive]
        Controls the brightness of the acquired image.
    --contrast -50..50% (in steps of 1) [inactive]
        Controls the contrast of the acquired image.
    --MultifeedDetection[=(yes|no)] [inactive]

    --AutoDocumentSize[=(yes|no)] [no] [advanced]

    --AutoDeskew[=(yes|no)] [no] [advanced]

    --SkipBlankPage[=(yes|no)] [inactive]

    --SkipBlankPageSensitivity 0..100% (in steps of 1) [inactive]

    -l 0..215.9mm (in steps of 0.0999908) [0]
        Top-left x position of scan area.
    -t 0..355.6mm (in steps of 0.0999908) [0]
        Top-left y position of scan area.
    -x 0..215.9mm (in steps of 0.0999908) [215.88]
        Width of scan-area.
    -y 0..355.6mm (in steps of 0.0999908) [355.567]
        Height of scan-area.
```

Try this for a better scan:

```
$ scanimage --device="brother5:bus4;dev2" \
  --AutoDeskew=yes \
  --AutoDocumentSize=yes \
  --resolution 300 \
  --format=jpeg \
  --output-file=scan.jpg
```

Note that some of the flags are in `--key value` format and others `--key=value`, and if you mess it up you get a cryptic error message.
