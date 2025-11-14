---
title: Linux on the Fujitsu Lifebook U729
---

This post describes my experience using Linux on the [Fujitsu Lifebook
U729][laptop]. The tl;dr is that this is a delightful laptop, and Linux runs
flawlessly, and all the hardware things I've needed run OOTB. The only
difficulty I had was in disabling Secure Boot, but I figured out how to do it,
which I explain below.

# Contents
{: .no_toc }

1. toc
{:toc}

# Background

From early 2024 my daily driver was an [M2 MacBook Air], until earlier this year
I broke the screen, and the repair was quoted at almost 1000 AUD. Since I used
it as a desktop most of the time, this didn't affect me much. After some
flip-flopping I decided to get an [M4 Mac mini]. Partly for the faster CPU and
more RAM, but partly because I liked the idea of LARPing like it's the 2000s,
when computers, and by extension the Internet, where fixed in physical space,
rather than following everyone around.

Of course this was a terrible idea. I had three working computers---a
Linux+Windows desktop, a Mac Mini, and a MacBook Air that I could use as a
desktop---and none of them were portable. When I went to [RustForge 2025][rf] I
just brought my phone. If I wanted to travel, even within Sydney, to a demo
night or math club or some such, I didn't have a laptop to bring with me.

So I needed a new laptop. And the [Tahoe release][tahoe] of macOS was so ugly it
made me boot up the old Linux desktop, and start playing around with [NixOS]
again. And I fell in love with Linux again: with the tinkering and the
experimentation and the freedom it affords you.

So, I wanted a Linux laptop. I had a [ThinkPad X1][tp] some years ago and it was
terribly: flimsy plastic build and hardware that vastly underperformed its
price. I looked around for old, refursbished workstation laptops, and, randomly,
I ran into an eBay seller offering a refurbished Fujitsu laptop.

The specs/price ratio was pretty good: 16 GiB of RAM and 512GiB of SSD, all for
250 AUD. And it was 12in and 1.1kg, which I like: laptops should be small and
lightweight. But the thing that got me, in all honesty, was the brand. "Fujitsu
laptop" sounds like colour in a William Gibson novel: "crawling into the
avionics bay, Case took out a battered Fujitsu refurb, and stuck a JTAG port in
the flight computer---". I already use NixOS and a [trackball][elecom] and a
[mechanical keyboard][mech], so a laptop that's even more obscure than a
ThinkPad is perfect for me. And it was only 250 AUD. So I got it.

The only problem I had was disabling Secure Boot in order to install
Linux. Otherwise: I love it. It's small and lightweight, feels solid, the
keyboard is good, all the hardware works out of the box with NixOS, and the
battery life is pretty good.

# Troubleshooting

This section describes the problems I encountered.

## Secure Boot

I tried to install [Debian] the usual way, when I was greeted by this:

[TODO: photo]

Going into the BIOS, the option to disable [Secure Boot][sb] was greyed out. I
tried a bunch of random bullshit: wiping the TPM, disabling the TPM. That didn't
work.

What did work was this:

1. Install Windows 11. This came with the laptop. And the installation makes
   installing Linux feel easy: I had to do so many weird tricks to avoid having
   to create an account with Microsoft during the installation.
1. Once Windows is installed, go into Windows Update. Under "Advanced Options >
   Optional Updates", there should be an option to install Fujitsu-specific
   drivers. Install those. And for good measure, do a general Windows update.
1. There was a program called DeskUpdate on the Desktop. This is the Fujitsu
   BIOS update tool. Run this and go through the instructions: this should
   update the BIOS (the ordering seems to be important: first update the Fujitsu
   firmware through Windows Update, then the BIOS through DeskUpdate).
1. Reboot and go into the BIOS (F2). You should have a new BIOS version. In my
   case, I went from BIOS 2.17 to 2.31 which was released on 2025-03-28.
1. You now have the option to disable SecureBoot.

After this, I was able to install Linux from a live USB.

## Spyware

The laptop comes with this corporate spyware thing called [Absolute
Persistence][ap]. It's some anti-theft tracking device. Since the Lifebook is
typically an enterprise laptop, it makes sense that it comes with this type of
thing.

I only noticed this because I was searching the BIOS thoroughly for a way to
disable Secure Boot. The good news is disabling it is pretty straightforward:
you just disable it in the BIOS.

As I understand it, Absolute Persistence requires an agent running in the OS, so
the BIOS support, by itself, doesn't do anything once disabled.

# Non-Problems

The following work flawlessly OOTB:

- WiFi
- Bluetooth
- Sound (using [pipewire])
- Display brightness control (using [brightnessctl][bctl])
- Touchscreen (I didn't realize the screen was actually a touchscreen until I
  touched it by accident and saw the mouse move)
- Webcam (not winning any awards on quality, but it works)

Things I have not tested:

- Microphone

# BIOS Notes

To enter the BIOS: smash `F2` until you hear the beep. No need to hold down the
`Fn` key.

To enter the boot menu: as above but with `F12`.

# Links

- [Fujitsu product page][laptop] ([archive.org][laptop-ar])
- [Data sheet (PDF)][ds]

[laptop]: https://www.fujitsu.com/my/products/computing/pc/ap/notebooks/lifebook-u729/
[laptop-ar]: https://web.archive.org/web/20230923090211/https://www.fujitsu.com/my/products/computing/pc/ap/notebooks/lifebook-u729/
[rf]: https://rustforgeconf.com/
[ds]: https://www.fujitsu.com/hk/Images/ds-LIFEBOOK%20U729%20%28APAC%29.pdf
