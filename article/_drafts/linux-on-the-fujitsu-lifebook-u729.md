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

# Secure Boot

I tried to install [Debian] the usual way, when I was greeted by this:

[TODO: photo]

- secureboot
	- things that didn't work
	- how to disable
- once disabling secure boot, installing nixos was easy

# Spyware

- weird shitty spyware thing

# Review

# Non-Problems

- things that work flawlessly ootb
  - display brightness
  - sound
  - wifi
  - bluetooth
  - the screen is actually a touchscreen and i only realized this accidentally
  - battery life
  - webcam
	- not winning any awards
  - mic?

# Notes

- enter bios: smash f2 until you hear the beep
- enter boot select: smash f12 until you hear the beep

# Links

- [Fujitsu product page][laptop] ([archive.org][laptop-ar])

[laptop]: https://www.fujitsu.com/my/products/computing/pc/ap/notebooks/lifebook-u729/
[laptop-ar]: https://web.archive.org/web/20230923090211/https://www.fujitsu.com/my/products/computing/pc/ap/notebooks/lifebook-u729/
[rf]: https://rustforgeconf.com/
