---
title: Adding Planets to Celestia on macOS
summary: A beginner's guide to modding Celestia.
---

tl;dr: you have to modify the application bundle.

[Celestia] is a space simulator: you can fly around space and look at moons and exoplants, fast forward time. It is sometimes [used][caz] by sci-fi artists for worldbuilding because you can easily add new stars/planets/megastructures/spacecraft. Some people have built [whole virtual worlds][ran] for storytelling in Celestia. The [Orion's Arm][oa] collaborative worldbuilding project has a collection of [Celestia addons][oac] so you can explore the world of the year 10,000 AT.

But the documentation is sparse and old. As with many things: the biggest hurdle to starting is just knowing which files go in which directories.

Celestia uses `.ssc` (solar system catalogue) files to define planets. These are plain-text files with a syntax resembling [HCL]. Let's create baby's first planet: below is a minimal `.ssc` file that adds a planet "Alpha" around the star [Gliese 555][gl555]:

```
"Alpha" "Gliese 555"
{
  Texture   "asteroid.*"
  Mass      1             # Earth masses
  Radius    6378          # km
  EllipticalOrbit {
    Period          1.0 # years
    SemiMajorAxis   1.0 # long axis
    Eccentricity    0.0 # circular
  }
}
```

Now, what you would hope is that there exists a standard directory like `~/.celestia/planets/` you can put this into. I spent a lot of time looking through old docs and source code for this, and I'm writing this so others don't have to. Unfortunately, at least on macOS, you have to modify the application [bundle] itself. This feels morally wrong, but it works.

Save the above code as `alpha.ssc`, and execute:

```sh
$ mkdir -p /Applications/Celestia.app/Contents/Resources/CelestiaResources/extras/
$ cp alpha.ssc /Applications/Celestia.app/Contents/Resources/CelestiaResources/extras/alpha.ssc
```

Open Celestia, and navigate to Gliese 555 (press enter, type "Gliese 555", press enter, press g). You should see a new planet:

![A screenshot of Celestia showing the star Gliese 555, with the orbit of a new planet around it.](/assets/content/adding-planets-to-celestia-on-macos/far.webp)

Zooming in, you can see it's using the built-in asteroid texture:

![A screenshot of Celestia showing the planet Alpha around Gliese 555.](/assets/content/adding-planets-to-celestia-on-macos/near.webp)

To verify it's reading the right file, press tilde and use the arrow keys to scroll up the logs, and you should see a line like:

```
Loading solar system catalog: extras/alpha.ssc
```

Celestia traverses the `extras` directory recursively, so you can put your `.ssc` files inside folders to organize large worldbuilding projects.

[oa]: https://www.orionsarm.com/
[HCL]: https://github.com/hashicorp/hcl
[oac]: https://www.orionsarm.com/page/326
[Celestia]: https://celestiaproject.space/
[ran]: https://no56.neocities.org/articles/ran
[gl555]: https://en.wikipedia.org/wiki/HN_Librae
[bundle]: https://en.wikipedia.org/wiki/Bundle_(macOS)
[caz]: https://bsky.app/profile/timberwind.bsky.social/post/3ldamhmnp622j
