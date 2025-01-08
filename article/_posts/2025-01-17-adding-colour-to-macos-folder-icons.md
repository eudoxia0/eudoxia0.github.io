---
title: Adding Colour to macOS Folder Icons
summary: A short experiment with Python, ImageMagick, and iconutil.
---

The other day I saw [this tweet][tw]:

![Screenshot of the linked Tweet, showing macOS folders with colour-coded icons on the dock.](/assets/content/adding-colour-to-macos-folder-icons/tweet.webp)

I use a [PARA method][para]-derived folder scheme, so I thought colour-coding the folders would be neat. But I was disappointed to find this isn't a native macOS feature. The OP made the icons with [Figma] which seems very labour-intensive. Can we do better?

The default macOS folder icon is here:

```
/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/GenericFolderIcon.icns
```

This is an [ICNS][icns] file, an archive of PNG images. Using [`iconutil`][iu] you can explode t:

```
$ iconutil -c iconset GenericFolderIcon.icns
```

This creates a `GenericFolderIcon.iconset` directory with these contents:

```
icon_128x128.png
icon_128x128@2x.png
icon_16x16.png
icon_16x16@2x.png
icon_256x256.png
icon_256x256@2x.png
icon_32x32.png
icon_32x32@2x.png
icon_512x512.png
icon_512x512@2x.png
```

And with [ImageMagick][im] you can apply colour changes. I experimented with a few options, the hardest parts are getting away from the initial blue bias, and preserving the drop shadow. This works somewhat:

```
$ magick default.png -modulate 100,0,100 -fill '#ef4444' -colorize 70 red.png
```

Turning this:

![](/assets/content/adding-colour-to-macos-folder-icons/default.png)

Into this:

![](/assets/content/adding-colour-to-macos-folder-icons/red.png)

If you  this for each PNG in the iconset, the dual command:

```
$ iconutil -c icns Icon.iconset
```

Will turn the `Icon.iconset` folder into an ICNS folder `Icon.icns`.

Okay, so we have the existence proof that we can make a tolerable icon, and we know the mechanics of `ICNS -> List[PNG] -> ICNS`. Now we have to scale this up.

I started out with a [Makefile][make], but make is really only tractable if you hardcode the dependency DAG. If you want to _derive_ the DAG from a data structure (e.g., the colour palette) the code quickly becomes write-only.

Fortunately I have a Python interpreter, and an [infinitely-industrious alien buddy who turns natural language into code][claude]. So I had Claude write a Python script that generates a tinted ICNS file for each colour in the [Tailwind][tail] [colour palette][twcolour]. And this is the result:

![](/assets/content/adding-colour-to-macos-folder-icons/result.png)

Which I think is pretty good for a half hour project. The code is [here][repo].

This is great example of how LLMs reduce the activation energy for tiny projects like this. Without Claude, I probably would not have bothered writing the entire Python script from scratch.

[Figma]: https://www.figma.com/
[claude]: https://claude.ai/
[icns]: https://en.wikipedia.org/wiki/Apple_Icon_Image_format
[im]: https://www.imagemagick.org
[iu]: https://www.unix.com/man-page/osx/1/iconutil/
[make]: https://www.gnu.org/s/make/manual/make.html
[para]: https://fortelabs.com/blog/para/
[repo]: https://github.com/eudoxia0/macos-tinted-folders
[tail]: https://tailwindcss.com/
[tw]: https://x.com/AetherAurelia/status/1875020494770241797
[twcolour]: https://tailwindcss.com/docs/customizing-colors
