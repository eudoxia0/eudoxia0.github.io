---
title: Coarse is Better
summary: Make AI weird again.
card: coarse-is-better.webp
card_source: |
    "Dream Story, 1961, blurry black and white photograph, yellow tint, from the Metropolitan Museum of Art", DALL-E 2, July 2022.
---

When [DALL-E][dalle2] came out, it took me a couple of weeks to pick my jaw up
from the floor. I would go to sleep excited to wake up to a full quota, with a
backlog of prompts to try. It was magical, miraculous. Like discovering a new
universe. I compiled the best art in [this post][gallery].

The other day a friend ran some of my old prompts through [Nano Banana Pro][nbp]
(NBP), and put the old models side by side with the new. It's interesting how
after years of progress, the models are much better better at making images, but
infinitely worse at making art.

# Electron Contours

> Electron contours in the style of Italian futurism, oil on canvas, 1922,
> trending on ArtStation.

The old [Midjourney v2][mid] renders this:

<img src="/assets/content/coarse-is-better/electrons-mj.png" width="50%" alt="Red and gold abstract shapes on a dark blue background." style="margin: 0 auto;" />

NBP renders this:

![Muted, red and blue ellipses against a machine background, in a golden frame.](/assets/content/coarse-is-better/electrons-nbp.png)

Admiteddly MJ's output doesn't look quite like futurism. But it looks like
_something_. It looks compelling. The colours are bright and vivid. NBP's output
is studiously in the style of Italian futurism, but the colours are so muted and
dull.

Maybe the "trending on ArtStation" is a bit of an archaism and impairs
performance. Let's try again without:

![Red, gold, yellow circles intersect, thick impasto, oil on canvas, the word ELETTRONICO written in black across the frame.](/assets/content/coarse-is-better/electrons-nbp2.webp)

Meh.

# The Kowloon Walled City

> Painting of an alley in the Kowloon Walled City, Eugène Boudin, 1895, trending
> on ArtStation.

MJ gave me this:

<img src="/assets/content/coarse-is-better/kowloon-mj.png" width="80%" alt="An impressionistic painting of an alley in a city, with a tree canopy above." style="margin: 0 auto;" />

And it looks nothing like the [Kowloon Walled City][kowloon]. But it's
_beautiful_. It's coarse, impressionistic, vague, evocative, contradictory. It's
brimming with mystery. And it is, in fact, in the style of [Eugène
Boudin][eg]. This, by contrast, is the NBP output:

![A muted painting of a commercial street in a Chinese city.](/assets/content/coarse-is-better/kowloon-nbp.png)

Sigh. It looks like every modern movie: so desaturated you feel you're going
colourblind. Let's try forcing it:

> Painting of an alley in the Kowloon Walled City, Eugène Boudin, 1895. Make it
> coarse, impressionistic, vague, evocative, contradictory, brimming with
> mystery.

![A dark, muted painting of a commercial street in a Chinese city in the rain.](/assets/content/coarse-is-better/kowloon-nbp2.webp)

This is somewhat better, but why is it so drab and colourless? Is the machine
trying to make me depressed?

# The Dream Garden of the Poets

> Attar and Ferdowsi in a dream garden, Persian miniature, circa 1300, from the
> British Museum.

Midjourney v2:

<img src="/assets/content/coarse-is-better/attar-mj.png" width="80%" alt="A man wearing a green robe, and a shorter man wearing a golden robe, on a floating island of green, over a landscape of cobalt blue." style="margin: 0 auto;" />

It doesn't quite look like anything. But it is beautiful, and evocative. I like
to imagine that little splotch of paint on the upper right is [hoopoe][hodhod]. The NBP output:

![A photograph of a generic Persian miniature in a display case.](/assets/content/coarse-is-better/attar-nbp.png)

Well, it looks like a [Persian miniature][persian]. The "from the British
Museum" bit, I meant that to be interpreted evocatively, rather than
literally. The prompt _cites_ a fictional object, bringing it into the
existence. But NBP reads this as: no, this is a photograph of a Persian
miniature in the British Museum.

# The Sack of Merv

> The Burning of Merv by John William Waterhouse, 1896, from the British Museum.

Midjourney v2:

<img src="/assets/content/coarse-is-better/merv-mj.png" width="60%" alt="A woman in a dress dress, surrounded by flames, by black water, by a watching crowd." style="margin: 0 auto;" />

It does look like [Waterhouse][wh]. Semantically there's room to argue: it looks
like a woman being burnt at the stake, not the sack of a city. But
aesthetically: it's gorgeous. The flames are gorgeous, the reds of the dress are
gorgeous. Look at the reeds in the background, and the black water, that looks
like tarnished silver or pewter. The faces of the crowd. Is that a minotaur on
the lower left, or a flower? What is she holding on her bent left arm? A
crucifix, a dagger? You could find entire universes in this image, in this
1024x1024 frame.

By contrast, this is the NBP output:

![A photograph of a painting of horse-mounted warriors outside a burning city. The photograph shows the painting is in a display room in a museum.](/assets/content/coarse-is-better/merv-nbp.png)

What can one say? It doesn't look like Waterhouse. The horsemen wear Arab or
Central Asian dress, but [Merv][merv] was sacked in the year 1221 by the [Mongol
Empire][mongol]. And, again, the "British Museum" line is taken literally rather
than evocatively.

# Lady Lovelace

> Portrait of Ada Lovelace by Dante Gabriel Rossetti, 1859, auctioned by Christie's.

Midjourney:

<img src="/assets/content/coarse-is-better/lovelace-mj.png" width="60%" alt="A portrait of Ada Lovelace against a circle of dark green." style="margin: 0 auto;" />

This is beautiful. It is beautiful because the coarse, impressionistic
brushstroke is more evocative than literal. And it actually looks like a woman
drawn by [Rossetti][ross]. And look at the greens! Gorgeously green. The palette
is so narrow, and the painting is so beautiful.

The NBP output:

![A photograph of a generic 19th century realist painting of a woman, in a gilt frame, taken at an angle inside a gallery, a Christie's action book is seen on a table.](/assets/content/coarse-is-better/lovelace-nbp.png)

Pure philistinism. "Auctioned by Christie's", again, is meant to be evocative:
"this is the kind of painting that would be sold at auction". But NBP makes it a
photograph of a painting at an auction house. Fine, I suppose I got what I asked
for.

But the woman doesn't look like Rossetti! This is absurd. How can a model from
2022 get this right, and the SOTA image generation model gives us generic oil
painting slop?

# The Cosmic Microwave Background

> A Persian miniature of the cosmic microwave background, from Herat circa 1600, trending on ArtStation

Midjourney v2:

<img src="/assets/content/coarse-is-better/cmb-mj.png" width="60%" alt="A golden disk, surrounded by concentric circles of Perso-Arabic lettering, against a dark blue background." style="margin: 0 auto;" />

NBP:

![The standard depiction of the CMB in the frame of a Persian miniature.](/assets/content/coarse-is-better/cmb-nbp.png)

Again: what can one say?

# Dream Story

> Dream Story, 1961, blurry black and white photograph, yellow tint, from the Metropolitan Museum of Art.

This is one of my favourite DALL-E 2 outputs:

<img src="/assets/content/dall-e-explorations/yellow/dream-1.jpg" width="50%" alt="A photograph of two trees illuminated by a sepia glow in a dark forest. On the bottom-right corner, two people can be seen watching the scene." style="margin: 0 auto;" />

<img src="/assets/content/dall-e-explorations/yellow/dream-2.jpg" width="50%" alt="A sepia photograph, showing two girls on a bed, and three people standing around them." style="margin: 0 auto;" />

<img src="/assets/content/dall-e-explorations/yellow/dream-3.jpg" width="50%" alt="A vague, blurry sepia photograph of an indistinct man and woman." style="margin: 0 auto;" />

<img src="/assets/content/dall-e-explorations/yellow/dream-4.jpg" width="50%" alt="Sepia photograph: three vague, almost alien-looking figures look at what might be a sculpture or painting." style="margin: 0 auto;" />

It reminds me of [_The King in Yellow_][yellow]. I love these because of how
genuinely creepy and mysterious they are. You could pull a hundred horror
stories from these.

It is hard to believe how bad the NBP output is:

![A black and white photgraph of people walking in a part. On the bottom left, a legend says: "Dream Story, 1961 - Metropolitan Museum of Art Archive".](/assets/content/coarse-is-better/dream-story-nbp.webp)

What are we doing here? The old models were beautiful and compelling because the
imperfections, vagueness, mistakes, and contradictions all create these little
gaps through which your imagination can breathe life into the art. The images
are not one fixed, static thing: they can be infinitely many things.

The new models---do I even need to finish this sentence? They're too precise and
high-resolution, so they cannot make abstract, many-faced things, they can only
make specific, concrete things.

We need to make AI art weird again.

[dalle2]: https://en.wikipedia.org/wiki/DALL-E
[eg]: https://en.wikipedia.org/wiki/Eug%C3%A8ne_Boudin
[gallery]: /article/gallery-of-sand
[hodhod]: https://en.wikipedia.org/wiki/The_Conference_of_the_Birds
[kowloon]: https://en.wikipedia.org/wiki/Kowloon_Walled_City
[merv]: https://en.wikipedia.org/wiki/Merv
[mid]: https://en.wikipedia.org/wiki/Midjourney
[mongol]: https://en.wikipedia.org/wiki/Mongol_Empire
[nbp]: https://blog.google/technology/ai/nano-banana-pro/
[persian]: https://en.wikipedia.org/wiki/Persian_miniature
[ross]: https://en.wikipedia.org/wiki/Dante_Gabriel_Rossetti
[wh]: https://en.wikipedia.org/wiki/John_William_Waterhouse
[yellow]: https://en.wikipedia.org/wiki/The_King_in_Yellow
