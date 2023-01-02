CHAPTERS: list[dict[str, str]] = [
    {
        "title": "The Cartesian Theatre",
        "poem": "Gentzen trees — concave ocean — paradise regained",
    },
    {
        "title": "Wepwawet",
        "poem": "breathed water — K2 — had been human",
    },
    {
        "title": "Objects in Space",
        "poem": "Nimrud — travel backwards — what hope?",
    },
    {
        "title": "Without Organs",
        "poem": "drowned — starlings — this is all that you are",
    },
    {
        "title": "Ring Zero",
        "poem": "fire and light — Wang tiles — Brodmann 39",
    },
    {
        "title": "The Book of Days",
        "poem": "Stay — among the gods — before a wing",
    },
    {
        "title": "The Lunar Surface",
        "poem": "life in amber — nothing left — Rijndael",
    },
    {
        "title": "La Sienza Nuova",
        "poem": "That I had wings like a dove — for then I would fly away — and be at rest — I would wander far away — and in the wilderness remain",
    },
    {
        "title": "Colophon",
        "poem": "Acknowledgments, things stolen, and astronomical math.",
    },
]

assert len(CHAPTERS) == 9

EPIGRAPH: str = """<div class="epigraph">
<p>For a great deal is said about the forms of the gods, and about their locality, dwelling-places, and mode of life, and these points are disputed with the utmost difference of opinion among philosophers.</p>
<p class="cite">
&mdash; Cicero, <i>On the Nature of the Gods</i>
</p>
</div>

<div class="epigraph">
<p>The whole of the developments and operations of analysis are now capable of being executed by machinery.</p>
<p class="cite">
&mdash; Charles Babbage, 1864
</p>
</div>"""

def slugify(s: str) -> str:
    return s.lower().replace(" ", "-")

# Generate pages.
for idx, chapter in enumerate(CHAPTERS):
    idx += 1
    # Parse.
    title: str = chapter["title"]
    poem: str = chapter["poem"]
    # Construct slug, file paths.
    slug: str = slugify(title)
    source_file: str = f"{idx}-{slug}.md"
    target_file: str = f"../fiction/_posts/eog581/2022-11-15-{slug}.md"
    # Navigation.
    backward: str | None
    forward: str | None
    if idx == 1:
        backward = None
        forward = slugify(CHAPTERS[idx]["title"])
    elif idx == 8:
        backward = slugify(CHAPTERS[idx-2]["title"])
        forward = "colophon"
    elif idx == 9:
        backward = None
        forward = None
    else:
        backward = slugify(CHAPTERS[idx-2]["title"])
        forward = slugify(CHAPTERS[idx]["title"])
    # Synthesize front matter.
    front_matter: str
    if idx == 9:
        front_matter = f"""---
title: {title}
permalink: /fiction/eog581/{slug}
summary: {poem}
card: eog581/{slug}.jpg
---"""
    else:
        front_matter = f"""---
title: {title}
permalink: /fiction/eog581/{slug}
summary: {poem}
card: eog581/{slug}.jpg
forward: {forward or "null"}
back: {backward or "null"}
---"""
    # Synthesize navigation.
    nav: str
    if forward and backward:
        nav = f"""
<nav class="chapter-nav">
  <ul>
    <li>
      <a href="/fiction/eog581/{{{{ page.back }}}}">
        Back
      </a>
    </li>
    <span>❖</span>
    <li>
      <a href="/fiction/eog581/{{{{ page.forward }}}}">
        {"Colophon" if idx == 8 else "Forward"}
      </a>
    </li>
  </ul>
</nav>"""
    elif forward and (not backward):
        nav = """
<nav class="chapter-nav">
  <ul>
    <li>
      <a href="/fiction/eog581/{{ page.forward }}">
        Forward
      </a>
    </li>
  </ul>
</nav>"""
    elif (not forward) and (not backward):
        nav = ""
    # Read source.
    body: str
    with open(source_file, "r") as stream:
        body = stream.read()
    # Construct file.
    start: str
    if idx == 1:
        start = f"""

{EPIGRAPH}

<div class="chapter-start">

_{poem.replace("—", "---")}_

</div>"""
    elif idx == 9:
        start = ""
    else:
        start = f"""

<div class="chapter-start">

_{poem.replace("—", "---")}_

</div>"""
    page_contents: str = f"""{front_matter}{start}

{body}{nav}
"""
    # Create page.
    with open(target_file, "w") as stream:
        stream.write(page_contents)

PREAMBLE: str = f"""% The Epiphany of Gliese 581
% Fernando Borretti

{EPIGRAPH}

"""

def concatenate():
    # Concatenate to a single Markdown file
    compiled: str = PREAMBLE
    target_file: str = "compiled.md"
    for idx, chapter in enumerate(CHAPTERS):
        idx += 1
        title: str = chapter["title"]
        poem: str = chapter["poem"]
        slug: str = slugify(title)
        source_file: str = f"{idx}-{slug}.md"
        body: str
        with open(source_file, "r") as stream:
            body = stream.read()
        start: str
        if idx == 1:
            start = f"""<div class="chapter-start">

_{poem.replace("—", "---")}_

</div>"""
        elif idx == 9:
            start = ""

        compiled += f"""# {title}

{start}

{body}

"""

    with open(target_file, "w") as stream:
        stream.write(compiled)

concatenate()
