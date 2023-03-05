---
title: The Gallery of Sand
summary: .
---

This is a collection of images I made with [DALL-E][dalle]. Most of these were made between late June and early July 2022.

[dalle]: https://openai.com/product/dall-e-2

I didn't experiment too much with the prompt strategy, and at the time the only prompt engineering trick I was aware of was to append "trending on ArtStation" to the prompt. My first thought was to simply cite, rather than describe, the work, so most of the prompts are of the form:

```
[title], [artist], [year], [medium]. [optional description]. From the [institution].
```

e.g.:

>Forest and Exit Wounds, Paul Klee, watercolor, 1927, from the British Museum.

This works mainly by lowering your expectations: images have too many degrees of freedom to be described. Citing them is more like drilling core samples from latent space, and less like specifying every feature of the image you want.

Many of the prompts are anachronistic: I wanted DALL-E to try and imagine what an electron beam lithography machine would look like as a Persian miniature from medieval Herat, for example.

Much of DALL-E's output is disappointing, but you can play into this: it's good at impressionism, abstraction, and [ostranenie][ost].

[ost]: https://en.wikipedia.org/wiki/Defamiliarization

# Contents

<ol>
{% for section in site.data.dalle.sections %}
<li>
<a href="#{{ section.id }}">{{ section.title }}</a>
</li>
{% endfor %}
</ol>

{% for section in site.data.dalle.sections %}

# {{ section.title }} {#{{section.id}}}

{% for prompt in section.prompts %}

<div class="prompt">
“{{ prompt.prompt }}”
</div>

<div class="gallery">
{% for file in prompt.files %}
<div class="gallery-entry">
<a href="/assets/content/dall-e-explorations/{{ file }}">
<img src="/assets/content/dall-e-explorations/{{ file }}">
</a>
</div>
{% endfor %}
</div>

{% endfor %}
{% endfor %}