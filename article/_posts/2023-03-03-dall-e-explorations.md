---
title: DALL-E Explorations
summary: .
---

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