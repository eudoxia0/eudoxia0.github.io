---
title: About
layout: common
permalink: /about/
---

<article>

# About Me

I'm a software engineer. In my free time I write [code][port], a [blog][blog],
[fiction][fiction], and [build a programming language][austral].

[port]: /portfolio/
[austral]: https://github.com/austral/austral
[blog]: /article/
[fiction]: /fiction/
[gh]: https://github.com/eudoxia0

I have too many interests to list exhaustively, but this is a representative
sample of how I spend my time:

1. Software engineering, theory and practice. Compilers, programming languages,
   type systems, parsers; not abstractly but with the goal of improving
   engineering practices.
1. Data modeling, ontologies, logic, information management: choosing the right
   formalisms to model the world.
1. Self-improvement: autodidactism, lifting, productivity, time-tracking.
1. The future: what does life look like after biology, and what does the
   universe look like after intelligence?

If this is interesting to you, [send me an email][mail].

# Contact

- Email: [`fernando@borretti.me`][mail]
- Twitter: [`@zetalyrae`](https://twitter.com/zetalyrae)
- Bluesky: [`@eudoxia.bsky.social`](https://staging.bsky.app/profile/eudoxia.bsky.social)
- GitHub: [`eudoxia0`][gh]
- LinkedIn: [`fborretti`][ln]
- Hacker News: [submissions][hnsub], [comments][hncom].
- Goodreads: [`zetalyrae`](https://www.goodreads.com/zetalyrae)

[mail]: mailto:fernando@borretti.me
[ln]: https://www.linkedin.com/in/fborretti
[hnsub]: https://news.ycombinator.com/submitted?id=zetalyrae
[hncom]: https://news.ycombinator.com/threads?id=zetalyrae

# About This Site

This is a repository for my [projects][port], [blog][blog], and
[fiction][fiction].

# Best Posts

1. [Effective Spaced Repetition](/article/effective-spaced-repetition)
1. [Unbundling Tools for Thought](/article/unbundling-tools-for-thought)
1. [Introducing Austral: A Systems Language with Linear Types and Capabilities](/article/introducing-austral)
1. [Language Pragmatics Engineering](/article/language-pragmatics)
1. [Lessons from Writing a Compiler](/article/lessons-writing-compiler)
1. [Signed Integers are Asymmetrical](/article/signed-integers-asymmetrical)

# Podcasts

Podcasts I have been on:

1. [The Stewart Mackenzie Indaba #31 Fernando Borretti](https://www.youtube.com/watch?v=qMjxZHZkPIM)
2. [Fernando Borretti: How to Balance Coding, Writing, and Creativity | Glasp Talk #12](https://www.youtube.com/watch?v=L6VPuYVzLjA)
3. [Func Prog Podcast Episode 6 - Fernando Borretti](https://www.youtube.com/watch?v=QcBaJBAQfQo)

# Colophon

This website is built with [Emacs][emacs], [Jekyll][jekyll], and
[Sass][sass]. Hosting is provided by [GitHub Pages][pages].

[emacs]: https://www.gnu.org/software/emacs/
[jekyll]: http://jekyllrb.com/
[sass]: http://sass-lang.com/
[pages]: https://pages.github.com/

## Icon Sources

1. **About:** [_Pandora_][pandora] by [John William Waterhouse][jww].
2. **Portfolio:** [_An Iron Forge_][iron] by [Joseph Wright of Derby][derby].
3. **Articles:** [_Jason and Medea_][jason] by John William Waterhouse.
4. **Fiction:** [_The Mermaid_][mermaid] by John William Waterhouse.

[pandora]: https://en.wikipedia.org/wiki/Pandora_(painting)
[jww]: https://en.wikipedia.org/wiki/John_William_Waterhouse
[iron]: https://commons.wikimedia.org/wiki/File:Joseph_Wright_-_An_Iron_Forge_-_Google_Art_Project.jpg
[derby]: https://en.wikipedia.org/wiki/Joseph_Wright_of_Derby
[jason]: https://en.wikipedia.org/wiki/Jason_and_Medea_(painting)
[mermaid]: https://commons.wikimedia.org/wiki/File:John_William_Waterhouse_A_Mermaid.jpg

# 🍵

This website is part of the [matcha monday webring][mmw].

- Previous: <span id="prev"><i>Loading...</i></span>
- Next: <span id="next"><i>Loading...</i></span>

<script type="module">
  import { getRingNeighbors } from 'https://cdn.jsdelivr.net/gh/joshrees77/Matcha-Monday-/webring.mjs';
  const neighbors = getRingNeighbors("https://borretti.me/about");
  const prev = document.getElementById('prev');
  const next = document.getElementById('next');
  prev.innerHTML = `<a href="${neighbors.previous.url}">${neighbors.previous.name}</a>`
  next.innerHTML = `<a href="${neighbors.next.url}">${neighbors.next.name}</a>`
</script>

[mmw]: https://www.matchamonday.net/

</article>
