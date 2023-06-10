---
title: Second-Class References
summary: On Graydon's Rust, Val, and mutable value semantics.
card: second-class-references.jpg
card_source: |
    [_American Landscape_][link], Charles Sheeler, 1930.

    [link]: https://www.moma.org/collection/works/79032
---

[Graydon Hoare][gh], the creator of Rust, posted an article: [The Rust I Wanted
Had No Future][future] ([HN][hn], [Lobsters][lb]). It's an interesting
discussion of programming language evolution, the competing pressure of
expedience and technical perfection, and the social consequences of technical
decisions. He concludes that the language he would have built, given absolute
creative control, would not have been as popular as Rust is today.

[gh]: https://github.com/graydon
[future]: https://graydon2.dreamwidth.org/307291.html
[hn]: https://news.ycombinator.com/item?id=36193326
[lb]: https://lobste.rs/s/47amaq/rust_i_wanted_had_no_future

But on the technical side, this, in particular, stood out to me as things
Graydon would have dropped from Rust:

>**First-class &.** I wanted & to be a "second-class" parameter-passing mode,
>not a first-class type, and I still think this is the sweet spot for the
>feature. In other words I didn't think you should be able to return & from a
>function or put it in a structure. I think the cognitive load doesn't cover the
>benefits. Especially not when it grows to the next part.
>
>**Explicit lifetimes.** The second-class & types in early Rust were analyzed
>for aliasing relationships to ensure single-writer / multi-reader (as today's
>borrows are) but the analysis was based on type and path disjointness and (if
>necessary) a user-provided address-comparison disambiguation. It did not reason
>about lifetime compatibility nor represent lifetimes as variables, and I
>objected to that feature, and still think it doesn't really pay for
>itself. They were supposed to all be inferred, and they're not, and "if I were
>BDFL" I probably would have aborted the experiment once it was obvious they are
>not in fact all inferred. (Note this is interconnected with previous points:
>the dominant use-cases have to do with things like exterior iterators and
>library-provided containers).

This post is about the idea of doing away with lifetimes in Rust, what that
would bring to the table and how much it would cost.

# Outline

- first-class references and explicit lifetimes
- second-class references
- pros
- cons
- reference transforms
