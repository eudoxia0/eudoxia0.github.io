---
title: Roam, Twenty Years Before Roam
summary: Wikipedia has had bidirectional links for twenty years and nobody noticed.
card: roam-twenty-years-before-roam.png
---

When Ted Nelson invented [hypertext][hy], his ideas were much more ambitious
than any one of their realizations. In particular, links were always meant to be
bidirectional: if A links to B, B knows this. But when Tim Berners-Lee invented
the web, he had to throw a lot overboard to meet the constraints of the
time. The early web was served as static HTML on file servers, dynamic
server-side rendering was a few years away, and this precludes a lot of features
that require live servers and databases.

[hy]: https://www.nngroup.com/articles/trip-report-hypertext-87/

The release of [Roam][roam] was a Cambrian explosion in the previously moribund
 field of personal information management, where up to that time your choices
 had been [Evernote][ever] and [org-mode][org]. It led to a steady stream of
 copycats: [Obsidian][obs], [RemNote][rem], [Logseq][log], etc. Roam's key
 feature was bidirectional links, like in the dreams of Nelson. All of its
 successors have brought this feature along.

[roam]: https://roamresearch.com/
[ever]: https://evernote.com/
[org]: https://orgmode.org/
[rem]: https://www.remnote.com/
[obs]: https://obsidian.md/
[log]: https://logseq.com/

And here's the twist: [MediaWiki][mw] has had bidirectional links for twenty
years. But they are extremely deemphasized: there's a tiny link in the sidebar,
"What links here", that when you click on it shows you the list of backlinks.

[mw]: https://en.wikipedia.org/wiki/MediaWiki

I thought this was relatively common knowledge, and was surprised when I
[tweeted about it][tweet]:

[tweet]: https://twitter.com/zetalyrae/status/1606755348697735168

[![A screenshot of the tweet linked above.](/assets/content/roam-twenty-years-before-roam/tweet.png)][tweet]

It turns out many people don't know this! I suppose this is a lesson in how bad
UX can condemn a feature that could have been revolutionary to obscurity, such
that it took twenty more years for someone to realize that backlinks deserve to
be put front-and-center, and ignite a revolution in information management.

And how old is this feature? On a quick search, the earliest mention I found is
on the [release notes for version 1.1][rel], from 8 December 2003:

>Whatlinkshere now sorted alphabetically

Nineteen years.

[rel]: https://www.mediawiki.org/wiki/Release_notes/1.1
