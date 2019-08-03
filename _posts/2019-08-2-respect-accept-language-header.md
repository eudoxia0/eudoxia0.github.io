---
title: Respect the Accept-Language Header
summary: Internet is bad
tags: []
---

You have a website in multiple languages. How do you decide which language to
serve a user with? Easy, the `Accept-Language` header exists for this very
purpose: it allows a user agent to provide servers with a list of prefered
languages weighed by preference.

But nobody uses it. Instead, servers deduce the user agent's location from the
IP address, and serve content accordingly. This is terrible.

My operating system's language is English. The only locale is
`en_US.UTF-8`. Firefox is set to English, and sends `Accept-Language: en`.

I might be in the minority, but people travel. And if an L1 English speaker
visits Uruguay, they'll be treated to the same terrible experience.

# Examples

## Google

## Airbnb

Airbnb implements what I can only term Heisenlocalization: place names are
either in English or localized, seemingly at random.

Examples:

So this tells me they localize everything behind the scenes, and then translate
it back to English in the frontend, because I've the language set to English.

## eBay

If I browse US listings from Uruguay, by default, eBay will machine-translate
the English to Spanish, with predictably awful results. This is so stupefyingly
bad, I have no words to describe it. Instead, I will paraphrase Neal Stephenson:

>The dimensions of this [design decision] were so vast that Hackworth's mind was
>still reeling through it, bouncing haplessly from one wall to another [...]

The very real possibility that this decision was empirically good for eBay --
that conversion and retention were improved by it -- is too terrifying to
ponder.

# Conclusion
