---
title: Respect the Accept-Language Header
summary: Internet is bad
tags: []
---

You have a website with content in multiple languages. How do you decide which language to
serve a user with? Easy, the [`Accept-Language` header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language) exists for this very
purpose: it allows a user agent to provide servers with a list of prefered
languages, weighed by preference.

But not everyone uses it. Instead, websites often [deduce](https://en.wikipedia.org/wiki/Geolocation_software) the user agent's location from the
IP address, and serve content accordingly. This is terrible.

My operating system's language is English. The only locale is
`en_US.UTF-8`. Firefox is set to English, and sends `Accept-Language: en` exclusively. Despite this, I am often served content in Spanish, because websites think they're smarter than the user's own preferences.

And I might be in the minority, but people travel. And if an L1 English speaker
visits Uruguay, they'll be treated to the same terrible experience.

# Examples

## Google

When logged out, Google ignores `Accept-Language` and uses geolocation. When logged
in, it does the exact same thing.

My account language is English, but the search language (even when logged in) is Spanish unless I tell
it to use English. But this isn't an account-wide setting, it's a client setting. So every time I use Google from a new
browser, a new computer, or simply after deleting local storage, I have to
remind Google for the billionth time that I want English search results.

Even after changing the language, search queries are parameterized by
location. Which is determined by geoip. And geoip here is very coarse, so Google
thinks I live in a different city, and this is reflected in the search results.

DuckDuckGo and StartPage both respect `Accept-Language`, which is another reason
to prefer either over Google.

## Airbnb

Airbnb implements what I can only term Heisenlocalization: place names are
either in English or localized, seemingly at random.

Examples:

![An Airbnb search box, showing "London, United Kingdom" as the autocompletion of the string "London".]({{ site.post_images }}/accept-language/london.png)

![An Airbnb search box, showing "Cardiff, Reino Unido" as the autocompletion of the string "Cardiff".]({{ site.post_images }}/accept-language/cardiff.png)

So this tells me they localize everything behind the scenes, and then translate
it back to English in the frontend, because I've the language set to English.

## eBay

If I browse US listings from Uruguay, by default, eBay will machine-translate
the English to Spanish, with predictably awful results. This is so stupefyingly
bad, I have no words to describe it.

The very real possibility that this decision was empirically good for eBay --
that conversion and retention were improved by it -- is too terrifying to
ponder.

# Accessibility

Some of thse terrible websites implement a dropdown for choosing the language,
and store the preference in a cookie or local storage -- essentially
reimplementing `Accept-Language` one level up, a common pattern among JavaScript
programmers.

Sometimes this is useless because the language names themselves are localized to
whatever locale the computer has decided you use. If you use a screenreader,
you'll presumably hear ten minutes of Thai until you hear อังกฤษ.

# Counterarguments

"But A/B testing shows it improves our conversion."

If you're willing to give your users a terrible experience for some dubious
quantitative metric, you might as well implement every other dark pattern. Also,
your A/B testing is unsound because it has no cause and effect model.

# Conclusion

Old man yells at ~~cloud~~ the corporate race to the bottom that is the modern
Internet.
