---
title: The Uselessness of the Accept-Language Header
summary: Old man yells at cloud.
tags: []
---

You have a website with content in multiple languages. How do you decide which
language to serve a user with? Easy, the [`Accept-Language` header][header]
exists for this very purpose: it allows a user agent to provide servers with a
list of acceptable languages, weighed by preference.

But not everyone uses it. Instead, websites will [infer][geoip] the user agent's
location from the IP address, and serve content accordingly. This is universally
terrible.

My operating system's language is English. The only locale is
`en_US.UTF-8`. Firefox is set to English, and sends `Accept-Language: en`
exclusively. That's how I do my computing. You are reading these words in
English. Nevertheless webdevs think they are smarter than the user agent.

# Examples

## Google

When logged out, Google ignores `Accept-Language` and uses geolocation. When
logged in, it does the exact same thing.

My account language is English, but the search language (even when logged in) is
Spanish unless I tell it to use English. But this isn't an account-wide setting,
it's a client setting. So every time I use Google from a new browser, a new
computer, or simply after deleting local storage, I have to remind Google for
the billionth time that I want English-language search results.

Even after changing the language, search queries are parameterized by
location. Which is determined by geoip, and geoip here is very coarse, so Google
thinks I live in a different city, and this is reflected in the search results.

[DuckDuckGo][ddg] and [StartPage][startpage] both respect `Accept-Language`,
which is another reason to prefer either over Google.

## Airbnb

Airbnb implements what I can only term Heisenlocalization: place names are
either in English or localized, seemingly at random. For example:

![An Airbnb search box, showing "London, United Kingdom" as the autocompletion of the string "London".]({{ site.post_images }}/accept-language/london.png)

![An Airbnb search box, showing "Cardiff, Reino Unido" as the autocompletion of the string "Cardiff".]({{ site.post_images }}/accept-language/cardiff.png)

## eBay

eBay's approach is fascinating: if I browse US listings from Uruguay, eBay will
machine-translate the English to Spanish, with predictably unreadable results. I
can't apprehend the motivation behind this decision.

# Accessibility

Some of these terrible websites implement a dropdown for choosing the language,
and store the preference in a cookie or local storage -- essentially
reimplementing `Accept-Language` one level up. Implementing things one level up
is a common pattern in the web ([example][nav]) and it always destroys
performance and usability.

Sometimes this is useless because the language names themselves are localized to
whatever locale the server has decided you use. If you use a screen reader and
can't see the little flag icons, or the screen reader doesn't work with the
JavaScript [`<select>`][select] replacement du jour, there's little you can do.

# Conclusion

I don't think this is incompetence, but rather the result of the metrics-driven
race to the bottom. There's a spreadsheet somewhere that says geoip makes some
ill-conceived metric go up, so let's use that.

[header]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Language
[geoip]: https://en.wikipedia.org/wiki/Geolocation_software
[nav]: https://carter.sande.duodecima.technology/javascript-page-navigation/
[ddg]: https://en.wikipedia.org/wiki/DuckDuckGo
[startpage]: https://en.wikipedia.org/wiki/Startpage.com
[select]: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
