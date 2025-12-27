---
title: 1Password Dependency Breaks Syntax Highlighting
summary: Why does a password manager need a syntax highlighter?
date: "2025-12-27 01:00:00"
---

Earlier today I noticed the syntax highlighting on this website was broken. But
not fully: on reload I'd see a flash of highlighted text, that then turned
monochrome. The raw HTML from `curl` showed [rouge] tags, but the web inspector
showed raw text inside the `<code>` elements. This didn't happen in Chromium.

My first thought was: there's malformed HTML, and Firefox is recovering in a way
that loses the DOM inside `<code>` tags. Then I noticed it doesn't happen in
incognito. Turning my extensions off one by one, I found that 1Password is
responsible. Others ([1][tweet], [2][forum]) have reported this also. If you
extract the [latest XPI][xpi], unzip it, and dig around, you'll find they're
using [Prism.js][prism], a JavaScript syntax highlighter.

I don't know why a password manager needs a syntax highlighter. I imagine it has
to do with the app feature where, if you have an SSH key, you can open a [modal]
that tells you how to configure Git commit signing using. Maybe they want to
highlight the SSH configuration code block (which is unnecessary anyways, since
you could write that HTML by hand). But I can't know for sure.

Why write about this? Because 1Password is a security critical product, and they
are apparently pulling random JavaScript dependencies and unwittingly running
them _**in the tab context**_, where the code has access to everything. This is
no good. I don't need to explain how bad a supply-chain attack on the 1Password
browser extension would be.

I like 1Password and I was sad when Apple [Sherlocked][sherlock] them with the
[Passwords][pw] app, but this is a bad sign about their security practices.

[rouge]: https://github.com/rouge-ruby/rouge
[xpi]: https://addons.mozilla.org/firefox/downloads/file/4607619/1password_x_password_manager-8.11.23.2.xpi
[prism]: https://prismjs.com/
[modal]: https://developer.1password.com/docs/ssh/git-commit-signing/
[tweet]: https://x.com/RickStrahl/status/2003992055236686022
[forum]: https://www.1password.community/discussions/1password/bug-beta-and-nightly-extension-degrade-pages-original-functionallity/165329
[pw]: https://en.wikipedia.org/wiki/Passwords_(Apple)
[sherlock]: https://en.wikipedia.org/wiki/Sherlock_(software)#Sherlocked_as_a_term
