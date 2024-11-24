---
title: Client Freedom
summary: On the freedom to bring your own client.
card: client-freedom.webp
card_source: |
    “Cover for Neverness, David Zindell, 1988, oil on canvas, trending on ArtStation”, DALL-E, June 2022.
---

Client freedom, by analogy to the [four freedoms][four], is the freedom to
operate a third-party client to an API. Email, IRC, and RSS are open protocols
with client freedom. You can read email through a [native GUI app][mail], a
[spartan text-only client][tui], and even an [Emacs mode][em].

[four]: https://www.gnu.org/philosophy/free-sw.en.html#four-freedoms
[mail]: https://en.wikipedia.org/wiki/Apple_Mail
[tui]: https://en.wikipedia.org/wiki/Mutt_(email_client)
[em]: https://en.wikipedia.org/wiki/Gnus

Client freedom peaked in the 90s when most communication was through open
protocls, and has been on the decline since. I remember, circa 2013, using
Facebook Messenger over [Pidgin][pid], through their [XMPP][xmpp] gateway, and
with [OTR][otr] messaging turned on. Then Facebook shut down its XMPP gateway,
as Slack shut down its IRC gateway.

[pid]: https://www.pidgin.im/
[xmpp]: https://xmpp.org/
[otr]: https://otr.cypherpunks.ca/

Twitter, Discord, and most proprietary applications are closed protocols because
I can't BYOC. Discord [explicitly bans][dis] third-party clients, and so does
Twitter[^f1]. This has an uncomfortable risk profile where, if you use a
third-party client, most of the time nothing happens, until the company installs
a new traffic analysis package and bans your account unappealably.

[dis]: https://x.com/discord/status/1229357198918197248

I wish Twitter in particular had client freedom. I would change literally
everything about the experience:

- No feed, just put people in lists and subscribe to them over RSS so I can read
  them on [NetNewsWire][nnw].
- Notifications are a batched into a once-a-day email.
- DMs and group chats entirely separated.
- On my phone I want a client _just_ for tweeting into the void. No timeline, no
  scrolling, notifications.[^f2]

[nnw]: https://netnewswire.com/

I don't even need client freedom to be the default. I would happily pay between
50 or 100AUD a month for a tier where I can just BYOC against a stable-ish API
without risking a ban.

For most SaaS applications, I wish I could use a paid native app instead of a
garbage SPA. I like native apps because performance is the default and not a
distant post-PMF fantasy, and because they all look the same. Increasingly I
think design is an unwelcome form of narcissism. Every day a thousand designers
reinvent the `<button>` _ab initio_, and for what purpose? Your app has not
earned the right to have a design language. Give me the most boring AppKit
interface imaginable and I'm happy.

Why don't companies let users BYOC? Because there is a cost to stability: closed
protocols can evolve faster than open ones, and SaaS applications evolve fastest
because developers can update the client software on page load. Controlling the
client makes monetization easier, because you can serve people ads, and they
can't use ad-free clients.

The peak of the [Laffer curve][laf] is when the company that runs the protocol
can iterate fast and monetize its users sustainably. The peak is passed when, to
squeeze more money out of users, the user experience is made worse, and the
users, lacking choice, simply leave.

[laf]: https://en.wikipedia.org/wiki/Laffer_curve

---

[^f1]:
    According to the [developer agreement][tw]: "You shall not [...] create or
    attempt to create a substitute or similar service or product to the X
    Applications".

[^f2]:
    My current approach, when I want to tweet something but not get distracted,
    is to add the tweet to my todo list.

[tw]: https://developer.x.com/en/developer-terms/agreement-and-policy
