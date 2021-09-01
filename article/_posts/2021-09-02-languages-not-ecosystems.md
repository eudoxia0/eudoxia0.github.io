---
title: Languages are not Ecosystems
summary: The size of language ecosystems obscures the inherent design problems of programming languages.
tags: [plt]
---

When discussing the relative merits of programming languages, people speak of
the language and its ecosystem interchangeably. For good reason: the two are not
separable; with few exceptions, you can't mix-and-match language and ecosystem.

Ecosystems are important. Things like HTTP servers, JSON/YAML/etc. libraries,
database access and migration tools, etc. are commodities. If you're building a
commercial product, and development regularly halts for two weeks while the team
builds some infrastructural component that is widely available in the ecosystems
of every major programming language, you should reconsider your priorities.

But what is rarely appreciated is that the importance of the ecosystem is not
constant with time.

Software projects have two phases: in the **early phase**, development is
frenetic, and you're adding new libraries to the dependency manifest on a
near-daily basis as large volumes of the codebase spring out of the
aether. Projects that are not abandoned eventually reach the **late phase**, a
stage of maturity where work mainly consists of maintenance and refinement of
existing components, and rarely are new things added to the dependency
manifest. Features are still being added, but these features are typically more
of the same (e.g.: a new endpoint, a new database access object), which is to
say they use the existing dependencies.

In the early phase, _the ecosystem matters most_. Adding a line to a dependency
manifest is more conducive to a flow state than implementing an entire library
yourself. Getting to a demoable product earliest matters.

In the late phase, where the role of the ecosystem is fixed, _the language
matters most_. When the ecosystem is all used up, the intrinsic properties of
the language become visible. The reliability of software, the ease with which it
can be maintained or refactored, the ease with which new people can be
onboarded: these all stem from the intrinsic properties of the language, not the
ecosystem. No amount of libraries is going to help your productivity if the
highly-dynamic nature of the language makes confident refactoring impossible.

This is why [Dropbox][dropbox], [Instagram][insta], and [Facebook][fb] have
expended huge resources on enabling typechecking of their Python codebases, and
why Facebook created a [typed PHP][hack].

[dropbox]: https://dropbox.tech/application/our-journey-to-type-checking-4-million-lines-of-python
[insta]: https://github.com/Instagram/MonkeyType
[fb]: https://github.com/facebook/pyre-check
[hack]: https://en.wikipedia.org/wiki/Hack_(programming_language)
