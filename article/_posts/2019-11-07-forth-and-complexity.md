---
title: Forth and Complexity
summary: On the unsustainable complexity of modern computing.
tags: [plt, forth]
---

[_My history with Forth & stack machines_][yosef] is one of my favourite
technical posts. While the author decides [Forth][forth] is not for him, it's
written from a place of respect and admiration to the philosophy behind it:

>Forth is the approach to engineering aiming to produce as small, simple and
>optimal system as possible, by shaving off as many requirements of every
>imaginable kind as you can.
>
>[...]
>
>This stack business? Just a tiny aspect of the matter. You have complicated
>expression graphs? _Why_ do you have complicated expression graphs? The reason
>Forth the language doesn't have variables is because you can eliminate them,
>therefore they are _junk_, therefore you _should_ eliminate them. What about those
>expressions in your Forth program? Junk, most likely. _Delete!_

Modern computer systems are too complex for any one person to understand.

In many contexts this is regrettable: whenever I see [a touchscreen on a
safety-critical system][tesla] I cringe because I know that no one person can
understand the system, from gates to software, let alone prove it correct. The
text rendering component in a modern operating system alone is the work of a
lifetime[^freetype].

The system as a whole is the program, virtual machine, process, kernel,
firmware, SoC, and each of these is understood by a disjoint set of people, and
every one of those people has only a partial view of it. The [Alto][alto] was
probably the last productive system fully understood by the number of people you
can fit around a table.

Since programming languages are the means with which we build software, it's
sensible to ask what part of the blame they are to bear.

Most programming languages manage complexity by hiding it either
administratively (at module and datatype boundaries, through visibility
qualifiers) or at runtime (information hiding).

The Forth approach is to throw complexity in your face: it is
_complexity-evident_ the way [Merkle trees][merkle] are tamper-evident. Stack
shuffling words stick out. Lengthy definitions are hard to follow: the reader
needs to simulate the stack in their head, and the longer the definition, the
greater the incentive to factor it. There are no namespaces and no hierarchy of
visibility and no type declarations, because these would allow you to build
programs larger than you can read in one sitting. Even C has more affordances:
data structure declarations, [`sizeof()`][sizeof], and a nominal type system are
an embarrassment of riches in comparison.

Factoring words is the only mechanism of abstraction in Forth, consequently
after a certain threshold the system is too large to be added to. This is a
feature.

Forth won't help you build large systems. Instead it asks: why are you building
large systems? Why are you building systems more complex than what trivially
fits in your head? The Forth approach is to constantly confront the programmer
and force them to trim what is unnecessary.

We can apply this argument in almost every direction, to every language feature:
you need a language with a module system and separate compilation to speed up
your builds. Why? Why do you need hundreds of separate modules? Why don't you
build something simple enough that it builds instantly? You need a type
system. Why are you building something so large you can't understand it without
mechanical aid? You need information hiding. Why are your data structures so
complex you need to hide them behind a fig leaf? _n_ is, after all, [always
small][pike].

I like proper modules and types, and don't advocate any of the above, but
[constrained writing][cw] of the kind Forth forces on the programmer can induce
creativity, which makes it a [tool of thought][iverson].

[^freetype]:
    [FreeType][freetype] alone is 120k lines of C. I haven't measured
    [Pango][pango].

[yosef]: http://yosefk.com/blog/my-history-with-forth-stack-machines.html
[forth]: https://www.forth.com/forth/
[tesla]: https://en.wikipedia.org/wiki/Tesla_Model_S#Instrument_panel
[alto]: https://en.wikipedia.org/wiki/Xerox_Alto
[merkle]: https://en.wikipedia.org/wiki/Merkle_tree
[sizeof]: https://en.wikipedia.org/wiki/Sizeof
[pike]: https://users.ece.utexas.edu/~adnan/pike.html
[cw]: https://en.wikipedia.org/wiki/Constrained_writing
[iverson]: https://dl.acm.org/citation.cfm?id=358899
[freetype]: https://en.wikipedia.org/wiki/FreeType
[pango]: https://en.wikipedia.org/wiki/Pango
