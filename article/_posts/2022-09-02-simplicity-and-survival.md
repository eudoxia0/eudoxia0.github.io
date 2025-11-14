---
title: Simplicity and Survival
summary: How do programming languages survive indefinitely?
tags: []
---

For programming languages to survive indefinitely, they have to either:

1. Be simple enough to be implemented with little cost.

2. Become irreplaceable critical infrastructure of many large organizations.

C++ will survive indefinitely. Even though it is so complex that no single
person understands the language in its entirety, many Fortune 500 companies have
multi-million SLOC C++ codebases as part of their critical infrastructure, and
they have neither the desire nor the ability to rewrite them. The mystery box
that nobody can characterize the behaviour of is irreplaceable until its
business function is obsolete. C++ is, in a sense, a [sunk cost][sunk]. Large
organizations can fund the maintanance of C++ compilers indefinitely into the
future.

Forth, as a family of languages, will exist forever since you can write an
interpreter in an evening. Scheme will exist indefinitely since there are many
implementations, built largely by a single author, that conform to the latest
standards. If all of them bitrot, a new implementation can be written for little
cost. The only tricky part of Scheme is continuations.

Consider [Ada][ada]. It's a beautiful, very unfairly maligned language, but it
is undeniably very big. If [AdaCore][adacore] folds, who's going to maintain GCC
Ada? The open source Ada community is very small and the commercial vendors have
done little to promote it. Writing an open source Ada compiler from scratch is a
tremendous undertaking. Ada can die.

Consider [Common Lisp][lisp]: it has many existing implementations, but the only
one that matters is [SBCL][sbcl], because it's the most widely-used open source
one. SBCL is actively maintained by a [small team][sbcl-team]. Google
contributes a lot because [Google Flights][flights] is internally an immense
Common Lisp codebase they bought from [ITA Software][ita].

But when Google moves flight search to something else, and if the SBCL
maintainers retire, who's going to pick up the maintenace of a ~460,000 SLOC
compiler written in a dynamically-typed language?

How about writing a new implementation? Common Lisp is a huge language: the
[spec][hyperspec] has over 1000 pages. While much of that can be implemented in
userspace, Common Lisp has significant implementation complexity, mostly in the
object system and the condition system. Building a high-performance, new
implementation is a Herculean task. Though, after a discussion with a fellow
lisper, I think [CLISP][clisp] could be cleaned up and turned into a
pedagogical/bootstrapping implementation. But, at present, Common Lisp can die
if a small number of SBCL contributors retire and the compiler starts
bit-rotting and nobody picks up the work.

A [central constraint][goals] in the design of [Austral][austral] is the
language should be easy to implement. Not just because I was the only person
writing the compiler, but because I want Austral to be a [hundred year
language][hundred], where you can reliably run code from decades ago (Common
Lisp is like this: it is possible). If, for whatever reason, the source code of
the bootstrapping compiler was lost, it would be trivial to rewrite it again
from the ~100 pages of [spec][austral-spec] (roughly half of which describes the
rationale for various design decisions).

Anyone who has built a compiler and reads the spec can build an Austral
compiler, the language is designed such that it is obvious what any piece of
code will compile down to. Because it's statically typed, it's more complex than
building a minimal Scheme interpreter, but it's infinitely less complex than a
C++, Ada, Common Lisp, Scala, etc. compiler.

So this is the fate of programming languages: past sunk cost escape velocity you
can be as heavy as the [Vasa][vasa]; only small languages, however, can
[abort-to-orbit][ato].

|                     | Small language | Big language |
| ------------------- | -------------- | ------------ |
| **Small community** | Live           | Die          |
| **Large community** | Live           | Live         |

[hundred]: http://www.paulgraham.com/hundred.html
[austral]: https://austral-lang.org/
[ada]: https://en.wikipedia.org/wiki/Ada_(programming_language)
[adacore]: https://www.adacore.com/
[flights]: https://www.google.com/flights
[ita]: https://www.itasoftware.com/
[sbcl]: https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
[sunk]: https://en.wikipedia.org/wiki/Sunk_cost
[lisp]: https://lisp-lang.org/
[hyperspec]: http://www.lispworks.com/documentation/HyperSpec/Front/
[sbcl-team]: https://github.com/sbcl/sbcl/pulse/monthly
[clisp]: https://clisp.sourceforge.io/
[austral-spec]: https://austral-lang.org/spec/spec.html
[goals]: https://austral-lang.org/spec/spec.html#goals
[vasa]: https://www.stroustrup.com/P0977-remember-the-vasa.pdf
[ato]: https://en.wikipedia.org/wiki/Space_Shuttle_abort_modes#Abort_to_orbit
