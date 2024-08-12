---
title: Language Pragmatics Engineering
summary: On best practices and gradient descent.
card: language-pragmatics.jpg
card_source: |
    “Mechanism in amber, photograph, 1997, from the British Museum”, DALL-E, June 2022.
---

Summary:

1. The code that gets written is the code that's easier to write.
2. Anything not forbidden by the language semantics will be done as a "temporary
   fix".
2. Codebases decay along the gradient of expedient hacks.

Programming languages have syntax, semantics, and [pragmatics][pragma]: how the
language is used in practice. The latter is harder to design for. Language
pragmatics is tooling, best practices, and the code you see in the
wild. Pragmatics matter because they determine the shape of the efficient
frontier between expedience and engineering quality, and from then on it's
gradient descent.

[pragma]: https://en.wikipedia.org/wiki/Pragmatics

# Contents

1. [Types](#types)
1. [Shotgun I/O](#shotgun)
1. [Tooling](#tooling)
1. [Tedium](#tedium)

# Types {#types}

**Summary:** make types easier to define.

One fundamental principle of language pragmatics: the code that gets written is
the code that's easier to write. Consider Java and Haskell.

In Java, defining a class is a monumental task. The language certainly makes it
feel that way. You have to create a whole new file just for that class. The
fields must be written in quadriplicate: the fields themselves then the
constructor, the getters, and setters. `equals` and `hashCode` methods must be
implemented. The consequence: Java projects have fewer classes than they
otherwise would have. Classes tend towards [God classes][god]: large classes
with multiple incompatible uses and responsibilities. They accumulate, for
example, fields that start out as `null` and become non-`null` after certain
operations are performed on the class.

[god]: https://en.wikipedia.org/wiki/God_object

Whereas in Haskell a type is a few lines of code. You `derive` the equivalents
of `equals` and `hashCode`. The consequence: Haskell projects have more types
than equivalent Java projects have classes. Those types are narrower, more
specific. Instead of mutating fields on the same class, you can have distinct
types to represent the different stages that a value goes through as it
traverses the codebase. `null` goes away.

In Java the problem is compounded by the fact that the simulationist school of
OOP endorses the idea that each concept in the domain of discourse object should
correspond to a class. So while in a Haskell project you may have fifteen types
to represent a purchase order (representing different stages of
validation/enrichment, like the intermediate representations in a compiler), in
Java you'd have one omnipresent `Order` class that is tied to every component in
the system. In Haskell the types are [anemic][anemia] and proudly so, in Java
this would be called an antipattern.

[anemia]: https://martinfowler.com/bliki/AnemicDomainModel.html

# Shotgun I/O {#shotgun}

**Summary:** ban "quick fixes" at the level of language semantics, or make them
hard to write.

By analogy to [shotgun parsers][shotgun], shotgun IO is when effectful code is
smeared all over the codebase.

[shotgun]: http://langsec.org/papers/langsec-cwes-secdev2016.pdf

Consider Haskell vs. everything else. Many language communities have some best
practice about [moving IO to the edges][edge]. But there is no strict
language-level enforcement of this rule. Because of expedience, because you have
to release this tonight or else, someone puts some database access code deep
inside otherwise pure data transform code, along with a `FIXME` comment.

[edge]: http://eweise.com/post/sideeffects/

Gradually you go down the slope of pragmatics: I/O is spread across the
codebase. In Haskell, there is no way out: you have to use the `IO` monad. And
so if you're mixing IO and pure code, you know it, because it's right there in
the function signature, and that knowledge gives you an opposite gradient, you
know the direction you have to move to make the code cleaner.

Or consider something like the [Django ORM][django]: you can write some database
access code that returns a [`QuerySet`][qs]. Then you can either read from that
`QuerySet`, or pass it on to some other function, which may perform further
queries on it.

[django]: https://docs.djangoproject.com/en/4.2/topics/db/
[qs]: https://docs.djangoproject.com/en/4.2/ref/models/querysets/

A consequence of this is that anything that touches a `QuerySet` can kick off
database access. There is no requirement that database access be
centralized---in a [DAO][dao] class, for example---and so because of the
pressures of expedience database queries crawl, hack after hack, like hydrogen
molecules through the interstitial spaces of a crystal, over the entire
codebase.

[dao]: https://en.wikipedia.org/wiki/Data_access_object

# Tooling {#tooling}

**Summary:** bad tooling goes unused but is hard to displace, lowering economic
productivity.

In C and C++ there are many build systems and package managers, but none is the
obvious [focal point][fp] in the way [cargo][cargo] is for Rust. So people
vendor their dependencies, sometimes downloading them by hand. Smaller libraries
are often made [header-only][header], this is advetised as a feature because
they require less build system overhead to integrate into your projects. This
discourages dependencies, which is basically the same as discouraging the
division of labour, with obvious costs.

[fp]: https://en.wikipedia.org/wiki/Focal_point_(game_theory)
[cargo]: https://doc.rust-lang.org/cargo/
[header]: https://en.wikipedia.org/wiki/Header-only

And even bad tooling is hard to replace, again, because of coordination costs
and the permanence of focal points.

Consider Common Lisp: [ASDF][asdf] is the standard build system, and
[Quicklisp][ql] the standard package manager. ASDF doesn't support specifying
version bounds, and even if it did, Quicklisp doesn't support using them. And
Quicklisp, despite being over 10 years old, is still in beta and feels like
alpha quality software: Quicklisp downloads packages over HTTP, and you are at
the mercy of God and the feds. So while the rest of the world has moved on to
reproducible builds, determinism, and SAT solving for dependency resolution, in
the Common Lisp world you can't even fix dependency versions.

And who's going to replace this? You're going to rewrite everyone's ASDF system
definition files? There's an alternative to Quicklisp, called [CLPM][clpm]. It's
abandonware. We'll fix Common Lisp tooling the day after [we all move to
Mastodon][masto].

[asdf]: https://asdf.common-lisp.dev/
[ql]: https://www.quicklisp.org/beta/
[clpm]: https://www.clpm.dev/
[masto]: /article/youre-not-going-anywhere

And somehow Common Lisp tooling is a pleasure to use compared to the tooling for
Python, a language that has multiple orders of magnitude more headcount and
investment.

Fixing entrenched tooling requires building an alternative that's 10x better,
making the transition process as seamless as possible, and then reaching out and
opening PRs in every repository under the sun making the necessary
changes. People could hate the old tooling and it's still costly for them to
change.

# Tedium {#tedium}

**Summary:** programmers will go to great lengths to avoid writing tedious code,
to the detriment of other things.

[Erik Naggum][erik] and the [parable of the catapult][parable]:

[erik]: http://www.loper-os.org/?p=165
[parable]: https://groups.google.com/g/comp.lang.lisp/c/Vn31kjztWpQ#52564cc186195b05

>People use C because it _feels_ faster. Like, if you build a catapult strong
>enough that it can hurl a bathtub with someone crouching inside it from London
>to New York, it will feel _very_ fast both on take-off and landing, and
>probably during the ride, too, while a comfortable seat in business class on a
>transatlantic airliner would probably take less time (except for getting to and
>from the actual plane, of course, what with all the "security"¹) but you would
>not _feel_ the speed nearly as much.

The argument here is there's two kinds of productivity: productivity in the
small (immediately) and productivity in the large (when you add up the cost of
testing, bugfixes, refactoring, onboarding etc.). C is productive in the small
and unproductive in the large.

You observe this with text editing. Moving with the arrow keys, erasing text by
holding backspace for an eternity _feels_ slow, but it is predictable and
reliable. Vim or Emacs-style editing, where you fly through the buffer with
keybindings, feels faster, but a single wrong keypress puts your editor in some
unpredictable state it can be hard to recover from. Sometimes the midwit with
Notepad gets to the destination faster because they're not fiddling with their
psychotically-optimized Emacs `neuralink-mode` bindings.

You observe this with types. Dynamic types feel faster, at the REPL, when you're
[coding at 1Hz][hz], because you're not factoring in the (unseen) cost of future
bugs, the cost of refactors you _won't_ do because you don't have the confidence
to refactor which static types give you, the cost of legacy software that can't
be replaced because it can't be understood by anyone, the cost of a Python
server doing four requests per second while you pay five figures to AWS every
month.

[hz]: https://medium.com/hackernoon/software-development-at-1-hz-5530bb58fc0e

You observe this with macros. To avoid writing four lines of
almost-but-not-quite duplicated code programmers will reach for a macro
system. One-tenth of every OCaml or Rust codebase by mass is `#[attributes()]`
to tell the macro system to generate e.g. JSON serialization or pretty-printing
code or database interfaces. I do it too. In the grand scheme of things this
doesn't save that much time: tedious code is fast to write but it only _feels_
slow. What is the cost? The cost is the codebase becomes harder and harder to
understand, because all of the interesting behaviour is _outside the source
code_.

It's like those photographs of distant quasars: the "photograph" is the output
of a pipeline of Fortran codes written to interpret the data from six different
instruments written between 1960 and 2005.

With metaprogramming, your codebase is an input to a vast and unseen system: a
pipeline of build-time macroexpansion that spits out something that may very
well be unrecognizable. And for what? To save a minute of typing?  Because we
are programmers, not typists, and it is below our dignity to type?

Some languages do macros right, like Common Lisp, and you should take advantage
of them. For essentially every other language, metaprogramming should produce
outputs you can check into source control. That is: generated code is fine, but
you must be able to see it.

The central, underlying fallacy here is [salience bias][bias]: measuring what is
seen and not what is unseen.

[bias]: https://en.wikipedia.org/wiki/Salience_(neuroscience)#Salience_bias
