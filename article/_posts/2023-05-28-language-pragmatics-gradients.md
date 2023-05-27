---
title: Language Pragmatics Engineering
summary: The code you can write is the code that gets written.
---

Programming languages have syntax, semantics, and pragmatics: how the language
is used in practice. The latter is harder to design for. Language pragmatics is
tooling, best practices, and the code you see in the wild. Pragmatics matter
because they determine the shape of the efficient frontier between expedience
and engineering quality, and then it's gradient descent.

# Types

One fundamental principle of language pragmatics: the code that gets written is
the code that's easier to write. Consider Java and Haskell.

In Java, defining a class is a monumental task. The language certainly makes it
feel that way. You have to create a whole new file just for that class. The
entire thing has to be written in triplicate. `equals` and `hashCode` methods
must be implemented. The consequence: Java projects have fewer classes than they
otherwise would have. Classes tend towards God classes: large classes with
multiple incompatible uses and responsibilities. They accumulate, for example,
fields that start out as `null` and become non-`null` after certain operations
are performed on the class.

Whereas in Haskell a type is a few lines of code. You `derive` the equivalents
of `equals` and `hashCode`. The consequence: Haskell projects have more types
than equivalent Java projects have classes. Those types are narrower, more
specific. Instead of mutating fields on the same class, you can have distinct
types to represent the different stages that a value goes through as it
traverses the codebase. `null` goes away.

In Java the problem is compounded by the fact that the simulationist school of
OOP endorses the idea that each concept in the domain of discourse object should
correspond to a class. So while in a Haskell project you may have fiften types
to represent a purchase order (representing different stages of
validation/enrichment, like the intermediate representations in a compiler), in
Java you'd have one omnipresent `Order` class that is tied to every component in
the system. In Haskell the types are anemic and proudly so, in Java this would
be called an antipattern.

# Shotgun I/O

By analogy to [shotgun parsers][shotgun], shotgun IO is when effectful code is
smeared all over the codebase.

Consider Haskell vs. everything else. Many language communities have some best
practice about moving IO to the edges. But there is no strict language-level
enforcement of this rule. Because of expedience, because you have to release
this tonight or else, someone puts some file access code deep inside otherwise
pure data-wrangling code, along with a `FIXME` comment.

Gradually you go down the slope of pragmatics: I/O is smeared across the
codebase. In Haskell, there is no way out: you have to use the `IO` monad. And
so if you're mixing IO and pure code, you know it, because it's right there in
the function signature.


Or consider something like the Django ORM: you can write some database access
code that returns a `QuerySet`. Then you can either read from that `QuerySet`,
or pass it on to some other function, which may perform further queries on it.

A consequence of this is that anything that touches a `QuerySet` can kick off a
database transaction. There is no requirement that database access be
centralized---in a [DAO][dao] class, for example---and so because of the
pressures of expedience database queries spread, one temporary hack at a time,
across the entire codebase.

# Tooling

In C and C++ there are many build systems and package managers, but none is the
obvious [focal point][fp] in the way [cargo][cargo] is for [Rust][rust]. So
people vendor their dependencies, sometimes downloading them by hand. Smaller
libraries are often advertised as being [header-only][header], i.e. requiring
less build system overhead to integrate into your projects. This discourages
dependencies, which is basically the same as discouraging the division of
labour, with obvious costs.
