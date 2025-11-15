---
title: A Wishlist of Zero-Cost Abstractions
summary: A wishlist of language features, their use cases, and how they affect language complexity.
tags: [plt]
---

This article is a list, in no particular order, of features I want to see in
programming languages. I note practical use cases, and the extent to which these
features affect language complexity.

# Units of Measure

Type systems allow us to catch errors along the lines of trying to push a square
peg into a round hole: adding integers to strings or calling `map` on an
integer. A units of measure system allows us to further annotate our values
with, well, units.

Ideally, the language would allow us to specify both magnitudes -- such as mass,
time and velocity -- and units belonging to that magnitude -- grams, pounds both
measure mass. But even without that, the compiler should provide these
guarantees:

1. Naturally, the compiler checks operations for dimensional validity: you can't
   add a time-annotated value to a velocity-annotated value without compilation
   failing.
2. Dimensional analysis is automatically performed: dividing a value
   representing distance over one representing time, the resulting value should
   be annotated with a velocity unit. Ideally, the language would allow units
   and magnitudes to be defined in terms of arithmetic operators composing
   others, so the compiler can simplify the units in complex computations to
   their simplest, human-readable representation.
3. Whether automated conversion between different units (e.g., adding Celsius to
   Fahrenheit would yield a value converted to the obviously correct and
   canonical choice of degrees Kelvin) should be performed by the language is a
   matter of taste and depends on whether the language designers' place emphasis
   on correctness or ease of use.

As a brief aside, this is what the infamous Hungarian notation set out to
correct: as Joel Spolsky [noted][hungarian], the original intent was to prefix
variables with a short string denoting, not their type, but their _semantic
intent_. In the example he quotes, integer variables are tagged with their
coordinate system.

Admittedly, phantom types give us 90% of type-level semantic distinction with
10% of the work involved in bolting units of measure into a language. I still
believe that a units of measure system like the one found in [F#][fsharp], where
dimensional analysis is automatically carried out by the compiler, is a
worthwhile addition to any language that emphasizes compile-time correctness.

In terms of language complexity, units of measure can be entirely orthogonal to
the rest of the language, being unrelated to -- but "parallel to", for lack of a
better term -- type information. Typechecking can be implemented while
maintaining total ignorance over the units of measure, with unit checking and
dimensional analysis being carried out in a later stage of compilation. All
unit-related code (conversion etc.) can safely be converted to arithmetic
operations so code further down the compiler pipeline doesn't need to be updated
to take unit information into account.

In others, a units of measure system doesn't increase language or implementation
complexity.

# Effects

Most programming languages, with the exception of languages using monads for
mutation like Haskell, have an "everything is allowed" model of effects. Logging
calls, memory allocation, file access, etc. can be performed from just about
anywhere. An effects system allows the programmer to assign permissions to
volumes of their code: these parts can't allocate memory -- or, consequently,
call parts that allocate memory -- and must run in constant space, this part
can't talk over the network, this callback must be a pure function, etc.

What does an effects system do, exactly?

1. Functions -- or lexical blocks, for increased granularity -- are annotated
   with the side-effects they will carry out, or won't (for example, we can
   annotate a function as being banned from using the standard input/output
   streams, a pervasive side-effect). The compiler determines the function's
   _actual_ effects signature by looking at its body: a function's effects are
   the union of the effects of all its forms. The base case of this recursion is
   that some language built-ins will have side effects (`new` in C++ allocates,
   `open` in Python interacts with the filesystem), and also that FFI
   declarations must allow some way to claim "this foreign function, whose
   definition is inaccessible to you, has such and such effects".

   This allows the compiler to know the difference between what the code
   _actually does_, and what it should and shouldn't do, and treat the
   difference as an error.
2. The function's declared effects signature, if any, is checked against its
   actual effects: if we allocate memory, or call a function that allocates
   memory, and so forth, in a function which we have sworn to the compiler
   allocates nothing, then the compiler signals an error. (An obvious problem
   arises here: what if we need to add a debugging `printf` in a function we've
   declared as being free of IO? Or which is called by a function, somewhere up
   the call tree, that is declared as being IO-free? The debugging facilities
   would presumably provide a escape hatch from strict effect system
   enforcement.)
3. Calls to higher order functions are checked for effects-correctness. Just as
   a type checker prevents you from adding integers to strings, an effects
   system might prevent you from passing an impure function as the function
   argument of `map` -- making `map` automatically and safely parallelizable,
   _without_ requiring the entire language to be pure. A useful tool.

To elaborate upon #3, consider a sorting function, whose arguments are a
sequence of `T` and a _comparer function_ `T -> T -> Ordering`, where `Ordering`
is just an enum of equal, greater than and less than, as
in [Haskell][haskell-ord].

An effects system would allow us to annotate the comparer function with, for
instance, a declaration that it should not allocate memory. So, only functions
that don't allocate can be passed as arguments to the sorting function. If the
sorting function doesn't allocate memory, then this allows us to make precise
guarantees about the time-boundedness of the sorting function as a
whole[^real-time]. Other effects are also useful: annotating the function as
being pure would rule out a large category of behaviour inside of it, possibly
enabling better optimization.

Another use case would be for a library like [`libdill`][libdill], which implements
Go-style concurrency in C. Briefly, the function that takes a thunk and spawns a
thread for it, using an effects system, could be annotated to only allow pure
thunks to be passed to it, thus avoiding a whole category of concurrency bugs
without any special infrastructure (see [Rust][rust]), with the obvious downsides of
only being able to spawn purely-functional threads.

Do effect systems increase language or implementation complexity? As with the
case of units of measure, I don't think so. Obviously this depends on how
exactly you want them to work. A function's effects declaration can be totally
independent of its type signature, effects checking is also completely
independent from type checking, like units of measure.

Unlike units of measure, an effects system would not be a curiosity to be
brought up only on rare circumstances: side effects are everywhere and nearly
everywhere they are implicit, documented in comments and docstrings, or not
accounted for at all. An effects system makes them visible.

# Extensible Iteration

This is something I am less certain about. I've always looked at libraries
like [Iterate][iterate] and [For][for] as examples of what a good macro system
allows you to do -- not to mention Common Lisp's built-in Swiss Army knife
iteration construct, the [`loop`][loop] macro.

Still, I am not sure macro-based extensible iteration is the right approach for
a language having a sophisticated type system. Maybe there's a sweet spot, a
system having both a type-centric extensibility mechanism, _plus_ macros to make
it look more like a part of the language itself, more like a DSL.

I don't know what this would look like -- combining the terseness and
obviousness of `loop` code with a type-driven iterator mechanism -- but I would
like to explore this in the future. I am certain something worthwhile hides
here.

# Refinement Types

Refinement types allow us to annotate terms with predicates, e.g., an integer
term with `n != 0` or a collection with a 'non-empty' tag. Functions can be
annotated with refinement predicates -- the obvious example is division
requiring the denominator be non-zero -- and their calls are checked
automatically -- that is, every division must be in a branch where the
denominator can be proved to be non-zero.

This is obviously useful, but less straightforward to implement than either
units of measure and an effects system. Additionally, unlike any of the previous
features I mentioned, refinement types are not orthogonal to the type system but
a fundamental change to how it works. You can't bolt this onto an existing
language, at least not without almost certainly losing important guarantees of
your typechecking algorithm, like being able to complete in finite time). So any
language that implements this universally useful feature will almost certainly
have to be designed from the ground up, or be a feature-focused fork of a
language like Rust.

# Acknowledgements

[Tim Herd][eqdw] helpfully proofread this post.

# Footnotes

[^real-time]:
    This is especially useful for real-time domains,
    see [this post][audio-realtime]. Specifically, rules 3 and 4 could be
    enforced trivially using an effects system. Being able to move informal
    guidelines into hard, verifiable constraints in a _huge_ improvement.

[hungarian]: https://www.joelonsoftware.com/2005/05/11/making-wrong-code-look-wrong/
[fsharp]: https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/units-of-measure

[audio-realtime]: https://atastypixel.com/four-common-mistakes-in-audio-development/

[haskell-ord]: https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Ord.html#t:Ordering
[libdill]: https://github.com/sustrik/libdill
[rust]: https://rust-lang.org/

[iterate]: https://iterate.common-lisp.dev/
[for]: https://shinmera.github.io/for/
[loop]: https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm

[eqdw]: http://eqdw.net/
