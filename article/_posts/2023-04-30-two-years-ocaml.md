---
title: Two Years of OCaml
summary: My thoughts on OCaml after two years of building a compiler with it.
---

The other day I this [this post on OCaml][post] discussed in [Hacker News][hn] and [Lobsters][lob].

[post]: https://osa1.net/posts/2023-04-24-ocaml-thoughts.html
[hn]: https://news.ycombinator.com/item?id=35699697
[lob]: https://lobste.rs/s/jvxb8s/my_thoughts_on_ocaml

Almost two years ago I rewrote the [Austral compiler][austral] from [Standard ML][sml] to [OCaml][ocaml], so I thought I'd share my thoughts on OCaml after using it in writing a complex software project, explaining what is good and what is bad and how it compares mainly to Haskell.

[austral]: https://github.com/austral/austral
[sml]: https://en.wikipedia.org/wiki/Standard_ML
[ocaml]: https://ocaml.org/

# Syntax {#syntax}

- disclaimers
    - de gustibus yadda yadda
    - I am not a partisan about syntax.
    - I genuinely think C, Java, Lisp, Pascal and ML can be beautiful in their own ways.

## Aesthetics {#aesthetics}

- good for math
    - ml was born a theorem prover
- bad for everything else
- ML syntax feels like it's "hanging in the air"
- expression orientation cuts both ways
    - pro: simple and general
    - cons: it's easy to create complicated, arbitrarily nested code
        - requires discipline to avoid doing this
        - statement oriented languages force you to flatten things

## declaration order {#order}

- the following code
    - example
- will not compile, you have to rewrite it like this
    - example
- to have the declarations appear in order. alternatively, you can use an and chain
    - example
- same is true for types
- and chains let you put declarations in the right order
- having an entire module be a `let rec ... and ... and ... and` feels brittle
- you also can't interleave functions and types
- so you have to put all your types upfront

## currying is bad {#currying}

- punctuation is good
- adjacency is not syntax
- it's "cute"
- comes at a high cost
- every time you
    - forget an argument
    - add an argument
    - mess up argument order
- you don't get an error to that effect
    - rather, you get a type error that is a consequence of your mistake
- you learn gradually to pattern-match error messages to actual errors
    - if you see an error like "something something is a function typoe", you forgot a type
    - if you see an error like "foo is not a function type", you added an extra type
- You can avoid it with tuples but it makes type annotations harder

## awkward comment syntax {#comments}

- no single-line comment syntax
- only block comments
- `(* derp *)` is torture to write
- comments can be nested, as in Common Lisp, which is good for commenting-out large chunks of code

## type annotation syntax {#type-annot}

- inconsistent
    - tuples
        - `a * b` is a type
        - `(a, b)` is a tuple constructor
        - haskell does this right
    - generics are kinda weird
        - `int list` means `list[int]`
        - modern languages have converged on a notation like `name[tyarg, ..., tyarg]`.
        - haskell not much better: `Maybe Int`. For the love of God: give me punctuation.

## semicolons work sometimes {#semicolons}

- semicolons are, as you'd expect, for sequencing
- example that works
- example that doesn't work
- makes it hard to insert a debugging print somewhere

## weirdnesses {#weird-syntax}

- as above: tuple type vs. tuple constructor
- tuple is `(a,b,c)` list is `[a;b;c]`
- i guess `,` is an infix operator or something

## nested match statements {#nested-match}

- which appear everywhere
- have to be parenthesized, so larger functions end up looking lisp-ish with a chain of close parentheses at the end
    - you can avoid this by refactoring each match into a separate function, but then you get the long and chains

# PPX {#ppx}

- it's kind of necessary
- common lisp is the only language that really does macros right
- doesn't play well with modules

# Tooling {#tooling}

- docs are useless if i can't find them
- tooling is useless if only experts who have been using the language for years know how to get a good setup going
- my standard for tooling:
    1. i should be able to run the commands in the current version of the documentation and have them work
    2. there should be a project generator where i type in a name, and it spits out a project skeleton with:
        1. library code
        2. executable code
        3. unit tests
        4. stub documentation generation
        5. all the relevant commands (build, test, generate docs) should work for the generated skeleton right off the bat
- set up dune and opam, angrily
- haven't used it enough
- kinda works sometimes

# how the fuck do i profile {#profiling}

- seriously
- i looked this up
- all the information assumes you're running ocamlc manually
- like you're building a one-file program with no dependencies
- i need to run this at the dune/opam level
- cobbled something together with prof that kinda works
- but then I forgot what I did to run it

# testing {#testing}

- some tasks have a higher activation energy---effort to get started---than others
- the code that gets written is the code that is easy to write
- languages, tooling affect the shape of the activation energy landscape, and channel you in a particular direction
- different languages make writing unit tests easier or harder
    - python makes it easy
    - write a class with methods that have the right name, everything gets autodiscovered
    - unit test autodiscovery is a _huge_ boon
    - it encourages writing tests
- languages that require you to write and register your test functions, like OCaml and Haskell, raise the activation energy to do this
- maybe there is an OCaml library that does test autodiscovery and makes it easier to write tests
    - but the existence of better tooling is worthless
    - the Right Way to do things should be in the project skeleton generator
- if i had more time i'd install the ocaml tooling and try to get a simple hello world app with unit tests going and record all the horrors

# Modules: Better is Worse {#modules}

- central feature that makes sml and ocaml attractive
    - what they are
    - few languages have modules
        - ada works like this
- modules hide too much
    - example
    - sml deals with this
- coherence
    - requires special constraint syntax
- equality is a special case
    - shows things are bad
    - have the courage of your convictions
- implicit specialization is good
- integer ordering by divisibility
- ad-hoc or generic
    - unclear when to use generic types and when to specialize

# Minor Complains {#misc-complaint}

- compare returns an int
    - probably too late to change this but come on
- all sorts of language features i don't fucking know
- zoo of conversion functions
    - again, have the courage of your convictions

# At Least It's Not Haskell: {#haskell}

- Haskell is better:
    - separating type annotations
    - less custom syntax
    - no declaration order
    - fine-grained imports
    - where i've used haskell tooling it kinda feels better
        - this may be because i haven't used it enough
- Haskell is worse:
    - those dreadful infix operators
    - indentation sensitivity
    - lazy evaluation
    - lazy data structures
- Haskell is even:
    - purity
        - yeah i guess this has a higher perf ceiling given an arbitrarily smart compiler
        - compilers are not arbitrarily smart

# My OCaml Style {#my-style}

- tweet
- conservative ocaml
- my ocaml vs. all the ocaml i see in the wild

# When to use OCaml? {#when-to-use}

- ocaml is:
    - statically typed
    - has a solid type system
        - by which I mean algebraic data types with exhaustiveness checking
    - compiles to native
    - good performance
    - not too galaxy brained
    - lets you mutate and do IO to your heart's content
    - has a decent enough ecosystem