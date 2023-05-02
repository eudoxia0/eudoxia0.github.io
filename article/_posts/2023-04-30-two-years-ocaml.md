---
title: Two Years of OCaml
summary: My thoughts on OCaml after two years of building a compiler with it.
card: two-years-ocaml.jpg
card_source: |
    [_Prisoners on a Projecting Platform_][link], _Carceri d'invenzione_, Giovanni Battista Piranesi, 1761.

    [link]: https://en.wikipedia.org/wiki/File:Giovanni_Battista_Piranesi_-_Le_Carceri_d%27Invenzione_-_Second_Edition_-_1761_-_10_-_Prisoners_on_a_Projecting_Platform.jpg
---

The other day I saw [this post on OCaml][post] discussed in [Hacker News][hn]
and [Lobsters][lob].

[post]: https://osa1.net/posts/2023-04-24-ocaml-thoughts.html
[hn]: https://news.ycombinator.com/item?id=35699697
[lob]: https://lobste.rs/s/jvxb8s/my_thoughts_on_ocaml

Almost two years ago I rewrote the [Austral compiler][austral] from [Standard
ML][sml] to [OCaml][ocaml], so I thought I'd share my thoughts on OCaml after
using it in writing a complex software project, explaining what is good and what
is bad and how it compares mainly to [Haskell][hs].

[austral]: https://github.com/austral/austral
[sml]: https://en.wikipedia.org/wiki/Standard_ML
[ocaml]: https://ocaml.org/
[hs]: https://www.haskell.org/

If this seems overwhelmingly negative, it's because the things OCaml does right
are really just uncontroversial. They're _obviously_ right and hardly worth
pointing out. It's actually a weirdly optimistic thing: that a language with so
many glaring deficiencies stands far above everything else.

# Contents

1. [Syntax](#syntax)
    1. [Aesthetics](#aesthetics)
    1. [Declaration Order](#order)
    1. [Comments](#comments)
    1. [Type Specifiers](#type-annot)
    1. [Generic Types](#generic)
    1. [Type Annotations](#annot)
    1. [Semicolons Work Sometimes](#semicolons)
    1. [Inconsistencies](#inconsistency)
    1. [Nested Match Expressions](#nested-match)
1. [Modules: Better is Worse](#modules)
   1. [Modules Are Better](#better)
   1. [Modules Are Worse](#worse)
   1. [Equality](#equality)
   1. [Multiple Implementations Are Unnecessary](#multiple-impls)
1. [Semantics](#semantics)
    1. [Currying is Bad](#currying)
    1. [Type Inference is Bad](#inference)
    1. [Mutation](#mutation)
1. [Pragmatics](#pragmatics)
    1. [PPX](#ppx)
    1. [Tooling](#tooling)
    1. [How Do I Profile?](#profiling)
    1. [Testing](#testing)
    1. [Minor Complaints](#misc-complaint)
1. [At Least It's Not Haskell](#haskell)
1. [My OCaml Style](#my-style)
1. [Should You Use OCaml?](#should)

# Syntax {#syntax}

Yeah, yeah, _de gustibus_, and people spend [_way_ too much time whining about
syntax][wadler] and other superficial issues, rather than focusing on language
semantics and pragmatics.

[wadler]: https://en.wikipedia.org/wiki/Law_of_triviality

But I'm not a partisan about syntax. I genuinely think code written in C, Java,
Lisp, Pascal, and ML can be beautiful in different ways. Some of these
complaints will be personal, others will be more objective.

## Aesthetics {#aesthetics}

[ML][ml] was born as the implementation language of a [theorem prover][lcf], so
naturally the syntax is meant to look like whiteboard math.

[ml]: https://en.wikipedia.org/wiki/ML_(programming_language)
[lcf]: https://en.wikipedia.org/wiki/Logic_for_Computable_Functions

And it does look good for math. If you're writing something like a symbolic
differentiation engine:

```ocaml
type expr =
  | Const of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec diff (e: expr): expr =
  match e with
  (* c' = 0 *)
  | Const _ ->
     Const 0.0
  (* (f + g)' = f' + g' *)
  | Add (f, g) ->
     Add (diff f, diff g)
  (* (f - g)' = f' - g' *)
  | Sub (f, g) ->
     Sub (diff f, diff g)
  (* (fg)' = f'g + fg' *)
  | Mul (f, g) ->
     Add (Mul (diff f, g), Mul (f, diff g))
  (* (f/g)' = (f'g - g'f)/gg *)
  | Div (f, g) ->
     Div (Sub (Mul (diff f, g), Mul (f, diff g)), Mul (g, g))
```

Then it's simply delightful. It does tend to fall apart for everything else
however.

OCaml, like Haskell, is [expression-oriented][expr], meaning that there is no
separation of statements (control flow, variable assignment) and expressions
(evaluate to values) and instead everything is an expression. Most expressions
in OCaml tend not to have terminating delimiters.

[expr]: https://en.wikipedia.org/wiki/Expression-oriented_programming_language

This is very vague, but ML-family (meaning Standard ML, OCaml, Haskell and
derivatives) code often feels like the expressions are "hanging in the air", so
to speak. Terminating delimiters (like semicolons in C or `end` in
[Wirth-family][wirth] languages) make the code feel more "solid" in a way.

[wirth]: https://wiki.c2.com/?WirthLanguages

And expression orientation (which most modern languages advertise as a feature)
cuts both ways. The benefit is simplicity and symmetry: you don't need both an
`if` statement and a ternary if expression. You can have a big expression that
computes a value and then assigns it to a containing `let`, like so:

```ocaml
let a: ty =
  match foo with
  | Foo a ->
    (* ... *)
    let bar =
      (* ... *)
      (* imagine deeply nested expressions *)
in
(* etc *)
```

Without having to use an uninitialized variable or refactor your code into
too-small functions. However, this generality comes at a cost: you can write
arbitrarily deep and complex expressions, where a statement-oriented language
would force you to keep your code flatter and break it down into small
functions.

It takes discipline to write good code in an expression-oriented language. I
often see e.g. [Common Lisp][cl] code with functions hundreds of lines
long. It's almost impossible to track the flow of data in that context. This, by
the way, is why [Austral][aus] is statement-oriented, despite every modern
language moving towards expression-oriented syntax.

[aus]: https://austral-lang.org/
[cl]: https://lisp-lang.org/

## Declaration Order {#order}

In OCaml, like in C, declaration must appear in dependency order. That is, you
can't write this:

```ocaml
let foo _ =
  bar ()

let bar _ =
  baz ()

let baz _ =
  print_endline "muh one-pass compilation"
```

Instead you must write:

```ocaml
let baz _ =
  print_endline "muh one-pass compilation"

let bar _ =
  baz ()

let foo _ =
  bar ()
```

Alternatively, you can use `and` to chain your declarations:

```ocaml
let rec foo _ =
  bar ()

and bar _ =
  baz ()

and baz _ =
  print_endline "muh one-pass compilation"
```

And the same thing is true of types:

```ocaml
type foo = Foo of bar

and bar = Bar of baz

and baz = Baz of unit
```

_But_, you can't interleave an `and`-chain of functions with one of types. So
you have a choice:

1. You can write all of your code backwards, with the utility functions and the
   leaf-nodes of the call graph up front, and the important code at the bottom.

2. Or, you can write a big `and`-chain of types at the start of the file,
   followed by a big `and`-chain of functions for the remainder of the file.

Option one makes the code harder to read, and option two feels incredibly
brittle.

Haskell gets this right: declaration order is irrelevant. Austral also allows
declarations to appear in any order, partly because of my frustration with this
aspect of OCaml.

Note that having a module interface doesn't save you here, because interfaces
and modules are compiled separately. So if you have a `Foo.mli` file like this:

```ocaml
val foo : unit -> unit
val bar : unit -> unit
val baz : unit -> unit
```

The corresponding `.ml` file _still_ has to have the declarations appear in
dependency order.

## Comments {#comments}

OCaml has no single-line comment syntax. Instead, you have block comment syntax,
like so:

```ocaml
(* I'm a comment. *)
```

The double parenthesis-asterisk pair is torture to write on my fingers. Again,
Haskell does this right: single-line comments are a double hyphen. Quick and
easy.

Unlike C and other languages, comments can be nested, like in Common Lisp:

```ocaml
(* I'm a (* nested *) comment. *)
```

This is useful for commenting-out large chunks of code.

## Type Specifiers {#type-annot}

The syntax for type specifiers and type annotation is a bit of a pain.

First, there's inconsistency: `a * b` is the type specifier for a tuple
(asterisk as in [product][prod]), but the syntax for constructing a tuple is
`(a, b)`:

[prod]: https://en.wikipedia.org/wiki/Product_type

```ocaml
let derp: int * string = (0, "")
```

Again, Haskell gets this right:

```haskell
derp :: (Int, String)
derp = (0, "")
```

Similarly, the unit type is `unit` but its value is `()`. And, again, Haskell
gets this right: the unit type is the empty tuple, denoted `()`, and its sole
value is `()`.

## Generic Types {#generic}

Generics are weird. Most modern languages are moving towards `Name[Arg, ...,
Arg]` as the syntax for a generic type specifier. So in Swift you'd write
`List[Int]`, but in OCaml you write `int list`. The order is inverted, but I
think the argument is that you can read it like it's English?

Haskell is not much better: `List Int`. This obsession with terseness is a big
problem: please give me punctuation.

## Type Annotations {#annot}

Type annotations go in the same line as functions:

```
let derp (a: foo) (b: bar option) (c: baz * quux): herp =
  (* ... *)
```

Which isn't _bad_, but it's a functional language, so you end up passing more
stuff in. Haskell makes this a bit more comfortable:

```haskell
derp :: Foo -> Maybe Bar -> (Baz, Quux) -> Herp
derp a b c =
  -- ...
```

## Semicolons Work Sometimes {#semicolons}

Semicolons let you sequence statements. They work inconsistently. This works:

```ocaml
let foo _ =
  print_endline "Hello, world!";
  true
```

This doesn't:

```ocaml
let foo _ =
  if true then
    print_endline "Hello, world!";
    true
  else
    false
```

Which makes it hard to insert debugging `print` statements. You have to
transform the above into the more tiresome:

```
let foo _ =
  if true then
    let _ = print_endline "Hello, world!" in
    true
  else
    false
```

There's an easy way to solve this: add an `end if` delimiter. Again: terseness
bites.

## Inconsistencies {#inconsistency}

As above: the syntax for tuple and unit types and values is inconsistent.

The syntax for a list literal is `[1; 2; 3]`. This is because the comma is an
infix operator, so if you typo this as `[1, 2, 3]` you don't get a syntax error,
that's a singleton list with a tuple as its element type.

Types are defined with `type`, both in module interfaces and module bodies:

```ocaml
module type FOO = sig
  type t
end

module Foo: FOO = struct
  type t
end
```

But values are _defined_ with `let` and _declared_ with `val`:

```ocaml
module type FOO = sig
  val a: int
end

module Foo: FOO = struct
  let a: int = 10
end
```

And, as you can see above, the syntax for modules is inconsistent. In Standard
ML module interfaces are called _signatures_, and module bodies are called
_structures_. In OCaml, these are called _module types_ and _modules_
respectively---but it's like they forgot to fully update the syntax, so `sig`
defines a `module type` and `struct` defines a `module`.

In Standard ML you'd write:

```sml
signature FOO = sig
  (* ... *)
end

structure Foo: FOO = struct
  (* ... *)
end
```

Which is at least consistent.

## Nested Match Expressions {#nested-match}

Again, because `match` statements have no terminating delimiter, you can't nest
them in the obvious way:

```ocaml
let rec safe_eval (e: expr): float option =
  match e with
  | Const f -> Some f
  | Add (a, b) ->
    match safe_eval a, safe_eval b with
     | Some a, Some b -> Some (a +. b)
     | _, _ -> None
```

This will yield a confusing type (not syntax!) error. Instead, you have to
parenthesize:

```ocaml
let rec safe_eval (e: expr): float option =
  match e with
  | Const f -> Some f
  | Add (e1, e2) ->
    (match safe_eval e1, safe_eval e2 with
     | Some f1, Some f2 -> Some (f1 +. f2)
     | _, _ -> None)
  (* ... *)
```

So everything gets _slightly_ out of alignment, and when you have a few nested
`match` statements, the code starts to look like Lisp, with a trailing train of
close parentheses on the last line.

You can avoid this by refactoring each match into a separate function, but that
has other costs.

# Modules: Better is Worse {#modules}

<img src="/assets/content/two-years-ocaml/stanford-torus.jpg" alt="A drawing of the interior of a Stanford Torus space habitat, showing a beautiful city in space." width="600px" style="margin-left:auto;margin-right:auto;"/>

<div class="center">
**Fig 1.** Society if OCaml had type classes instead of modules.
</div>

The [module system][modsys] is the central feature that sets OCaml and Standard
ML apart. This is how OCaml does ad-hoc polymorphism with early binding.

[modsys]: https://v2.ocaml.org/manual/moduleexamples.html

The module system consists of:

1. **Module types**, which define the interface of a module. A module type is a
   collection of types and functions.
1. **Modules**, which conform to an interface and define its types and
   functions.
1. **Functors**, which are functions from modules to modules. They take modules
   as arguments and combine them into new modules.

Few other languages have anything like this. [Modula-2][m2] and [Ada][ada] work
kind of like this, but they are much lower-level languages than OCaml.

[ada]: https://en.wikipedia.org/wiki/Ada_(programming_language)
[m2]: https://en.wikipedia.org/wiki/Modula-2

## Modules Are Better {#better}

Modules are similar to [type classes][tc] in Haskell, but they are more general:

1. A module can have multiple types, not just one.
2. Multiple modules can implement the same interface, while in Haskell, a type
   can only implement a type class in one way.

[tc]: https://serokell.io/blog/haskell-typeclasses

## Modules Are Worse {#worse}

The drawback is you lose implicit instantiation. You have to manually
instantiate modules, and manually refer to them. You can't write `show x`, you
have to write `FooShow.show x`. This adds a baseline level of line noise to all
code that uses modules.

It makes composing code harder. In Haskell, you can define a type class and say
that the type parameters can only accept types that implement other type
classes. This lets you naturally compose implementations: for example, you can
make it so that if a type `A` implements the equality type class `Eq`, and a
type `B` also implements `Eq`, then the tuple `(A, B)` implements `Eq` in the
obvious way.

In OCaml the only way to do this is with functors, which, again, have to be
manually instantiated, and the resulting module referred to by name.

And this is also anti-modular, since you can have multiple modules created by
instantiating the same functor over the same structure, and this duplication may
not be trivial to erase. In Haskell, the type class database is global and there
is no duplication.

So modules are more general, more flexible, and more powerful. They are also
vastly more inconvenient to use, and their added power is more than undone by
how cumbersome they are to use. Type classes, on the other hand, give you 80% of
the features, let you implement the remaining 20% without much trouble, and are
easier to use and to compose.

It's not even fair to say type classes are [worse is better][worse]: type
classes are better is better.

[worse]: https://en.wikipedia.org/wiki/Worse_is_better

## Equality {#equality}

You'd think equality in OCaml would work like this:

```ocaml
module type EQUALITY = sig
  type t
  val eq : t -> t -> bool
end

module IntEquality: EQUALITY = struct
  type t = int

  let eq (a: int) (b: int): bool =
    a = b
end
```

Rather, equality in OCaml is special-cased. You have a [magical function][eq]
with signature `'a -> 'a -> bool` that the compiler implements for every
type. Standard ML does the same. Compare this to Haskell, where equality is
implemented entirely in userspace via a type class.

[eq]: https://v2.ocaml.org/api/Stdlib.html#VAL(=)

This should be a sign that modules are not good enough. You should either have
the courage of your convictions---and make equality into a module type---or you
should implement some bridging solution like [modular implicits][modimplicit] to
make modules have the convenience of type classes.

Modular implicits for OCaml were first proposed in 2015. There's an [open pull
request][pr] from 2019 implementing a prototype. I don't think this is going to
be merged any time soon.

[modimplicit]: https://arxiv.org/pdf/1512.01895.pdf
[pr]: https://github.com/ocaml/ocaml/pull/9187

## Multiple Implementations Are Unnecessary {#multiple-impls}

In Haskell, typically each type can implement each type class in one obvious
way. It's rare you need multiple distinct instances.

When you do, you can just use `newtype` wrappers:

```haskell
newtype IntAsc = IntAsc Int
  deriving (Eq, Show)

newtype IntDesc = IntDesc Int
  deriving (Eq, Show)

instance Ord IntAsc where
  compare (IntAsc a) (IntAsc b) = compare a b

instance Ord IntDesc where
  compare (IntDesc a) (IntDesc b) = compare b a
```

# Semantics {#semantics}

Haskellers don't read this.

## Currying is Bad {#currying}

Currying is bad. Punctuation is good. Adjacency is not punctiation.

It's "cute", I guess, if you like terse math notation, but it comes at huge
costs. In a normal language where you write `f(x,y,z)`, if you forget an
argument, or add another one, you get an error saying the arity doesn't
match. If you swap the order of two arguments of distinct types, you get a type
error.

In OCaml, if you make any of these mistakes, you don't get an error to that
effect. You get a type error _downstream_ of your typo. Consider:

```ocaml
let foo (a: int) (b: float) (c: string): unit =
  let _ = (a, b, c) in ()
```

Here's the error message for each kind of mistake:

| Error       | Code             | Message                                                                                                               |
|-------------|------------------|-----------------------------------------------------------------------------------------------------------------------|
| **Missing** | `foo 0 1.0`      | This expression has type `string -> unit` but an expression was expected of type `unit`.                              |
| **Extra**   | `foo 0 1.0 "" 1` | This function has type `int -> float -> string -> unit`. It is applied to too many arguments; maybe you forgot a `;`. |
| **Swap**    | `foo 1 "" 0.0`   | This expression has type `string` but an expression was expected of type `float`.                                     |

Only in the case where you swap two arguments do you get a reasonable error
message.

Gradually you learn, through trial an error, to pattern-match on the error
messages. When I see something like "this has type `a -> b`" I know I forgot an
argument. When I see "applied to too many arguments" I know I added an extra
one.

Partly this is a consequence of type inference, which is why I put this under
semantics rather than syntax. Also because I've complained enough about syntax.

You can avoid currying with tuples, but it makes function type annotations
harder to write. And it doesn't play well with much of the standard library.

## Type Inference is Bad {#inference}

It's bad because the type system doesn't know which type constraints are correct
and which are the result of errors the programmer made. So when you make a
mistake, you're still giving constraints to the inference engine. Erroneous type
inferences propagate in every direction. Error messages appear hundreds of lines
of code away from their origin.

In the worst cases, you end up adding type annotations, one `let` binding at a
time, until the error messages start zeroing in on the problem. And so you've
wrapped back around to writing down the types.

It's bad because types are (part of the) documentation, and so you end up
annotating the types of function arguments and return types anyways.

It's bad because when reading code, you either have to mentally reconstruct the
type of each variable binding, or rely on the tooling to tell you the type, and
the more obscure the language, the less reliable the tooling.

It's bad because there are many circumstances (reading a patch file, a GitHub
diff, a book, a blog post) where you doin't have access to a language server, so
you have to mentally reconstruct the types.

It's bad because for any non-trivial type system it's undecidable. It is not
robust to changes to the type system, and seemingly minor changes will tip you
over to undecidability.

Finally: type inference is not fundamental to functional programming, contrary
to popular belief. You can just annotate types. If the type of an expression is
hard to predict, that's probably a signal that the type system is too complex,
but you can always force a type error (or use [typed holes][holes] in languages
that support them) to see the actual type.

[holes]: https://wiki.haskell.org/GHC/Typed_holes

## Mutation {#mutation}

OCaml is impure: you can mutate memory and perform IO anywhere. Unlike Java or
Python, references are not implicit any time you have an object. Rather, you
have a (garbage-collected) reference type that you can dereference and store
things into.

This is good. In theory purely-functional languages have a higher performance
ceiling, because computation can be scheduled in parallel. In practice this is
rarely realized because the "sufficiently smart compiler" doesn't exist.

People have been saying, for some 20 years now, that single-core scaling has
stalled and the von Neumann architecture has no future and we'd better port
everything so parallel-by-default functional languages. What will actually
happen is we'll get better at designing semantics for fundamentally-imperative
languages with controlled aliasing and side effects, more Rust than Haskell. The
borrow checker might be hard, but I'll choose that over some trans-dimensional
monad optics stack that takes six GiB of RAM to print "Hello, world!".

# Pragmatics {#pragmatics}

The stuff outside the spec: the tooling, community etc.

## PPX {#ppx}

OCaml doesn't have macros built into the language. Instead you use [PPX][ppx],
which lets you write programs that manipulate OCaml source code at the AST
level.

Mostly this is used for the equivalent of Haskell's `derive`. So you can write:

```ocaml
type expr =
  | Const of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
[@@deriving show, eq]
```

And the `@@deriving` annotation is replaced with the functions:

```ocaml
show_expr : expr -> string
equal_expr : expr -> expr -> bool
```

There's not much to say about this. It's convenient. But it doesn't play well
with [functors][derivingfunctor].

[ppx]: https://ocaml.org/docs/metaprogramming
[derivingfunctor]: https://stackoverflow.com/questions/70816473/how-to-apply-deriving-show-to-a-type-from-module-parameter-of-my-functor

## Tooling {#tooling}

My standard for language tooling:

1. I should be able to run the commands from the docs, in the order in which
   they appear in the docs, and things should work.
2. The standard Swiss army knife tool should have a project skeleton generator
   that gives me a project with:

   1. Library code.
   2. A basic "Hello, world!" CLI entrypoint.
   3. Unit tests.
   4. Stub documentation.

   And all relevant commands (`build`, `test`, `generate-docs`) should work from
   the project skeleton immediately.  entrypoint, and unit tests.

Surprisingly few languages clear this short bar.

What is OCaml tooling like? There's [`dune`][dune], the build system, and
[`opam`][opam], the package manager.

They're alright. I managed to get them working, somewhat, for Austral. But I
haven't touched the configuration since and every time I have to I sigh.

[Merlin][merlin] works with Emacs except when it doesn't. I recently switched to
NixOS so I might be able to get things working more reliably.

[dune]: https://dune.build/
[opam]: https://opam.ocaml.org/
[merlin]: https://ocaml.github.io/merlin/

This is infinitely better than the state of the art for e.g. Python (and the
difference is even more stark when you consider how much money and time has been
invested into the Python ecosystem compared to OCaml). The worst I experience
with OCaml tooling is, oh, `dune` is so tiresome to use, I don't know how to do
X, I'm lazy; whereas with Python any time you see an error message from `pip`
you might as well take a blowtorch to your laptop because that's going to be
easier to repair.

## How Do I Profile? {#profiling}

Seriously. I looked this up. All the [documentation][profiling] seems to assume
you're running `ocamlc` manually, like it's the 1990's and you're building a
one-file script, not a big application with tens of transitive dependencies. I
need to know how to do this at the `dune` level.

I managed to cobble together something using `prof` and successfully profiled
the Austral compiler, but then I forgot what I did to get that to work.

[profiling]: https://v2.ocaml.org/manual/profil.html

## Testing {#testing}

Different tasks have different activation energy---the amount of effort to
accomplish them. And in the end, the code that gets written is the code that is
easy to write.

That's why OCaml programs will have 10x more types than Java programs: because
in OCaml you can define a type in three lines of code, while in Java defining
the humblest class requires opening a new file and writing the entire
declaration out in triplicate.

Languages, tooling, and the community best practices affect the shape of the
activation energy landscape, and channel you into a particular way of writing
code.

In particular: the experience of writing unit tests varies markedly by
language. In Python I tend to write more tests, because Python is very dynamic
and test frameworks take advantage of that. I just add a class and write a few
methods with names starting with `test_` and I have my unit tests.

Test autodiscovery is a huge boon. Test frameworks where you have to manually
register tests make it more tiresome and time-consuming to write tests, and I
end up writing fewer.

Maybe there's a way to do quick, succint tests in OCaml with autodiscovery. If
there is, I haven't found it, because setting up even the most basic unit tests
with `dune` was already a pain.

The _existence_ of tooling is worthless. The Right Way to do things should be
included in the project skeleton generator, so it's not just experts who know
how to do it.

## Minor Complaints {#misc-complaint}

1. [`compare`][comp] returns in an `int`: like in C, `compare a b` returns 0 to
   indicate `a = b`, a negative integer to indicate `a < b`, and a positive
   integer to indicate `a > b`. This is so you can implement integer comparison
   by doing `b - a`.

    And needless to say this is an archaism. It's too late to change, but, for
    the _n_th time, Haskell does this right: comparison returns a type
    [`Ordering`][ordering] with constructors `LT`, `EQ`, `GT`.

1. `compare` is a special case: like equality, it's special-cased into the
   language. The type is `compare: 'a -> 'a -> int` which doesn't make sense.

   This should be implemented by a module analogous to Haskell's [`Ord`][ord]
   type class.

1. The zoo of conversion functions: `string_of_int`, `bool_of_string`,
   etc. Again, have the courage of your convictions and make this a module type.

[comp]: https://v2.ocaml.org/api/Stdlib.html#VALcompare
[ordering]: https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Ord.html#t:Ordering
[ord]: https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Ord.html

# At Least It's Not Haskell {#haskell}

Haskell is the main competitor to OCaml. The areas where Haskell is superior to
OCaml are:

1. Consistent syntax.
1. Declarations can appear in any order.
1. Better import system.
1. Tooling might be better (low-confidence, haven't used Haskell in anger much).
1. Type classes are better than modules.
1. `do` notation is great for error handling.

Where Haskell is worse:

1. Infix operators are bad. Custom infix operators are worse.
1. Haskell is very indentation-sensitive, more so than Python. Slight,
   harmless-looking cosmetic changes can break the parser.
1. Lazy evaluation is bad.
1. Lazy data structures are bad.
1. Is purity worth it? Not really.
1. Every file starts with declaring thirty language extensions.

# My OCaml Style {#my-style}

<a href="https://twitter.com/zetalyrae/status/1639474931086901248"><img src="/assets/content/two-years-ocaml/tweet.png" alt="A screenshot of the linked tweet." width="500px" style="margin-left:auto;margin-right:auto;"/></a>

I have a very conservative OCaml style. Types, functions, `let`, `match`,
`if`. That's it. What else do you need?

I never use polymorphic variants, or named arguments (just define a new record
type lol). I factor things out and try to keep expression nesting to a
minimum. I don't like functions that span multiple pages, but sometimes that's
hard to avoid, especially when you have large sum types.

Much of the OCaml I see in the wild is a mess of un-annotated, deeply-nested,
functorized code that's been PPX'd to death.

# Should You Use OCaml? {#should}

While there's a lot that makes me shake my head, there's really nothing in OCaml
that makes me scream in terror. The language:

1. Is statically typed.
1. Has a solid type system, by which I mean algebraic data types with
   exhaustiveness checking.
1. Is garbage collected.
1. Compiles to native.
1. Has good out of the box performance.
1. Is not too galaxy brained.
1. Lets you mutate and perform IO to your heart's content.
1. Has a decent enough ecosystem.

And surprisingly few languages check these boxes that don't also have
significant drawbacks.

If you want a statically and strongly typed garbage-collected language that
compiles to native and doesn't require you to change the way you work too much,
you should use OCaml.

Particularly if you what you want is "garbage collected Rust that's not Go",
OCaml is a good choice.
