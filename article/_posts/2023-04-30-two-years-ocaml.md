---
title: Two Years of OCaml
summary: My thoughts on OCaml after two years of building a compiler with it.
---

The other day I this [this post on OCaml][post] discussed in [Hacker News][hn] and [Lobsters][lob].

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
pointing out.

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
1. [Semantics](#semantics)
    1. [Currying is Bad](#currying)
    1. [Type Inference is Bad](#inference)
    1. [Modules: Better is Worse](#modules)
1. [Pragmatics](#pragmatics)
    1. [PPX](#ppx)
    1. [Tooling](#tooling)
    1. [How Do I Profile?](#profiling)
    1. [testing](#testing)
    1. [Minor Complains](#misc-complaint)
1. [At Least It's Not Haskell](#haskell)
1. [My OCaml Style](#my-style)
1. [When Should You Use OCaml?](#when-to-use)

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
separationg of statements (control flow, variable assignment) and expressions
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

## Modules: Better is Worse {#modules}

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

# Pragmatics {#pragmatics}

The stuff outside the spec: the tooling, community etc.

## PPX {#ppx}

OCaml doesn't have macros built into the language. Instead you use [PPX][ppx],
which lets you write programs that manipulate OCaml source code at the AST
level.

Mostly this is used for the equivalent of Haskell's `derive`. So you can write:

```
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

- docs are useless if i can't find them
- tooling is useless if only experts who have been using the language for years
  know how to get a good setup going
- my standard for tooling:
    1. i should be able to run the commands in the current version of the
       documentation and have them work
    2. there should be a project generator where i type in a name, and it spits
       out a project skeleton with:
        1. library code
        2. executable code
        3. unit tests
        4. stub documentation generation
        5. all the relevant commands (build, test, generate docs) should work
           for the generated skeleton right off the bat
- set up dune and opam, angrily
- haven't used it enough
- kinda works sometimes

## How Do I Profile? {#profiling}

Seriously. I looked this up. All the [documentation][profiling] seems to assume
you're running `ocamlc` manually, like it's the 1990's and you're building a
one-file script, not a big application with tens of transitive dependencies. I
need to know how to do this at the `dune` level.

I managed to cobble together something using `prof` and successfully profiled the Austral compiler, but then I forgot what I did to get that to work.

[profiling]: https://v2.ocaml.org/manual/profil.html

## testing {#testing}

- some tasks have a higher activation energy---effort to get started---than
  others
- the code that gets written is the code that is easy to write
- languages, tooling affect the shape of the activation energy landscape, and
  channel you in a particular direction
- different languages make writing unit tests easier or harder
    - python makes it easy
    - write a class with methods that have the right name, everything gets
      autodiscovered
    - unit test autodiscovery is a _huge_ boon
    - it encourages writing tests
- languages that require you to write and register your test functions, like
  OCaml and Haskell, raise the activation energy to do this
- maybe there is an OCaml library that does test autodiscovery and makes it
  easier to write tests
    - but the existence of better tooling is worthless
    - the Right Way to do things should be in the project skeleton generator
- if i had more time i'd install the ocaml tooling and try to get a simple hello
  world app with unit tests going and record all the horrors

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

# When Should You Use OCaml? {#when-to-use}

OCaml:

1. Is statically typed.
1. Has a solid type system, by which I mean algebraic data types with exhaustiveness checking.
1. Is garbage collected.
1. Compiles to native.
1. Has good out of the box performance.
1. Is not too galaxy brained.
1. Lets you mutate and perform IO to your heart's content.
1. Has a decent enough ecosystem.

And surprisingly few languages check these boxes that don't also have significant drawbacks.

If you want a statically and strongly typed garbage-collected language that
compiles to native and doesn't require you to change the way you work too much,
you should use OCaml.
