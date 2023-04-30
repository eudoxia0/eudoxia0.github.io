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

Yeah, yeah, _de gustibus_, and people spend [_way_ too much time whining about syntax][wadler] and other superficial issues, rather than focusing on language semantics and pragmatics.

[wadler]: https://en.wikipedia.org/wiki/Law_of_triviality

But I'm not a partisan about syntax. I genuinely think code written in C, Java, Lisp, Pascal, and ML can be beautiful in different ways. Some of these complaints will be personal, others will be more objective.

## Aesthetics {#aesthetics}

[ML][ml] was born as the implementation language of a [theorem prover][lcf], so naturally the syntax is meant to look like whiteboard math.

[ml]: https://en.wikipedia.org/wiki/ML_(programming_language)
[lcf]: https://en.wikipedia.org/wiki/Logic_for_Computable_Functions

And it does look good for math. If you're writing something like a symbolic differentiation engine:

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

Then it's simply delightful. It does tend to fall apart for everything else however.

OCaml, like Haskell, is [expression-oriented][expr], meaning that there is no separationg of statements (control flow, variable assignment) and expressions (evaluate to values) and instead everything is an expression. Most expressions in OCaml tend not to have terminating delimiters.

[expr]: https://en.wikipedia.org/wiki/Expression-oriented_programming_language

This is very vague, but ML-family (meaning Standard ML, OCaml, Haskell and derivatives) code often feels like the expressions are "hanging in the air", so to speak. Terminating delimiters (like semicolons in C or `end` in [Wirth-family][wirth] languages) make the code feel more "solid" in a way.

[wirth]: https://wiki.c2.com/?WirthLanguages

And expression orientation (which most modern languages advertise as a feature) cuts both ways. The benefit is simplicity and symmetry: you don't need both an `if` statement and a ternary if expression. You can have a big expression that computes a value and then assigns it to a containing `let`, like so:

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

Without having to use an uninitialized variable or refactor your code into too-small functions. However, this generality comes at a cost: you can write arbitrarily deep and complex expressions, where a statement-oriented language would force you to keep your code flatter and break it down into small functions.

It takes discipline to write good code in an expression-oriented language. I often see e.g. Common Lisp code with functions hundreds of lines long. It's almost impossible to track the flow of data in that context. This, by the way, is why [Austral][aus] is statement-oriented, despite every modern language moving towards expression-oriented syntax.

[aus]: https://austral-lang.org/

## Declaration Order {#order}

In OCaml, like in C, declaration must appear in dependency order. That is, you can't write this:

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

_But_, you can't interleave an `and`-chain of functions with one of types. So you have a choice:

1. You can write all of your code backwards, with the utility functions and the leaf-nodes of the call graph up front, and the important code at the bottom.

2. Or, you can write a big `and`-chain of types at the start of the file, followed by a big `and`-chain of functions for the remainder of the file.

Option one makes the code harder to read, and option two feels incredibly brittle.

Haskell gets this right: declaration order is irrelevant. Austral also allows declarations to appear in any order, partly because of my frustration with this aspect of OCaml.

Note that having a module interface doesn't save you here, because interfaces and modules are compiled separately. So if you have a `Foo.mli` file like this:

```ocaml
val foo : unit -> unit
val bar : unit -> unit
val baz : unit -> unit
```

The corresponding `.ml` file _still_ has to have the declarations appear in dependency order.

## Currying is Bad {#currying}

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

## Awkward Comment Syntax {#comments}

OCaml has no single-line comment syntax. Instead, you have block comment syntax, like so:

```ocaml
(* I'm a comment. *)
```

The double parenthesis-asterisk pair is torture to write on my fingers. Again, Haskell does this right: single-line comments are a double hyphen. Quick and easy.

Unlike C and other languages, comments can be nested, like in Common Lisp:

```ocaml
(* I'm a (* nested *) comment. *)
```

This is useful for commenting-out large chunks of code.

## Type Specifier Syntax {#type-annot}

The syntax for type specifiers and type annotation is a bit of a pain.

First, there's inconsistency: `a * b` is the type specifier for a tuple (asterisk as in [product][prod]), but the syntax for constructing a tuple is `(a, b)`:

[prod]: https://en.wikipedia.org/wiki/Product_type

```ocaml
let derp: int * string = (0, "")
```

Again, Haskell gets this right:

```haskell
derp :: (Int, String)
derp = (0, "")
```

Similarly, the unit type is `unit` but its value is `()`. And, again, Haskell gets this right: the unit type is the empty tuple, denoted `()`, and its sole value is `()`.

Generics are weird. Most modern languages are moving towards `Name[Arg, ..., Arg]` as the syntax for a generic type specifier. So in Swift you'd write `List[Int]`, but in OCaml you write `int list`. The order is inverted, but I think the argument is that you can read it like it's English?

Haskell is not much better: `List Int`. This obsession with terseness is a big problem: please give me punctuation.

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

Which makes it hard to insert debugging `print` statements. You have to transform the above into the more tiresome:

```
let foo _ =
  if true then
    let _ = print_endline "Hello, world!" in
    true
  else
    false
```

There's an easy way to solve this: add an `end if` delimiter. Again: terseness bites.

## Inconsistencies {#inconsistency}

As above: the syntax for tuple and unit types and values is inconsistent.

The syntax for a list literal is `[1; 2; 3]`. This is because the comma is an infix operator, so if you typo this as `[1, 2, 3]` you don't get a syntax error, that's a singleton list with a tuple as its element type.

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

And, as you can see above, the syntax for modules is inconsistent. In Standard ML module interfaces are called _signatures_, and module bodies are called _structures_. In OCaml, these are called _module types_ and _modules_ respectively---but it's like they forgot to fully update the syntax, so `sig` defines a `module type` and `struct` defines a `module`.

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

## Nested Match Statements {#nested-match}

Again, because `match` statements have no terminating delimiter, you can't nest them in the obvious way:

```ocaml
let rec safe_eval (e: expr): float option =
  match e with
  | Const f -> Some f
  | Add (a, b) ->
    match safe_eval a, safe_eval b with
     | Some a, Some b -> Some (a +. b)
     | _, _ -> None
```

This will yield a confusing type (not syntax!) error. Instead, you have to parenthesize:

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

So everything gets _slightly_ out of alignment, and when you have a few nested `match` statements, the code starts to look like Lisp, with a trailing train of close parentheses on the last line.

You can avoid this by refactoring each match into a separate function, but that has other costs.

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