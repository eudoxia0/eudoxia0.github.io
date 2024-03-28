---
title: Type Inference Was a Mistake
summary: On type inference as an academic computer science lab leak.
card: type-inference-was-a-mistake.webp
card_source: |
  Inference rules from [_Capabilities for Uniqueness and Borrowing_][link].

  [link]: https://link.springer.com/chapter/10.1007/978-3-642-14107-2_17
---

Type inference is bad. It makes code harder to read, and languages that use it
too much are harder to write. It's a false economy whereby you save unobservable
milliseconds of typing today and make everything else worse.

# Type Inference Makes Code Less Readable

If you have an IDE with a working LSP, you can hover over variables to see their
types. In VSCode with rust-analyzer I can see the type annotations as grayed-out
overlays---the ghosts of departed types.

But there are many other contexts where I read code: a book, a blog post, a Git
diff. When editing code in a limited environment, e.g. stock `vim` in a VM.

In all of those contexts, the code becomes less clear.

# In Ocaml, Type Inference is a Footgun

In Rust and Haskell you have to at least annotate the parameter types and return
type of functions. Type inference is only for variable bindings inside the
function body. This is a lot more tractable.

In OCaml type inference is a lot more powerful: you don't have to annotate
function signatures. And, since you're not required to, [you don't do it][post].

[post]: /article/language-pragmatics

The problem is the type inference engine doesn't know whether the code you've
written is correct or has a bug. It just takes what you give it and does
constraint solving on it.

So when I wrote OCaml I'd frequently make a mistake, but the compiler would
happily propagate it up, and I'd get an inscrutable error that's very far from
the actual location of the error in the code and involves types I can't figure
out.

So then you have to corner the bug by iteratively adding type annotations to
function signatures in the module and compiling again, and gradually the error
message moves closer and closer to the code that has the actual error. And if
it's a really bad one you have to annotate the types of individual varible
bindings. And then you find the error and it's something like, an extra argument
to a curried function, but because type inference is about propagating
constraints the error message---the point where inference gives up---can be
thrown arbitrarily far away from the actual mistake in the coe.

So you end up writing the types anyways, just in a much more frustrating and
flow-breaking way.

# Type Inference Wastes Academic Effort

I'll occasionally read a paper introducing a new type system. Usually there's
less than one page of motivation---why should I care about this?---followed by
the inference rules, and then pages and pages describing the type reconstruction
algorithm for the type system.

I think there's a pervasive view that if you don't have a type inference
algorithm (or a proof you can't have one), it's like not doing your homework,
and the paper is considered less legitimate.

This wastes a precious resource (academic paper page count) on solving the fake
problem of having to annotate types, a resource that would be better spent on
explaining the applications of the type system, or showing code examples that
help to understand how the type system works. Instead we get pages and pages
of inscrutable inference rules in whatever ad-hoc variant of Gentzen notation
the paper chooses to use.

# The Whole Idea is Backwards

I don't want to infer types from my code. I'd rather infer the code from the
types. Types are the spec, they are small and low in expressiveness, code is
big and has infinitely more degrees of freedom than types. The bug surface area
is smaller with types.

So it makes sense to use the types (simple, terse, constrained) to generate the
code (big, unconstrained, longer to write, bug-prone). Inferring types from code
is like building a complex machine without plans, and then using an X-ray
diffractometer to extract plans from the physical object.
