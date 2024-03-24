---
title: Type Inference is Bad
summary: On type inference as an academic computer science lab leak.
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

- in ocaml you dont need annotations of function arguments or return types
- the type inference is a lot more global
- you can annotate the types, but you don't have to
  - and as we know people are lazy on things that rae not required of them
  - link to blog post
- but hte compiler doesnt know where the code you;re giving it is correct or
  not
- it just takes the code you give it as input to the type inference algorithm
- so sometimes you make a mistake, but the mistake is not "immediate", so it
  propagates up, until eventually you get an error elsewhere
- so you get an error message from a totally different function, with types
  that don't remotely make sense, and it's impossible to make sense of it
- so what do you do
- you start going through each function in the module, adding types to the
  function arguments and the return types
- and sometimes this is not good enough, if the error is propagating within a
  sngle function body
- so what then?
- then you start adding annotations to variables, until you run into something
  that doesn't compile, and then you find the bug.
- if i was required to write the types at all times, this wouldn't happen

# Type Inference Wastes Academic Effort

- so many papers introduce a new type system for solvign x or y problem
- and then they spend pages and pages introducing a type reconstruction
  algorithm for it
- there's this pervasive idea that if a type system can't be inferred, or you
  don't try to make an inference algorithm for it, it's less legitimate, like
  you haven't done your homework
- but it's wasting precious resources (academic paper page count) on solving a
  fake problem, the supposed lack of ergonomics of explicit typing
- instead of explaining the applications of the type system, or showing
  example programs to help understand it through examples, you get pages and
  pages of inscrutable inference rules in Gentzen notation that are just
  pointless

# The Whole Idea is Backwards

I don't want to infer types from my code. I'd rather infer the code from the
types. Types are the spec, they are small and low in expressiveness, code is
big and has infinitely more degrees of freedom than types. The bug surface area
is smaller with types.

So it makes sense to use the types (simple, terse, constrained) to generate the
code (big, unconstrained, longer to write, bug-prone). Inferring types from code
is like building a complex machine without plans, and then using an X-ray
diffractometer to extract plans from the physical object.
