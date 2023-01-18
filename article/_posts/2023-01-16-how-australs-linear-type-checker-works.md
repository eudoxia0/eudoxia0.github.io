---
title: How Austral’s Linear Type Checker Works
summary: A walkthrough of Austral's linearity checker algorithm.
---

>What I cannot create, I do not understand.
>
> <p class="cite">—  Richard Feynman</p>

# Introduction

[Austral][austral] is a new programming language featuring linear types and
capability-based security. Linear types give you compile-time memory and
resource safety, capability-based security allows you to constrain the kind of
side effects a program can do.

In my [last post][intro] introducing the language I went over what linear types
are, how they work, and the rules that characterize them In this post I'll
explain linear types from another perspective: the compiler's point of
view. That is, how does the Austral compiler actually enforce the linearity
rules?

This article is a walkthrough of the Austral compiler's linearity checker: the
compiler pass that enforces the linear type rules.

# The Goal

We want one language feature that gives us:

1. Leak freedom.
1. Memory safety without garbage collection.
1. Resource safety more broadly: ensure file handles, sockets etc. are cleaned
   up properly.
1. Safe concurrency.
1. Capability-based security.
1. The ability to enforce high-level protocols in APIs, e.g. that certain
   operations can't be performed multiple times, or must be performed in a
   particular order.

And all this should be done at compile time, with no runtime cost, and it should
be as fast as C, and the rules should be simple enough they fit in a page of
text. And the implementation should be simple enough for anyone to understand.

Linear types are this feature. The rules fit in a page. The implementation of
Austral's linearity checking algorithm is 600 lines of OCaml, most of which is
error reporting, comments, and utility functions.

# What Linear Types Are

I don't want to endlessly repeat introductory information, because that leads to
fragmentation (i.e. which of these ten articles should I read?), so for an
overview of linear types, check [_Introducing Austral_][intro].

# Use-Once Rules

A linear type is a type whose values must be used once. Not zero times or
multiple times: once and exactly once.

How do we enforce this at compile time? We can understand this on a case-by-case
basis, in order to gradually build up a general set of rules.

For the remainder of this section, let:

1. `Lin` be some linear type.
2. `make()` be a function that takes no arguments and returns an instance of
   `Lin`.
3. `consume()` be a function that takes an instance of `Lin`, consumes it, and
   returns nothing.

In Austral syntax:

```austral
type Lin: Linear;

function make(): Lin;

function consume(x: Lin): Unit;
```

## Variable Discarding

The simplest case is something like this:

```austral
let x: Lin := make();
```

Here, we're declaring a variable `x` of type `Lin`, and its initial value is the
result of evaluating `make()`, which is a value of type `Lin`. So from the type
checker's perspective this checks out: the type on the left is the same as the
type on the right.

But the linearity checker will reject this code because the variable `x` is used
zero times. And that gives us our first rule.

**Rule 1:** Variables of a linear type cannot appear zero times in the scope in
which they are defined.

To make the compiler happy, we should rewrite the above to this:

```austral
let x: Lin := make();
consume(x);
```

Then `x` is used once and the rules are respected.

## Expression Discarding

A kind of corollary to the above is we can't do this either:

```austral
make();
```

Here, we're calling `make()` and discarding its result. Obviously this is not
allowed, because while the value is not bound to any variable, it is still a
value of a linear type and it's still being discarded. So we get another rule:

**Rule 2:** values of a linear type cannot be silently discarded.

## If Statements

"Used once" does not mean "appears once". Consider this:

```austral
let x: Lin := make();
if foo then
    consume(x);
else
    -- Do nothing.
    skip;
end if;
```

The compiler will reject this. Here, `x` appears once in its scope, but it is
not used once: it's used once if `foo` is true and zero times otherwise. To make
this code pass, we have to write:

```austral
let x: Lin := make();
if foo then
    consume(x);
else
    consume(x);
end if;
```

Which leads us to another rule:

**Rule 3:** linear variables (i.e.: variables of a linear type) defined outside
an `if` statement must be used consistently in all branches: they must either be
consumed in every branch or appear zero times in every branch.

Note that this is perfectly fine:

```austral
if foo then
    let x: Lin := make();
    consume(x);
else
    -- Do nothing.
    skip;
end if;
```

Because the variable exists only in one branch of the `if` statement, it is
created there and consumed there, once and exactly once. The key distinction
here is whether the variable was defined outside the statement.

## Case Statements

`case` statements are just `if` statements but for taking apart sum types. So if
we had:

```austral
let x: Lin := make();
case binary of
    when One do
        consume(x);
    when Zero do
        -- Nothing.
        skip;
end case;
```

Again the compiler will reject this, for the obvious reason: like the branches
in an `if` statement, the cases of a `case` statement are all disjoint, and
therefore linear variables defined outside the statement must be used
consistently inside of it. Which leads to a rule analogous to the previous one.

**Rule 4:** linear variables defined outside a `case` statement must be used
consistently in all `when` clauses, that is, they must either be consumed in
every clause or appear zero times in every clause.

## Loops

Consider:

```austral
while true do
    consume(x);
end while;
```

The problem here is that `x` is being consumed an infinite number of times,
which is obviously contrary to the rules. Then:

**Rule 5:** linear variables defined outside a loop cannot be consumed inside
the loop.

Note that this is also fine:

```austral
while true do
    let x: Lin := make();
    consume(x);
end while;
```

Because `x` is being created inside the loop, and definitely consumed once. Each
iteration of the loop has a new and distinct value of `x`.

## A Brief Comment on Loops

What about this?

```austral
let x: Lin := make();
let cond: Bool := true;
while cond do
    cond := false;
    consume(x);
end while;
```

Here, it is obvious that the loop executes only once, and therefore the compiler
could prove that `x` is consumed exactly once.

The compiler will still reject this. Why? Because there is an _infinite number_
of cases like this, where the code lies _just_ outside the rules, where a
compiler could be expected to apply "common sense" and be more lenient.

Each of these special cases requires a new heuristic be added to the compiler,
heuristics which must be maintained over time and which can interact with each
other in unpredictable ways.

This is not just a problem for compiler engineers, it's a problem for anyone
learning or using the language, because there is an ocean of difference between
a small set of simple rules vs. an ever-growing pile of heuristics. The former
you can understand, the other you can only get used to.

## Borrowing: The Simple Case

## Borrowing: The General Case

# The Algorithm, In Prose

# The Algorithm, In Code

# Conclusion

---

- Type checking and linearity checking are separated
  - Type checking happens before
    - In the typingpass module
    - AST is augmented with the type information of every expression
  - Linearity checking happens after, using the type information acquired by the typing pass
- LinearityCheck
  - 600 lines
  - most complex data structure is a record with four integers
- rules
- examples
- algorithm

https://github.com/austral/austral/blob/b8c4da80228952c33a1bdc06a2742b0767572afd/lib/LinearityCheck.mli

https://github.com/austral/austral/blob/b8c4da80228952c33a1bdc06a2742b0767572afd/lib/LinearityCheck.ml

- describing the algo as of `b8c4da8`
  - https://github.com/austral/austral/commit/b8c4da80228952c33a1bdc06a2742b0767572afd
  - haven't touched most of the code in june 2022

- outline
  - overview
  - what linear types are
  - how linear types work
  - the linearity checking algorithm
    - basic types
    - state table
    - appearance record
    - counting appearances
    - checking


Outline:

1. Introduction
   - what is austral
     - new PL
     - designed
     - built
     - previous post introduced it
     - explained linear types at a high level
     - explain linear types from the compiler's point of view
     - that is, walk throiugh the code that enforces the linearity views to help readers understand how they work
   - Feynman said: what i cannot create, i cannot understand. This is why explaining the algorithm is useful.
   - The algorithm is very short: 600 lines, mostly data structures.
   - The most complex data structure is a table that maps variable names to their state.
2. What linear types give us
   1. Memory safety
   2. Capability-based security
   3. Enforce high-level protocols in API.
3. What linear types are
   - Linear universes
   - How they affect generics
   - Declaring types
   - Linear types are viral: anything that's in a linear type is viral
   - Borrowing: relaxing constraints
     - Short form
     - Long form
5. How linear types work
   - Go through each case
   - Each case creates a general rule
6. Abstract description of the algorithm
7. The linearity checking algorithm
   1. Basic types
   2. state table
   3. appearance record
   4. counting appearances
   5. checking
8. An Example of linear types: Heap-Allocated Arrays
9. conclusion