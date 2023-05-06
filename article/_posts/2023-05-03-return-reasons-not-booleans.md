---
title: Return Reasons, Not Booleans
summary: A short post on a consequence of Boolean blindness.
---

When you have a predicate that acts on a tree data structure, rather than return
a Boolean, you should return an object that represents success/failure and
carries explanatory information in the failure case.

That is, rather than:

```ocaml
type tree

val pred : tree -> bool
```

You should write:

```ocaml
type tree

type result =
  | Pass
  | Fail of (* error message or data *)

val pred : tree -> result
```

With the failure case explaining what failed, where, and why.

Why? Because otherwise you will find yourself staring at a [Sentry][sentry] log
where some process failed, and all you have to go on is a solitary `false`
value. And it can be hard to debug locally.

[sentry]: https://sentry.io/welcome/

**Example:** if you have a function that compares two trees for equality on an
exact, bit-by-bit basis, then if the trees are potentially deeply nested you
might want the comparison function to tell you which subtrees don't match and
where in the tree the mismatch happens. In this particular case you don't need
to know _why_ the predicate failed (since you're doing strict equality), but you
want to know where the failure happened and what are the values that don't
match.

**Example:** sometimes you want to implement something like relaxed
equality---for example, you might have a data type that represents mathematical
expressions, and you want to compare them for equality while applying certain
equivalence rules, like accepting `a + b` and `b + a` as equal, or `x + 0` as
equal to `x`. In cases where the predicate is not trivial, you want to know not
just where and what subtrees don't match but also why.
