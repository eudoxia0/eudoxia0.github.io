---
title: Writing a SAT Solver in OCaml
summary: A step-by-step approach.
math: yes
---

TODO

# Overview

TODO

# Propositional Logic

Propositional logic has two components: formulas, and their interpretations (for this post we're leaving out proof).

```ocaml
loom:include(expr)
```

```ocaml
loom:include(impl)
```

```ocaml
loom:include(string_of_expr)
```

An _interpretation_ is a function that maps variables to Boolean values. Given an interpretation $I$, we can evaluate a formula:

$$
\begin{align*}
e(\top) &= \top \\
e(\bot) &= \bot \\
e(v) &= I(v) \\
e(\neg p) &= \neg e(p) \\
e(p \land q) &= e(p) \land e(q) \\
e(p \lor q) &= e(p) \lor e(q)
\end{align*}
$$

We'll implement evaluation in two pieces. First, with a function that replaces a specific variable in a formula with a Boolean constant:

```ocaml
loom:include(replace)
```

And a function `eval` that takes a formula with no variables and evaluates it recursively:

```ocaml
loom:include(eval)
```

```ocaml
loom:include(free)
```

# Brute Forcing

```ocaml
loom:include(modtype)
```

```ocaml
loom:include(any)
```

```ocaml
loom:include(bfmodule)
```

Let's try it with $P \land (Q \lor \neg R)$:

```ocaml
loom:include(ex1)
```

This prints:

```
loom:include(ex1out)
```

![](/assets/content/writing-sat-solver-ocaml/bruteforcing.svg)

# Brute Forcing with Assignments

Determining whether a formula is satisfiable is not always useful. We often want the interpretation: a mapping of variables to Booleans that makes the formula true.