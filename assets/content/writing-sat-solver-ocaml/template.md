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

```ocaml
loom:include(replace)
```

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