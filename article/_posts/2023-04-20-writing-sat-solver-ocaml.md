---
title: Writing a SAT Solver in OCaml
summary: A step-by-step approach.
math: yes
---

TODO

# Overview

TODO

# Propositional Logic

```ocaml
type expr =
    | Const of bool
    | Var of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr
```