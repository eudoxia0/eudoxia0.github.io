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
type expr =
    | True
    | False
    | Var of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr
```

```ocaml
let impl (p: expr) (q: expr): expr =
  Or (Not p, q)
  
let iff (p: expr) (q: expr): expr =
  And (impl p q, impl q p)
```

```ocaml
let rec string_of_expr (e: expr): string =
  match e with
  | True ->
     "⊤"
  | False ->
     "⊥"
  | Var v ->
     v
  | Not e ->
     "¬" ^ (string_of_expr e)
  | And (p, q) ->
     "(" ^ (string_of_expr p) ^ " ∧ " ^ (string_of_expr q) ^ ")"
  | Or (p, q) ->
     "(" ^ (string_of_expr p) ^ " ∨ " ^ (string_of_expr q) ^ ")"
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
let rec replace (e: expr) (name: string) (value: bool): expr =
  match e with
  | True -> True
  | False -> False
  | Var v ->
     if v = name then
       if value then True else False
     else
       Var v
  | Not e ->
     Not (replace e name value)
  | And (p, q) ->
     And (replace p name value, replace q name value)
  | Or (p, q) ->
     Or (replace p name value, replace q name value)
```

And a function `eval` that takes a formula with no variables and evaluates it recursively:

```ocaml
let rec eval (e: expr): bool =
  match e with
  | True -> true
  | False -> false
  | Var n -> raise (Failure ("eval: the variable " ^ n ^ "has not been replaced."))
  | Not e -> not (eval e)
  | And (p, q) -> (eval p) && (eval q)
  | Or (p, q) -> (eval p) || (eval q)
```

```ocaml
module SS = Set.Make(String);;

type string_set = SS.t

let rec free (e: expr): string_set =
  match e with
  | True -> SS.empty
  | False -> SS.empty
  | Var n -> SS.singleton n
  | Not e -> free e
  | And (p, q) -> SS.union (free p) (free q)
  | Or (p, q) -> SS.union (free p) (free q)
```

# Brute Forcing

```ocaml
module type SAT = sig
  val satisfiable : expr -> bool
end
```

```ocaml
let any (e: expr): string option =
  match (SS.elements (free e)) with
  | []   -> None
  | a::_ -> Some a
```

```ocaml
module Brute: SAT = struct  
  let rec satisfiable (e: expr): bool =
    match any e with
    | None ->
      (* No free variables. *)
      eval e
    | Some var ->
      (* Replace the variable with T and F and recur. *)
      let et: expr = replace e var true
      and ef: expr = replace e var false in
      (satisfiable et) || (satisfiable ef)
end;;
```

Let's try it with $P \land (Q \lor \neg R)$:

```ocaml
let p: expr = And (Var "P", Or (Var "Q", Not (Var "R")));;
print_endline (string_of_expr p);;
print_endline (Bool.to_string (Brute.satisfiable p));;
```

This prints:

```
(P ∧ (Q ∨ ¬R))
true
```

![](/assets/content/writing-sat-solver-ocaml/bruteforcing.svg)

# Brute Forcing with Assignments

Determining whether a formula is satisfiable is not always useful. We often want the interpretation: a mapping of variables to Booleans that makes the formula true.

# Simplifying

`simplify` is like a "best-effort `eval`".