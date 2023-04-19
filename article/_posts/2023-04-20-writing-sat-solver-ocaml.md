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
    | Const of bool
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
let rec replace (e: expr) (name: string) (value: bool): expr =
  match e with
  | Const b ->
     Const b
  | Var v ->
     if v = name then
       Const value
     else
       Var v
  | Not e ->
     Not (replace e name value)
  | And (p, q) ->
     And (replace p name value, replace q name value)
  | Or (p, q) ->
     Or (replace p name value, replace q name value)
```

```ocaml
let rec eval (e: expr): bool =
  match e with
  | Const b -> b
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
  | Const _ -> SS.empty
  | Var n -> SS.singleton n
  | Not e -> free e
  | And (p, q) -> SS.union (free p) (free q)
  | Or (p, q) -> SS.union (free p) (free q)
```

# Brute Forcing

```ocaml
module type BRUTE = sig
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
module Brute: BRUTE = struct  
  let satisfiable (e: expr): bool =
    match any e with
    | None ->
      (* No free variables. *)
      eval e
    | Some var ->
      (* Replace the variable with T and F and recur. *)
      let et: expr = replace e var true
      and ef: expr = replace e var false in
      (satisfiable et) || (satisfiable ef)
end
```

# Brute Forcing with Assignments