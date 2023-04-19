(* loom:start(expr) *)
type expr =
    | Const of bool
    | Var of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr
(* loom:end(expr) *)

(* loom:start(impl) *)
let impl (p: expr) (q: expr): expr =
  Or (Not p, q)
  
let iff (p: expr) (q: expr): expr =
  And (impl p q, impl q p)
(* loom:end(impl) *)

(* loom:start(replace) *)
let rec replace (e: expr) (name: string) (value: expr): expr =
  match e with
  | Const b ->
     Const b
  | Var v ->
     if v = name then
       value
     else
       Var v
  | Not e ->
     Not (replace e name value)
  | And (p, q) ->
     And (replace p name value, replace q name value)
  | Or (p, q) ->
     Or (replace p name value, replace q name value)
(* loom:end(replace) *)
