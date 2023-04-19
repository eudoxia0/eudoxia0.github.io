(* loom:start(expr) *)
type expr =
    | Const of bool
    | Var of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr
(* loom:end(expr) *)