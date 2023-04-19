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

(* loom:start(eval) *)
let rec eval (e: expr): bool =
  match e with
  | Const b -> b
  | Var n -> raise (Failure ("eval: the variable " ^ n ^ "has not been replaced."))
  | Not e -> not (eval e)
  | And (p, q) -> (eval p) && (eval q)
  | Or (p, q) -> (eval p) || (eval q)
(* loom:end(eval) *)

(* loom:start(free) *)
module SS = Set.Make(String);;

type string_set = SS.t

let rec free (e: expr): string_set =
  match e with
  | Const _ -> SS.empty
  | Var n -> SS.singleton n
  | Not e -> free e
  | And (p, q) -> SS.union (free p) (free q)
  | Or (p, q) -> SS.union (free p) (free q)
(* loom:end(free) *)

(* loom:start(bftype) *)
module type BRUTE = sig
  val satisfiable : expr -> bool
end
(* loom:end(bftype) *)

(* loom:start(any) *)
let any (e: expr): string option =
  match (SS.elements (free e)) with
  | []   -> None
  | a::_ -> a
(* loom:end(any) *)

(* loom:start(bfmodule) *)
module Brute: BRUTE = struct  
  let satisfiable (e: expr): bool =
    not_done_yet
end
(* loom:end(bfmodule) *)