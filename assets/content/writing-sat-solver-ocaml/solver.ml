(* loom:start(expr) *)
type expr =
    | True
    | False
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

(* loom:start(string_of_expr) *)
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
(* loom:end(string_of_expr) *)

(* loom:start(replace) *)
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
(* loom:end(replace) *)

(* loom:start(eval) *)
let rec eval (e: expr): bool =
  match e with
  | True -> true
  | False -> false
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
  | True -> SS.empty
  | False -> SS.empty
  | Var n -> SS.singleton n
  | Not e -> free e
  | And (p, q) -> SS.union (free p) (free q)
  | Or (p, q) -> SS.union (free p) (free q)
(* loom:end(free) *)

(* loom:start(modtype) *)
module type SAT = sig
  val satisfiable : expr -> bool
end
(* loom:end(modtype) *)

(* loom:start(any) *)
let any (e: expr): string option =
  match (SS.elements (free e)) with
  | []   -> None
  | a::_ -> Some a
(* loom:end(any) *)

(* loom:start(bfmodule) *)
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
(* loom:end(bfmodule) *)

print_endline ("loom:" ^ "start(ex1out)");;

(* loom:start(ex1) *)
let p: expr = And (Var "P", Or (Var "Q", Not (Var "R")));;
print_endline (string_of_expr p);;
print_endline (Bool.to_string (Brute.satisfiable p));;
(* loom:end(ex1) *)

print_endline ("loom:" ^ "end(ex1out)");;

(* loom:start(simplify) *)
(*let rec simplify (e: expr): expr =
  match e with
  (* T/F and vars can't be simplified further. *)
  | Const b -> Const b
  | Var v -> v
  *)
(* loom:end(simplify) *)