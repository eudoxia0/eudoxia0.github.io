---
title: How Austral’s Linear Type Checker Works
summary: A walkthrough of Austral's linearity checker algorithm.
---

>What I cannot create, I do not understand.
>
> <p class="cite">—  Richard Feynman</p>

# Introduction

[Austral][austral] is a new programming language featuring linear types and
capability-based security. Linear types give you compile-time memory and
resource safety, capability-based security allows you to constrain the kind of
side effects a program can do.

In my [last post][intro] introducing the language I went over what linear types
are, how they work, and the rules that characterize them In this post I'll
explain linear types from another perspective: the compiler's point of
view. That is, how does the Austral compiler actually enforce the linearity
rules?

This article is a walkthrough of the Austral compiler's linearity checker: the
compiler pass that enforces the linear type rules.

# The Goal

We want one language feature that gives us:

1. Leak freedom.
1. Memory safety without garbage collection.
1. Resource safety more broadly: ensure file handles, sockets etc. are cleaned
   up properly.
1. Safe concurrency.
1. Capability-based security.
1. The ability to enforce high-level protocols in APIs, e.g. that certain
   operations can't be performed multiple times, or must be performed in a
   particular order.

And all this should be done at compile time, with no runtime cost, and it should
be as fast as C, and the rules should be simple enough they fit in a page of
text. And the implementation should be simple enough for anyone to understand.

Linear types are this feature. The rules fit in a page. The implementation of
Austral's linearity checking algorithm is 600 lines of OCaml, most of which is
error reporting, comments, and utility functions.

# What Linear Types Are

I don't want to endlessly repeat introductory information, because that leads to
fragmentation (i.e. which of these ten articles should I read?), so for an
overview of linear types, check [_Introducing Austral_][intro].

# Use-Once Rules

A linear type is a type whose values must be used once. Not zero times or
multiple times: once and exactly once.

How do we enforce this at compile time? We can understand this on a case-by-case
basis, in order to gradually build up a general set of rules.

For the remainder of this section, let:

1. `Lin` be some linear type.
2. `make()` be a function that takes no arguments and returns an instance of
   `Lin`.
3. `consume()` be a function that takes an instance of `Lin`, consumes it, and
   returns nothing.

In Austral syntax:

```austral
type Lin: Linear;

function make(): Lin;

function consume(x: Lin): Unit;
```

## Variable Discarding

The simplest case is something like this:

```austral
let x: Lin := make();
```

Here, we're declaring a variable `x` of type `Lin`, and its initial value is the
result of evaluating `make()`, which is a value of type `Lin`. So from the type
checker's perspective this checks out: the type on the left is the same as the
type on the right.

But the linearity checker will reject this code because the variable `x` is used
zero times. And that gives us our first rule.

**Rule 1:** Variables of a linear type cannot appear zero times in the scope in
which they are defined.

To make the compiler happy, we should rewrite the above to this:

```austral
let x: Lin := make();
consume(x);
```

Then `x` is used once and the rules are respected.

## Expression Discarding

A kind of corollary to the above is we can't do this either:

```austral
make();
```

Here, we're calling `make()` and discarding its result. Obviously this is not
allowed, because while the value is not bound to any variable, it is still a
value of a linear type and it's still being discarded. So we get another rule:

**Rule 2:** values of a linear type cannot be silently discarded.

## If Statements

"Used once" does not mean "appears once". Consider this:

```austral
let x: Lin := make();
if foo then
    consume(x);
else
    -- Do nothing.
    skip;
end if;
```

The compiler will reject this. Here, `x` appears once in its scope, but it is
not used once: it's used once if `foo` is true and zero times otherwise. To make
this code pass, we have to write:

```austral
let x: Lin := make();
if foo then
    consume(x);
else
    consume(x);
end if;
```

Which leads us to another rule:

**Rule 3:** linear variables (i.e.: variables of a linear type) defined outside
an `if` statement must be used consistently in all branches: they must either be
consumed in every branch or appear zero times in every branch.

Note that this is perfectly fine:

```austral
if foo then
    let x: Lin := make();
    consume(x);
else
    -- Do nothing.
    skip;
end if;
```

Because the variable exists only in one branch of the `if` statement, it is
created there and consumed there, once and exactly once. The key distinction
here is whether the variable was defined outside the statement.

## Case Statements

`case` statements are just `if` statements but for taking apart sum types. So if
we had:

```austral
let x: Lin := make();
case binary of
    when One do
        consume(x);
    when Zero do
        -- Nothing.
        skip;
end case;
```

Again the compiler will reject this, for the obvious reason: like the branches
in an `if` statement, the cases of a `case` statement are all disjoint, and
therefore linear variables defined outside the statement must be used
consistently inside of it. Which leads to a rule analogous to the previous one.

**Rule 4:** linear variables defined outside a `case` statement must be used
consistently in all `when` clauses, that is, they must either be consumed in
every clause or appear zero times in every clause.

## Loops

Consider:

```austral
while true do
    consume(x);
end while;
```

The problem here is that `x` is being consumed an infinite number of times,
which is obviously contrary to the rules. Then:

**Rule 5:** linear variables defined outside a loop cannot be consumed inside
the loop.

Note that this is also fine:

```austral
while true do
    let x: Lin := make();
    consume(x);
end while;
```

Because `x` is being created inside the loop, and definitely consumed once. Each
iteration of the loop has a new and distinct value of `x`.

## A Brief Comment on Loops

What about this?

```austral
let x: Lin := make();
let cond: Bool := true;
while cond do
    cond := false;
    consume(x);
end while;
```

Here, it is obvious that the loop executes only once, and therefore the compiler
could prove that `x` is consumed exactly once.

The compiler will still reject this. Why? Because there is an _infinite number_
of cases like this, where the code lies _just_ outside the rules, where a
compiler could be expected to apply "common sense" and be more lenient.

Each of these special cases requires a new heuristic be added to the compiler,
heuristics which must be maintained over time and which can interact with each
other in unpredictable ways.

This is not just a problem for compiler engineers, it's a problem for anyone
learning or using the language, because there is an ocean of difference between
a small set of simple rules vs. an ever-growing pile of heuristics. The former
you can understand, the other you can only get used to.

## Borrowing: The Simple Case

## Borrowing: The General Case

# The Algorithm, In Prose

# The Algorithm, In Code

I'll walk through the code as of commit [`7ed2a1b`][commit]. The code is in two files:

- [`LinearityCheck.mli`][mli] is the OCaml module interface file.
- [`LinearityCheck.ml`][ml] is the OCaml module body file.

[commit]: https://github.com/austral/austral/commit/7ed2a1b8cf4b808933fe9ddbc880aa0aa6c12fc0
[mli]: https://github.com/austral/austral/blob/a9cdfc77cd129afbcd6a4c5e2879109adeb76492/lib/LinearityCheck.mli
[ml]: https://github.com/austral/austral/blob/a9cdfc77cd129afbcd6a4c5e2879109adeb76492/lib/LinearityCheck.ml

```ocaml
(* Data structures *)

type loop_depth = int
[@@deriving show]

type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed
[@@deriving show]

type state_tbl = (identifier * loop_depth * var_state) list
[@@deriving show]

let empty_tbl: state_tbl = []

let get_entry (tbl: state_tbl) (name: identifier): (loop_depth * var_state) option =
  match (List.find_opt (fun (n, _,_) -> equal_identifier name n) tbl) with
  | Some (_, depth, state) ->
     Some (depth, state)
  | None ->
     None

let get_entry_or_fail (tbl: state_tbl) (name: identifier): (loop_depth * var_state) =
  match get_entry tbl name with
  | Some p -> p
  | None ->
     internal_err ("variable `"
          ^ (ident_string name)
          ^ "` not in state table. Table contents: \n\n"
          ^ (show_state_tbl tbl))

let add_entry (tbl: state_tbl) (name: identifier) (depth: loop_depth): state_tbl =
  match get_entry tbl name with
  | None ->
     (name, depth, Unconsumed) :: tbl
  | Some _ ->
     (* The justification for this being an internal error is that the compiler
        should already have caught a duplicate variable. *)
     internal_err "An entry exists in the state table with this name."

let update_tbl (tbl: state_tbl) (name: identifier) (state: var_state): state_tbl =
  match get_entry tbl name with
  | None ->
     (* The justification for this being an internal error is the compiler
        should have caught a use of a variable that doesn't exist. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (depth, _) ->
     let other_entries = List.filter (fun (n, _,_) -> not (equal_identifier name n)) tbl
     in
     (name, depth, state) :: other_entries

let remove_entry (tbl: state_tbl) (name: identifier): state_tbl =
  match get_entry tbl name with
  | None ->
     (* Internal because it should have been caught by the compiler. *)
     internal_err ("Tried to update the state of the variable `"
                   ^ (ident_string name)
                   ^ "`, but no such variable exists in the state table. Table contents: \n\n"
                   ^ (show_state_tbl tbl))
  | Some (_, state) ->
     if state = Consumed then
       let others = List.filter (fun (n, _,_) -> not (equal_identifier name n)) tbl
       in
       others
     else
       austral_raise LinearityError [
           Text "Forgot to consume a linear variable: ";
           Code (ident_string name);
           Text "."
         ]

let rec remove_entries (tbl: state_tbl) (names: identifier list): state_tbl =
  match names with
  | first::rest ->
     remove_entries (remove_entry tbl first) rest
  | [] ->
     tbl

let tbl_to_list (tbl: state_tbl): (identifier * loop_depth * var_state) list =
  tbl

type appearances = {
    consumed: int;
    read: int;
    write: int;
    path: int;
  }

let zero_appearances: appearances = {
    consumed = 0;
    read = 0;
    write = 0;
    path = 0;
  }

let consumed_once: appearances = {
    consumed = 1;
    read = 0;
    write = 0;
    path = 0;
  }

let read_once: appearances = {
    consumed = 0;
    read = 1;
    write = 0;
    path = 0;
  }

let write_once: appearances = {
    consumed = 0;
    read = 0;
    write = 1;
    path = 0;
  }

let path_once: appearances = {
    consumed = 0;
    read = 0;
    write = 0;
    path = 1;
  }

let merge (a: appearances) (b: appearances): appearances =
  {
    consumed = a.consumed + b.consumed;
    read = a.read + b.read;
    write = a.write + b.write;
    path = a.path + b.path;
  }

let merge_list (l: appearances list): appearances =
  List.fold_left merge zero_appearances l

(* Counting appearances of variables in expressions *)

let rec count (name: identifier) (expr: texpr): appearances =
  let c = count name in
  match expr with
  | TNilConstant ->
     zero_appearances
  | TBoolConstant _ ->
     zero_appearances
  | TIntConstant _ ->
     zero_appearances
  | TFloatConstant _ ->
     zero_appearances
  | TStringConstant _ ->
     zero_appearances
  | TConstVar _ ->
     (* Constants variables can't be linear. *)
     zero_appearances
  | TParamVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TLocalVar (name', _) ->
     if equal_identifier name name' then
       consumed_once
     else
       zero_appearances
  | TFunVar _ ->
     zero_appearances
  | TFuncall (_, _, args, _, _) ->
     merge_list (List.map c args)
  | TMethodCall (_, _, _, args, _, _) ->
     merge_list (List.map c args)
  | TVarMethodCall { args; _ } ->
     merge_list (List.map c args)
  | TFptrCall (_, args, _) ->
     merge_list (List.map c args)
  | TCast (e, _) ->
     c e
  | TComparison (_, lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TConjunction (lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TDisjunction (lhs, rhs) ->
     merge (c lhs) (c rhs)
  | TNegation e ->
     c e
  | TIfExpression (e, t, f) ->
     merge (c e) (merge (c t) (c f))
  | TRecordConstructor (_, args) ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TUnionConstructor (_, _, args) ->
     merge_list (List.map (fun (_, e) -> c e) args)
  | TPath { head; elems; _ } ->
     let head_apps: appearances =
       (* If the head of the path is a variable, check if it is the one we are
          looking for. If it is, count that as a path appearance. *)
       (match head with
        | TParamVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | TLocalVar (name', _) ->
           if equal_identifier name name' then
             path_once
           else
             zero_appearances
        | _ ->
           (* Otherwise, just count the appearances inside the expression. *)
           c head)
     and path_apps: appearances = merge_list (List.map (count_path_elem name) elems)
     in
     merge head_apps path_apps
  | TEmbed (_, _, args) ->
     merge_list (List.map c args)
  | TDeref e ->
     c e
  | TSizeOf _ ->
     zero_appearances
  | TBorrowExpr (mode, name', _, _) ->
     if equal_identifier name name' then
       (match mode with
        | ReadBorrow ->
           read_once
        | WriteBorrow ->
           write_once)
     else
       zero_appearances

and count_path_elem (name: identifier) (elem: typed_path_elem): appearances =
  match elem with
  | TSlotAccessor _ ->
     zero_appearances
  | TPointerSlotAccessor _ ->
     zero_appearances
  | TArrayIndex (e, _) ->
     count name e

(* Linearity checking *)

type partitions =
  | Zero
  | One
  | MoreThanOne

let partition (n: int): partitions =
  if n > 1 then
    MoreThanOne
  else
    if n = 1 then
      One
    else
      if n = 0 then
        Zero
      else
        internal_err "Impossible"

let rec linearity_check (params: value_parameter list) (body: tstmt): unit =
  (* Initialize the loop depth to zero, *)
  let depth: int = 0 in
  (* Initialize the state table to the empty table. *)
  let tbl: state_tbl = empty_tbl in
  (* Populate the table with the linear parameters. *)
  let tbl: state_tbl = init_tbl tbl params in
  (* Traverse the code in execution order. *)
  let _ = check_stmt tbl depth body in
  ()

and init_tbl (tbl: state_tbl) (params: value_parameter list): state_tbl =
  let f (tbl: state_tbl) (ValueParameter (name, ty)): state_tbl =
    if universe_linear_ish (type_universe ty) then
      (* Add this parameter to the list at depth zero. *)
      add_entry tbl name 0
    else
      tbl
  in
  Util.iter_with_context f tbl params

and check_stmt (tbl: state_tbl) (depth: loop_depth) (stmt: tstmt): state_tbl =
  match stmt with
  | TSkip _ ->
     tbl
  | TLet (_, name, ty, expr, body) ->
     (* First, check the expression. *)
     let tbl: state_tbl = check_expr tbl depth expr in
     (* If the type is linear, add an entry to the table. *)
     if universe_linear_ish (type_universe ty) then
       let tbl: state_tbl = add_entry tbl name depth in
       let tbl: state_tbl = check_stmt tbl depth body in
       (* Once we leave the scope, remove the variable we added. *)
       let tbl: state_tbl = remove_entry tbl name in
       tbl
     else
       check_stmt tbl depth body
  | TDestructure (_, bindings, expr, body) ->
     (* First, check the expression. *)
     let tbl: state_tbl = check_expr tbl depth expr in
     (* Iterate over the bidings, for each that is linear, add an entry to the
        table. Also, keep track of the names of the linear variables. *)
     let linear_names: identifier list =
       List.filter_map (fun (TypedBinding { rename; ty; _ }) ->
           if universe_linear_ish (type_universe ty) then
             Some rename
           else
             None)
         bindings
     in
     let tbl: state_tbl =
       Util.iter_with_context
         (fun tbl (TypedBinding { rename; ty; _ }) ->
           if universe_linear_ish (type_universe ty) then
             add_entry tbl rename depth
           else
             tbl)
         tbl
         bindings
     in
     let tbl: state_tbl = check_stmt tbl depth body in
     (* Once we leave the scope, remove the linear variables we added. *)
     let tbl: state_tbl = remove_entries tbl linear_names in
     tbl
  | TAssign (_, lvalue, expr) ->
     let tbl: state_tbl = check_lvalue tbl depth lvalue in
     let tbl: state_tbl = check_expr tbl depth expr in
     tbl
  | TIf (_, cond, tb, fb) ->
     let tbl: state_tbl = check_expr tbl depth cond in
     let true_tbl: state_tbl = check_stmt tbl depth tb in
     let false_tbl: state_tbl = check_stmt tbl depth fb in
     let _ = tables_are_consistent "an if" true_tbl false_tbl in
     true_tbl
  | TCase (_, expr, whens) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     let tbls: state_tbl list = check_whens tbl depth whens in
     let _ = table_list_is_consistent tbls in
     (match tbls with
      | first::rest ->
         let _ = rest in
         first
      | [] ->
         tbl)
  | TWhile (_, cond, body) ->
     let tbl: state_tbl = check_expr tbl depth cond in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
  | TFor (_, _, start, final, body) ->
     let tbl: state_tbl = check_expr tbl depth start in
     let tbl: state_tbl = check_expr tbl depth final in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
  | TBorrow { original; mode; body; _ } ->
     (* Ensure the original variable is unconsumed to be borrowed. *)
     if is_unconsumed tbl original then
       let tbl: state_tbl =
         match mode with
         | ReadBorrow ->
            update_tbl tbl original BorrowedRead
         | WriteBorrow ->
            update_tbl tbl original BorrowedWrite
       in
       (* Traverse the body. *)
       let tbl: state_tbl = check_stmt tbl depth body in
       (* After the body, unborrow the variable. *)
       let tbl: state_tbl = update_tbl tbl original Unconsumed in
       tbl
     else
       let state: var_state = get_state tbl original in
       austral_raise LinearityError [
           Text "Cannot borrow the variable ";
           Code (ident_string original);
           Text " because it is already ";
           Text (humanize_state state);
           Text "."
         ]
  | TBlock (_, a, b) ->
     let tbl: state_tbl = check_stmt tbl depth a in
     let tbl: state_tbl = check_stmt tbl depth b in
     tbl
  | TDiscarding (_, expr) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     tbl
  | TReturn (_, expr) ->
     let tbl: state_tbl = check_expr tbl depth expr in
     (* Ensure that all variables are Consumed. *)
     let _ =
       List.map (fun (name, _, state) ->
           if state = Consumed then
             ()
           else
             austral_raise LinearityError [
                 Text "The variable ";
                 Code (ident_string name);
                 Text " is not consumed by the time of the return statement. Did you forget to call a destructure, or destructure the contents?"
               ])
         (tbl_to_list tbl)
     in
     tbl

and check_whens (tbl: state_tbl) (depth: loop_depth) (whens: typed_when list): state_tbl list =
  List.map (check_when tbl depth) whens

and check_when (tbl: state_tbl) (depth: loop_depth) (whn: typed_when): state_tbl =
  let TypedWhen (_, bindings, body) = whn in
  (* Iterate over the bidings, for each that is linear, add an entry to the
     table. Keep track of the names of the linear variables we added. *)
  let linear_names: identifier list =
    List.filter_map (fun (TypedBinding { ty; rename; _ }) ->
        if universe_linear_ish (type_universe ty) then
          Some rename
        else
          None)
      bindings
  in
  let tbl: state_tbl =
    Util.iter_with_context
      (fun tbl (TypedBinding { rename; ty; _ }) ->
        if universe_linear_ish (type_universe ty) then
          add_entry tbl rename depth
        else
          tbl)
      tbl
      bindings
  in
  (* Check the body. *)
  let tbl: state_tbl = check_stmt tbl depth body in
  (* Once we leave the scope, remove the linear variables we added. *)
  let tbl: state_tbl = remove_entries tbl linear_names in
  tbl

and check_lvalue (tbl: state_tbl) (depth: loop_depth) (lvalue: typed_lvalue): state_tbl =
  (* TODO: lvalue semantics should be better defined. *)
  let _ = (depth, lvalue) in
  tbl

and tables_are_consistent (stmt_name: string) (a: state_tbl) (b: state_tbl): unit =
  (* Tables should have the same set of variable names. *)
  let names_a: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list a)
  and names_b: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list b)
  in
  if List.equal equal_identifier names_a names_b then
    (* Make a list of triples with the variable name, its state in A, and its
       state in B. *)
    let common: (identifier * var_state * var_state) list =
      List.filter_map (fun (name, _, state_a) ->
          match get_entry b name with
          | Some (_, state_b) ->
             Some (name, state_a, state_b)
          | None ->
             None)
        (tbl_to_list a)
    in
    (* Ensure the states are the same. *)
    List.iter (fun (name, state_a, state_b) ->
        if state_a <> state_b then
          austral_raise LinearityError [
              Text "The variable ";
              Code (ident_string name);
              Text " is used inconsistently in the branches of ";
              Text stmt_name;
              Text " statement. In one branch it is ";
              Text (humanize_state state_a);
              Text " while in the other it is ";
              Text (humanize_state state_b);
              Text "."
            ]
        else
          ()) common
  else
    (* I *think* this is an internal error. *)
    internal_err ("Consumption state tables are inconsistent. This is likely a bug in the linearity checker. Table contents:\n\n"
                  ^ (show_state_tbl a)
                  ^ "\n\n"
                  ^ (show_state_tbl b))

and table_list_is_consistent (lst: state_tbl list): unit =
  match lst with
  | a::b::rest ->
     let _ = tables_are_consistent "a case" a b in
     table_list_is_consistent rest
  | [a] ->
     let _ = a in
     ()
  | [] ->
     ()

and check_expr (tbl: state_tbl) (depth: loop_depth) (expr: texpr): state_tbl =
  (* For each variable in the table, check if the variable is used correctly in
     the expression. *)
  let names: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list tbl) in
  let f (tbl: state_tbl) (name: identifier): state_tbl =
    check_var_in_expr tbl depth name expr
  in
  Util.iter_with_context f tbl names

and check_var_in_expr (tbl: state_tbl) (depth: loop_depth) (name: identifier) (expr: texpr): state_tbl =
  (* Count the appearances of the variable in the expression. *)
  let apps: appearances = count name expr in
  let { consumed: int; read: int; write: int; path: int } = apps in
  (* Perform the checks *)
  match partition consumed with
  | MoreThanOne ->
     (* The variable is consumed more than once: signal an error. *)
     austral_raise LinearityError [
         Text "The variable ";
         Code (ident_string name);
         Text " is consumed more than once."
       ]
  | One ->
     (* The variable is consumed exactly once. Check that:

        1. x is Unconsumed.

        2. `read`, `write`, and `path` are zero.

        3. the current loop depth is the same as the depth where the
        variable is defined.

      *)
     if (is_unconsumed tbl name) then
       if ((read = 0) && (path = 0)) then
         if depth = (get_loop_depth tbl name) then
           (* Everything checks out. Mark the variable as consumed. *)
           let tbl = update_tbl tbl name Consumed in
           tbl
         else
           austral_raise LinearityError [
               Text "The variable ";
               Code (ident_string name);
               Text " was defined outside a loop, but you're trying to consume it inside a loop.";
               Break;
               Text "This is not allowed because it could be consumed zero times or more than once."
             ]
       else
         austral_raise LinearityError [
             Text "Cannot consume the variable ";
             Code (ident_string name);
             Text " in the same expression as it is borrowed or accessed through a path."
           ]
     else
       austral_raise LinearityError [
           Text "Trying to consume the variable ";
           Code (ident_string name);
           Text " which is already ";
           Text (humanize_state (get_state tbl name));
           Text "."
         ]
  | Zero ->
     (* The variable is not consumed. *)
     (match partition write with
      | MoreThanOne ->
         (* The variable is borrowed mutably more than once. Signal an error. *)
         austral_raise LinearityError [
             Text "The variable ";
             Code (ident_string name);
             Text " is borrowed mutably more than once within a single expression."
           ]
      | One ->
         (* The variable was borrowed mutably once. Check that:

            1. It is unconsumed.
            2. `read`, `path` are zero. *)
         if is_unconsumed tbl name then
           if ((read = 0) && (path = 0)) then
             (* Everything checks out. *)
             tbl
           else
             (* Signal an error: cannot borrow mutably while also borrowing
                immutably or reading through a path. *)
             austral_raise LinearityError [
                 Text "The variable ";
                 Code (ident_string name);
                 Code " is borrowed mutably, while also being either read or read mutably."
               ]
         else
           austral_raise LinearityError [
               Text "Trying to mutably borrow the variable ";
               Code (ident_string name);
               Text " which is already consumed."
             ]
      | Zero ->
         (* The variable is neither consumed nor mutably borrowed, so we can
            read it (borrow read-only or access through a path) iff it is
            unconsumed. *)
         if read > 0 then
           (* If the variable is borrowed read-only, ensure it is unconsumed. *)
           if is_unconsumed tbl name then
             (* Everything checks out. *)
             tbl
           else
             austral_raise LinearityError [
                 Text "Trying to borrow the variable ";
                 Code (ident_string name);
                 Text " as a read reference, but the variable is already ";
                 Text (humanize_state (get_state tbl name));
                 Text "."
               ]
         else
           if path > 0 then
             (* If the variable is accessed through a path, ensure it is unconsumed. *)
             if is_unconsumed tbl name then
               (* Everything checks out. *)
               tbl
             else
               austral_raise LinearityError [
                   Text "Trying to use the variable ";
                   Code (ident_string name);
                   Text " as the head of a path, but the variable is already ";
                   Text (humanize_state (get_state tbl name));
                   Text "."
                 ]
           else
             (* The variable is not used in this expression. *)
             tbl)

and get_state (tbl: state_tbl) (name: identifier): var_state =
  let (_, state) = get_entry_or_fail tbl name in
  state

and get_loop_depth (tbl: state_tbl) (name: identifier): loop_depth =
  let (depth, _) = get_entry_or_fail tbl name in
  depth

and is_unconsumed (tbl: state_tbl) (name: identifier): bool =
  let state = get_state tbl name in
  match state with
  | Unconsumed -> true
  | _ -> false

and universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | _ -> false

and humanize_state (state: var_state): string =
  match state with
  | Unconsumed -> "not yet consumed"
  | BorrowedRead -> "borrowed (read-only)"
  | BorrowedWrite -> "borrowed (read-write)"
  | Consumed -> "consumed"

(* Linearity checking of whole modules *)

let rec check_module_linearity (TypedModule (_, decls)): unit =
  with_frame "Linearity Checker"
    (fun _ ->
      let _ = List.map check_decl_linearity decls in
      ())

and check_decl_linearity (decl: typed_decl): unit =
  match decl with
  | TFunction (_, _, name, _, params, _, b, _) ->
     with_frame ("Checking linearity of function " ^ (ident_string name))
       (fun _ -> linearity_check params b)
  | TInstance (_, _, _, _, _, methods, _) ->
     let _ = List.map check_method_linearity methods in
     ()
  | _ ->
     ()

and check_method_linearity (TypedMethodDef (_, name, params, _, b)) =
  with_frame ("Checking linearity of method " ^ (ident_string name))
    (fun _ -> linearity_check params b)
```

# Conclusion

---

- Type checking and linearity checking are separated
  - Type checking happens before
    - In the typingpass module
    - AST is augmented with the type information of every expression
  - Linearity checking happens after, using the type information acquired by the typing pass
- LinearityCheck
  - 600 lines
  - most complex data structure is a record with four integers
- rules
- examples
- algorithm


- outline
  - overview
  - what linear types are
  - how linear types work
  - the linearity checking algorithm
    - basic types
    - state table
    - appearance record
    - counting appearances
    - checking


Outline:

1. Introduction
   - what is austral
     - new PL
     - designed
     - built
     - previous post introduced it
     - explained linear types at a high level
     - explain linear types from the compiler's point of view
     - that is, walk throiugh the code that enforces the linearity views to help readers understand how they work
   - Feynman said: what i cannot create, i cannot understand. This is why explaining the algorithm is useful.
   - The algorithm is very short: 600 lines, mostly data structures.
   - The most complex data structure is a table that maps variable names to their state.
2. What linear types give us
   1. Memory safety
   2. Capability-based security
   3. Enforce high-level protocols in API.
3. What linear types are
   - Linear universes
   - How they affect generics
   - Declaring types
   - Linear types are viral: anything that's in a linear type is viral
   - Borrowing: relaxing constraints
     - Short form
     - Long form
5. How linear types work
   - Go through each case
   - Each case creates a general rule
6. Abstract description of the algorithm
7. The linearity checking algorithm
   1. Basic types
   2. state table
   3. appearance record
   4. counting appearances
   5. checking
8. An Example of linear types: Heap-Allocated Arrays
9. conclusion
