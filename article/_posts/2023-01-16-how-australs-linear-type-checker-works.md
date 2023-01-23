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

## Paths

This rule is a concession to usability. Consider a `Linear` record with `Free`
contents:

```austral
record Position: Linear is
    x: Int32;
    y: Int32;
end;
```

Then, if we have a variable `pos: Position`, we can do something like this:

```austral
print("x = ");
printLn(pos.x);
print("y = ");
printLn(pos.y);
```

And this doesn't count as consuming the value, because we're only accessing the
`Free` components.

**Rule 6:** a record access expression that starts with a linear variable
doesn't count as consuming the variable if it ends in a `Free` value.

## Borrowing: Overview

## Borrowing: The Simple Case

## Borrowing: The General Case

# The Algorithm, In Prose

# The Algorithm, In Code

I'll walk through the code as of commit [`811b001`][commit]. The code is in two files:

- [`LinearityCheck.mli`][mli] is the OCaml module interface file.
- [`LinearityCheck.ml`][ml] is the OCaml module body file.

[commit]: https://github.com/austral/austral/commit/811b001f4bd8848fe11f8e03a633db01e6caec38
[mli]: https://github.com/austral/austral/blob/811b001f4bd8848fe11f8e03a633db01e6caec38/lib/LinearityCheck.mli
[ml]: https://github.com/austral/austral/blob/811b001f4bd8848fe11f8e03a633db01e6caec38/lib/LinearityCheck.ml

## Entrypoint

The entrypoint to the linearity checker is the function `check_module_linearity`, which takes a typed module and runs the linearity checker over every declaration in the module:

```ocaml
let rec check_module_linearity (TypedModule (_, decls)): unit =
  with_frame "Linearity Checker"
    (fun _ ->
      let _ = List.map check_decl_linearity decls in
      ())
```

The only declarations we have to check are places where code is: functions and typeclass methods:

```ocaml
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

The `linearity_check` function takes the list of value parameters for a function
or method, and the function's body, and runs the linearity checker over the
body. The value parameter is list is necessary because some parameters have
linear types and therefore count as linear variables.

```ocaml
let linearity_check (params: value_parameter list) (body: tstmt): unit =
  (* Initialize the loop depth to zero. *)
  let depth: int = 0 in
  (* Initialize the state table to the empty table. *)
  let tbl: state_tbl = empty_tbl in
  (* Populate the table with the linear parameters. *)
  let tbl: state_tbl = init_tbl tbl params in
  (* Traverse the code in execution order. *)
  let _ = check_stmt tbl depth body in
  ()
```

## The State Table

The state table maps a linear variable's name to its consumption state, and the
loop depth of the statement where the variable was defined. The purpose of the
table is to track the consumption state of a variable throughout the linearity
checking process.

```ocaml
(** The state table maps linear variables to the loop depth at the point where
    they are defined and their consumption state. *)
type state_tbl

(** The loop depth represents, for a particular piece of code, how many loops
    into the function that piece of code is. *)
type loop_depth = int

(** The consumption state of a variable. *)
type var_state =
  | Unconsumed
  | BorrowedRead
  | BorrowedWrite
  | Consumed
```

It's essentially abstract interpretation: we traverse the code in execution
order, and when we encounter a linear variable, we add it to the table with
state `Unconsumed`. When it is borrowed we mark its state as `BorrowedRead` or
`BorrowedWrite` within the scope of the `borrow` statement. When the variable is
consumed, we mark it as `Consumed`.

The loop depth is just an integer that's used to check whether a variable
defined outside a loop is being consumed in a loop. The loop depth for code at
the toplevel of a function is zero. When we enter the body of a loop, we
increase it by one. We keep track of the loop depth where the variable is
defined, and the loop depth at the point where it's consumed, in order to check
they're the same.

The code for the state table is just CRUD, so I won't post the code, just the
API definition:

```ocaml
(** The empty state table. *)
val empty_tbl : state_tbl

(** Find an entry in the state table. *)
val get_entry : state_tbl -> identifier -> (loop_depth * var_state) option

(** Add a new entry to the state table. Throws an error if an entry with that name already exists. All new variables start as unconsumed. *)
val add_entry : state_tbl -> identifier -> loop_depth -> state_tbl

(** Update the state of a variable in the table. Throws an error if there is no entry with this name. *)
val update_tbl : state_tbl -> identifier -> var_state -> state_tbl

(** Remove a row from the table. If the variable is not consumed, throws an error. *)
val remove_entry : state_tbl -> identifier -> state_tbl

(** Remove a set of variables from the table. If any of them are not consumed, throws an error. *)
val remove_entries : state_tbl -> identifier list -> state_tbl

(** Return all entries as a list. *)
val tbl_to_list : state_tbl -> (identifier * loop_depth * var_state) list
```

## Statement Checking

And `check_stmt` recursively traverses the code in execution order (depth
first). It takes the initial state table and returns the final state table.

```ocaml
let rec check_stmt (tbl: state_tbl) (depth: loop_depth) (stmt: tstmt): state_tbl =
  match stmt with
  | TSkip _ -> tbl (* do nothing *)
  (* ... *)
```

The code is too long to put it one big code block, so I'll go through the
individual cases separately.

The first non-trivial case is the `let` statement. Here we just check that if a
variable is of a linear type[^linearish], and if so, add it to the state table
with the initial state of `Unconsumed`, recur into the body of the `let`
statement, which is where the variable is defined, and then take it out of the
table.

```ocaml
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
```

When entering a loop, we recur into the loop's body, and increase the loop depth
by one:

```ocaml
  | TWhile (_, cond, body) ->
     let tbl: state_tbl = check_expr tbl depth cond in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
  | TFor (_, _, start, final, body) ->
     let tbl: state_tbl = check_expr tbl depth start in
     let tbl: state_tbl = check_expr tbl depth final in
     let tbl: state_tbl = check_stmt tbl (depth + 1) body in
     tbl
```

The `if` and `case` statements are a special case, because of rules 3 and 4,
respectively. Here we hace to check that variables defined outside the statement
are used consistently inside the statement. Essentially, we run the linearity
checker "in parallel" for each branch, and then check that the state table at
the each of each branch is identical:

```ocaml
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
```

The consistency checking is done by the function `tables_are_consistent`:

```ocaml
let tables_are_consistent (stmt_name: string) (a: state_tbl) (b: state_tbl): unit =
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
```

In a borrow statement, we have to check that the variable's state is `Unconsumed`:

```ocaml
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
```

Finally, when we reach a `return` statement, we have to check that all the
variables in the state table have been consumed. And if they haven't, show the
user an error.

```ocaml
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
```

## Expression Checking

```ocaml
let check_expr (tbl: state_tbl) (depth: loop_depth) (expr: texpr): state_tbl =
  (* For each variable in the table, check if the variable is used correctly in
     the expression. *)
  let names: identifier list = List.map (fun (name, _, _) -> name) (tbl_to_list tbl) in
  let f (tbl: state_tbl) (name: identifier): state_tbl =
    check_var_in_expr tbl depth name expr
  in
  Util.iter_with_context f tbl names
```

```ocaml
let rec check_var_in_expr (tbl: state_tbl) (depth: loop_depth) (name: identifier) (expr: texpr): state_tbl =
  (* Count the appearances of the variable in the expression. *)
  let apps: appearances = count name expr in
  (* Destructure apps. *)
  let { consumed: int; write: int; read: int; path: int } = apps in
  (* What is the current state of this variable? *)
  let state: var_state = get_state tbl name in
  (* Make a tuple with the variable's state, and the partitioned appearances. *)
  let tup = (state, partition consumed, partition write, partition read, partition path) in
  match tup with
  (*       State        Consumed      WBorrow       RBorrow      Path    *)
  (* ---------------|-------------|-------------|------------|---------- *)
  | (     Unconsumed,         Zero,         Zero,           _,           _) -> (* Not yet consumed, and at most used through immutable borrows or path reads. *)
     tbl
  | (     Unconsumed,         Zero,          One,        Zero,        Zero) -> (* Not yet consumed, borrowed mutably once, and nothing else. *)
     tbl
  | (     Unconsumed,         Zero,          One,           _,           _) -> (* Not yet consumed, borrowed mutably, then either borrowed immutably or accessed through a path. *)
     error_borrowed_mutably_and_used name
  | (     Unconsumed,         Zero,  MoreThanOne,           _,           _) -> (* Not yet consumed, borrowed mutably more than once. *)
     error_borrowed_mutably_more_than_once name
  | (     Unconsumed,          One,         Zero,        Zero,        Zero) -> (* Not yet consumed, consumed once, and nothing else. Valid IF the loop depth matches. *)
     consume_once tbl depth name
  | (     Unconsumed,          One,            _,           _,           _) -> (* Not yet consumed, consumed once, then either borrowed or accessed through a path. *)
     error_consumed_and_something_else name
  | (     Unconsumed,  MoreThanOne,            _,           _,           _) -> (* Not yet consumed, consumed more than once. *)
     error_consumed_more_than_once name
  | (   BorrowedRead,         Zero,         Zero,        Zero,           _) -> (* Read borrowed, and at most accessed through a path. *)
     tbl
  | (   BorrowedRead,            _,            _,           _,           _) -> (* Read borrowed, and either consumed or borrowed again. *)
     error_read_borrowed_and_something_else name
  | (  BorrowedWrite,         Zero,         Zero,        Zero,        Zero) -> (* Write borrowed, unused. *)
     tbl
  | (  BorrowedWrite,            _,            _,           _,           _) -> (* Write borrowed, used in some way. *)
     error_write_borrowed_and_something_else name
  | (       Consumed,         Zero,         Zero,        Zero,        Zero) -> (* Already consumed, and unused. *)
     tbl
  | (       Consumed,            _,            _,           _,           _) -> (* Already consumed, and used in some way. *)
     error_already_consumed name
```

```ocaml
and consume_once (tbl: state_tbl) (depth: loop_depth) (name: identifier): state_tbl =
   if depth = get_loop_depth tbl name then
      update_tbl tbl name Consumed
   else
      austral_raise LinearityError [
         Text "The variable ";
         Code (ident_string name);
         Text " was defined outside a loop, but you're trying to consume it inside a loop.";
         Break;
         Text "This is not allowed because it could be consumed zero times or more than once."
       ]
```

```ocaml
and error_borrowed_mutably_and_used (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " is borrowed mutably, while also being either borrowed or used through a path.";
      Break;
      Text "Mutable borrows cannot appear in the same expression where the variable is used elsewhere."
   ]

and error_borrowed_mutably_more_than_once (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Code " is borrowed mutably multiple times in the same expression."
   ]

and error_consumed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Code " is consumed in the same expression where it is used in some other way.";
      Break;
      Text "A linear variable cannot appear multiple times in the expression that consumes it.";
   ]

and error_consumed_more_than_once (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " is consumed multiple times within the same expression.";
   ]

and error_read_borrowed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " cannot be consumed or borrowed again while it is borrowed (immutably).";
   ]

and error_write_borrowed_and_something_else (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text "cannot be used in any way while it is vorrowed (mutably).";
   ]

and error_already_consumed (name: identifier) =
   austral_raise LinearityError [
      Text "The variable ";
      Code (ident_string name);
      Text " has already been consumed.";
   ]
```

## Counting Appearances

```ocaml
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
```

```ocaml
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
```


# Conclusion

WIP

# Footnotes

[^linearish]:
    The type parameters of a generic function can be constrained to
    accept only types in the linear universe, or only types in the free
    universe, or, for more general code, they can accept types from either
    universe but treatment as if they were linear, since that's the lowest
    common denominator behaviour. Hence "linearish".
