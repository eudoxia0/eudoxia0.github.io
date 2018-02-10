---
title: The Twelve Islanders Problem
summary: A brain teaser implemented in OCaml.
tags: [ocaml]
---

# Statement

There are twelve people on an island, and your task is to mass them. All twelve
of them mass the same, except one, who masses slightly more or less than the
others. You don't know ahead of time whether the odd one out is heavier or
lighter.

To measure them, you have a seesaw, which seats any number of people (but the
same number on both sides). This is a discrete seesaw: either it's horizontal,
or it dips fully in one or the other direction.

# Solution

~~~ocaml
open List

type person = Lighter | Normal | Heavier;;

type seesaw_state = DipsLeft | Balanced | DipsRight;;

(* Assumptions: one non-Normal person in the union of both lists, and both
 * lists are the same length
 *)
let seesaw (left: person list) (right: person list): seesaw_state =
  if List.mem Heavier left || List.mem Lighter right then DipsLeft
  else if List.mem Heavier right || List.mem Lighter left then DipsRight
  else Balanced;;

let solution (people: person list): int * person =
  let test (left: int list) (right: int list): seesaw_state =
    let f (i: int): person = List.nth people (i-1) in
    seesaw (List.map f left) (List.map f right)
  in
  let first_test = test [1;2;3;4] [9;10;11;12] in
  if first_test = Balanced then
    (* All the people we tested mass the same, so we can rule out {1,2,3,4} and
     * {9,10,11,12}
     *)
    let second_test = test [4;5] [6;7] in
    match second_test with
      Balanced ->
       (* {4,5,6,7} mass the same, so we know {8} is the odd one out. We use the
        * third test to determine whether they are heavier or lighter by
        * comparing {8} to any other known-mass person.
        *)
       begin
         let third_test = test [8] [1] in
         match third_test with
           DipsLeft -> (8, Heavier)
         | DipsRight -> (8, Lighter)
         | Balanced -> (* Impossible *) (-1, Normal)
       end
    | DipsLeft ->
       (* Either {5} is heavier, or {6,7} is lighter. *)
       begin
         let third_test = test [6] [7] in
         match third_test with
           Balanced -> (5, Heavier)
         | DipsLeft -> (7, Lighter)
         | DipsRight -> (6, Lighter)
       end
    | DipsRight ->
       (* Either {5} is lighter, or {6,7} is heavier. *)
       begin
         let third_test = test [6] [7] in
         match third_test with
           Balanced -> (5, Lighter)
         | DipsLeft -> (6, Heavier)
         | DipsRight -> (7, Heavier)
       end
  else
    (* {5,6,7,8} mass the same *)
    let second_test = test [2;3;9] [8;4;10] in
    if second_test = Balanced then
      (* {2,3,4,8,9,10} mass the same, so the odd one out is one of {1,11,12} *)
      if first_test = DipsLeft then
        (* Either {1} is heavier, or one of {11,12} is lighter *)
        match test [11] [12] with
          Balanced -> (1, Heavier)
        | DipsLeft -> (12, Lighter)
        | DipsRight -> (11, Lighter)
      else (* First test dipped right *)
        (* Either {1} is lighter, or one of {11,12} is heavier *)
        match test [11] [12] with
          Balanced -> (1, Lighter)
        | DipsLeft -> (11, Heavier)
        | DipsRight -> (12, Heavier)
    else
      (* The second test was unbalanced *)
      if first_test = second_test then
        (* Both tests dipped in the same direction, so {4,9} mass the same, and
         * {1,11,12} mass the same since they were excluded from the test.
         * The odd one out is one of {2,3,10}.
         *)
        if first_test = DipsLeft then
          (* Either one of {2,3} is heavier, or {10} is lighter.*)
          let third_test = test [2] [3] in
          match third_test with
            Balanced -> (10, Lighter)
          | DipsLeft -> (2, Heavier)
          | DipsRight -> (3, Heavier)
        else
          (* Either one of {2,3} is lighter, or {10} is heavier.*)
          let third_test = test [2] [3] in
          match third_test with
            Balanced -> (10, Heavier)
          | DipsLeft -> (3, Lighter)
          | DipsRight -> (2, Lighter)
      else
        (* The second test dipped in the opposite direction. {2,3,10} mass the
         * same, and {1,11,12} mass the same since they were excluded. The
         * odd one out is one of {4,9} which is heavier or lighter.
         *)
        if first_test = DipsLeft && second_test = DipsRight then
          (* Either {4} is heavier, or {9} is lighter *)
          let third_test = test [4] [1] in
          if third_test = Balanced then
            (9, Lighter)
          else
            (4, Heavier)
        else
          (* First test dips right and second test dips left, so either {9}
           * is heavier or {10} is lighter.
           *)
          let third_test = test [9] [1] in
          if third_test = Balanced then
            (4, Lighter)
          else
            (9, Heavier);;
~~~

# Verification

~~~ocaml
let make_people (pos: int) (kind: person): person list =
  let rec internal (cur: int): person list =
    if cur = 12 then
      []
    else
      (if pos = cur then kind else Normal) :: (internal (cur + 1))
  in internal(0);;

let verify_solution (kind: person) =
  for pos = 0 to 11 do
    let (pos', kind') = solution (make_people pos kind) in
    let correct = ((pos = (pos' - 1)) && (kind = kind')) in
    Printf.printf ("kind = %s, pos = %2d, pos' = %2d => %s\n")
                  (if kind = Lighter then "lighter" else "heavier")
                  (pos + 1)
                  pos'
                  (if correct then "correct" else "WRONG")
  done;;

verify_solution(Lighter);;
verify_solution(Heavier);;
~~~

Build with:

~~~bash
ocamlopt -o solution solution.ml
~~~

Run:

~~~
$[ fernando@sextant ] ~/code/tests/12-islanders
$> ./solution
kind = lighter, pos =  1, pos' =  1 => correct
kind = lighter, pos =  2, pos' =  2 => correct
kind = lighter, pos =  3, pos' =  3 => correct
kind = lighter, pos =  4, pos' =  4 => correct
kind = lighter, pos =  5, pos' =  5 => correct
kind = lighter, pos =  6, pos' =  6 => correct
kind = lighter, pos =  7, pos' =  7 => correct
kind = lighter, pos =  8, pos' =  8 => correct
kind = lighter, pos =  9, pos' =  9 => correct
kind = lighter, pos = 10, pos' = 10 => correct
kind = lighter, pos = 11, pos' = 11 => correct
kind = lighter, pos = 12, pos' = 12 => correct
kind = heavier, pos =  1, pos' =  1 => correct
kind = heavier, pos =  2, pos' =  2 => correct
kind = heavier, pos =  3, pos' =  3 => correct
kind = heavier, pos =  4, pos' =  4 => correct
kind = heavier, pos =  5, pos' =  5 => correct
kind = heavier, pos =  6, pos' =  6 => correct
kind = heavier, pos =  7, pos' =  7 => correct
kind = heavier, pos =  8, pos' =  8 => correct
kind = heavier, pos =  9, pos' =  9 => correct
kind = heavier, pos = 10, pos' = 10 => correct
kind = heavier, pos = 11, pos' = 11 => correct
kind = heavier, pos = 12, pos' = 12 => correct
~~~
