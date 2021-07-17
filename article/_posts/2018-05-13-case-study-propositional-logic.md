---
title: A Case Study in Propositional Logic
summary: Design decisions, and using types to prevent illegal states.
tags: [plt, logic, sml]
---

This is something of a followup to [a previous post][prev], expanding upon the
idea of using types to constrain the set of a program's reachable states.

This case study deals with implementing [propositional logic][proplog] in
Standard ML, the design decisions involved in both representing the data and
transforming it, and how types help us prevent errors.

# Propositional Logic

Propositional logic, also called zeroth-order logic, is a simple classical logic
where all logical formulae are either atoms (which are replaced by Boolean
values during evaluation), or logical connectives that implement the Boolean
operations we're familiar with, like negation, conjunction, etc.

## Definition

A formula in propositional logic is one of:

- An atom `P`. Essentially a variable that is assigned a truth value by an
  interpretation when evaluating the formula.

- A negation `¬P`, which is true whenever `P` is false.

- A conjunction `P ∧ Q`, which is true whenever both `P` and `Q` are true.

- A disjunction `P ∨ Q`, which is true whenever at least one of `P` or `Q` are
  true.

- An implication `P -> Q`, which is false when `P` is true and `Q` is false, and
  is true otherwise.

- Biconditional implication `P <-> Q`, read as "`P` if and only if `Q`", is true
  when both `P` and `Q` have the same truth value.

The last two connectives are not primitive: they are easily translated to simpler formulae:

- `P -> Q = (¬P) ∨ Q`
- `P <-> Q = (P -> Q) ∧ (Q -> P)`

## Representation

Fred Brooks said representation is the essence of programming, so first we'll
define the structure of propositional formulae.

The fact that two of these logical connectives are not primitive induces a
design decision.

One approach would be to leave the implication and the biconditional off the
`formula` datatype, and just have two constructors, `implication` and `iff`,
that expand to their definitions:

~~~sml
type atom = char

datatype formula = Atom of atom
                 | Negation of formula
                 | Conjunction of formula * formula
                 | Disjunction of formula * formula

fun implication (p: formula, q: formula): formula =
  Conjunction (Negation p, q)

fun iff (p: formula, q: formula): formula =
  Conjunction (implication (p, q), implication (q, p))
~~~

This is the simplest approach, in the Kolmogorov sense. The problem is that we
lose information. Suppose this is the backend of some sort of interactive logic
program: the user types a propositional formula in the REPL and has it echoed
back at them. If we exhaustively expand complex connectives to their definitions
at construction time, we worsen the output:

~~~
> (p -> q) <-> (r ∨ s)
it = ((¬(¬p ∨ q) ∨ (r ∨ s)) ∧ (¬(r ∨ s) ∨ (¬p ∨ q)))
~~~

## Simplification

An alternative approach that preserves information is to define a datatype of
formulae that includes cases for implication and biconditional implication, and
simplify from there:

~~~sml
type atom = char

datatype formula = Atom of atom
                 | Negation of formula
                 | Conjunction of formula * formula
                 | Disjunction of formula * formula
                 | Implication of formula * formula
                 | Biconditional of formula * formula
~~~

What we want now is to have a simplification function which takes a formula and
removes non-primitive logical connectives until only the primitive ones
remain. The problem with having a function `simplify :: formula -> formula` is
that we can't be certain, at compile time, that the function actually removes
the superfluous connectives.

In dynamically typed languages, a common error when recurring down a
heterogeneous tree is forgetting to go deep enough: you forget to recur down the
subterms of a particular case, and the output of your transformation has
leftovers. The problem with these errors is they require extensive testing in
order to be discovered. As a motivating example, here's an incorrect
implementation of `simplify` in Common Lisp:

~~~common-lisp
(defun simplify (formula)
  (if (atom formula)
      formula
      (destructuring-bind (f &rest args)
          formula
        (case f
          (:~    `(:~ ,(simplify (first args))))
          (:conj `(:conj ,(simplify (first args)) ,(simplify (second args))))
          (:disj `(:disj ,(simplify (first args)) ,(simplify (second args))))
          (:impl `(:disj (:~ ,(simplify (first args)))
                         ,(simplify (second args))))
          (:iff `(:conj (:implication ,(simplify (first args))
                                      ,(simplify (second args)))
                        (:implication ,(simplify (second args))
                                      ,(simplify (first args)))))))))
~~~

And here's the error:

~~~common-lisp
* (simplify '(:iff a b))

(:CONJ (:IMPLICATION A B) (:IMPLICATION B A))
~~~

We are left with unexpanded connectives, and it's easy to see how this would
cause errors further down the line, in code that assumes those connectives
aren't there.

The converse of this error is a style of defensive programming -- typical of
dynamically-typed languages -- where to prevent this type of error we add
unnecessary recursion, usually in the tail position, to ensure, by brute force,
that we've exhaustively expanded all subterms and there's nothing left to
transform.

A function `simplify :: formula -> formula` wouldn't protect us from this
failure mode. To demonstrate, here's the Standard ML version of the above
`simplify` function:

~~~sml
fun simple (Atom a) = Atom a
  | simple (Negation p) = Negation (simple p)
  | simple (Conjunction (p, q)) = Conjunction (simple p, simple q)
  | simple (Disjunction (p, q)) = Disjunction (simple p, simple q)
  | simple (Implication (p, q)) = Disjunction (Negation (simple p), simple q)
  | simple (Biconditional (p, q)) = Conjunction (Implication (simple p, simple q),
                                                 Implication (simple q, simple p))
~~~

The function typechecks, and is wrong in the same way as the above:

~~~sml
- simple (Biconditional (Atom #"a", Atom #"b"));
val it = Conjunction (Implication (Atom #,Atom #),Implication (Atom #,Atom #))
  : formula
~~~

## Reduced Formulae

To solve this problem, we define a new datatype of reduced or primitive
formulae, that is, those which lack implication and biconditional implication. A
function from the type of formulae to the type of reduced formulae prevents this
whole category of errors:


~~~sml
(* Connective elimination *)

datatype rformula = RAtom of atom
                  | RNegation of rformula
                  | RConjunction of rformula * rformula
                  | RDisjunction of rformula * rformula

fun simplify (Atom a) = RAtom a
  | simplify (Negation p) = RNegation (simplify p)
  | simplify (Conjunction (p, q)) = RConjunction (simplify p, simplify q)
  | simplify (Disjunction (p, q)) = RDisjunction (simplify p, simplify q)
  | simplify (Implication (p, q)) = RDisjunction (RNegation (simplify p), simplify q)
  | simplify (Biconditional (p, q)) = RConjunction (simplify (Implication (p, q)),
                                                    simplify (Implication (q, p)))
~~~

# Normal Forms

Translating a formula into a normal form by carrying out equivalence-preserving
transformations can simplify computation and theorem proving. The following two
sections describe a way to transform primitive propositional formulae into
Negation Normal Form, as a stepping stone to translating them into Conjunctive
Normal Form.

## Negation Normal Form

In [Negation Normal Form][nnf], a negation can only be applied to an atom, so we
add another type definition, that of formulae that are in NNF:

~~~sml
datatype nnf = NAtom of atom
             | NNegation of atom
             | NConjunction of nnf * nnf
             | NDisjunction of nnf * nnf
~~~

Defining a new datatype for such a small change can seem overkill: the only
delta from `rformula` is a negation is now only applied to an atom rather than
an arbitrary formula. It's admittedly a small step, but it's nevertheless
shrinking the space of possible formulae, and we know it preserves validity.

To convert an arbitrary formula to Negation Normal Form, we have to apply the
following two rules:

1. A negation of a conjunction is a disjunction of negations.
2. A negation of a disjunction is a conjunctin of negations.

Implemented as follows:

~~~sml
fun nnf (RAtom a) = NAtom a
  | nnf (RNegation (RAtom a)) = NNegation a
  | nnf (RNegation (RNegation f)) = nnf f
  | nnf (RNegation (RConjunction (p, q))) = nnf (RDisjunction (RNegation p, RNegation q))
  | nnf (RNegation (RDisjunction (p, q))) = nnf (RConjunction (RNegation p, RNegation q))
  | nnf (RConjunction (p, q)) = NConjunction(nnf p, nnf q)
  | nnf (RDisjunction (p, q)) = NDisjunction(nnf p, nnf q)
~~~

## Conjunctive Normal Form

In [Conjunctive Normal Form][cnf], every formula is in the form of a conjunction
of clauses, a clause being a disjunction of literals, and a literal being either
an atom or its negation.

First, the types:

~~~sml
datatype cnf_literal = CNFAtom of atom
                     | CNFNegation of atom

type cnf_clause = cnf_literal list

type cnf = cnf_clause list
~~~

And, finally, the function to convert NNF to CNF:

~~~sml
fun cnf (NAtom a) = [[CNFAtom a]]
  | cnf (NNegation a) = [[CNFNegation a]]
  | cnf (NConjunction (p, q)) = cnf p @ cnf q
  | cnf (NDisjunction (p, q)) = List.concat (List.map (fn b' => List.map (fn a' => a' @ b')
                                                                         (cnf p))
                                                      (cnf q))
~~~

The first two cases are obvious, the third requires a little thinking, but the
fourth case sticks out like a sore thumb. It is easily rewritten:

~~~sml
fun cartesian a b =
  List.concat (List.map (fn b' => List.map (fn a' => a' @ b')
                                           a)
                        b)

fun cnf (NAtom a) = [[CNFAtom a]]
  | cnf (NNegation a) = [[CNFNegation a]]
  | cnf (NConjunction (p, q)) = cnf p @ cnf q
  | cnf (NDisjunction (p, q)) = cartesian (cnf p) (cnf q)
~~~

The distributive property is just a special case of the Cartesian product.

# Conclusion

Use strong, static types. Use as many intermediate representations as needed to
constrain the set of program states. Replace functions that raise exceptions on
"impossible" cases with functions that process these intermediate
representations.

[prev]: /article/inheritance-disjunctions
[proplog]: https://en.wikipedia.org/wiki/Propositional_calculus
[nnf]: https://en.wikipedia.org/wiki/Negation_normal_form
[cnf]: https://en.wikipedia.org/wiki/Conjunctive_normal_form
