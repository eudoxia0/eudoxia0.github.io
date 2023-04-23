---
title: Dependency Resolution Made Simple
summary: Resolving dependencies with a homebrew SAT solver.
card: dependency-resolution-made-simple.jpg
card_source: |
  [_Dimostrazione in grande del capitello di una delle colonne del Tempio di Giunone dentro il Portici d`Ottavia_][link], Giovanni Battista Piranesi, 1784.

  [link]: https://it.wikisource.org/wiki/Le_antichit%C3%A0_Romane_(Piranesi)/4-XLV
math: yes
---

$$
\gdef\true{\mathbf{T}}
\gdef\false{\mathbf{F}}
$$

Lately, I've been thinking about what the package manager for [Austral][austral]
is going to look like. Dependency resolution is a [surprisingly complex
problem][np], so I did a deep dive, and this post explains how to solve the
problem in a way that is tractable and doesn't require reinventing the wheel too much.

[austral]: https://austral-lang.org/
[np]: https://research.swtch.com/version-sat

# The Problem

If packages specified the exact version of every one of their dependencies,
there would be no dependency _resolution_ problem. There would however be
constant dependency _integration_ problems.

You're writing an application that depends on libraries A and B. Library A
depends on Foo v1.0.2, library B depends on Foo v1.0.3. Now you have a
dependency conflict[^same]. The brute-force solution is to vendor one of A and B
and adjust their dependencies. And you may have to do this recursively. Then you
miss out on improvements, security fixes, etc.

"Fine", you said. "I'll allow depending on _major_ versions. Then A and B can
just depend on Foo v.1.\*.\*, and the solver picks the latest version."

And this mainly solves the problem, assuming everyone sticks to [semver]. But
there are two problems:

[semver]: https://semver.org/

1. Packages can hold back from upgrading because a new (minor or patch) version
   introduces either a bug or a performance regression that affects their usage
   of that package. The developers of A might want to exclude everything above
   1.7.\*, while the developers of B are indifferent as long as the major
   version is 1. So picking e.g. 1.9.0 satisfies B but is bad for A.

2. Sometimes you want to depend on a library across major versions (e.g. the
   standard library) because you're only using the parts of the code that are
   highly conserved across major versions.

Even with strict semver enforcement (like [Elm does][elm]), there are observable
features of libraries that fall outside of what an API spec can describe.

[elm]: https://elm-lang.org/

In other words, there's a tradeoff:

1. Less expressive version ranges make things easier for the package manager,
   while making the user experience hellish and involving a lot of manual work.
1. More expressive version ranges allow users greater freedom to allow or deny
   specific package versions, while complicating the work that the package
   manager has to perform.

The **dependency resolution problem** is:

>Given a set of package dependency constraints, find an assignment---a map of
>package names to specific package versions---that satisfies every constraint
>without having multiple versions of the same package.

This turns out to be a surprisingly hard problem. NP-complete,
actually. Manifest/lock files are orthogonal to this: manifest files are about
making builds reproducible, but to make a manifest file you have to have
resolved the dependencies in the first place.

The dependency resolution problem is solved, in wildly different ways, by
different package managers:

1. TODO

There exists a simple and satisfactory solution that involves translating the
problem into logic, applying a [SAT solver][sat] (a mature, well-understood
piece of technology), and translating the solution back to the original problem
domain. This approach is correct, it is free of ad-hockery, _and_ it benefits
from the fact that SAT solvers have been optimized to death.

[sat]: https://en.wikipedia.org/wiki/SAT_solver

# Propositional Logic

[Propositional logic][proplog] is the simplest kind of logic. It has two
components:

[proplog]: https://en.wikipedia.org/wiki/Propositional_calculus

1. **Syntax:** the rules for writing _logical sentences_, which are expression
   that can be evaluated to one of two values: true and false, just as
   arithmetic expressions can be evaluated to a number.
2. **Semantics:** the rules for evaluating logical expressions.

The syntax of propositional logic is simple: expressions are built up, starting
from constants (true/false) and variables, by joining them through logical
operators. More formally, an expression in propositional logic can be one of:

1. $\false$, which stands for false.
1. $\true$, which stands for true.
1. A variable (typically $A$, $B$, $C$, etc.) is an expression that can be
   replaced with true or false. Variables represent the inputs to a problem in
   logic.
1. $\neg P$, read "not $P$".
1. $P \land Q$, read "$P$ and $Q$".
1. $P \lor Q$, read "$P$ and or $Q$".
1. $P \implies Q$, read "$P$ implies $Q$".
1. $P \iff Q$, read "$P$ if and only if $Q$".

Or, in Haskell:

```haskell
data Expr = CFalse
          | CTrue
          | Var String
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
```

Implication and if-and-only-if don't need their own constructors, because they
are not primitive:

1. Implication should be thought of as a predicate. $P \implies Q$ means:
    1. _If_ $P$ is true and $Q$ is true, then $P \implies Q$ is true.
    2. _If_ $P$ is true, and $Q$ is false, then $P \implies Q$ is false.
    3. _If_ $P$ is false, the value of $Q$ doesn't matter, and the implication
  $P \implies Q$ as a whole evaluates to true.  If you draw the truth table this
  is the same as writing $(\neg P) \lor Q$.
1. $P \iff Q$ is true when $P$ and $Q$ have the same truth value. This is the
   same as writing $(P \implies Q) \land (Q \implies P)$.

An _assignment_ is map from variables to truth values. Given an assignment $v$,
expressions can be evaluated like so:

```haskell
eval :: (String -> Bool) -> Expr -> Bool
eval v expr = case expr of
    CFalse  -> False
    CTrue   -> True
    Var s   -> env s
    Not p   -> not (eval v p)
    And p q -> (eval v p) && (eval v q)
    Or p q  -> (eval v p) || (eval v q)
```

For example, given:

$$ ((A \land B) \lor C) \rightarrow (\lnot B \land C) $$

And an assignment:

1. $A \rightarrow \true$
2. $B \rightarrow \false$
3. $C \rightarrow \true$

We can evaluate the expression in a step by step manner like so:

| Expression                                                                 | Step                           |
|----------------------------------------------------------------------------|--------------------------------|
| $((A \land B) \lor C) \rightarrow (\lnot B \land C)$                       | Original.                      |
| $(\neg ((A \land B) \lor C)) \lor (\lnot B \land C)$                       | Simplify implication.          |
| $(\neg ((\true \land \false) \lor \true)) \lor (\lnot \false \land \true)$ | Replace variables.             |
| $(\neg (\false \lor \true)) \lor (\lnot \false \land \true)$               | $\true \land \false$ is false. |
| $(\neg \true) \lor (\lnot \false \land \true)$                             | $\false \lor \true$ is true.   |
| $\false \lor (\lnot \false \land \true)$                                   | $\neg \true$ is false.         |
| $\false \lor (\true \land \true)$                                          | $\neg \false$ is true.         |
| $\false \lor \true$                                                        | $\true \land \true$ is true.   |
| $\true$                                                                    | $\false \lor \true$ is true.   |

The **Boolean satisfiability problem** is this:

>Given a logical expression, find an assignment of variables to truth values
>that makes it true.

Solutions to this problem have surprisingly many useful applications. You'll
notice this is worded similarly to the statement of the dependency resolution
problem above.

Boolean satisfiability is pretty thoroughly solved. By translating from one
problem to the next, we can profit from decades of work in this area. The next
section explains how.

# Translating the Problem

We will translate the set of dependency constraints to a logical expression,
according to the following rules:

1. **Package Versions are Variables:** a variable "foo-v1.2.0" being assigned
   $\true$ means all packages in this build DAG will use `v.1.2.0` for `foo`,
   $\false$ means we're using some other version of `foo`.
1. **Version Ranges are Disjunctions:** suppose you have a version constraint
   like `foo >= 1.2 && foo < 1.5`. Then you consult the package index for all
   known versions of `foo`. Say it looks like this:

    - `1.1`
    - `1.2`
    - `1.3`
    - `1.4`
    - `1.5`

    Then the versions that satisfy this constraint are (`1.2`, `1.3`,
    `1.4`). This constrain gets translated into the following logical sentence:

    $$ \text{foo-v1.2} \lor \text{foo-v.1.3} \lor \text{foo-v1.4} $$

    Arbitrarily complex version range checks can be implemented, by simply
    treating the check as a predicate that returns either true or false for
    every version in the package index. Then you make a disjunction of all the
    versions for which it returned true.

1. **Dependencies are Implications:** `foo-v1` depends on `bar-v2` or `bar-3`
   means "one of `bar-v2` or `bar-v3` must be used, _conditional on_ `foo-v1`
   being used". So we translate this as an implication:

    $$ \text{foo-v1} \implies (\text{bar-v1} \lor \text{bar-v2}) $$

1. **Consistency:** the constraint that we can't have different versions of the
   same package is expressed by negating the conjunction (and) of every _pair_
   of versions of the same package. So if `foo` has versions `[1,2,3,4]` the
   expression:

    $$ \begin{align*} (\neg (\text{foo-v1} & \land \text{foo-v2})) \, \land \\
    (\neg (\text{foo-v1} & \land \text{foo-v3})) \, \land \\ (\neg
    (\text{foo-v1} & \land \text{foo-v4})) \, \land \\ (\neg (\text{foo-v2} &
    \land \text{foo-v3})) \, \land \\ (\neg (\text{foo-v2} & \land
    \text{foo-v4})) \, \land \\ (\neg (\text{foo-v3} & \land \text{foo-v4}))
    \end{align*} $$

    Ensures we only have one version of `foo`.

1. **The Root of the DAG:** the package we're resolving dependencies for is the
   root of the build DAG. The version of the package we're building must be true
   on all assignments, so we have to $\land$ it together with every other
   term.

# A Simple SAT Solver

In this section I'll show you how to build a simple SAT solver in Python. This
is the simplest possible SAT solver: there is not one optimization and the time
complexity is exponential. For real-life use, you'll either want to use a
commercial off-the-shelf solver or optimize this further (there's a great deal
of literature on how to do this). The point is to de-mistify the process
somewhat (SAT solving sounds very ivory tower) and also to provide an actual,
working implementation of this post as a software object you can experiment
with.

First, some types to represent logic expressions:

```python
loom:include(classes)
```

The basic algorithm (and this is almost too stupid to be considered) is:

1. **Start:** we have a logical expression.
1. Does it have any variables?
    1. If so: pick a variable arbitrarily (but not randomly, because we want the
       algorithm to be deterministic). Then fork:
        1. In one branch, replace that variable with $\true$, and go to the
           start.
        1. In another, replace that variable with $\false$, and go to the start.
    1. If there are no variables, evaluate the expression. If it evaluates to
       $\true$, return the list of variable replacements it took to get here.

The core logic is implemented like this:

```python
loom:include(solver)
```

`any_var` is a function that takes an expression, and returns an
arbitrarily-chosen variable. This is implemented by recursively building up the
set of free variables, then sorting the variables (for determinism)
alphabetically and picking the first one:

```python
loom:include(free)
```

Evaluation is done in the base case of the recursion, when all variables have
been replaced:

```python
loom:include(eval)
```

Finally, the `replace` function takes an expression, and replaces all instances
of the variable with the given name with a Boolean constant:

```python
loom:include(replace)
```

# Example Run

Consider the following package database:

| Package   | Versions      |
|-----------|---------------|
| `app`     | 0             |
| `sql`     | 0, 1, 2       |
| `threads` | 0, 1, 2       |
| `http`    | 0, 1, 2, 3, 4 |
| `stdlib`  | 0, 1, 2, 3, 4 |

We have an application (the root of the build DAG) and four dependencies, one of
which (`threads`) will be transitive. And we have the following constraints:

```python
loom:include(deps)
```

We can translate this into a logic formula as follows (using some helper
functions):

```python
loom:include(convert)
```

Converting our dependency constraints into a formula:

```python
loom:include(conversion)
```

The full formula looks like this:

$$
\begin{align*}
\land ~~ &\text{app-v0} \\
\land ~~ &(\text{app-v0}\implies(\text{sql-v2})) \\
\land ~~ &(\text{app-v0}\implies(\text{threads-v2})) \\
\land ~~ &(\text{app-v0}\implies(\text{http-v3} \lor \text{http-v4})) \\
\land ~~ &(\text{app-v0}\implies(\text{stdlib-v4})) \\
\land ~~ &(\text{sql-v1}\implies(\text{stdlib-v1} \lor \text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{sql-v1}\implies(\text{threads-v1})) \\
\land ~~ &(\text{sql-v2}\implies(\text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{sql-v2}\implies(\text{threads-v1} \lor \text{threads-v2})) \\
\land ~~ &(\text{threads-v0}\implies(\text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{threads-v1}\implies(\text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{threads-v2}\implies(\text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{http-v0}\implies(\text{stdlib-v0} \lor \text{stdlib-v1} \lor \text{stdlib-v2} \lor \text{stdlib-v3})) \\
\land ~~ &(\text{http-v1}\implies(\text{stdlib-v0} \lor \text{stdlib-v1} \lor \text{stdlib-v2} \lor \text{stdlib-v3})) \\
\land ~~ &(\text{http-v2}\implies(\text{stdlib-v1} \lor \text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{http-v3}\implies(\text{stdlib-v2} \lor \text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &(\text{http-v4}\implies(\text{stdlib-v3} \lor \text{stdlib-v4})) \\
\land ~~ &\neg(\text{stdlib-v0} \land \text{stdlib-v1}) \\
\land ~~ &\neg(\text{stdlib-v0} \land \text{stdlib-v2}) \\
\land ~~ &\neg(\text{stdlib-v0} \land \text{stdlib-v3}) \\
\land ~~ &\neg(\text{stdlib-v0} \land \text{stdlib-v4}) \\
\land ~~ &\neg(\text{stdlib-v1} \land \text{stdlib-v2}) \\
\land ~~ &\neg(\text{stdlib-v1} \land \text{stdlib-v3}) \\
\land ~~ &\neg(\text{stdlib-v1} \land \text{stdlib-v4}) \\
\land ~~ &\neg(\text{stdlib-v2} \land \text{stdlib-v3}) \\
\land ~~ &\neg(\text{stdlib-v2} \land \text{stdlib-v4}) \\
\land ~~ &\neg(\text{stdlib-v3} \land \text{stdlib-v4}) \\
\land ~~ &\neg(\text{threads-v0} \land \text{threads-v1}) \\
\land ~~ &\neg(\text{threads-v0} \land \text{threads-v2}) \\
\land ~~ &\neg(\text{threads-v1} \land \text{threads-v2}) \\
\land ~~ &\neg(\text{http-v0} \land \text{http-v1}) \\
\land ~~ &\neg(\text{http-v0} \land \text{http-v2}) \\
\land ~~ &\neg(\text{http-v0} \land \text{http-v3}) \\
\land ~~ &\neg(\text{http-v0} \land \text{http-v4}) \\
\land ~~ &\neg(\text{http-v1} \land \text{http-v2}) \\
\land ~~ &\neg(\text{http-v1} \land \text{http-v3}) \\
\land ~~ &\neg(\text{http-v1} \land \text{http-v4}) \\
\land ~~ &\neg(\text{http-v2} \land \text{http-v3}) \\
\land ~~ &\neg(\text{http-v2} \land \text{http-v4}) \\
\land ~~ &\neg(\text{http-v3} \land \text{http-v4}) \\
\land ~~ &\neg(\text{sql-v1} \land \text{sql-v2}) \\
\end{align*}
$$

Running the solver on this formula:

```python
loom:include(solution)
```

Yields the following assignment:

| Variable     | Value    |
|--------------|----------|
| `app-v0`     | $\true$  |
| `http-v0`    | $\false$ |
| `http-v1`    | $\false$ |
| `http-v2`    | $\false$ |
| `http-v3`    | $\true$  |
| `http-v4`    | $\false$ |
| `sql-v1`     | $\false$ |
| `sql-v2`     | $\true$  |
| `stdlib-v0`  | $\false$ |
| `stdlib-v1`  | $\false$ |
| `stdlib-v2`  | $\false$ |
| `stdlib-v3`  | $\false$ |
| `stdlib-v4`  | $\true$  |
| `threads-v0` | $\false$ |
| `threads-v1` | $\false$ |
| `threads-v2` | $\true$  |

Or, put another way:

| Package   | Version |
|-----------|---------|
| `http`    | 3       |
| `sql`     | 2       |
| `stdlib`  | 4       |
| `threads` | 2       |

# Heuristics

One reason to roll your own SAT solver would be to introduce domain-specific
heuristics. For example: we'd generally want a package manager to prefer the
latest version of a package that satisfies the constraints.

We can implement this by redefining the function that chooses a variable, to pick variables representing higher version numbers first:

```python
loom:include(any_var_latest)
```

And using this in the resolver:

```python
loom:include(solver_latest)
```

Running the same formula over this new solver yields:

| Package   | Version |
|-----------|---------|
| `http`    | 4       |
| `sql`     | 2       |
| `stdlib`  | 4       |
| `threads` | 2       |


# See Also

- [Version SAT](https://research.swtch.com/version-sat)
- [So you want to write a package manager](https://medium.com/@sdboyer/so-you-want-to-write-a-package-manager-4ae9c17d9527)
- [Documentation on Dart's dependency solver](https://github.com/dart-lang/pub/blob/master/doc/solver.md)

# Footnotes

[^same]:
    This depends on the specifics of the programming language and package
    manager you're working on. Some languages are perfectly fine with different
    libraries depending on different versions of the same package. But it's a
    recipe for subtle bugs. Generally it makes things more tractable if the set
    of all transitive dependencies for an application has exactly one version
    for each package.
