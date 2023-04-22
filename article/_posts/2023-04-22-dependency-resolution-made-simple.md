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
problem in a tractable way.

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
of literature on how to do this).

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

First, we need a way to represent logical expressions:

```python
class Expr:
    pass


class FalseExpr(Expr):
    pass


class TrueExpr(Expr):
    pass


class Var(Expr):
    def __init__(self, name: str):
        self.name = name


class Not(Expr):
    def __init__(self, expr: Expr):
        self.expr = expr


class And(Expr):
    def __init__(self, exprs: list[Expr]):
        self.exprs = exprs


class Or(Expr):
    def __init__(self, exprs: list[Expr]):
        self.exprs = exprs
        
class Impl(Expr):
    def __init__(self, p: Expr, q: Expr):
        self.p = p
        self.q = q
```

```python
def replace(e: Expr, name: str, value: bool) -> Expr:
    if isinstance(e, FalseExpr):
        return FalseExpr()
    elif isinstance(e, TrueExpr):
        return TrueExpr()
    elif isinstance(e, Var):
        if e.name == name:
            return TrueExpr() if value else FalseExpr()
        else:
            return Var(e.name)
    elif isinstance(e, Not):
        return Not(replace(e.expr, name, value))
    elif isinstance(e, And):
        return And([replace(expr, name, value) for expr in e.exprs])
    elif isinstance(e, Or):
        return Or([replace(expr, name, value) for expr in e.exprs])
    elif isinstance(e, Impl):
        return Impl(replace(e.p, name, value), replace(e.q, name, value))
    else:
        raise TypeError("Invalid expression type")
```

```python
def eval_expr(e: Expr) -> bool:
    if isinstance(e, FalseExpr):
        return False
    elif isinstance(e, TrueExpr):
        return True
    elif isinstance(e, Var):
        raise ValueError(f"eval: the variable {e.name} has not been replaced.")
    elif isinstance(e, Not):
        return not eval_expr(e.expr)
    elif isinstance(e, And):
        return all(eval_expr(expr) for expr in e.exprs)
    elif isinstance(e, Or):
        return any(eval_expr(expr) for expr in e.exprs)
    elif isinstance(e, Impl):
        return (not eval_expr(e.p)) or eval_expr(e.q)
    else:
        raise TypeError("Invalid expression type")
```

```python
def free(e: Expr) -> set[str]:
    if isinstance(e, FalseExpr) or isinstance(e, TrueExpr):
        return set()
    elif isinstance(e, Var):
        return {e.name}
    elif isinstance(e, Not):
        return free(e.expr)
    elif isinstance(e, And):
        return set.union(*(free(expr) for expr in e.exprs))
    elif isinstance(e, Or):
        return set.union(*(free(expr) for expr in e.exprs))
    elif isinstance(e, Impl):
        return free(e.p).union(free(e.q))
    else:
        raise TypeError("Invalid expression type")
```

```python
def any_var(e: Expr) -> str | None:
    variables: list[str] = list(free(e))
    if len(variables) == 0:
        return None
    else:
        return variables[0]
```

```python
Bindings = dict[str, bool]


def join(a: Bindings | None, b: Bindings | None) -> Bindings | None:
    if a is not None:
        return a
    else:
        return b


def solve(e: Expr) -> Bindings | None:
    return solver(e, {})


def solver(e: Expr, bs: Bindings) -> Bindings | None:
    free_var = any_var(e)
    if free_var is None:
        if eval_expr(e):
            return bs
        else:
            return None
    else:
        # Replace with True.
        t: Expr = replace(e, free_var, True)
        t_bs: Bindings = dict(bs)
        t_bs[free_var] = True
        # Replace with False.
        f: Expr = replace(e, free_var, False)
        f_bs: Bindings = dict(bs)
        f_bs[free_var] = False
        # Solve both branches, and join them.
        return join(solver(t, t_bs), solver(f, f_bs))
```

# Example Run

```python
def dep(p: str, deps: list[str]) -> Expr:
    return Impl(Var(p), Or([Var(d) for d in deps]))


def notboth(a: str, b: str) -> Expr:
    return Not(And([Var(a), Var(b)]))


formula: And = And(
    [
        Var("Alpha-v1"),
        dep("Alpha-v1", ["Beta-v2", "Beta-v3"]),
        dep("Alpha-v1", ["Gamma-v3"]),
        dep("Beta-v1", ["Delta-v1"]),
        dep("Beta-v2", ["Delta-v2"]),
        dep("Beta-v3", ["Delta-v2", "Delta-v3"]),
        dep("Gamma-v1", ["Delta-v1"]),
        dep("Gamma-v2", ["Delta-v2"]),
        dep("Gamma-v3", ["Delta-v3"]),
        notboth("Beta-v1", "Beta-v2"),
        notboth("Beta-v2", "Beta-v3"),
        notboth("Beta-v3", "Beta-v1"),
        notboth("Delta-v1", "Delta-v2"),
        notboth("Delta-v2", "Delta-v3"),
        notboth("Delta-v3", "Delta-v1"),
        notboth("Gamma-v1", "Gamma-v2"),
        notboth("Gamma-v2", "Gamma-v3"),
        notboth("Gamma-v3", "Gamma-v1"),
    ]
)
```

$$
\begin{align*}
~~ &\text{Alpha-v1} \\
\land ~~ &(\text{Alpha-v1} \implies (\text{Beta-v2} \lor \text{Beta-v3})) \\
\land ~~ &(\text{Alpha-v1} \implies \text{Gamma-v3}) \\
\land ~~ &(\text{Beta-v1} \implies \text{Delta-v1}) \\
\land ~~ &(\text{Beta-v2} \implies \text{Delta-v2}) \\
\land ~~ &(\text{Beta-v3} \implies (\text{Delta-v2} \lor \text{Delta-v3})) \\
\land ~~ &(\text{Gamma-v1} \implies \text{Delta-v1}) \\
\land ~~ &(\text{Gamma-v2} \implies \text{Delta-v2}) \\
\land ~~ &(\text{Gamma-v3} \implies \text{Delta-v3}) \\
\land ~~ &\neg(\text{Beta-v1} \land \text{Beta-v2}) \\
\land ~~ &\neg(\text{Beta-v2} \land \text{Beta-v3}) \\
\land ~~ &\neg(\text{Beta-v3} \land \text{Beta-v1}) \\
\land ~~ &\neg(\text{Delta-v1} \land \text{Delta-v2}) \\
\land ~~ &\neg(\text{Delta-v2} \land \text{Delta-v3}) \\
\land ~~ &\neg(\text{Delta-v3} \land \text{Delta-v1}) \\
\land ~~ &\neg(\text{Gamma-v1} \land \text{Gamma-v2}) \\
\land ~~ &\neg(\text{Gamma-v2} \land \text{Gamma-v3}) \\
\land ~~ &\neg(\text{Gamma-v3} \land \text{Gamma-v1})
\end{align*}
$$

```python
bs: Bindings | None = solve(formula)
if bs is not None:
    for k, v in sorted(bs.items(), key=lambda p: p[0]):
        print(k, v)
```

| Variable | Value |
|----------|-------|
| Alpha-v1 | True  |
| Beta-v1  | False |
| Beta-v2  | False |
| Beta-v3  | True  |
| Delta-v1 | False |
| Delta-v2 | False |
| Delta-v3 | True  |
| Gamma-v1 | False |
| Gamma-v2 | False |
| Gamma-v3 | True  |


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
