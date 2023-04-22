---
title: Dependency Resolution Made Simple
summary: Resolving dependencies with a homebrew SAT solver.
math: yes
---

$$
\gdef\true{\mathbf{T}}
\gdef\false{\mathbf{F}}
$$

Lately, I've been thinking about what the package manager for [Austral][austral] is going to look like. Dependency resolution is a [surprisingly complex problem][np], so I did a deep dive, and this post explains how to solve the problem in a tractable way.

[austral]: https://austral-lang.org/
[np]: https://research.swtch.com/version-sat

# The Problem

If packages specified the exact version of every one of their dependencies, there would be no dependency _resolution_ problem. There would however be constant dependency _integration_ problems.

You're writing an application that depends on libraries A and B. Library A depends on Foo v1.0.2, library B depends on Foo v1.0.3. Now you have a dependency conflict[^same]. The brute-force solution is to vendor one of A and B and adjust their dependencies. And you may have to do this recursively. Then you miss out on improvements, security fixes, etc.

"Fine", you said. "I'll allow depending on _major_ versions. Then A and B can just depend on Foo v.1.\*.\*, and the solver picks the latest version."

And this mainly solves the problem, assuming everyone sticks to [semver]. But there are two problems:

[semver]: https://semver.org/

1. Packages can hold back from upgrading because a new (minor or patch) version introduces either a bug or a performance regression that affects their usage of that package. The developers of A might want to exclude everything above 1.7.\*, while the developers of B are indifferent as long as the major version is 1. So picking e.g. 1.9.0 satisfies B but is bad for A.

2. Sometimes you want to depend on a library across major versions (e.g. the standard library) because you're only using the parts of the code that are highly conserved across major versions.

Even with strict semver enforcement (like [Elm does][elm]), there are observable features of libraries that fall outside of what an API spec can describe.

[elm]: https://elm-lang.org/

In other words, there's a tradeoff:

1. Less expressive version ranges make things easier for the package manager, while making the user experience hellish and involving a lot of manual work.
1. More expressive version ranges allow users greater freedom to allow or deny specific package versions, while complicating the work that the package manager has to perform.

Finding an assignment (a map from package names to the package version to use) that satisfies all of your transitive dependencies and that is free of colissions is a hard problem. It is NP-complete, actually. Manifest/lock files are orthogonal to this: manifest files are about making builds reproducible, but to make a manifest file you have to have resolved the dependencies in the first place.

The dependency resolution problem is solved, in wildly different ways, by different package managers:

1. TODO

There exists a simple and satisfactory solution that involves translating the problem into logic, applying a [SAT solver][sat] (a mature, well-understood piece of technology), and translating the solution back to the original problem domain. This approach is correct, it is free of ad-hockery, _and_ it benefits from the fact that SAT solvers have been optimized to death.

[sat]: https://en.wikipedia.org/wiki/SAT_solver

# Propositional Logic

[Propositional logic][proplog] is the simplest kind of logic. It has two components:

[proplog]: https://en.wikipedia.org/wiki/Propositional_calculus

1. **Syntax:** the rules for writing _logical sentences_, which are expression that can be evaluated to one of two values: true and false, just as arithmetic expressions can be evaluated to a number.
2. **Semantics:** the rules for evaluating logical expressions.

The syntax of propositional logic is simple: expressions are built up, starting from constants (true/false) and variables, by joining them through logical operators. More formally, an expression in propositional logic can be one of:

1. $\false$, which stands for false.
1. $\true$, which stands for true.
1. A variable (typically $A$, $B$, $C$, etc.) is an expression that can be replaced with true or false. Variables represent the inputs to a problem in logic.
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

Implication and if-and-only-if don't need their own constructors, because they are not primitive:

1. Implication should be thought of as a predicate. $P \implies Q$ means:
    1. _If_ $P$ is true and $Q$ is true, then $P \implies Q$ is true.
    2. _If_ $P$ is true, and $Q$ is false, then $P \implies Q$ is false.
    3. _If_ $P$ is false, the value of $Q$ doesn't matter, and the implication $P \implies Q$ as a whole evaluates to true.
  If you draw the truth table this is the same as writing $(\neg P) \lor Q$.
1. $P \iff Q$ is true when $P$ and $Q$ have the same truth value. This is the same as writing $P \iff Q$ is $(P \implies Q) \land (Q \implies P)$.

An _assignment_ is map from variables to truth values. Given an assignment $v$, expressions can be evaluated like so:

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

$$
((A \land B) \lor C) \rightarrow (\lnot B \land C)
$$

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
| $\false \lor (\true \land \true)$                                          | $\neg \false$ is true.          |
| $\false \lor \true$                                                        | $\true \land \true$ is true.    |
| $\true$                                                                    | $\false \lor \true$ is true.    |


The Boolean satisfiability problem is this: given an expression in logic, is there some assignment that makes it true? (And, if there is, ideally, we'd like to find at least one of those assignments.)

Solutions to this problem have surprisingly many useful applications, including dependency resolution.

# Translating the Problem

# A Simple SAT Solver

# Example Run

# See Also

# Footnotes

[^same]:
    This depends on the specifics of the programming language and package manager you're working on. Some languages are perfectly fine with different libraries depending on different versions of the same package. But it's a recipe for subtle bugs. Generally it makes things more tractable if the set of all transitive dependencies for an application has exactly one version for each package.
