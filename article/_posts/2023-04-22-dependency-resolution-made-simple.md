---
title: Dependency Resolution Made Simple
summary: Resolving dependencies with a homebrew SAT solver.
---

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

# Propositional Logic

# Translating the Problem

# A Simple SAT Solver

# Example Run

# See Also

# Footnotes

[^same]:
    This depends on the specifics of the programming language and package manager you're working on. Some languages are perfectly fine with different libraries depending on different versions of the same package. But it's a recipe for subtle bugs. Generally it makes things more tractable if the set of all transitive dependencies for an application has exactly one version for each package.