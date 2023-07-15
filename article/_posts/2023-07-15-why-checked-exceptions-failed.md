---
title: Why Checked Exceptions Failed
---

Outline:

    a function sig is its args and return type

    some languages add information to the signature orthogonal to the types
        java: checked exceptions
        effects: etc.

    the language as a whole is not updated to this

        interface method foo, implementation method has a checked exception
            can i write this code?
            how can i specify in the interface that an implementation of this method can throw any, some, or no checked exceptions?
            anti-modularity and brittle hierarchies
            flexible interfaces have to be written with the lowest-common-denominator interface in mind

        function type
            how do i annotate the type of a function with the set of exceptions it can throw?
            how can i say "no exceptions" or "all" or "some" or "only exceptions that are a subclass of this exceptino"

        soon you're overhwhelmed
            subtyping relationship between checked exception sets
            which means lattices
            covariance and contravariance

    it starts out as a simple idea
        we can trivially annotate functions with extra information: failure modes, computational effects (uses the network, filesystem, needs a high-precision clock), recursion, stack usage
        the whole language has to be updated

    big risk

        languages are tools for managing complexity

        intrinsically this cannot be tested on small examples
            andrew plotkin

        this means that launching a language with a new novel feature is risky

        you need to write, tens, hundreds, millions of lines of code to see how it behaves at scale
