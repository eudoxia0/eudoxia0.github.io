---
title: Second-Class References
summary: On Graydon's Rust, Val, and mutable value semantics.
card: second-class-references.jpg
card_source: |
    [_American Landscape_][link], Charles Sheeler, 1930.

    [link]: https://www.moma.org/collection/works/79032
---

[Graydon Hoare][gh], the creator of Rust, posted an article: [The Rust I Wanted
Had No Future][future] ([HN][hn], [Lobsters][lb]). It's an interesting
discussion of programming language evolution, the competing pressure of
expedience and technical perfection, and the social consequences of technical
decisions. He concludes that the language he would have built, given absolute
creative control, would not have been as popular as Rust is today.

[gh]: https://github.com/graydon
[future]: https://graydon2.dreamwidth.org/307291.html
[hn]: https://news.ycombinator.com/item?id=36193326
[lb]: https://lobste.rs/s/47amaq/rust_i_wanted_had_no_future

But on the technical side, this, in particular, stood out to me as things
Graydon would have dropped from Rust:

>**First-class &.** I wanted & to be a "second-class" parameter-passing mode,
>not a first-class type, and I still think this is the sweet spot for the
>feature. In other words I didn't think you should be able to return & from a
>function or put it in a structure. I think the cognitive load doesn't cover the
>benefits. Especially not when it grows to the next part.
>
>**Explicit lifetimes.** The second-class & types in early Rust were analyzed
>for aliasing relationships to ensure single-writer / multi-reader (as today's
>borrows are) but the analysis was based on type and path disjointness and (if
>necessary) a user-provided address-comparison disambiguation. It did not reason
>about lifetime compatibility nor represent lifetimes as variables, and I
>objected to that feature, and still think it doesn't really pay for
>itself. They were supposed to all be inferred, and they're not, and "if I were
>BDFL" I probably would have aborted the experiment once it was obvious they are
>not in fact all inferred. (Note this is interconnected with previous points:
>the dominant use-cases have to do with things like exterior iterators and
>library-provided containers).

This post is about the idea of doing away with lifetimes in Rust, what that
would bring to the table and how much it would cost.

# Outline

- first-class references and explicit lifetimes
  - references are pointers with a compile-time tag that helps enforce safety guarantees, namely
    - reference does not dangle
    - only one writer, or infinitely many readers
  - in rust, references are first class values
    - they can be passed to functions
    - returned from functions
    - stored in data structures and passed around
  - reference types are like a generic type with two arguments
    - the type they reference
    - and the lifetime
    - the lifetime is kind of like a singleton type that the compiler generates at compile time to distinguish one reference from another
  - you can store any pointer value in a pointer-typed variable
  - you can't store a reference in a variable that has a reference type for another lifetime
  - the types are not the same
  - this is scratching the surface
    - rust has a sophisticated ownership tracking system
    - can track ownership of, e.g., individual slot fields
    - non-lexical lifetimes
  - the benefit of first-class lifetimes is generality
    - references are just another value
    - reference types just another type
  - the cost is the borrow checker is very complex, in rustc, it's 27k lines of code

- second-class references
  - second class references are a restriction of first-class references
  - lose expressivity, gain simplicity
  - in second-class references
    - references cannot be returned from functions
    - references cannot be stored in data structures
    - references can only be created at function call sites
    - references are a parameter passing mode
      - parameter passing models are kind of an obscure concept nowadays
        - you don't see them much
        - they're in Pascal and Ada, as a feature to reduce the need for explicit pointers
      - basically you get three parameter passing modes:
        - by value: parameters are passed by value. in Rust's case, this is by move, ownership is passed to the function.
        - by reference: &x
        - by mutable reference
  - prior art
    - swift
    - the val language

- pros
  - simplicity
    - because references can only be created at the start of a function, and cannot be returned from functions, or stored in data structures
    - lifetimes become redundant
    - you no longer need lifetimes to enforce safety properties
    - so references go from being a two argument type (pointed to type, lifetime)
    - to a plain old fashioned "reference to T"
    - the only check you have to make is, at a call site, where a reference is created
      - you have to check the variable you're taking a reference to hasn't already been consumed
        - example
      - if mutable references are involved, you have to check the references don't overlap

- doable
  - surprisingly, yes
  - if you grep a large rust codebase
    - either libraries or applications
  - you find that:
    - overwhelmingly, references are just arguments to functions
    - sometimes, rarely, they're returned
    - sometimes, even more rarely, references are stored inside data structures
  - this usage pattern is the reason why lifetime elision works
  - rust has a feature called lifetime elision that allows you to skip writing lifetimes much of the time
  - like if you have a function that takes references, but doesn't return them, you can write:
    - example
  - and the compiler transforms this to
    - example
  - where references are used in a way that's more involved (e.g., being stored in data structures) lifetimes usually have to be written

- cons
  - loss of generality
    - the main place where references are stored in data structures is iterators
    - it's not clear how you'd do iterators with second-class references
      - the val language has a discussion page about that: link
    - graydon's post addresses this, i think, with "exterior iteration"
    - basically coroutines
    - it might be that to implement iterators you have to go down the escape hatch and use the unsafe subset of the language
    - using unsafe code for something as simple as iterators is a big ask
  - performance
   - a benefit of references is they're ultimately just pointers
   - getting the thing they point to is just a pointer dereference, a simple instruction
   - a design pattern that is widely used in rust to avoid reference headaches is to replace references with typed ID's, usually wrappers around integer indices into an array
   - so for example, suppose you have a string pool
     - a data structure that stores strings
     - you give it a string and it gives you back a reference to the string
     - if you give it a string already in the pool, it gives you back a reference to the same old string
     - this saves memory
     - you can do this with references
       - every string reference has to have the lifetime of the pool
       - the benefit of this approach:
         - leverage lifetimes to avoid use-after-free or dangling reference issues
       - the problem with this approach is:
         - references propagate everywhere
         - everywhere that uses string references you have to pass the lifetime of the pool
         - most of the time you're just working with a single string pool
         - so there's no room for confusion about where to get it from
     - so instead of returning references, you could return a `StringRef` struct, which is just a wrapper around an `usize` index into an array of strings, for example.
         - the benefit of this: the string ref is't tied to a lifetime
         - the problem is we've opened ourselves up to dangling reference issues
         - but the bigger practical problem is that a `StringRef` _by itself_ is useless. It's just a handle.
         - to do anything with the string, say, print it, we have to go to the string pool, pass in the `StringRef`, in order to get the actual text.
         - if you're in some code, twenty functions call, and you want to print a `StringRef`, no luck, you have to have planned to pass the string pool or a reference to it first.
         - across thread boundaries this gets worse

- reference transforms
  - sometimes you want to return a reference
  - usually this is in the context of data structures:
    - you have a reference to an array, and want a reference to the nth element
    - you have a reference to a hash map, and want a reference to the value
    - etc.
  - with second-class references you can't do that
  - but you can relax the "no returning references from functions" restriction somewhat
  - you can create a special class of function---call it a "reference transform"---that takes references and returns references
  - the only restriction is that, just as references can only be created as arguments to functions, reference transforms can only be called as arguments to functions
  - they is, they appear "in between" a call to a regular function, and a reference expression
  - so instead of `f(&x)`, you can have `f(t(&x))`, where `t` is a reference transform
  - Val has something like this, as far as I've been able to understand, in the form of subscripts, but subscripts are actually coroutines
  - to be honest I don't understand Val well enough

- conclusion
  - i wrote this article mostly to help myself understand mutable value semantics, and if they might be useful for Austral
  - key design constraint for Austral is simplicity
  - a semantics that lets you do away with 99% of the borrow checker obviously caught my attention
  - but is it worth it?
    - the benefits is simplicity are great
    - the loss of generality is also great
      - unclear how you'd go about implementing iterators
        - without entirely changing the model to iteration based on coroutines or something
      - you can't, for example, have Rust's HashMap entry API
      - if you want to use references to improve performance by using actual pointers (where the underlying value is one CPU instruction away) vs. indirection, you're out of luck, you have to go down to unsafe features
      - so second-class references put the "unsafe burden" very high, you have to break out unsafe features to do relatively quotidian things
      - it's a selling point of rust that when you want to use references, and have an intricately-connected data structure, you are free to do so, but you have to use lifetimes
        - this preserves safety
    - for austral, the wins are not that clear
    - austral's linear type system is already very simple
      - a more restricted version of rust
      - the linearity checker is ~700 lines of code
      - dropping down to second-class references simplifies it further
      - but at the cost of losing a great deal of safety and generality
