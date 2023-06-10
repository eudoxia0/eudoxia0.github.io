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

# Contents

1. [First-Class References](#first)
1. [Second-Class References](#second)
1. [Realizability](#realize)
1. [Benefits](#pros)
1. [Costs](#cons)
   1. [Loss of Generality](#loss)
   1. [Separation](#sep)
1. [Reference Transforms](#transform)
1. [Conclusion](#conclusion)

# First-Class References {#first}

In Rust, references are pointers with a compile-time tag called a lifetime that
helps enforce safety guarantees at compile-time. The safety guarantees are:

1. **No dangling references:** references cannot outlive the lifetime of the
   thing they reference.
1. **Exclusivity:** at every point in time, every value has, either one mutable
   reference XOR one or more immutable references, but not both. This gives you
   thread-safety, or "fearless concurrency".

Reference types are basically a generic type with two parameters: the pointed-to
type, and the lifetime. If you want to think of a lifetime as a type, you can
think of it being like an empty struct type that the compiler generates in order
to tag the reference and distinguish references that have different lifetimes.

This is scratching the surface. Rust has a sophisticated ownership-tracking
system that can track e.g. ownership of individual struct fields and has fancy
ergonomic features like [non-lexical lifetimes][nll].

[nll]: https://rust-lang.github.io/rfcs/2094-nll.html

The benefit of having references be first-class is generality: in Rust,
references are first-class objects. They can be passed into functions, returned
from functions, and stored in datastructures. Reference types are types like any
other, references are values like any other.

The cost is complexity. The borrow checker is hard to understand, and hard to
implement. People learning Rust struggle with the borrow checker, and even
professional Rust programmers don't necessarily understand all of the rules,
rather, they run into borrow-checking errors time and time again and gradually
develope a kind of intuitive understanding of it.

# Second-Class References {#second}

Second-class references are a restriction of first-class references. You lose
some generality, and gain simplicity.

The idea is that second-class references:

1. Can't be returned from functions.
1. Can't be stored in data structures.
1. Can only be created at function call sites, as a special parameter-passing
   mode.

"Parameter passing modes" are an obscure concept nowadays. You see them in Ada,
where a function parameter can be marked [`in out`][inout], which lets you write
to it from the body of the function. Essentially it's an implicit mutable
reference.

[inout]: https://stackoverflow.com/questions/3003480/the-use-of-in-out-in-ada

In a language with second-class references, you'd have three parameter passing
modes:

1. By value, or, in Rust terminology, by move: `f(x: Foo) -> Bar`
1. By immutable reference: `g(x: &Foo) -> Bar`
1. By mutable reference: `h(x: &mut Foo) -> Bar`

And dually the call sites would look like this:

```
f(x);
g(&x);
h(&mut x);
```

Has this been implemented anywhere? Yes, the [Val][val] programming language
does this under the name ["mutable value semantics"][mvs].

[val]: https://www.val-lang.dev/
[mvs]: https://www.jot.fm/issues/issue_2022_02/article2.pdf

# Benefits {#pros}

So why would we do this? What's gained from this loss of expressive power?

The main benefit is simplicity: because references can only be created at
function calls, cannot be stored (leaked) anywhere, and cannot be returned,
borrow checking becomes trivial.

In fact, lifetimes become redundant: you no longer need lifetime annotations
_anywhere_ to enforce safety properties. Reference types become just `&T`,
without the associated lifetime.

All the borrow checker has to ensure is that when you're taking a reference to a variable:

1. The reference expression is an argument to a function.
1. The variable being referenced is not already dropped.
1. If mutable references are involved, you have to check the paths don't
   overlap.

The last point means if you have a function like:

```
fn store<T>(place: &mut T, value: &T) { ... }
```

You can't call it like:

```
store(&mut x, &x);
```

Because having a mutable reference and an immutable reference at the same time
breaks exclusivity.

# Realizability {#realize}

Is this doable? Surprisingly, yes. If you grep a large Rust codebase---either
libraries or applications---you'll find that the usage pattern of references is:

1. The overwhelming majority of references are just arguments to functions.
2. Sometimes you see a reference being returned from a function.
3. Rarely, very rarely, are references stored in structs, and often the lifetime
   is `'static`.

This usage pattern is why Rust's [lifetime elision][elide] feature works. This
lets you avoid writing lifetimes in most code. For example, you can write this:

```rust
fn concatenate_strings(s1: &str, s2: &str, s3: &str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

And the compiler will expand it to this:

```rust
fn concatenate_strings<'a, 'b, 'c>(s1: &'a str, s2: &'b str, s3: &'c str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

This simple case doesn't require any sophisticated analysis either (it gets more
complicated if you're returning a reference).

Where references are used in a way that's more involved, however, you usually
have to write down the lifetimes, but then you're trading off code complexity
for whatever gain you expect to get (usually performance and safety) from using
references.

The main exception to this is iteration: iterators are the most widely-used Rust
feature that involves storing a reference in a data structure. More on this
later.

# Costs {#cons}

The drawbacks are significant.

## Loss of Generality {#loss}

  - loss of generality
    - the main place where references are stored in data structures is iterators
    - it's not clear how you'd do iterators with second-class references
      - the val language has a discussion page about that: link
    - graydon's post addresses this, i think, with "exterior iteration"
    - basically coroutines
    - it might be that to implement iterators you have to go down the escape hatch and use the unsafe subset of the language
    - using unsafe code for something as simple as iterators is a big ask


## Separation {#sep}

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

# Reference Transforms {#transform}

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

# Conclusion {#conclusion}

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
