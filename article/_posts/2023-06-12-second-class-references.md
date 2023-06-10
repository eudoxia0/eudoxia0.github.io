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
   1. [Example: String Pools](#pools)
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

The main thing we lose is expresive power. While references in Rust are
overwhelmingly used in a way that would allow them to be degraded into
second-class references, there are places where we need first-class references.

The main pain point is iterators. In Rust, iterators are (far and above) the
main place where you find yourself storing a reference in a struct. It's not
clear to me how you would do iterators with second-class references. The Val
language has a [discussion page][iter] about this.

[iter]: https://github.com/val-lang/val-lang.github.io/discussions/44

Graydon Hoare's solution is to outright change how iteration happens:

>Exterior iteration. Iteration used to be by stack / non-escaping coroutines,
>which we also called "interior" iteration, as opposed to "exterior" iteration
>by pointer-like things that live in variables you advance. Such coroutines are
>now finally supported by LLVM (they weren't at the time) and are actually a
>fairly old and reliable mechanism for a linking-friendly,
>not-having-to-inline-tons-of-library-code abstraction for iteration. They're
>in, like, BLISS and Modula-2 and such. Really normal thing to have, early Rust
>had them, and they got ripped out for a bunch of reasons that, again, mostly
>just form "an argument I lost" rather than anything I disagree with today. I
>wish Rust still had them. Maybe someday it will!

So if you want Rust or C++ style iterators with second-class references, your choices are:

1. Coroutines.
2. Typed index values (but without lifetimes, there's no way to tie those
indices to the collection they index into).
3. Opening the escape hatch and using unsafe pointers. But using unsafe language
   features for something as quotidian as iteration is a big degradation in
   safety.

## Separation {#sep}

One benefit of references is they're ultimately just pointers: getting the data
they point to is one CPU instruction away.

A design pattern that's widely used in Rust is to replace references with typed
indices. That is, instead of:

```c
struct Tree {
  Tree* left;
  Tree* right;
};
```

You'd have:

```rust
struct Tree {
  nodes: Vec<TreeNode>,
};

type TreeIndex = usize;

struct TreeNode {
  left:  TreeIndex,
  right: TreeIndex,
};
```

The benefit: this avoids lifetime headaches and lets you implement things
otherwise inexpressible in safe Rust, like cyclical references or graphs. The
cost: we're back to dangling-reference bugs, because a `TreeIndex` is just an
integer, and may outlive the tree it indexes into. Similarly, you can use a
`TreeIndex` on the wrong `Tree`.

With second-class references you're pretty much forced into this design pattern.

There's an extra cost to this pattern: you can't dereference a `usize`. For
certain tasks, you can't operate on _parts_ of data structures, you have to
operate on the _whole_, that is, to do things with trees you need not just the
`TreeNode` and the `TreeIndex` but the tree itself which holds the pool of
nodes.

It's debatable whether this is actually a bad thing. I increasingly think that
shallow data structures with integer indices are infinitely better that
intricately-connected graphs of pointers.

## Example: String Pools {#pools}

Consider a string pool. A string pool lets you "intern" strings: you give it a
string, and get back a reference to the string in the pool. If you give it the
same string multiple times, you get a reference to the same string. This saves
memory.

A string pool built with lifetimes has the benefit that the string references
will never dangle (they cannot outlive the lifetime of the pool), and you can't
use a string reference on the wrong pool because the lifetimes are
different. The downside is everywhere you use a string reference, you have to
carry the lifetime of the pool, complicating the code.

A string pool built with typed indices saves you the headache of thinking about
lifetimes, at the cost of, again, references can dangle and they can be used on
the wrong pool.

But there's the problem of the part-whole conflict: you can't sort an array of
pool indices, for example, because they're just integers that denote an index in
an array. You need access to the pool. If you're twenty stack frames deep and
want to print the contents of a pool index, you need to pass the pool in. This
is worse when thread are involved.

Again, it's debatable whether this is bad at all. The fact that you have to
bring in the string pool arguably aids in local reasoning: there's no spooky
action at a distance, functions must pull in their dependencies. This problem is
discussed at greater length [here][implicits].

[implicits]: https://matklad.github.io/2023/05/02/implicits-for-mvs.html

# Reference Transforms {#transform}

Sometimes you want to return a reference from a function. Usually this is in the
context of data structures: if you have a reference to an array, you want a way
to get a reference to the _n_-th element and such.

With second class references you can't do that. But there is a simple relaxation
of the "no returning references from functions" restriction that works.

You can define a special class of functions---call the _reference
transforms_---that can take references and return more interior references. In
turn, reference transforms are restricted:

1. Like reference expressions, reference transforms can only appear as an
   argument to a function.
2. The arguments to a reference transform are either a reference expression or
   another call to a reference transform.

In other words: reference transforms can only appear _between_ function calls
and reference expressions. So if you can write `f(&x)`, you can also write
`f(t(&x))`, or `f(t(u(v(&x))))`, where `t`, `u`, and `v,` are reference
transforms.

Val has something like this, as far as I've been able to understand, in the form
of [subscripts][sub], but subscripts are actually coroutines. To be honest I
don't understand Val well enough to explain it in detail.

[sub]: https://tour.val-lang.dev/subscripts

# Conclusion {#conclusion}

This post was an attempt to understand mutable value semantics, organize my
thoughts about it, and see if they might be useful for Austral. A key design
constraint in Austral is simplicity, and a semantics that does away with 99% of
borrow checking obviously caught my attention. I thought: if this is compelling
enough I'll throw out the existing borrowing model for Austral, and implement
second-class references.

But is it worth it? Again, the tradeoff is expressivity vs. simplicity. The
gains in simplicity are great, but the costs---_especially_ around iteration and
the part-whole conflict---are also great.

A selling point of Rust is, if you want an intricately-connected data structure
built from references, you can build it. You pay a cost in complexity, _but not
in loss of safety_. You don't have to break out raw pointers to use references
in a complex way. In other words, first-class references give you a midpoint
between raw pointers and second-class references, where you can do things
second-class references cannot, without losing safety.

If you compare Val and Rust, the simplicity gains are tremendous, because Rust's
borrow checker is very complex, especially with newer language features like
async that complicate lifetime analysis.

But if you're comparing Austral and Rust, the benefits are less clear, because
Austral's linear types and borrowing is already so simple. Austral's equivalent
of a borrow checker is [~700 lines of OCaml][lincheck]. The only downside of
Austral is right now you have to write the lifetimes of the references you pass
to functions, but I will probably implement lifetime (region) elision. At which
point, you get most of the ergonomics of second-class references, but the linear
type system and borrow-checker rules still fit [on a page][ref].

[lincheck]: https://github.com/austral/austral/blob/8568383ed373d0df85cdf69e752130fb259f5949/lib/LinearityCheck.ml
[ref]: https://austral-lang.org/spec/spec.html#linearity

Another thing I didn't touch on is closures. Val [has closures][closure], and
they can capture immutable as well as mutable references, but it's not clear to
me what restrictions Val imposes here to preserve safety. Also, Val has a
concept of [remote parts][remote], which I think is a way to store references in
data structures without involving lifetimes.

[closure]: https://tour.val-lang.dev/functions-and-methods#closures
[remote]: https://github.com/val-lang/val/blob/8d4dadc3ecb3d8f098a4e4f12139eb8bcd3950e4/Docs/RemoteParts.md?plain=1#L1

Second-class references are appealing, but before giving up on first-class
references in Austral, I'd like to have a better grasp of how patterns like
iterators and core language features like closures can be carried over without
loss of generality.
