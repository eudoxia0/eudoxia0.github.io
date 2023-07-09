---
title: "Type Systems for Memory Safety"
summary: A survey of type systems for memory safety.
card: type-systems-memory-safety.jpg
card_source: |
    [_The Signing of Peace in the Hall of Mirrors_][link], William Orpen, oil on canvas, 1919.

    [link]: https://en.wikipedia.org/wiki/The_Signing_of_Peace_in_the_Hall_of_Mirrors
---

Manual memory management and memory safety used to be incompatible. But it is
possible to design programming languages and type systems that provide memory
safety at compile time, combining the safety of high-level languages with the
performance and low-level control of languages like C.

# Contents

1. [Memory Safety](#safety)
1. [Resource Safety](#resource)
1. [Approaches](#approaches)
    1. [Option Types](#option)
    1. [Region-Based Memory Management](#region)
    1. [Linear Types](#linear)
    1. [Affine Types](#affine)
    1. [Borrowing](#borrow)
    1. [First-Class References](#ref1)
    1. [Second-Class References](#ref2)
1. [Languages](#languages)
    1. [Austral](#austral)
    1. [Cyclone](#cyclone)
    1. [LinearML](#linearml)
    1. [Rust](#rust)
    1. [Val](#val)
    1. [Vale](#vale)
    1. [Vault](#vault)
    1. [Verona](#verona)
1. [Conclusion](#conclusion)
1. [See Also](#see)

# Memory Safety {#safety}

Memory safety is a bundle of things:

1. **Null Safety:** dereferencing a `NULL` pointer is bad. This causes a
   segfault or a `NullPointerException` or `undefined is not a function`,
   depending on your language. This is the easiest one to solve and arguably
   isn't about memory at all.

1. **Buffer Overflow:** indexing past the end of a contiguous chunk of
   memory. This is solved by storing the length of arrays and checking it.

1. **No Use-After-Free:** using a chunk of memory after it has been deallocated,
   e.g.:

    ```c
    free(ptr);
    f(ptr);
    ```

    This is a source of too many security vulnerabilities to count.

1. **Leak Freedom:** all memory that is allocated is freed.

1. **Data Race Freedom:** memory can be used by multiple threads without complex
   runtime access checks (locks, mutexes etc.)

Null safety and buffer overflows are solved by quotidian solutions: option types
and range checks. Use-after-free and leak freedom are harder to enforce, and
require potentially a lot more compile time machinery.

# Resource Safety {#resource}

Use-after-free and leak freedom generalize beyond memory. Some values are also a
_resource_: something that has a lifetime and has to be acquired, used, and
disposed in a particular way.

Bytes and floats are not resources. Heap-allocated memory, file handles, network
sockets, database handles, locks, mutexes, etc. are resources. They have a
contract: the API functions have to be used in a particular order, in a
particular number of times.

# Approaches {#approaches}

This section describes the different approaches to solving memory safety.

## Option Types {#option}

Option types solve null safety. These can be hardcoded into the language
(e.g. Dart, Kotlin) or implemented as a library (OCaml, Haskell, Rust).

Typically, a type `Option<T>` has the size of `T` plus the size of the tag that
says if it's empty or not. If pointers in the language are required to be
non-null, then there is an opportunity for an optimization: types like
`Option<Pointer<T>>` can be the same size as ordinary pointers, because `NULL`
can represent the empty case.

For this reason, option types do not incur performance penalties. In fact, they
can increase performance, by reducing the need to defensively check for `NULL`.

## Region-Based Memory Management {#region}

Region-based memory management is like [arena allocation][arena] at compile
time. It addresses use-after-free errors and memory leaks. Pointers are tagged
with a compile-time tag called a _region_, which is lexically scoped, like so:

[arena]: https://en.wikipedia.org/wiki/Region-based_memory_management

```c
// Declare a lexically-scoped region `R`.
region R {
  // Allocate some data in the region `R`.
  Pointer<int, R> ptr = allocate(10, R);
};
// When the region ends, its memory is deallocated.
```

Leak freedom is guaranteed because when a region's scope ends, its memory is
deallocated. When you reach the end of the program's entrypoint, all regions
have ended and been deallocated.

Use-after-free bugs are solved by the fact that pointers in different regions
are different types. That is, `Pointer<int, R>` is a different type than
`Pointer<int, S>`. You can't do this:

```c
Pointer<int, R> escape;
region S {
  Pointer<int, S> foo = ...;
  escape = foo;
};
```

Because the types of `escape` and `foo` are different. Therefore, pointers
cannot escape the region they are defined in, and therefore their memory cannot
outlive the region.

Regions are both a compile-time tag and a runtime object. The region object is
basically a [slab allocator][slab]. This can improve performance, because most
allocation happens from userspace rather than actually going to `malloc`.

[slab]: https://en.wikipedia.org/wiki/Slab_allocation

Pros:

- Simple to understand.
- Simple to implement.
- Can improve performance by minimizing syscalls.

Cons:

- Does not address concurrency and data race freedom.
- Does not generalize to resource safety.
- If a type has to hold a pointer, the region has to be moved up to a generic
  type parameter, e.g.:

    ```c
    struct<region R> Foo {
      ptr: Pointer<Bar, R>;
    };
    ```

    This is similar to lifetime parameters in Rust.

## Linear Types {#linear}

Linear types are types whose values must be used once, and exactly once.

I've written a lot about this, so in the interest of not repeating myself:

1. [_Introducing Austral_][auintro] explains linear types from the perspective
   of a programmer.
1. [_How Austral’s Linear Type Checker Works_][aucheck] explains linear types
   from the perspective of a compiler.

[auintro]: /article/introducing-austral#linear
[aucheck]: /article/how-australs-linear-type-checker-works

But the basic idea is what it says on the tin: linear values must be used
once. Using a linear value is called _consuming_ it. Ensuring that all linear
values are consumed can be done at compile time.

For example, let `Foo` be some linear type. Then the following doesn't work:

```rust
let x: Foo = f();
// Error: x not consumed
```

And neither does this:

```rust
let x: Foo = f();
g(x);
g(x); // Error: x consumed twice.
```

But this is just right:

```rust
let x: Foo = f();
g(x);
```

We can't do this, for example:

```rust
let x: Foo = f();
if whatever() {
  g(x);
} else {
  // Nothing
}
```

Because the variable `x` is consumed inconsistently: it's consumed in one
branch, but not another. Analogously:

```rust
let x: Foo = f();
while whatever () {
  g(x); // Error: consumed in loop
}
```

Here `x` is potentially consumed infinitely many times. The rules are short,
simple, and easily enforced: the linearity checker in the Austral compiler is
[~700 lines of OCaml][checker].

[checker]: https://github.com/austral/austral/blob/master/lib/LinearityCheck.ml

Leak freedom is solved, trivially: values must be consumed, which ultimately
means deallocated. Use-after-free is solved, trivially: you literally can't use
values twice. And this generalizes to resource safety: linear types need not be
pointers, but can be file handles, sockets, etc. And since every value can only
have one owner, concurrency is solved, trivially.

Oh, and you get [capability-based security][cap] for free.

[cap]: /article/how-capabilities-work-austral

The tradeoff is linear types are very onerous to write. For example, consider a
function that returns the length of a (linear) array. In Rust we might write:

```rust
fn length<T>(array: Array<T>) -> usize { ... }
```

But this doesn't work: because calling this function consumes (and ultimately
deallocates) the array:

```rust
let len = length(arr);
do_something_else(arr); // Error: `arr` already consumed
```

We would have to change the function to instead return the array alongside the
result:

```rust
fn length<T>(array: Array<T>) -> (Array<T>, usize) { ... }
```

And use it like this:

```rust
let (arr2, len) = length(arr);
do_something_else(arr2);
```

Needless to say: this is insane. Nobody wants to write code like this, where the
logic is buried under repeated threading of use-once variables through function
calls. Most languages that use linear types add rules on top that improve
ergonomics while preserving safety. In fact, these relaxations of the linearity
rules constitute most of the semantics. There's a reason Rust's borrow checker
is called a borrow checker and not an ownership checker.

Furthermore, ownership is equivalent to root permissions on a value, so passing
a value to a function lets that function do anything. Borrowing can be thought
of as not just a mechanism for better ergonomics but for graduated permissions:
immutable references, for example, don't allow you to mutate or deallocate data
since they don't own that data.

Another limitation of linear types is they can only represent trees rooted at
variables: interconnected, graph or DAG-like data structures are not directly
representable. More on this below.

Pros:

- Simple semantics.
- Easy to implement.
- Solves general resource safety.
- Improved performance by allowing in-place mutation.
- Solved data races and concurrency.

Cons:

- Linear types [are incompatible with exception handling][exception], at least
  the C++ or Java-style of exception handling.
- Linear types solve resource safety the way a gamma ray burst solves
  hospital-acquired infections. They are not really usable alone: you need extra
  rules to soften the restrictions while preserving safety. More on this below.

[exception]: /article/linear-types-exceptions

## Affine Types {#affine}

Affine types are a slight twist on linear types. Instead of "used exactly once"
it's "used _at most_ once". That is: values can be silently dropped. When a
linear value goes out of scope, the compiler inserts calls to the
destructor. This requires a way to associate a linear type with a destructor
function.

Rust has a sophisticated ownership-tracking system that resembles affine types
if you squint. In Rust, the `Drop` trait is used to tie types to their
destructors. Mostly this is implemented automatically by the compiler, you
usually implement this yourself when dropping a value involves calling a foreign
function (e.g. closing a socket).

The main benefit of affine types is they are more compatible with traditional
exception handling, since the compiler knows how to dispose of values, and
therefore how to call destructors when unwinding the stack.

Pros:
- Less code.
- Exception handling.

Cons:
- Sometimes you do want the compiler to tell you when you've forgotten a value
  (in Rust, the `must_use` attribute exists for this).
- The double-throw problem still exists in Rust: a destructor that fails
  (because e.g. closing the file handle failed, or closing a socket failed) will
  simply abort the program.

## Borrowing {#borrow}

The core idea of borrowing is to suspend linear/affine type rules for some
delimited time. It's kind of a combination of region-based memory management
with linear/affine types.

There's two ways to do this: first-class and second-class references.

## First-Class References {#ref1}

First-class references are "first-class" because they are values like everything
else: they can be passed to functions, returned from functions, stored in
structures etc.

A reference is like a pointer in region-based memory management: a generic type
with two components, the pointed-to type and the lifetime. The lifetime is just
a compile-time tag, references compile down to just plain old fashioned
pointers.

There's usually two kinds of references: read-only (immutable) and read-write
(mutable) references.

In the above example, the function that returns the length of an array would
instead be written:

```rust
fn length<T, 'L>(arr: ReadRef<T, L>) -> usize { ... }
```

(This isn't actual Rust syntax but I'm desugaring things somewhat to make it
easier to understand.)

Here, `'L` is a lifetime parameter. Lifetimes are analogous to regions, except
that, unlike in region-based memory management, where regions are both a
compile-time tag and a runtime storage pool, here lifetimes are just a
compile-time tag. `ReadRef<T, L>` is a read-only (immutable) reference to a type
`T` in the generic lifetime `L`. You would call this like so:

```rust
let arr = make_array(1, 2, 3);
let len = length(&arr);
```

Where `&arr` means "take a read-only reference to the variable
`arr`". Desugaring further for clarity (again not valid Rust):

```rust
let arr: Array<int> = make_array(1, 2, 3);
{
    // The lifetime `L` is valid only in this scope.
    letlifetime L;
    let ref0: ReadRef<int, L> = &arr;
    let len: usize = length(ref0);
}
```

So, rather than write code like this:

```rust
let foo = make_foo();
let (foo1, bar) = do_something(foo);
let (foo2, baz) = do_something_else(foo1);
let (foo3, quux) = do_whatever(foo2);
dispose(foo3);
```

We can write code like this:

```rust
let foo = make_foo();
let bar = do_something(&foo);
let baz = do_something_else(&mut foo);
let quux = do_whatever(&foo);
dispose(foo);
```

The code is cleaner and easier to read. Another difference is the functions get
just enough permissions to get by, and no more: in the before example,
`do_something` took ownership of `foo` and could do anything with it. In the
after example, `do_something` takes an immutable reference to `foo`, so it can
only read data from it, or call functions that take an immutable reference to
the same type. Analogously, `do_something_else` takes a mutable reference to
`foo`, so it can read or write to `foo` (or, transitively, call functions that
take an immutable or mutable reference to the type of `foo`) but it can't call
`dispose` because it doesn't have full ownership.

In case this helps understand, let's desugar the code:

```rust
let foo: Foo = make_foo();
{
    letlifetime L0;
    // `ref0` can't leak because the lifetime `L`
    // is only valid in this scope.
    let ref0: ReadRef<Foo, L0> = &foo;
    let bar = do_something(ref0);
}
{
    letlifetime L1;
    let ref1: MutRef<Foo, L1> = &mut foo;
    let baz = do_something_else(ref1);
}
{
    letlifetime L2;
    let ref2: ReadRef<Foo, L2> = &foo;
    let quux = do_whatever(ref2);
}
dispose(foo);
```

We can also visually annotate the lifetimes:

```rust
let foo: Foo = make_foo();                   // ----------\
{                                            //           |
    letlifetime L0;                          //  ---\     |
    let ref0: ReadRef<Foo, L0> = &foo;       //     | L1  |
    let bar = do_something(ref0);            //  ---/     |
}                                            //           |
{                                            //           |
    letlifetime L1;                          // ---\      |
    let ref1: MutRef<Foo, L1> = &mut foo;    //    | L2   | Foo
    let baz = do_something_else(ref1);       // ---/      |
}                                            //           |
{                                            //           |
    letlifetime L2;                          // ---\      |
    let ref2: ReadRef<Foo, L2> = &foo;       //    | L3   |
    let quux = do_whatever(ref2);            // ---/      |
}                                            //           |
dispose(foo);                                // ----------/
```

So, borrowing improves ergonomics and gives us more granular access control. But
how can we convince ourselves that it preserves the safety properties?

Borrowing preserves safety in three ways:

1. As in region-based memory management, references are lexically scoped and
   have a lifetime. The compiler ensures the lifetime of the reference does not
   exceed the lifetime of the value. This means no use-after-free bugs: the
   reference cannot outlive the thing it points to.
2. Again as in region-based memory management, the fact that lifetimes are part
   of the reference means references to different lifetimes are considered
   different types. This means you can't leak references by storing them in a
   data structure or variable that is out of the lexical scope where the
   reference is defined because the lifetimes don't match (usually however there
   are subtyping rules between lifetimes, to make things more convenient).
3. Finally, for references to be sound they must implement the law of
   exclusivity.

The **Law of Exclusivity** is this: at all times, a value is either:

1. Not borrowed (owned).
2. Borrowed _immutably_, with any number of immutable references live at the
   same time.
3. Borrowed _mutably_, with one and only one mutable reference.

Mutable and immutable references are mutually exclusive: you can't have an
immutable and mutable reference to the same value live at the same time. To
understand why, imagine a type like this:

```
enum MaybeBox {
  Empty,
  Full(Box<String>),
};
```

That is: an `EmptyBox` is either a pointer to a string, or nothing. Suppose you
have both an immutable reference `readref` and a mutable reference `mutref` to
the box at the same time, and that the box has contents:

<img src="/assets/content/type-systems-memory-safety/box1.svg" style="margin-left: auto; margin-right: auto;"/>

Then you transform the read-reference to the box to a reference to its contents:

<img src="/assets/content/type-systems-memory-safety/box2.svg" style="margin-left: auto; margin-right: auto;"/>

Then you use the mutable reference to empty the box. Now the read reference is dangling:

<img src="/assets/content/type-systems-memory-safety/box3.svg" style="margin-left: auto; margin-right: auto;"/>

And now you have unsoundness: your read reference is invalid. Mutable references
are mutually exclusive with each other for the same reason.

One more point. Functions that take references as parameters must make the
lifetimes into generic function parameters, like so:

```rust
fn concatenate_strings<'a, 'b, 'c>(s1: &'a str, s2: &'b str, s3: &'c str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

Rust has a feature called lifetime elision that simplifies writing code like
this. Because most functions that take references don't return a reference type,
the compiler lets you write the function signature without lifetime annotations,
and adds them for you. The following:

```rust
fn concatenate_strings(s1: &str, s2: &str, s3: &str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

Is equivalent to the function above. The compiler simply makes an implicit,
generic lifetime parameter for each reference type mentioned in the parameter
list. This is the most general solution: if different arguments have the same
lifetime it won't be a problem.

However, if you want to enforce that two parameters must come from the same
lifetime, you need to write the lifetime parameters explicitly.

Pros:

- **Ergonomics:** you can write code that reads data from linear/affine values,
  without consuming them or having to return them as tuples.
- **Permissions:** references are like permissions: a read-reference says "you
  can read, but not mutate or deallocate". A mutable reference says "you can
  read and mutate, but not deallocate". Linear ownership says "you can do
  anything".
- **Safety:** lifetimes soften the linearity rules while preserving safety, due
  to the law of exclusivity, and the fact that lifetimes are scoped.

Now references seem like a silver bullet: we get safety and ergonomics. What are
the problems?

Cons:

1. **Complexity:** lifetime analysis can be complex. The complexity is more or
   less proportional to how easy it is to write code that "does what you mean"
   and have it compile. In other words, ergonomics come at the cost of language
   complexity.

   To make code easier to write you need all sorts of heuristics and special
   cases. These complicate the analysis, making the compilers harder to
   implement, and making the language rules harder to understand.

2. **Learning Curve:** related to the above, the main problem people report with
   Rust is the learning curve. People often describe learning Rust as fighting
   the borrow checker until they develop an intuitive sense for how it
   works. It's hard to write down a small, core set of rules for Rust borrow
   checking.

   There are many things to understand: non-lexical lifetimes and implicit
   reborrowing for example.

3. **Lifetime Creep:** as in region-based memory management, if you want to
   store a lifetime in a data structure, you need to make that lifetime into a
   generic parameter. That is, if you want to write:

   ```rust
   struct Foo {
       bar: &Bar
   }
   ```

   You have to write:

   ```rust
   struct Foo<'L> {
       bar: &'L Bar,
   }
   ```

   And so, any data structure that stores references, the lifetime annotations
   never disappear. There isn't really a way around this: the compiler needs to
   know the lifetime to ensure safety.

   And so the usual advice for storing references in data structures in Rust is
   "don't". In fact, when I read Rust codebases, the main place where I see
   references stored in structures is with iterators.

   And so there's a tradeoff: you can write code where everything is deeply
   intertwined with pointers, but keeping the lifetimes straight can be a mess.

4. **Disuse:** because of the difficulty of writing code that uses lifetimes
   pervasively, many people don't use lifetimes, opting instead to use `Rc` or
   `Arc`, which lower compile-time complexity by introducing the run-time cost
   of reference counting.

   Another common pattern in Rust is to use indices into an array where you
   would otherwise use pointers. For example, if you were writing a binary tree
   in C, you might use:

   ```c
   struct Tree {
       Tree* left;
       Tree* right;
   };
   ```

   In Rust you will frequently see something like this:

   ```
   struct Tree {
       nodes: Vec<Node>,
       root: usize,
   };

   struct Node {
       left: usize,
       right: usize,
   };
   ```

   That is: the tree is a vector of nodes, and rather than use pointers or
   references, each node uses integer indices into the array to make the tree
   structure.


   There are many benefits to this pattern. For one, it is the only way to
   implement a graph-like data structure. It can improve performance, because
   allocating a node doesn't necesarilly require `malloc`, the vector acts as a
   resizable arena allocator. It also improves cache locality, because the nodes
   are not randomly distributed throughout the heap but kept close together.

   The problem is you lose safety:

   1. There is no guarantee that an index is in bounds.
   2. There is no guarantee that an index points to the right data. Any change
      to the node vector has to update the indices in all the nodes.
   3. There is no way to link indices and trees. If you use an index from one
      tree into another, you could trigger an index-out-of-bounds error or get
      wrong data.

   So we go right back to all the problems of memory-unsafe languages, but one
   level of abstraction up. These problems can be reduced by careful design: you
   can build a data structure that exposes a more-or-less safe API by having
   unsafe internals. But you can do the same thing in C or C++.

## Second-Class References {#ref2}

- study rust code
  - you may notice the way references are used
  - 99% of the time as function arguments
  - a few times, returned from functions
  - a few times, stored in data structures (other than iterators, this is very
    rare)
- second class references are called second class because
  - they can't be stored in structures
  - they can't be returned from functions
- this massively simplifies borrow checking
  - the properties of region-based memory management you get for free
    - since they can only be created at function calls, and cannot leave
      functions, they are scoped for free
    - can't leak them
    - can't store them in data structures
  - borrow checking without lifetimes
  - the only borrow checking needed is
    - at function call sites, analyze all the references being passed, and
      ensure they meet the law of exclusivity
    - can't have `f(&mut x, &mut x)`
    - also can't have `f(&x, &mut x)`
    - that's it
    - that's _all_ there is to the borrow checker
- is this realizable?
  - yes!
  - as stated above, most rust code is written this way already
  - also, the fact that lifetime elision works in Rust is due to this
- graydon hoare, the creator of rust, says lifetimes are a big problem that
  don't pay for themselves and he'd make them a second-class parameter passing
  mode
- what are the drawbacks?
  - iterators
  - storing references in data structures is rarely done, but when it is done,
    it is necessary
  - the good thing about Rust references is they provide a middle ground: when
    you _need_ code that has deeply-intertwined pointers, you _can_ write that
    code. it is hard, it can be a complex mess of spaghetti lifetimes. but you
    _can_ write the code, and you _don't_ lose safety.
  - second-class references lose a lot of expressive power
  - so when you do need to write code that uses deeply intertwined pointers, you
    can't do it, but you have to write unsafe code
  - so it seems that second-class references are strictly less safe than rust
  - so there's a tradeoff here where increasing safety assurance means
    increasing language complexity, and different people can make different
    calls about where they want to make that tradeoff
- one thing you can't express: Rust's hash map entry API
- the val language
  - uses subscripts to return references
- perhaps we can soften some of these restrictions, preserving safety and
  simplicity while increasing expressive power
- reference transforms
  - a special class of functions that is allowed to return references that they
    take as arguments
  - however, they can only be called at function call sites
  - that is:
    - expression that take a reference
    - expressions that transform a reference
    - can only appear at function call sites
    - so where you write `foo(&x)`, you can also write `foo(bar(&x))`, where
      `bar` is a reference transform
- we can also soften the restriction of storing references in data structures
  - c# has a concept of "ref structs", which can hold references, but themselves
    inherit the restrictions of references
  - we can have a special kind of data structure that can hold references
  - has the same restrictions as references:
    - cannot be returned from functions (except reference transform functions)
    - cannot be stored in data structures (except other ref structs)
  - like reference expressions, and reference transforms, ref structs can only
    be created at function call sites
  - so if you have a function that needs to take a lot of references as context
    objects
  - `f(&a, &b, &c, &mut d, foo)`
  - you can wrap them up in a ref struct
  - `f(Context(&a, &b, &c, &mut d), foo)`
  - safety is preserved
- iterators?

# Languages {#languages}

This section describes how different languages implement memory and resource
safety.

## Austral {#austral}

## Cyclone {#cyclone}

## LinearML {#linearml}

## Rust {#rust}

## Val {#val}

## Vale {#vale}

## Vault {#vault}

## Verona {#verona}

# Conclusion {#conclusion}

# See Also {#see}

- Rust:
  - [Rust for "modern" C++
    devs](http://venge.net/graydon/talks/RustForModernCPPDevs.pdf): slides from
    a 2022 talk by Graydon Hoare.
- Linear Types:
- Region-Based Memory Management:
