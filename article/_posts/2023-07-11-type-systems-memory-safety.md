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
with two components, the pointed-to type and the lifetime. There's usually two
kinds of references: read-only (immutable) and read-write (mutable) references.

- safety is preserved by:
  - law of exclusivity
  - borrows are scoped, so they end
  - borrows cannot escape their scope because the types don't match
- law of exclusivity: at all times, a value is either:
  - not borrowed
  - immutably borrowed, with any number of immutable references
  - mutably borrowed, with one and only one mutable reference
  - mutable and immutable references are mutually exclusive: they can't both be live at the same time. mutable references are mutually exclusive with other mutable references.
  - in other words:
    - at all times a value has either
      - a single reader-writer (either a linear/affine owner or a mutable reference)
      - any number of readers
  - the reason for this is otherwise you get unsoundness
    - if you have an immutable and mutable reference to a struct
    - you can take an immutable reference to the struct's contents, and then use the mutable reference to replace the contents, invalidating the previous immutable reference

## Second-Class References {#ref2}

- second class references are called second class because
  - they can't be stored in structures
  - they can't be returned from functions
- this massively simplifies borrow checking

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
