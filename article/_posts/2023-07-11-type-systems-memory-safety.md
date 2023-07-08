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
        1. [Rules](#linear-rules)
        1. [Safety Properties](#linear-safety)
        1. [Problems](#linear-problem)
    1. [Affine Types](#affine)
    1. [Linear Observers](#obs)
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
time. It addresses use-after-free errors and memory leaks. Pointers are tagged with a compile-time tag called a _region_, which is lexically scoped, like so:

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

- linear types: values of a linear type must be used once, and exactly once

### Rules {#linear-rules}

- let's see what the rules are:
  - etc.

### Safety Properties {#linear-safety}

- let's see how it addresses safety:
  - etc.

### Problems {#linear-problem}

- linear types are incompatible with traditional (C++ or Java-style) exception
  handling
- linear types are very onerous to write
  - to make them ergonomic, you have to add rules on top
  - these rules make linear types usable, while preserving safety

## Affine Types {#affine}

- weakening of linear types
- instead of: use once and exactly once
- it is: use at most once
- implicit drop
- to implement this, the compiler needs to know how to drop a value
- rust uses an affine-like type system
  - actually a very sophisticated ownership-tracking scheme that is kind of
    affine if you squint
  - uses the `Drop` trait to know how to dispose of something
  - Drop is automatically implemented in the obvious way: by calling drop on all
    fields in a struct
- linear types are incompatible with exceptions, but affine types are not,
  because the compiler knows how to dispose of types, it knows how to insert
  destructor calls at the end of a scope or when unwinding the stack

## Linear Observers {#obs}

- linear observers are from linearml
- they are like a wrapper around a linear type, that can be used to read data
  from it, but they can't be stored in data structures

## Borrowing {#borrow}

- the core idea of borrowing is to suspend ownership for a duration in time that
  can be statically determined
  - usually a lexical scope
  - there's two ways to do this, and they have significant trade offs
  - one way is first-class references, how Rust does it
  - another is second-class references, how Val and C# do it
- rules:
  - at all times, a linear/affine variable can be:
    - not borrowed
    - borrowed immutable any number of times
    - borrowed mutable once
  - the point is that a variable can have multiple readers, but no writers, or,
    one and only one writer and no readers at the same time

## First-Class References {#ref1}

- first-class references: references are a value like anything else
  - can be returned from functions
  - stored in variables
- type is a generic type, with two components:
  - the type it points to
  - the reference lifetime
- the way austral does it is easy to understand
- two kinds of references:
  - immutable: can be used to read data
  - mutable: can be used to read and write data
- references are acquired by `&x`, mutable ones by `&!x`
- it's easiest to understand by desugaring
  - lift to borrow statement
  - borrow statement rules:
    - variable cannot be consumed inside the borrow statemnt
    - varaible cannot be borrowed mutable multiple nested times, because it
      would create multiple writes
  - similar to region-based memory management, a lifetime is like a region, it
    is a compile-time tag
- how safety is preserved:

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
