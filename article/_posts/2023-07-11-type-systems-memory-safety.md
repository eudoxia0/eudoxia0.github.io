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

# Resource Safety {#resource}

- the problems with memory safety generalize to things other than memory
- they generalize to _resources_
  - a resource is something that has a lifetime: it has to be created, used, and disposed in a particular way
  - bytes and floats and string literals are not resources
  - heap-allocated memory, file handles, network sockets, database handles, locks, mutexes, etc. are resources
  - they have a contract: the api has to be used in a particular order and number of times

# Approaches {#approaches}

## Option Types {#option}

- option types solve null safety
  - option types are largely orthogonal to other approaches
  - so option types solve null safety, and the rest of the approaches solve the other points
- can be hardcoded (Dart) or implemented in userspace (Rust)
  - in the latter case, compiler should special-case optionals of pointers
  - `option<pointer<t>>` should be implemented as `t*`, with `null` as the `None` case.

## Region-Based Memory Management {#region}

- tag pointers with compile-time tags
- how it works:
  - declare locally-scoped region
  - allocation happens in a region
  - deallocation happens automatically when the region ends
- how it enforces safety:
  - use-after-free:
    - pointers cannot be used outside of the lexical region where their region is defined
    - pointers can't escape
      - can't store it outside a region: types don't match
      - a type that stores a pointer has to shift the region to a generic type parameter
  - leak freedom
    - all regions close when their scope ends
  - data race freedom
    - not addressed

## Linear Types {#linear}

- linear types: values of a linear type must be used once, and exactly once
- let's see what the rules are:
  - etc.
- let's see how it addresses safety:
  - etc.
- linear types are incompatible with traditional (C++ or Java-style) exception handling
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
  - actually a very sophisticated ownership-tracking scheme that is kind of affine if you squint
  - uses the `Drop` trait to know how to dispose of something
  - Drop is automatically implemented in the obvious way: by calling drop on all fields in a struct
- linear types are incompatible with exceptions, but affine types are not,
  because the compiler knows how to dispose of types, it knows how to insert
  destructor calls at the end of a scope or when unwinding the stack

## Linear Observers {#obs}

- linear observers are from linearml
- they are like a wrapper around a linear type, that can be used to read data
  from it, but they can't be stored in data structures

## Borrowing {#borrow}

- the core idea of borrowing is to suspend ownership for a duration in time that can be statically determined
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
    - varaible cannot be borrowed mutable multiple nested times, because it would create multiple writes
  - similar to region-based memory management, a lifetime is like a region, it is a compile-time tag
- how safety is preserved:

## Second-Class References {#ref2}

- second class references are called second class because
  - they can't be stored in structures
  - they can't be returned from functions
- this massively simplifies borrow checking

# Languages {#languages}

## Austral {#austral}

## Cyclone {#cyclone}

## LinearML {#linearml}

## Rust {#rust}

## Val {#val}

## Vale {#vale}

## Vault {#vault}

## Verona {#verona}

# Conclusion {#conclusion}
