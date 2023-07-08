---
title: "Type Systems for Memory Safety"
summary: A survey of type systems for memory safety.
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

- null safety
  - dereferencing a null pointer is bad
- use-after-free
  - double free
  - read memory that has been freed
  - write memory that has been freed
- leak freedom
  - everything that is allocated is freed
- data race freedom
  - memory can safely be manipulated by multiple threads without runtime cost (locks etc)

# Resource Safety {#resource}

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

## Affine Types {#affine}

## Linear Observers {#obs}

## Borrowing {#borrow}

## First-Class References {#ref1}

## Second-Class References {#ref2}

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
