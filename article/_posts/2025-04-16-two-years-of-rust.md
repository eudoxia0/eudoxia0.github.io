---
title: Two Years of Rust
summary: TODO
card: two-years-of-rust.webp
---

- context
  - just finished a job
  - 2y working with rust
  - on b2b saas

# Learning {#learning}

- didn't let rust the usual way
- studied rust, more or less academically, as research for [Austral][au]
- when I started the job I had not written almost anything
- nevertheless I could jump right in

# The Good {#good}

- rust as a better go
- rust as a faster python

## Cargo {#cargo}

## Error Handling {#error}

## Type Safety {#types}

## The Borrow Checker {#borrow}

- fighting the borrow checker doesn't happen
- often, the borrow checker steers you towards designs with mechanical sympathy
- in rare circumstances, you get inscrutable lifetime errors, these often happen when multiple orthogonal features intersect: e.g. closures and async
- often this is a sign you have to simplify

# The Bad {#bad}

## Typechecker Performance {#type-perf}

- not great
- downstream of the fact that modules are not compilation units, but rather, crates are

## Build Performance {#build-perf}

- really bad
- usually blamed on [LLVM]
  - fair enough
- there's various tricks you can use to speed it up
- there's also crate splitting
- but it's hard to measure the performance impact of crate splitting, because, for a large project, refactoring on crate into many is very, very time-consuming and laborious
- just pay for bigger CI runners

## Mocking {#mock}

- dynamic dispatch is more complicated in rust due to lifetimes
- this makes mocking harder, because instead of being able to pass a new instance of a class, you have to move the mocking to the type-level

[au]: https://github.com/austral/austral
[LLVM]: https://llvm.org/
