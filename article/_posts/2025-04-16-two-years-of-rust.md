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

## Performance {#perf}

Rust is fast.

But the way this shows up is not, "this specific thing is fast", but rather "everything is not uniformly slow".

The usual cope about performance is: you shouldn't worry about it, because in most programs, 99% of the runtime is spent in 1% of the code, so once you have problems, you just profile and rewrite the "hot loops". But in web applications, that's not really true. Obviously you can write accidentally quadratic loops and n+1 queries in every language, but those are discrete performance problems that can be solved in isolation.

The much more common situation, when I worked in Python web applications, is not that you have a "hot region" of the call tree, but that _everything_, every function call, is 10x to 100x slower than it could be. And so there are no obvious places to cut, there are no pressing bottlenecks. You can optimize all the bottlencks and find that the performance bedrock is just too high. And then what can you do? You can scale the hardware vertically, and end up like those people who spend five figures a month on AWS to get four requests per second. You can keep your dependencies up to date, and hope that the community is doing the work of improving performance. And you can use async as much as possible on the belief that your code is I/O-bound, and be disappointed when it turns out that actually you're CPU-bound.

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
