---
title: Two Years of Rust
summary: TODO
card: two-years-of-rust.webp
---

I spent the last two years writing b2b SaaS in Rust, so now is the ideal time to reflect on the experience and write about it.

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

The worst thing about the Rust experience is the build times. This is usually blamed on [LLVM], which, fair enough, but I think part of it is just intrinsic features of the language, like the fact that modules are not independent compilation units, and of course monomorphization.

There are various tricks to speed up the builds: [caching][cache], [cargo chef][chef], [tweaking the configuration][matklad]. But these are tricks, and tricks are fragile. When you notice a build performance regression, it could be for any number of reasons:

1. The code is genuinely larger, and takes longer to build.
1. You're using language features that slow down the frontend (e.g. complex type-level code).
1. You're using language features that slow down the backend (e.g. excessive monomorphization).
1. A proc macro is taking a very long time (tracing macros in particular are fantastically slow).
1. The crate DAG has changed shape, and crates that used to be built in parallel are now being built serially.
1. Any of the above, but in the transitive closure of your dependencies.
1. You've added/updated an immediate dependency, which pulls in lots of transitive dependencies.
1. You're caching too little, causing dependencies to be downloaded.
1. You're caching _too much_, bloating the cache, which takes longer to download.
1. The cache was recently invalidated (e.g. by updating `Cargo.lock`) and has not settled yet.
1. The CI runners are slow today, for reasons unknowable.
1. The powerset of all of the above.
1. (Insert Russell's paradox joke)

It's not worth figuring out. Just pay for the bigger CI runners. Four or eight cores should be enough. Too much parallelism is wasted: run `cargo build` with the `--timings` flag, open the report in your browser, and look at the value of "Max concurrency". This tells you how many crates can be built in parallel, and, therefore, how many cores you can buy before you hit diminishing returns.

TODO the above is not exactly true

The main thing you can do to improve performance is to split your workspace into multiple crates, and arranging the crate dependencies such that as much of your workspace can be built in parallel. This is easy to do at the start of a project, and very time-consuming after.

## Mocking {#mock}

- dynamic dispatch is more complicated in rust due to lifetimes
- this makes mocking harder, because instead of being able to pass a new instance of a class, you have to move the mocking to the type-level

[LLVM]: https://llvm.org/
[au]: https://github.com/austral/austral
[cache]: https://github.com/Swatinem/rust-cache
[chef]: https://github.com/LukeMathWalker/cargo-chef
[matklad]: https://matklad.github.io/2021/09/04/fast-rust-builds.html
