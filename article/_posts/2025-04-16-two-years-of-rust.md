---
title: Two Years of Rust
summary: Reflections on using Rust professionally for two years.
card: two-years-of-rust.webp
---

I spent the last two years writing b2b SaaS in [Rust], so now is the ideal time to reflect on the experience and write about it.

# Learning {#learning}

I didn't learn Rust the usual way: by reading tutorials, or books; or just writing tiny projects. Rather, I would say that I studied Rust academically, as part of the research that went into building [Austral][au]. I would read papers about Rust, and the specification, and sometimes I'd go on the [Rust playground][play] and write a tiny program to understand how the borrow checker works on a specific edge case.

So, when I joined, my knowledge was very lopsided: I had an enyclopedic knowledge of the minutiae of the borrow checker, and couldn't have told you how to write "Hello, world!". The largest Rust program I had written was maybe 60 lines of code and it was to empirically test how trait resolution works.

This turned out fine. Within a day or two I was committing changes. The problem is when people ask me for resources for learning Rust, I draw a blank.

# The Good {#good}

The way I would summarize Rust is: it's a better Go, or a faster Python. It's fast and statically-typed, it has SOTA tooling and a great ecosystem. It's not hard to learn. It's an industrial language, not an academic language, and you can be immensely productive with it. It's a general-purpose language, so you can build [backends][axum], [CLIs][clap], [TUIs][tui], [GUIs][gtk], and embedded firmware. The two areas where it's not yet a good fit are web frontends (though you can try) and native macOS apps.

## Performance {#perf}

Rust is fast.

But the way this shows up is not, "this specific thing is fast", but rather "everything is not uniformly slow".

The usual cope about performance is: you shouldn't worry about it, because in most programs, 99% of the runtime is spent in 1% of the code, so once you have problems, you just profile and rewrite the "hot loops". But in web applications, that's not really true. Obviously you can write accidentally quadratic loops and n+1 queries in every language, but those are discrete performance problems that can be solved in isolation.

The much more common situation, when I worked in Python web applications, is not that you have a "hot region" of the call tree, but that _everything_, every function call, is 10x to 100x slower than it could be. And so there are no obvious places to cut, there are no pressing bottlenecks. You can optimize all the bottlenecks and find that the performance bedrock is just too high. And then what can you do? You can scale the hardware vertically, and end up like those people who spend five figures a month on AWS to get four requests per second. You can keep your dependencies up to date, and hope that the community is doing the work of improving performance. And you can use async as much as possible on the belief that your code is I/O-bound, and be disappointed when it turns out that actually you're CPU-bound.

In Rust, where things are slow, it's usually because of a discrete, isolated problem (e.g. an underlying SQL query is slow) rather than a pervasive problem.

## Cargo {#cargo}

[Cargo] has the best DX of any build system+package manager I have used. Typically you praise the features of a program, with cargo you praise the absences: there's no gotchas, no footguns, no lore you have to learn in anger, no weirdness, no environment variables to configure, no virtualenvs to forget to activate. When you copy a command from the documentation and run it, it works, it doesn't spit out a useless error message that serves only as a unique identifier to find the relevant StackOverflow/Discourse thread.

Much of the DX virtues are downstream of the fact that cargo is entirely declarative rather than stateful. An example: something that always trips me up with npm is when I update the dependencies in the `package.json`, running the type-checker/build tool/whatever doesn't pick up the change. I get an unexpected error and then I go, oh, right, I have to run `npm install` first. With cargo, if you update the dependencies in the `Cargo.toml` file, any subsequent command (`cargo check` or `build` or `run`) will first resolve the dependencies, update `Cargo.lock`, download any missing dependencies, and _then_ run the command. The state of (`Cargo.toml`, `Cargo.lock`, dependency sources) is always synchronized.

## Error Handling {#error}

TODO

## Type Safety {#types}

TODO

In e.g. Python the state of nature is you have zero confidence that the code won't blow up in your face, so you spend your time writing tests (to compensate for the lack of a type system) and waiting for the tests to clear CI (because it's slow as shit). In Rust you write the code and if it compiles, it almost always works. Writing tests can feel like a chore because of how rarely they surface defects.

## Refactoring {#refactoring}

It's paint by numbers. The type errors make refactoring extremely straightforward and safe.

## The Borrow Checker {#borrow}

- fighting the borrow checker doesn't happen
- often, the borrow checker steers you towards designs with mechanical sympathy
- in rare circumstances, you get inscrutable lifetime errors, these often happen when multiple orthogonal features intersect: e.g. closures and async
- often this is a sign you have to simplify
- When you get a design that uses lifetimes to have a completely clone()-free flow, it is really satisfying. And you can expect it to be fast.
- borrowing is like referential transparency but useful

## Hiring {#hiring}

Is it hard to hire Rust programmers? No.

First, some mainstream languages like Python and TypeScript are so easy to hire for that they wrap back around and become hard. How do you find a truly talented Python programmer? You have to sift through a thousand mediocre resumes.

Secondly, there's a selection effect for quality. Programmers who are curious, driven, and passionate will learn new technologies that attract them. "Has used Rust", "has written open-source projects in Rust", or "wants to use Rust professionally" is a huge positive signal about a candidate.

Personally I've never identified as a "Python programmer" or a "Rust programmer". I'm just a programmer! When you learn enough languages you can form an orthogonal basis set of programming languages and translate concepts across them. And I think the same is true for the really talented programmers: they will just pick up the language.

# The Bad {#bad}

TODO

## The Module System {#modules}

TODO

## Typechecker Performance {#type-perf}

TODO

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

It's not worth figuring out. Just pay for the bigger CI runners. Four or eight cores should be enough. Too much parallelism is waste: run `cargo build` with the `--timings` flag, open the report in your browser, and look at the value of "Max concurrency". This tells you how many crates can be built in parallel, and, therefore, how many cores you can buy before you hit diminishing returns.

TODO the above is not exactly true

The main thing you can do to improve performance is to split your workspace into multiple crates, and arranging the crate dependencies such that as much of your workspace can be built in parallel. This is easy to do at the start of a project, and very time-consuming after.

## Expressive Power {#power}

It's easy to go insane with proc macros and trait magic and build an incomprehensible codebase where it's impossible to follow the flow of control or debug anything. You have to rein it in.

## Mocking {#mock}

TODO

- dynamic dispatch is more complicated in rust due to lifetimes
- this makes mocking harder, because instead of being able to pass a new instance of a class, you have to move the mocking to the type-level

[Cargo]: https://doc.rust-lang.org/cargo/
[LLVM]: https://llvm.org/
[Rust]: https://www.rust-lang.org/
[au]: https://github.com/austral/austral
[axum]: https://github.com/tokio-rs/axum
[cache]: https://github.com/Swatinem/rust-cache
[chef]: https://github.com/LukeMathWalker/cargo-chef
[clap]: https://docs.rs/clap/latest/clap/
[gtk]: https://gtk-rs.org/
[matklad]: https://matklad.github.io/2021/09/04/fast-rust-builds.html
[play]: https://play.rust-lang.org/
[tui]: https://github.com/ratatui/ratatui
