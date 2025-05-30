---
title: Two Years of Rust
summary: Reflections on using Rust professionally for two years.
card: two-years-of-rust.webp
---

I recently wrapped up a job where I spent the last two years writing the backend of a B2B SaaS product in [Rust], so now is the ideal time to reflect on the experience and write about it.

# Contents
{: .no_toc }

1. toc
{:toc}

# Learning {#learning}

I didn't learn Rust the usual way: by reading tutorials, or books; or writing tiny projects. Rather, I would say that I studied Rust, as part of the research that went into building [Austral][au]. I would read papers about Rust, and the specification, and sometimes I'd go on the [Rust playground][play] and write a tiny program to understand how the borrow checker works on a specific edge case.

So, when I started working in Rust, my knowledge was very lopsided: I had an encyclopedic knowledge of the minutiae of the borrow checker, and couldn't have told you how to write "Hello, world!". The largest Rust program I had written was maybe 60 lines of code and it was to empirically test how trait resolution works.

This turned out fine. Within a day or two I was committing changes. The problem is when people ask me for resources to learn Rust, I draw a blank.

# The Good {#good}

The way I would summarize Rust is: it's a better Go, or a faster Python. It's fast and statically-typed, it has SOTA tooling, and a great ecosystem. It's not hard to learn. It's an industrial language, not an academic language, and you can be immensely productive with it. It's a general-purpose language, so you can build [backends][axum], [CLIs][clap], [TUIs][tui], [GUIs][gtk], and embedded firmware. The two areas where it's not yet a good fit are web frontends (though you can try) and native macOS apps.

## Performance {#perf}

Rust is fast.

You can write slow code in any language: quadratic loops and n+1 queries and bad cache usage. But these are _discrete_ bottlenecks. In Rust, when you fix the bottlenecks, the program is fast.

In other languages performance problems are often _pervasive_, so e.g. in Python it's very common to have a situation where you've fixed all the bottlenecks---and everything is still unacceptably slow. Why? Because in Python the primitives are 10x to 100x slower than in Rust, and the composition of slow primitives is a slow program. No matter how much you optimize _within_ the program, the performance ceiling is set by the language itself.

And when you find yourself in that situation, what is there to do? You can scale the hardware vertically, and end up like those people who spend five figures a month on AWS to get four requests per second. You can keep your dependencies up to date, and hope that the community is doing the work of improving performance. And you can use async as much as possible on the belief that your code is I/O-bound, and be disappointed when it turns out that actually you're CPU-bound.

By having a high performance ceiling, Rust lets you write programs that are default fast without thinking too much about optimization, and when you need to improve performance, you have a lot of room to optimize before you hit the performance ceiling.

## Tooling {#tooling}

[Cargo] has the best DX of any build system+package manager I have used. Typically you praise the features of a program, with cargo you praise the absences: there's no gotchas, no footguns, no lore you have to learn in anger, no weirdness, no environment variables to configure, no virtualenvs to forget to activate. When you copy a command from the documentation and run it, it works, it doesn't spit out a useless error message that serves only as a unique identifier to find the relevant StackOverflow/Discourse thread.

Much of the DX virtues are downstream of the fact that cargo is entirely declarative rather than stateful. An example: something that always trips me up with npm is when I update the dependencies in the `package.json`, running the type-checker/build tool/whatever doesn't pick up the change. I get an unexpected error and then I go, oh, right, I have to run `npm install` first. With cargo, if you update the dependencies in the `Cargo.toml` file, any subsequent command (`cargo check` or `build` or `run`) will first resolve the dependencies, update `Cargo.lock`, download any missing dependencies, and _then_ run the command. The state of (`Cargo.toml`, `Cargo.lock`, local dependency store) is always synchronized.

## Type Safety {#types}

Rust has a good type system: sum types with exhaustiveness checking, option types instead of `null`, no surprising type conversions. Again, as with tooling, what makes a type system good is a small number of features, and a thousand absences, mistakes that were not made.

The practical consequence is you have a high degree of confidence in the robustness of your code. In e.g. Python the state of nature is you have zero confidence that the code won't blow up in your face, so you spend your time writing tests (to compensate for the lack of a type system) and waiting for the tests to clear CI (because Python is slow as shit). In Rust you write the code and if it compiles, it almost always works. Writing tests can feel like a chore because of how rarely they surface defects.

To give an example: I don't really know how to debug Rust programs because I never had to. The only parts of the code I had to debug were the SQL queries, because SQL [has many deficiencies][sql]. But the Rust code itself was overwhelmingly solid. When there were bugs, they were usually conceptual bugs, i.e., misunderstanding the specification. The type of bugs that you can make in any language and that testing would miss.

## Error Handling {#error}

There's two ways to do errors: traditional exception handling (as in Java or Python) keeps the happy path free of error-handling code, but makes it hard to know the set of errors that can be raised at a given program point. Errors-as-values, as in Go, makes error handling more explicit at the cost of being very verbose.

Rust has a really nice solution where errors are represented as ordinary values, but there's syntactic sugar that means you don't have to slow down to write `if err != nil` a thousand times over.

In Rust, an error is any type that implements the `Error` trait. Then you have the `Result` type:

```rust
enum Result<T, E: Error> {
    Ok(T),
    Err(E)
}
```

Functions which are fallible simply return a `Result`, e.g.:

```rust
enum DbError {
    InvalidPath,
    Timeout,
    // ...
}

fn open_database(path: String) -> Result<Database, DbError>
```

The question mark operator, `?`, makes it possible to write terse code that deals with errors. Code like this:

```rust
fn foo() -> Result<(), DbError> {
    let db = open_database(path)?;
    let tx = begin(db)?;
    let data = query(tx, "...")?;
    rollback(tx)?;
    Ok(())
}
```

Is transformed to the much more verbose:

```rust
fn foo() -> Result<(), DbError> {
    let db = match open_database(path) {
        Ok(db) => db,
        Err(e) => {
            // Rethrow.
            return Err(e);
        }
    };
    let tx = match begin(db) {
        Ok(tx) => tx,
        Err(e) => {
            return Err(e);
        }
    };
    let data = match query(tx, "...") {
        Ok(data) => data,
        Err(e) => {
            return Err(e);
        }
    };
    match rollback(tx) {
        Ok(_) => (),
        Err(e) => {
            return Err(e);
        }
    };
    Ok(())
}
```

When you need to explicitly handle an error, you omit the question mark operator and use the `Result` value directly.

## The Borrow Checker {#borrow}

The borrow checker is Rust's headline feature: it's how you can have memory safety without garbage collection, it's the thing that enables "fearless concurrency". It's also, for most people, the most frustrating part of learning and using Rust.

Personally I didn't have borrow checker problems, but that's because before I started using Rust at work I'd [designed and built my own borrow checker][check]. I don't know if that's a scalable pedagogy. Many people report they have to go through a lengthy period of fighting the borrow checker, and slowly their brain discovers the implicit ruleset, and eventually they reach a point where they can write code without triggering inscrutable borrow checker errors. But that means a lot of people drop out of learning Rust because they don't like fighting the borrow checker.

So, how do you learn Rust more effectively, without building your own compiler, or banging your head against the borrow checker?

Firstly, it's useful to understand the concepts behind the borrow checker, the "aliased XOR mutable" rule, the motivation behind linear types, etc. Unfortunately I don't have a canonical resource that explains it _ab initio_.

Secondly, a change in mindset is useful: a lot of people's mental model of the borrow checker is as something bolted "on top" of Rust, like a static analyzer you can run on a C/C++ codebase, which just happens to be built into the compiler. This mindset leads to fighting the system, because you think: my code is legitimate, it type-checks, all the types are there, it's only this final layer, the borrow checker, that objects. It's better to think of the borrow checker as an intrinsic part of the language semantics. Borrow checking happens, necessarily, after type-checking (because it needs to know the types of terms), but a program that fails the borrow checker is as invalid as a program that doesn't type-check. Rather than mentally implementing something in C/C++, and then thinking, "how do I translate this to Rust in a way that satisfies the borrow-checker?", it's better to think, "how can I accomplish the goal within the semantics of Rust, thinking in terms of linearity and lifetimes?". But that's hard, because it requires a high level of fluency.

When you are comfortable with the borrow checker, life is pretty good. "Fighting the borrow checker" isn't something that happens. When the borrow checker complains it's either because you're doing something where multiple orthogonal features impinge on each other (e.g. async + closures + borrowing) or because you're doing something that's too complex, and the errors are a signal you have to simplify. Often, the borrow checker steers you towards designs that have mechanical sympathy, that are aligned with how the hardware works. When you converge on a design that leverages lifetimes to have a completely `clone()`-free flow of data, it is really satisfying. When you design a linearly-typed API where the linearity makes it really hard to misuse, you're grateful for the borrow checker.

## Async {#async}

Everyone complains about async. They complain that it's too complex or they invoke that thought-terminating cliche about "coloured functions". It's easy to complain about something when comparing it to some vague, abstract, ideal state of affairs; but what, exactly, is the concrete and existing alternative to async?

The binding constraint is that OS threads are slow. Not accidentally but intrinsically, because of the kernel, and having to swap the CPU state and stack on each context switch. OS threads are never going to be fast. If you want to build high-performance network services, it matters a lot how many concurrent connections and how much throughput you can get per CPU.  So you need an alternative way to do concurrency that lets you maximize your hardware resources.

And there are basically two alternatives.

1. Green threads, which give programmers the same semantics as OS threads (good!) but often leave a lot of performance on the table (bad!) because you need to allocate memory for each thread's stack and you need a runtime scheduler to do preemptive multitasking.
2. Stackless coroutines, as in Rust, which add complexity to the language semantics and implementation (bad!) but have a high performance ceiling (good!).

From the perspective of a language implementor, or someone who cares about specifying the semantics of programming languages, async is not a trivial feature. The intersection of async and lifetimes is hard to understand. From the perspective of a library implementor, someone who writes the building blocks of services and is down in the trenches with `Pin`/`Poll`/`Future`, it's rough.

But from the perspective of a user, async Rust is pretty good. It mostly "just works". The user perspective is you put `async` in front of function definitions that perform IO and you put `await` at the call sites and that's it. The only major area where things are unergonomic is calling async functions inside iterators.

## Refactoring {#refactoring}

It's paint by numbers. The type errors make refactoring extremely straightforward and safe.

## Hiring {#hiring}

Is it hard to hire Rust programmers? No.

First, mainstream languages like Python and TypeScript are so easy to hire for that they wrap back around and become hard. To find a truly talented Python programmer you have to sift through a thousand resumes.

Secondly, there's a selection effect for quality. "Has used Rust", "has written open-source code in Rust", or "wants to use Rust professionally" are huge positive signals about a candidate because it says they are curious and they care about improving their skills.

Personally I've never identified as a "Python programmer" or a "Rust programmer". I'm just a programmer! When you learn enough languages you can form an orthogonal basis set of programming concept and translate them across languages. And I think the same is true for the really talented programmers: they are able to learn the language quickly.

## Affect {#affect}

Enough about tech. Let's talk about feelings.

When I worked with Python+Django the characteristic feeling was _anxiety_. Writing Python feels like building a castle out of twigs, and the higher you go, the stronger the wind gets. I expected things to go wrong, I expected the code to be slow, I expected to watch things blow up for the most absurd reasons. I had to write the code defensively, putting type assertions everywhere.

Rust feels good. You can build with confidence. You can build things that not only work as desired but which are also _beautiful_. You can be proud of the work that you do, because it's not slop.

# The Bad {#bad}

This section describes the things I don't like.

## The Module System {#modules}

In Rust, there's two levels of code organization:

- **Modules** are namespaces with visibility rules.
- **Crates** are a collection of modules, and they can depend on other crates. Crates can be either executables or libraries.

A project, or workspace, can be made up of multiple crates. For example a web application could have library crates for each orthogonal feature and an executable crate that ties them together and starts the server.

What surprised me was learning that modules are not compilation units, and I learnt this by accident when I noticed you can have a circular dependency between modules within the same crate[^reg]. Instead, crates are the compilation unit. When you change any module in a crate, the _entire_ crate has to be recompiled. This means that compiling large crates is slow, and large projects should be broken down into many small crates, with their dependency DAG arranged to maximize parallel compilation.

This is a problem because creating a module is cheap, but creating a crate is slow. Creating a new module is just creating a new file and adding an entry for it in the sibling `mod.rs` file. Creating a new crate requires running `cargo new`, and don't forget to set `publish = false` in the `Cargo.toml`, and adding the name of that crate in the workspace-wide `Cargo.toml` so you can import it from other crates. Importing a symbol within a crate is easy: you start typing the name, and the LSP can auto-insert the `use` declaration, but this doesn't work across crates, you have to manually open the `Cargo.toml` file for the crate you're working on and manually add a dependency to the crate you want to import code from. This is very time-consuming.

Another problem with crate-splitting is that `rustc` has a really nice feature that warns you when code is unused. It's very thorough and I like it because it helps to keep the codebase tidy. But it only works within a crate. In a multi-crate workspace, declarations that are exported publicly in a crate, but not imported by any other sibling crates, are not reported as unused.[^mach]

So if you want builds to be fast, you have to completely re-arrange your architecture and manually massage the dependency DAG and also do all this make-work around creating and updating crate metadata. And for that you gain... intra-crate circular imports, which are a horrible antipattern and make it much harder to understand the codebase. I would much prefer if modules were disjoint compilation units.

I also think the module system is just a hair too complex, with re-exports and way too many ways to import symbols. It could be stripped down a lot.

## Build Performance {#build-perf}

The worst thing about the Rust experience is the build times. This is usually blamed on [LLVM], which, fair enough, but I think part of it is just intrinsic features of the language, like the fact that modules are not independent compilation units, and of course monomorphization.

There are various tricks to speed up the builds: [caching][cache], [cargo chef][chef], [tweaking the configuration][matklad]. But these are tricks, and tricks are fragile. When you notice a build performance regression, it could be for any number of reasons:

1. The code is genuinely larger, and takes longer to build.
1. You're using language features that slow down the frontend (e.g. complex type-level code).
1. You're using language features that slow down the backend (e.g. excessive monomorphization).
1. A proc macro is taking a very long time ([`tracing::instrument`][inst] in particular is fantastically slow).
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

The main thing you can do to improve build performance is to split your workspace into multiple crates, and arranging the crate dependencies such that as much of your workspace can be built in parallel. This is easy to do at the start of a project, and very time-consuming after.

## Mocking {#mock}

Maybe this is a skill issue, but I have not found a good way to write code where components have swappable dependencies and can be tested independently of their dependencies. The central issue is that lifetimes impinge on late binding.

Consider a workflow for creating a new user in a web application. The three external effects are: creating a record for the user in the database, sending them a verification email, and logging the event in an audit log:

```rust
fn create_user(
    tx: &Transaction,
    email: Email,
    password: Password
) -> Result<(), CustomError>  {
    insert_user_record(tx, &email, &password)?;
    send_verification_email(&email)?;
    log_user_created_event(tx, &email)?;
    Ok(())
}
```

Testing this function requires spinning up a database and an email server. No good! We want to detach the workflow from its dependencies, so we can test it without transitively testing its dependencies. There's three ways to do this:

1. Use traits to define the interface, and pass things at compile-time.
1. Use traits to define the interface, and use dynamic dispatch to pass things at run-time.
1. Use function types to define the interface, and pass dependencies as closures.

And all of these approaches work. But they require a lot of make-work. In TypeScript or Java or Python it would be painless, because those languages don't have lifetimes, and so dynamic dispatch or closures "just work".

For example, say we're using traits and doing everything at compile-time. To minimize the work let's just focus on the dependency that writes the user's email and password to the database. We can define a trait for it:

```rust
trait InsertUser<T> {
    fn execute(
        &mut self,
        tx: &T,
        email: &Email,
        password: &Password
    ) -> Result<(), CustomError>;
}
```

(We've parameterized the type of database transactions because the mock won't use a real database, therefore, we won't have a way to construct a `Transaction` type in the tests.)

The real implementation requires defining a placeholder type, and implementing the `InsertUser` trait for it:

```rust
struct InsertUserAdapter {}

impl InsertUser<Transaction> for InsertUserAdapter {
    fn execute(
        &mut self,
        tx: &Transaction,
        email: &Email,
        password: &Password
    ) -> Result<(), CustomError> {
        insert_user_record(tx, email, password)?;
        Ok(())
    }
}
```

The mock implementation uses the unit type `()` as the type of transactions:

```rust
struct InsertUserMock {
    email: Email,
    password: Password,
}

impl InsertUser<()> for InsertUserMock {
    fn execute(
        &mut self,
        tx: &(),
        email: &Email,
        password: &Password
    ) -> Result<(), CustomError> {
        // Store the email and password in the mock object, so
        // we can afterwards assert the right values were passed
        // in.
        self.email = email.clone();
        self.password = password.clone();
        Ok(())
    }
}
```

Finally we can define the `create_user` workflow like this:

```rust
fn create_user<T, I: InsertUser<T>>(
    tx: &T,
    insert_user: &mut I,
    email: Email,
    password: Password,
) -> Result<(), CustomError> {
    insert_user.execute(tx, &email, &password)?;
    // Todo: the rest of the dependencies.
    Ok(())
}
```

The live, production implementation would look like this:

```rust
fn create_user_for_real(
    tx: &Transaction,
    email: Email,
    password: Password,
) -> Result<(), CustomError> {
    let mut insert_user = InsertUserAdapter {};
    create_user(tx, &mut insert_user, email, password)?;
    Ok(())
}
```

While in the unit tests we would instead create a `InsertUserMock` and pass it in:

```rust
#[test]
fn test_create_user() -> Result<(), CustomError> {
    let mut insert_user = InsertUserMock {
        email: "".to_string(),
        password: "".to_string()
    };
    let email = "foo@example.com".to_string();;
    let password = "hunter2".to_string();

    create_user(&(), &mut insert_user, email, password)?;

    // Assert `insert_user` was called with the right values.
    assert_eq!(insert_user.email, "foo@example.com");
    assert_eq!(insert_user.password, "hunter2");

    Ok(())
}
```

Obviously this is a lot of typing. Using traits and dynamic dispatch would probably make the code marginally shorter. Using closures is probably the simplest approach (a function type with type parameters is, in a sense, a trait with a single method), but then you run into the ergonomics issues of closures and lifetimes.

Again, this might be a skill issue, and maybe there's an elegant and idiomatic way to do this.

Alternatively, you might deny the entire necessity of mocking, and write code without swappable implementations, but that has its own problems: tests become slower, because you have to spin up servers to mock things like API calls; tests require a lot of code to set up and tear down these dependencies; tests are necessarily end-to-end, and the more end-to-end your tests, the more test cases you need to check every path because of the combinatorial explosion of inputs.

## Expressive Power {#power}

It's easy to go insane with proc macros and trait magic and build an incomprehensible codebase where it's impossible to follow the flow of control or debug anything. You have to rein it in.

# Footnotes
{: .no_toc }

[^reg]: If modules were separate compilation units this wouldn't work. If module A depends on B, to compile A you need to first compile B to know what declarations it exports and what their types are. But if B also depends on A, you have an infinite regression.

[^mach]: One way to fix this is to make extremely fine-grained crates, and rely on `cargo-machete` to identify unused code at the dependency level. But this would take up way too much time.

[Cargo]: https://doc.rust-lang.org/cargo/
[LLVM]: https://llvm.org/
[Rust]: https://www.rust-lang.org/
[au]: https://github.com/austral/austral
[axum]: https://github.com/tokio-rs/axum
[cache]: https://github.com/Swatinem/rust-cache
[check]: /article/how-australs-linear-type-checker-works
[chef]: https://github.com/LukeMathWalker/cargo-chef
[clap]: https://docs.rs/clap/latest/clap/
[gtk]: https://gtk-rs.org/
[inst]: https://docs.rs/tracing/latest/tracing/attr.instrument.html
[matklad]: https://matklad.github.io/2021/09/04/fast-rust-builds.html
[play]: https://play.rust-lang.org/
[sql]: /article/composable-sql
[tui]: https://github.com/ratatui/ratatui
