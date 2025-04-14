---
title: Two Years of Rust
summary: Reflections on using Rust professionally for two years.
card: two-years-of-rust.webp
---

I spent the last two years writing b2b SaaS in [Rust], so now is the ideal time to reflect on the experience and write about it.

# Contents

TODO

# Learning {#learning}

I didn't learn Rust the usual way: by reading tutorials, or books; or writing tiny projects. Rather, I would say that I studied Rust, as part of the research that went into building [Austral][au]. I would read papers about Rust, and the specification, and sometimes I'd go on the [Rust playground][play] and write a tiny program to understand how the borrow checker works on a specific edge case.

So, when I joined, my knowledge was very lopsided: I had an enyclopedic knowledge of the minutiae of the borrow checker, and couldn't have told you how to write "Hello, world!". The largest Rust program I had written was maybe 60 lines of code and it was to empirically test how trait resolution works.

This turned out fine. Within a day or two I was committing changes. The problem is when people ask me for resources for learning Rust, I draw a blank.

# The Good {#good}

The way I would summarize Rust is: it's a better Go, or a faster Python. It's fast and statically-typed, it has SOTA tooling and a great ecosystem. It's not hard to learn. It's an industrial language, not an academic language, and you can be immensely productive with it. It's a general-purpose language, so you can build [backends][axum], [CLIs][clap], [TUIs][tui], [GUIs][gtk], and embedded firmware. The two areas where it's not yet a good fit are web frontends (though you can try) and native macOS apps.

## Performance {#perf}

Rust is fast.

You can write slow code in any language: quadratic loops and n+1 queries and bad cache usage. But these are _discrete_ problems. In Rust, when you fix the bottlenecks, the program is fast.

In other languages performance problems are often _pervasive_, so e.g. in Python it's very common to have a situation where you've fixed all the bottlenecks—any everything is still unacceptably slow. Why? Because in Python the primitives are 10x to 100x slower than in Rust, and the composition of slow primitives is a slow program. No matter how much you optimize _within_ the program, the performance ceiling is set by the language itself.

And when you find yourself in that situation, what is there to do? You can scale the hardware vertically, and end up like those people who spend five figures a month on AWS to get four requests per second. You can keep your dependencies up to date, and hope that the community is doing the work of improving performance. And you can use async as much as possible on the belief that your code is I/O-bound, and be disappointed when it turns out that actually you're CPU-bound.

By having a high performance ceiling, Rust lets you write programs that are default fast without thinking too much about optimization, and when you need to improve performance, you have a low of room to optimize before you hit the performance ceiling.

## Tooling {#tooling}

[Cargo] has the best DX of any build system+package manager I have used. Typically you praise the features of a program, with cargo you praise the absences: there's no gotchas, no footguns, no lore you have to learn in anger, no weirdness, no environment variables to configure, no virtualenvs to forget to activate. When you copy a command from the documentation and run it, it works, it doesn't spit out a useless error message that serves only as a unique identifier to find the relevant StackOverflow/Discourse thread.

Much of the DX virtues are downstream of the fact that cargo is entirely declarative rather than stateful. An example: something that always trips me up with npm is when I update the dependencies in the `package.json`, running the type-checker/build tool/whatever doesn't pick up the change. I get an unexpected error and then I go, oh, right, I have to run `npm install` first. With cargo, if you update the dependencies in the `Cargo.toml` file, any subsequent command (`cargo check` or `build` or `run`) will first resolve the dependencies, update `Cargo.lock`, download any missing dependencies, and _then_ run the command. The state of (`Cargo.toml`, `Cargo.lock`, local dependency store) is always synchronized.

## Type Safety {#types}

Rust has a good type system: sum types with exhaustiveness checking, option types instead of `null`, no surprising type conversions. Again, as with tooling, what makes a type system good is a small number of features, and a thousand _abscences_, mistakes that were not made.

The practical consequence is you can have a high degree of confidence in the robustness of your code. In e.g. Python the state of nature is you have zero confidence that the code won't blow up in your face, so you spend your time writing tests (to compensate for the lack of a type system) and waiting for the tests to clear CI (because it's slow as shit). In Rust you write the code and if it compiles, it almost always works. Writing tests can feel like a chore because of how rarely they surface defects.

To given an example: I don't really know how to debug Rust programs because I never had to. The only parts of the code I had to debug were the SQL queries, because SQL [has many deficiencies][sql]. But the Rust code itself was overwhelmingly solid. When there were bugs, they were overwhelmingly conceptual bugs, i.e., misunderstanding the specification. The type of bugs that you can make in any language and that testing might miss.

## Error Handling {#error}

There's two ways to do errors: traditional exception handling (as in Java or Python) keeps the happy path free of error-handling code, but makes it hard to know, at a given point in the program, what sort of errors could happen. Errors-as-values, as in Go, makes error handling more explicit at the cost of being very verbose.

Rust has a really nice solution where errors are represented as ordinary values, but there's syntactic sugar that means you don't have to slow down to write `if err != nil` a thousand times over.

Basically, an error is any type that implements the `Error` trait. Then you have the `Result` type, which is basically:

```rust
enum Result<T, E: Error> {
    Ok(T),
    Err(E)
}
```

Fuctions which are fallible simply return a `Result`, e.g.:

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

When you need to explicitly handle an error, you simply omit the question mark operator and then you can deal with the `Result` value, instead of the underlying success value.

## The Borrow Checker {#borrow}

TODO

- fighting the borrow checker doesn't happen
- often, the borrow checker steers you towards designs with mechanical sympathy
- in rare circumstances, you get inscrutable lifetime errors, these often happen when multiple orthogonal features intersect: e.g. closures and async
- often this is a sign you have to simplify
- When you get a design that uses lifetimes to have a completely clone()-free flow, it is really satisfying. And you can expect it to be fast.
- borrowing is like referential transparency but useful
- i didn't find myself fighting the borrow checker too much
  - probably because I learnt Rust in an unusual way
- a lot of people's mental model of the borrow checker is something that is "on top" of Rust, in the same way you might add a static analyzer on top of C/C++
  - that mindset leads to fighting the system
  - because you think, my code is legitimate, it type-checks, all the types make sense, it's only this final layer, the borrow checker, that objects
  - it's better to think of the borrow checker as an intrinsic part of the language semantics
  - and think, not, how can I translate the C++ in my head to Rust, but, rather, how can I accomplish the goal within the semantics of Rust?
  - but that's hard, because it requires a high level of fluency

## Refactoring {#refactoring}

It's paint by numbers. The type errors make refactoring extremely straightforward and safe.

## Hiring {#hiring}

Is it hard to hire Rust programmers? No.

First, some mainstream languages like Python and TypeScript are so easy to hire for that they wrap back around and become hard. How do you find a truly talented Python programmer? You have to sift through a thousand mediocre resumes.

Secondly, there's a selection effect for quality. Programmers who are curious, driven, and passionate will learn new technologies that attract them. "Has used Rust", "has written open-source projects in Rust", or "wants to use Rust professionally" is a huge positive signal about a candidate.

Personally I've never identified as a "Python programmer" or a "Rust programmer". I'm just a programmer! When you learn enough languages you can form an orthogonal basis set of programming languages and translate concepts across them. And I think the same is true for the really talented programmers: they will just pick up the language.

## Affect {#affect}

Enough about tech. Let's talk about feelings.

When I worked with Python+Django the characteristic feeling is _anxiety_. Writing Python feels like building a castle out of twigs, and the higher you go, the stronger the wind gets. You expect things to go wrong, you expect the code to be slow, you expect that when you run a script you wait for a few hundred milliseconds before anything happens. In JavaScript the characteristic feeling is _disappointment_: at least with Python the twigs are _rigid_. Writing JavaScript feels like trying to shape mud into a house, you constantly have to go back and forth re-shaping the parts that are melting in the Sun, and when you suggest using non-liquid building material you're told you're too ivory tower. Nobody knows what a "brick" is because they don't give a shit, they don't see anything wrong with how they build.

Rust feels good. You can build with confidence. You can build things that not only work as desired and don't fail overnight but which are also _beautiful_. You can be proud of the work that you do, because it's not slop.

# The Bad {#bad}

This section describes the things I don't like.

## The Module System {#modules}

In Rust, there's two levels of code organization:

- **Modules** are namespaces with visibility rules.
- **Crates** are a collection of modules, and they can have dependencies on other crates. Crates can be either executables or libraries.

A project, or workspace, can be made up of multiple crates. For example a web application could have library crates for each orthogonal feature and an executable crate that takes care of actually starting the server.

What surprised me was learning that modules are not compilation units, and I learnt this by accident when I noticed you can have circular dependencies between modules within a crate[^reg]. Instead, crates are the compilation unit. When you change any module in a crate, the _entire_ crate has to be recompiled. This means that compiling large crates is slow, and large projects should be broken down into many small crates, with their dependency DAG arranged to maximize parallel compilation.

This is a problem because creating a module is cheap, but creating a crate is slow. Creating a new module is just creating a new file and adding an entry for it in the sibling `mod.rs` file. Creating a new create requires running `cargo new`, and don't forget to set `publish = false` in the `Cargo.toml`, and adding the name of that crate in the workspace-wide `Cargo.toml` so you can import it from other crates. Importing a symbol within a crate is easy: you start typing the name, and the LSP can auto-insert the `use` declaration, but this doesn't work across crates, you have to manually open the `Cargo.toml` file for the crate you're working on and manually add a dependency to the crate you want to import code from. This is very time-consuming.

Another problem with crate-splitting is that `rustc` has a really nice feature that warns you when code is unused. It's very thorough and I like it because it helps to keep the codebase tidy. But it only works within a crate. In a multi-crate workspace, declarations that are exported publicly in a crate, but not imported by any other sibling crates, are not reported as unused.[^mach]

So if you want builds to be fast, you have to completely re-arrange your architecture and manually massage the dependency DAG and also do all this make-work around creating crates. And for that you gain... intra-crate circular imports, which are a horrible antipattern and make it much harder to understand any codebase. I would much prefer if modules were disjoint compilation units.

I also think the module system is just a hair too complex, with re-exports and way too many ways to import symbols. It could be stripped down a lot.

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

The main thing you can do to improve performance is to split your workspace into multiple crates, and arranging the crate dependencies such that as much of your workspace can be built in parallel. This is easy to do at the start of a project, and very time-consuming after.

## Mocking {#mock}

Maybe this is a skill issue, but I have not found a good way to write code where components have swappable dependencies, so that components can be tested independently of their dependencies. The central issue is that lifetimes impinge on late binding.

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

(We've parameterized the type of database transactions because the mock won't use transactions.)

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

While in the unit tests we would instead create a `InsertUserMock` and pass it in.

Obviously this is a lot of typing. Using traits and dynamic dispatch would probably make the code marginally shorter. Using closures is probably the simplest approach (a function type with type parameters is, in a sense, a trait with a single method), but then you run into the ergonomics issues of closures and lifetimes.

Again, this might be a skill issue, and maybe there's an elegant and idiomatic way to do this.

Alternatively, you might deny the entire necessity of mocking, and write code without swappable implementations, but that has its own problems: tests become slower, because you have to spin up servers to mock things like API calls; tests require a lot of code to set up and tear down these dependencies; tests are necessarily end-to-end, and the more end-to-end your tests, the more test cases you need to check every path because of the combinatorial explosion of inputs.

## Expressive Power {#power}

It's easy to go insane with proc macros and trait magic and build an incomprehensible codebase where it's impossible to follow the flow of control or debug anything. You have to rein it in.

# Footnotes

[^reg]: If modules were separate compilation units this wouldn't work. If module A depends on B, to compile A you need to first compile B to know what declarations it exports and what their types are. But if B also depends on A, you have an infinite regression.

[^mach]: One way to fix this is to make extremely fine-grained crates, and rely on `cargo-machete` to identify unused code at the dependency level. But this would take up way too much time.

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
[sql]: /article/composable-sql
[tui]: https://github.com/ratatui/ratatui
