---
title: 'Introducing Austral: A Systems Language with Linear Types and Capabilities'
summary: Introducing a new programming language.
tags: []
card: introducing-austral.jpg
---

[**Austral**][austral] is a new systems programming language. You can think of
it as Rust: The Good Parts or a modernized, stripped-down Ada. It features a
strong static type system, linear types, capability-based security, and strong
modularity.

This article is an introduction to the language. The first few sections are
high-level: they are about the design and the mindset of the language. The next
two sections, about linear types and capability-based security, are much more
detailed and technical: they are meant to prove to the reader that the claims
being made about security and correctness are true.

# Contents

1. [Design Goals](#goals)
2. [Anti-Features](#anti-features)
3. [Features](#features)
4. [Language Overview](#lang-overview)
5. [Linear Types](#linear)
   1. [Motivation](#linear-motivation)
   2. [What Linear Types Are](#linear-what)
   3. [Universes](#linear-universes)
   4. [The Use-Once Rule](#linear-once)
   5. [Linear Types and Safety](#linear-safety)
   6. [A Safe Database API](#linear-db)
   7. [Borrowing](#linear-borrowing)
6. [Capability-Based Security](#cap)
   1. [Linear Capabilities](#cap-linear)
   2. [A Capability-Secure Filesystem API](#cap-fs)
   3. [The Root Capability](#cap-root)
7. [Status and Future Work](#status)
8. [Getting Involved](#involved)
9. [Conclusion](#conclusion)

# Design Goals {#goals}

There is a [section][goals] in the rationale that explains the design goals for
Austral, but it boils down to two things:

1. Simplicity
2. Strictness

_Simplicity_ means different things to different people. Some use it to mean
familiarity, or ease of use, or even terseness. Simplicity here has a
straightforward definition: it is the amount of information it takes to describe
a system.

Complex systems, with lots of moving parts that impinge on one another, cannot
be described briefly. Rube Goldberg machines, biology, and C++ are complex
systems. Python is a complex system, despite being "easy" to
use[^python]. Simple systems have short descriptions.

Simplicity is an overriding goal because programming languages _are not
tools_. A programming language is the toolbox, plus the building material, plus
the laws of physics of the universe where the product is being built. You can,
sometimes, in rare situations, hide a complex system under a simple
interface. But not programming languages, because complex programming languages
are like a universe where the gravitational constant changes daily.

There's this famous [quiz][quiz] of the C language, where you have all these
strange-looking programs and have to decide what they output. And people who
have been working with the language for years struggle to answer correctly
because the questions refer to subtle and obscure features of the C
specification.

If you think figuring out what the program does is a fun puzzle, Austral is not
for you. [Language lawyering][lawyering] is a design flaw: if two people can
look at the same basic program and disagree about its behaviour, that's a
problem.

Austral is simple. Short spec, thin runtime, small compiler.

To give a concrete example: the linear type system was designed with brutal
simplicity in mind. Consequently, Austral's equivalent of a borrow checker is
[less than 600 lines of code][linearitycheck], including the implementation of
borrowing and other ergonomic features.

The goal here is that the entire programming language should fit in your head,
that you should be able to read the [specification][spec] from beginning to end
and know all there is to know about the language.

_Strictness_ is half language features, and half a change in mindset.

If planes were flown like we write code, we'd have daily crashes, of course, but
beyond that, the response to every plane crash would be: "only a bad pilot
blames their plane! If they'd read subparagraph 71 of section 7.1.5.5 of the
C++, er, 737 spec, they'd know that at 13:51 PM on the vernal equinox the wings
fall off the plane."

This doesn't happen in aviation, because in aviation we have decided, correctly,
that _human error is an intrinsic and inseparable part of human activity_. And
so we have built concentric layers of mechanical checks and balances around
pilots, to take on part of the load of flying. Because humans are tired, they
are burned out, they have limited focus, limited working memory, they are
traumatized by writing executable YAML, _etc_.

Mechanical processes---such as type systems, type checking, formal verification,
design by contract, static assertion checking, dynamic assertion checking---are
independent of the skill of the programmer. Mechanical processes _scale_, unlike
berating people to simply write fewer bugs.

Strictness is rarely one big language feature, rather, it's about learning from
the design flaws in other languages, the "death by a thousand cuts"[^cuts], and
preventing the causes of each of them. This can be hard because programmers get
very attached to the flaws.

An example: there is a feature of C syntax where, for terseness, you can write
`if` statements without braces. This introduces a syntactic ambiguity: it's
called the ["dangling else"][else] problem. The fact that there's a Wikipedia
article about it should suggest that this is bad. This isn't some abstract
academic concern: it has caused [real-world security vulnerabilities][gotofail].

Now, if you suggest that this is a flaw, some programmers will invoke the old
[thought-terminating cliche][cliche]: "only a bad craftsman blames his
tools!". But the tradeoff here is obvious: you save a few bytes and a few
milliseconds of typing, but you roll the dice and possibly introduce a CVE that
causes billions of dollars of harm. It's self-evidently a design flaw, but if
you suggest to programmers that they should add the braces, they will kick and
scream as if you're taking away some fundamental freedom.

Austral's syntax was designed with [language security][langsec] principles in
mind: it is context-free, it can be parsed from a grammar, there's no ["lexer
hack"][lexerhack], there are no [strange, ad-hoc ambiguity-resolution
mechanisms][vexing]. The pragmatics of the syntax are designed to minimize
confusion and ambiguity.

For another example, consider the problem of operator precedence: anyone can
remember [PEMDAS][pemdas], but programming languages have many categories of
binary operators---arithmetic, comparison, bitwise, Boolean---and mixing them
together creates room for error (what does `x ^ y && z / w` evaluate to?). So in
Austral there is simply no operator precedence: any binary expression deeper
than one level is fully parenthesized. You have to type more, but we are not
typists, we are programmers, and our task is to communicate _to others_ what we
want computers to do. When in doubt: simplify by paring down the language.

This isn't for everyone. But it is for me, because after ten years in the
industry, the last thing I want from a programming language is "power". What I
want are fewer nightmares. The "liberties" that programming languages provide
feel like expressive power _until_ your codebase becomes a mental health
superfund site.

# Anti-Features {#anti-features}

Before going over the language features, I'd like to list the
_anti-features_. Here are the things Austral proudly doesn't have:

1. There are no pervasive `NULL`s, and therefore no null pointer dereference
   errors. You have to use an explicit `Option` type.

2. There is no garbage collection, so the runtime can be thin and performance is
   predictable.

4. There are no exceptions and no stack unwinding and no destructors.

5. There is no surprise control flow: you have conditionals, loops, and function
   calls. And nothing else.

6. There are no implicit type conversions anywhere.

7. More generally: _there are no implicit function calls_. If it's not in the
   source code, it's not happening, and you're not paying the cost of it.

8. There is no global state.

9. There is no runtime reflection.

10. There are no macros.

11. There are no Java or Python-style `@Annotations`.

12. There is no type inference: type information flows in only one direction,
    and function parameters, local variables, _etc._ have to have their types
    annotated.

13. There is no first-class async[^async].

14. Function overloading is very restricted through typeclasses (think [C++
    concepts][concepts]). And the basic arithmetic operators cannot be
    overloaded[^overload].

15. There is no syntactic ambiguity: no [dangling else][else] (and, therefore,
    no [`gotofail`][gotofail]), no arithmetic precedence, no syntactic
    precedence rules of any kind.

16. There is no syntactic extension: you can't, for example, introduce new infix
    operators.

# Features {#features}

What Austral _does_ have:

1. A strong, static type system that's not too big-brained.

2. A type system which allows resources to be handled correctly and safely
   without runtime overhead. "Resource" here means memory and anything that has
   an explicit lifecycle of create-use-destroy: file handles, sockets, database
   handles.

3. [Capability-based security][cap], which prevents [supply chain
   attacks][supplychain]. Your left-pad dependency can't be compromised to
   upload the contents of your disk to a remote server. This is because code
   that performs network access has to be explicitly given a network capability,
   code that performs filesystem access needs a filesystem capability, etc. A
   string-padding function that claims to need network access instantly stands
   out. Capabilities are unforgeable, secure authorization tokens for code.

4. A strong, Ada-inspired module system which is not tied to filesystem
   structure and which separates module interfaces from implementations.

5. [Sum types][sum] with pattern matching and exhaustiveness checking.

6. [Type classes][typeclass], as in Haskell, for restricted function
   overloading. As in Haskell, type parameters can be constrained to only accept
   types that implement a given typeclass.

7. A strict, context-free, unambiguous syntax, informed by [langsec][langsec]
   ideas.

# Language Overview {#lang-overview}

The largest unit of code organization is the module. Modules have explicit names
and are decoupled from the filesystem, like Haskell and unlike Python. Modules
are divided into an interface and an implementation, like Ada or OCaml.

This is _not_, as in C or C++, a hack to enable separate compilation. It's about
readability and separation of concerns. The interface file contains declarations
that are public, but no code. The implementation file contains the
implementations of what is in the interface file, as well as private
declarations.

There are five kinds of declaration:

1. Constants.
2. Types.
3. Functions.
4. Typeclasses.
5. Typeclass instances.

Each of these can either be public (by appearing in the interface file) or
private, which determines whether they are importable by other modules. Types
have an additional visibility state: _opaque_, which means they can be imported
by other modules, but they cannot be constructed or their contents accessed
outside the module, except through the module's public API. Opaque types are the
obvious choice for data structures whose internals are hidden.

Functions work like you expect: they take values and return them. Instead of
`void` there is a `Unit` type with a constant called `nil`.

Typeclasses define an interface that types can conform to, and instances define
how a particular type implements a particular typeclass.

Types and functions can be generic. The way generics work is slightly different
than in most languages, due to the linearity system.

# Linear Types {#linear}

It is difficult to advertise a language as being "simple" and then start talking
about "linear types" and "type universes", but it is only the words that are
new. The concepts are simple. Austral's entire linear type system [fits in a
page][spec-linearity]. So, this isn't some abstract ivory tower feature that you
need a PhD in category theory to understand.

Linear types let us have manual memory management, without runtime overhead, and
without security vulnerabilities: they prevent memory leaks, use-after-free, and
double-free errors.

This extends beyond memory to anything that has a lifecycle, where we have to
create it, use it, and destroy it in a certain order. File handles, network
sockets, database handles, locks and mutexes: the correct usage of these objects
can be enforced at compile time.

First, I'll explain the motivation: why do we need linear types? Then I'll
explain what they are, and how they provide safety.

## Motivation {#linear-motivation}

Consider a file handling API:

```
type File
File openFile(String path)
File writeString(File file, String content)
void closeFile(File file)
```

An experienced programmer understands the _implicit lifecycle_ of the `File`
object:

1. We create a `File` handle by calling `openFile`.
2. We write to the handle zero or more times.
3. We close the file handle by calling `closeFile`.

We can depict this graphically like this:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'.](/assets/content/introducing-austral/file-api.svg)

But, crucially: this lifecycle is _not enforced by the compiler_. There are a
number of erroneous transitions that we don't consider, but which are
technically possible:

![The graph from the previous figure, with a new node labeled 'leak', and with four new arrows in red: one from 'closeFile' to itself labeled 'double close', one from 'closeFile' to 'writeString' labeled 'use after close', one from 'openFile' to 'leak' labeled 'forgot to close', and one from 'writeString' to 'leak' also labeled 'forgot to close'.](/assets/content/introducing-austral/file-api-errors.svg)

These fall into two categories:

1. **Leaks:** we can forget to call `closeFile`, e.g.:

    ```
    let file = openFile("hello.txt");
    writeString(file, "Hello, world!");
    // Forgot to close
    ```

2. **Use-After-Close:** and we can call `writeString` on a `File` object that
   has already been closed:

   ```
   closeFile(file);
   writeString(file, "Goodbye, world!");
   ```

   And we can close the file handle after it has been closed:

   ```
   closeFile(file);
   closeFile(file);
   ```

In a short linear program like this, we aren't likely to make these
mistakes. But when handles are stored in data structures and shuffled around,
and the lifecycle calls are separated across time and space, these errors become
more common.

And they don't just apply to files. Consider a database access API:

```
type Db
Db connect(String host)
Rows query(Db db, String query)
void close(Db db)
```

Again: after calling `close` we can still call `query` and `close`. And we can
also forget to call `close` at all.

And --- crucially --- consider this memory management API:

```
type Pointer<T>
Pointer<T> allocate(T value)
T load(Pointer<T> ptr)
void store(Pointer<T> ptr, T value)
void free(Pointer<T> ptr)
```

Here, again, we can forget to call `free` after allocating a pointer, we can
call `free` twice on the same pointer, and, more disastrously, we can call
`load` and `store` on a pointer that has been freed.

Everywhere we have _resources_ --- types with an associated lifecycle, where
they must be created, used, and destroyed, in that order --- we have the same
kind of errors: forgetting to destroy a value, or using a value after it has
been destroyed.

In the context of memory management, pointer lifecycle errors are so disastrous
they have their own names:

1. [Double free errors](https://owasp.org/www-community/vulnerabilities/Doubly_freeing_memory).
2. [Use-after-free errors](https://owasp.org/www-community/vulnerabilities/Using_freed_memory).

Naturally, computer scientists have attempted to attack these problems. The
traditional approach is called _static analysis_: a group of PhD's will write a
program that goes through the source code and performs various checks and finds
places where these errors may occur.

Reams and reams of papers, conference proceedings, university slides, etc. have
been written on the use of static analysis to catch these errors. But the
problem with static analysis is threefold:

1. It is a moving target. While type systems are relatively fixed --- i.e., the
   type checking rules are the same across language versions --- static
   analyzers tend to change with each version, so that in each newer version of
   the software you get more and more sophisticated heuristics.

2. Like unit tests, it can usually show the _presence_ of bugs, but not their
   _absence_. There may be false positives --- code that is perfectly fine but
   that the static analyzer flags as incorrect --- but more dangerous is the
   false negative, where the static analyzer returns an all clear on code that
   has a vulnerability.

3. Static analysis is an opaque pile of heuristics. Because the analyses are
   always changing, the programmer is not expected to develop a mental model of
   the static analyzer, and to write code with that model in mind. Instead, they
   are expected to write the code they usually write, then throw the static
   analyzer at it and hope for the best.

What we want is a way to solve these problems that is _static_ and
_complete_. Static in that it is a fixed set of rules that you can learn once
and remember, like how a type system works. _Complete_ in that is has _no false
negatives_, and _every_ lifecycle error is caught.

And, above all: we want it to be simple, so it can be wholly understood by the
programmer working on it.

So, to summarize our requirements:

1. **Correctness Requirement:** We want a way to ensure that resources are used
   in the correct lifecycle.

2. **Simplicity Requirement:** We want that mechanism to be simple, that is, a
   programmer should be able to hold it in their head. This rules out
   complicated solutions involving theorem proving, SMT solvers, symbolic
   execution, etc.

3. **Staticity Requirement:** We want it to be a fixed set of rules and not an
   ever changing pile of heuristics.

All these goals are achievable: the solution is _linear types_.

## What Linear Types Are {#linear-what}

In the physical world, an object occupies a single point in space, and objects
can move from one place to the other. Copying an object, however, is
impossible. Computers invert this: copying is the primitive operation. While an
object in memory resides in a single place, _references_ or pointers to that
object can be copied any number of times, and passed around through threads, and
this freedom to copy things wildly is at the root of all resource-related
security vulnerabilities.

A _type_ is a set of values that share some structure. A _linear type_ is a type
whose values can only be used once.

Linear values work like real-world objects: they occupy a single point in space,
and they can be passed around, but _not duplicated_. This restriction may sound
onerous (and unrelated to the problem) but we will see it isn't.

Austral's linear type system is defined by just two rules: the **Linear Universe
Rule** and the **Use-Once Rule**.

## Universes {#linear-universes}

First, the set of types is divided into two _universes_: the _free_ universe,
containing types which can be used any number of times (like booleans, machine
sized integers, floats, records containing free types, etc.); and the _linear_
universe, containing linear types, which usually represent resources (pointers,
file handles, database handles, etc.).

Types enter the linear universe in one of two ways:

The first is by fiat: a type can simply be declared linear, even though it only
contains free types. We'll see later why this is useful.

```austral
-- `Pos` is declared to be linear, even though it
-- only contains free types.
record Pos: Linear is
    x: Int32;
    y: Int32;
end;
```

The second is by containment: linear types can be thought of as being
"viral". If a type contains a value of a linear type, it automatically becomes
linear.

So, if you have a linear type `T`, then a record like:

```austral
record Example: Linear is
    a: A;
    b: B;
    c: Pair[T, A];
end;
```

is linear because the field `c` contains a type which in turn contains `T`. A
union or enum where one of the variants contains a linear type is,
unsurprisingly, linear. You can't sneak a linear type into a free type.

The virality of linear types ensures that you can't escape linearity by accident.

## The Use-Once Rule {#linear-once}

A value of a linear type must be used once and only once. Not _can_: _must_. It
cannot be used zero times. This can be enforced entirely at compile time through
a very simple set of checks.

To understand what "using" a linear value means, let's look at some examples.

Suppose you have a function `f` that returns a value of a linear type `L`.

Then, the following code:

```austral
begin
    let x: L := f();
end;
```

is incorrect. `x` is a variable of a linear type, and it is used zero
times. The compiler will complain that `x` is being silently discarded.

Similarly, if you have:

```austral
begin
    f();
end;
```

The compiler will complain that the return value of `f` is being silently
discarded, which you can't do to a linear type.

If you have:

```austral
begin
    let x: L := f();
    g(x);
    h(x);
end;
```

The compiler will complain that `x` is being used twice: it is passed into
`g`, at which point is it said to be _consumed_, but then it is passed into
`h`, and that's not allowed.

This code, however, passes: `x` is used once and exactly once:

```austral
begin
    let x: L := f();
    g(x);
end;
```

"Used" does not, however, mean "appears once in the code". Consider how `if`
statements work. The compiler will complain about the following code, because
even though `x` appears only once in the source code, it is not being "used
once", rather it's being used --- how shall I put it? 0.5 times?:

```austral
begin
    let x: L := f();
    if cond() then
        g(x);
    else
        -- Do nothing.
        skip;
    end if;
end;
```

The variable `x` is consumed in one branch but not the other, and the compiler
isn't happy. If we change the code to this:

```austral
begin
    let x: L := f();
    if cond() then
        g(x);
    else
        h(x);
    end if;
end;
```

Then we're good. The rule here is that a variable of a linear type, defined
outside an `if` statement, must be used either zero times in that statement,
or exactly once in each branch.

A similar restriction applies to loops. We can't do this:

```austral
begin
    let x: L := f();
    while cond() do
        g(x);
    end while;
end;
```

Because even though `x` appears once, it is _used_ more than once: it is used
once in each iteration. The rule here is that a variable of a linear type,
defined outside a loop, cannot appear in the body of the loop.

And that's it. That's all there is to it. We have a fixed set of rules, and
they're so brief you can learn them in a few minutes. So we're satisfying the
simplicity and staticity requirements listed in the previous section.

But do linear types satisfy the correctness requirement? In the next section,
we'll see how linear types make it possible to enforce that a value should be
used in accordance to a lifecycle.

## Linear Types and Safety {#linear-safety}

Let's consider a linear file system API. We'll use the syntax for Austral module specifications:

```austral
module Files is
    type File : Linear;
    function openFile(path: String): File;
    function writeString(file: File, content: String): File;
    function closeFile(file: File): Unit;
end module.
```

The `openFile` function is fairly normal: takes a path and returns a linear
`File` object.

`writeString` is where things are different: it takes a linear `File` object
(and consumes it), and a string, and it returns a "new" linear `File`
object. "New" is in quotes because it is a fresh linear value only from the
perspective of the type system: it is still a handle to the same file. But don't
think about the implementation too much: we'll look into how this is implemented
later.

`closeFile` is the destructor for the `File` type, and is the terminus of the
lifecycle graph: a `File` enters and does not leave, and the object is disposed
of. Let's see how linear types help us write safe code.

Can we leak a `File` object? No:

```austral
let file: File := openFile("test.txt");
-- Do nothing.
```

The compiler will complain: the variable `file` is used zero
times. Alternatively:

```austral
let file: File := openFile("test.txt");
writeString(file, "Hello, world!");
```

The return value of `writeString` is a linear `File` object, and it is being
silently discarded. The compiler will complain at us.

We can strike the "leak" transitions from the lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four black arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'. There are two red arrows: one from 'closeFile' to 'writeString' labeled 'use after close', and one from 'closeFile' to itself labeled 'double close'.](/assets/content/introducing-austral/file-api-without-leaks.svg)

Can we close a file twice? No:

```austral
let file: File := openFile("test.txt");
closeFile(file);
closeFile(file);
```

The compiler will complain that you're trying to consume a linear variable
twice. So we can strike the "double close" erroneous transition from the
lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four black arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'. There is one red arrow: from 'closeFile' to 'writeString' labeled 'use after close'.](/assets/content/introducing-austral/file-api-without-leaks-and-double-close.svg)

And you can see where this is going. Can we write to a file after closing it?
No:

```austral
let file: File := openFile("test.txt");
closeFile(file);
let file2: File := writeString(file, "Doing some mischief.");
```

The compiler will, again, complain that we're consuming `file` twice. So we can
strike the "use after close" transition from the lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'.](/assets/content/introducing-austral//file-api.svg)

And we have come full circle: the lifecycle that the compiler enforces is
exactly, one-to-one, the lifecycle that we intended.

There is, ultimately, one and only one way to use this API such that the
compiler doesn't complain:

```austral
let f: File := openFile("test.txt");
let f1: File := writeString(f, "First line");
let f2: File := writeString(f1, "Another line");
...
let f15: File := writeString(f14, "Last line");
closeFile(f15);
```

Note how the file value is "threaded" through the code, and each linear variable
is used exactly once.

And now we are three for three with the requirements we outlined in the previous
section:

1. **Correctness Requirement:** Is it correct? Yes: linear types allow us to
   define APIs in such a way that the compiler enforces the lifecycle perfectly.

2. **Simplicity Requirement:** Is it simple? Yes: the type system rules fit in a
   napkin. There's no need to use an SMT solver, or to prove theorems about the
   code, or do symbolic execution and explore the state space of the
   program. The linearity checks are simple: we go over the code and count the
   number of times a variable appears, taking care to handle loops and `if`
   statements correctly. And also we ensure that linear values can't be
   discarded silently.

3. **Staticity Requirement:** Is it an ever-growing, ever-changing pile of
   heuristics? No: it is a fixed set of rules. Learn it once and use it forever.

## A Safe Database API {#linear-db}

And does this solution generalize? Let's consider a linear database API:

```austral
module Database is
    type Db: Linear;
    function connect(host: String): Db;
    function query(db: Db, query: String): Pair[Db, Rows];
    function close(db: Db): Unit;
end module.
```

This one's a bit more involved: the `query` function has to return a tuple
containing both the new `Db` handle, and the result set.

Again: we can't leak a database handle:

```austral
let db: Db := connect("localhost");
-- Do nothing.
```

Because the compiler will point out that `db` is never consumed. We can't `close` a database handle twice:

```austral
let db: Db := connect("localhost");
close(db);
close(db); -- error: `db` consumed again.
```

Because `db` is used twice. Analogously, we can't query a database once it's closed:

```austral
let db: Db := connect("localhost");
close(db);
-- The below is tuple destructuring notation.
let { first as db1: Db, second: Rows } := query(db, "SELECT ...");
close(db); -- error: `db` consumed again.
-- another error: `db1` never consumed.
```

For the same reason. The only way to use the database correctly is:

```austral
let db: Db := connect("localhost");
let { first as db1: Db, second: Rows } = query(db, "SELECT ...");
// Iterate over the rows or some such.
close(db1);
```

## Borrowing {#linear-borrowing}

Returning tuples from every function and threading linear values through the
code is very verbose.

It is also often a violation of the principle of least privilege: linear values,
in a sense, have "root permissions". If you have a linear value, you can destroy
it. Consider the linear pointer API described above: the `load` function could
internally deallocate the pointer and allocate it again.

We wouldn't _expect_ that to happen, but the whole point is to be defensive. We
want the language to give us some guarantees: if a function should only be
allowed to read from a linear value, but not deallocate it or mutate its
interior, we want a way to represent that.

[_Borrowing_][borrowing] is stolen lock, stock, and barrel from Rust. It
improves ergonomics by allowing us to treat a Linear value as Free within a
delineated context. And it allows us to degrade permissions: functions that
should only be able to read data from a linear value can take a read-only
reference, functions that should be able to mutate (but not destroy) a linear
value can take a mutable reference.

Passing the linear value itself is the highest level of permissions: it allows
the receiving function to do anything whatever with that value, by taking
complete ownership of it.

Unlike Rust, Austral's borrowing is more restricted. The tradeoff is: you have
to type more, but it's syntactically clearer where region lifetimes end, and the
model is conceptually simpler. This is why the linearity checker is a mere 600
lines of OCaml.

# Capability-Based Security {#cap}

If you read software engineering literature from the 80's, the overwhelming
concern is about software reuse. Today, we have the opposite problem: package
ecosystems contain hundreds of thousands of packages, written by many authors,
and applications transitively have thousands of dependencies. This introduces a
new category of security vulnerability: the [supply chain attack][supplychain],
where an attacker adds malware to a single library used transitively by
millions of computers.

But why is a _single_ malware dependency out of thousands enough to compromise
security? Because code is overwhelmingly permissionless. Or, rather: all code
has the same permission level: do anything. Without inspecting the code, you
have no way of knowing whether `leftPad` pads a string _or_ reads your entire
home directory and uploads it to a remote server.

Austral's solution is [capability-based security][cap]. Code should be
permissioned. To access the filesystem, or the network, or other privileged
resources, libraries should require permission to do so. Then it is evident,
from function signatures, what each library is able to do, and what level of
auditing is required.

Furthermore: capabilities can be arbitrarily granular. Beneath the capability to
access the entire filesystem, we can have a capability to access a specific
directory and its contents, or just a specific file, further divided into read,
write, and read-write permissions. For network access, we can have capabilities
to access a specific host, or capabilities to read, write, and read-write to a
socket.

Access to the computer clock, too, [should be restricted][clock], since accurate
timing information can be used by malicious or compromised dependencies to carry
out a [timing attack][timing] or exploit a [side-channel vulnerability][side]
such as [Spectre][spectre].

## Linear Capabilities {#cap-linear}

A capability is a value that represents an unforgeable proof of having
permission to perform an action. They have the following properties:

1. Capabilities can be destroyed.

2. Capabilities can be surrendered by passing them to others.

3. Capabilities cannot be duplicated.

4. Capabilities cannot be acquired out of thin air: they must be passed by the
   client.

Capabilities in Austral are implemented as linear types: they are destroyed by
being consumed, they are surrendered by simply passing the value to a function
(i.e., by being consumed), they are non-duplicable since linear types cannot be
duplicated. The fourth restriction must be implemented manually by the
programmer.

## A Capability-Secure Filesystem API {#cap-fs}

Consider a non-capability-secure filesystem API:

```austral
module Files is
    -- File and directory paths.
    type Path: Linear;
    -- Creating and disposing of paths.
    function Make_Path(value: String): Path;
    function Dispose_Path(path: Path): Unit;
    -- Reading and writing.
    generic [R: Region]
    function Read_File(path: &[Path, R]): String;
    generic [R: Region]
    function Write_File(path: &![Path, R], content: String): Unit;
end module.
```

(Error handling _etc._ omitted for clarity.)

Here, any client can construct a path from a string, then read the file pointed
to by that path or write to it. A compromised transitive dependency could then
read the contents of your home directory, or any file in the filesystem that the
process has access to, like so:

```austral
let p: Path := Parse_Path(Make_String("/etc/passwd"));
let secrets: String := Read_File(&p);
-- Send this over the network, using an equally capability-insecure network
-- API.
uploadToCompromisedServer(secrets);
```


In the context of code running on a programmer's personal computer, that means
personal information. In the context of code running on an application server,
that means confidential bussiness information.

What does a capability-secure filesystem API look like? Like this:

```austral
module Files is
    type Path: Linear;
    -- The filesystem access capability.
    type Filesystem: Linear;
    -- Given a read reference to the filesystem access capability,
    -- get the root directory.
    generic [R: Region]
    function Get_Root(fs: &[Filesystem, R]): Path;
    -- Given a directory path, append a directory or
    -- file name at the end.
    function Append(path: Path, name: String): Path;
    -- Reading and writing.
    generic [R: Region]
    function Read_File(path: &[Path, R]): String;
    generic [R: Region]
    function Write_File(path: &[Path, R], content: String): Unit;
end module.
```

This demonstrates the hierarchical nature of capabilities, and how granular we
can go:

1. If you have a `Filesystem` capability, you can get the `Path` to the root
   directory. This is essentially read/write access to the entire filesystem.

2. If you have a `Path` to a directory, you can get a path to a subdirectory or
   a file, but you can't go _up_ from a directory to its parent.

3. If you have a `Path` to a file, you can read from it or write to it.

Each capability can only be created by providing proof of a higher-level, more
powerful, broader capability.

Then, if you have a logging library that takes a `Path` to the logs directory,
you know it has access to that directory and to that directory only[^dotdot]. If
a library doesn't take a `Filesystem` capability, it has no access to the
filesystem.

But: how do we create a `Filesystem` value? The next section explains this.

## The Root Capability {#cap-root}

Capabilities cannot be created out of thin air: they can only be created by
proving proof that the client has access to a more powerful capability, for
example by passing a reference to that capability. This recursion has to end
somewhere.

The root of the capability hierarchy is a value of type `Root_Capability`. This
is the first argument to the entrypoint function of an Austral program. For our
capability-secure filesystem API, we'd add a couple of functions:

```austral
-- Acquire the filesystem capability from a reference
-- to the root capability.
generic [R: Region]
function Get_Filesystem(root: &[Root_Capability, R]): Filesystem;

-- Relinquish the filesystem capability.
function Release_Filesystem(fs: Filesystem): Unit;
```

And we can use it like so:

```austral
import Files (
    Filesystem,
    Get_Filesystem,
    Path,
    Get_Root,
    Append,
    Relase_Filesystem,
);
import Dependency (
    Do_Something
);

function Main(root: Root_Capability): Root_Capability is
    -- Acquire a filesystem capability.
    let fs: Filesystem := Get_Filesystem(&root);
    -- Get the root directory.
    let r: Path := Get_Root(&fs);
    -- Get the path to the `/var` directory.
    let p: Path := Append(p, "var");
    -- Do something with the path to the `/var` directory, confident that nothing
    -- this dependency does can go outside `/var`.
    Do_Something(p);
    -- Afterwards, relinquish the filesystem capability.
    Release_Filesystem(fs);
    -- Finally, end the program by returning the root capability.
    return root;
end;
```

# Status and Future Work {#status}

There is a bootstrapping [compiler][austral] written in OCaml, and a
[specification][spec]. Software is only ever asymptotically finished, but the
compiler implements the entire language, and the compiler and spec are at a
level of maturity where I can write simple programs and talk about it in public.

The compiler is a very simple, whole program compiler that outputs C. Separate
compilation, for added performance, is not yet implemented but is in the
roadmap.

The immediate next steps are:

1. Write the basic parts of the standard library.
2. A build system and package manager.
3. Fast separate compilation support.

If I can allocate the time, I'd like to use OpenAI's finetuning API to teach a
code completion model to write Austral for me. I had some promising results
teaching Austral to ChatGPT interactively. I think the language is uniquely good
for this, because the separation of module interfaces and implementations means
I can probably design an interface and have a model complete the implementation
for me.

# Getting Involved {#involved}

A beginning is a unique time. Like annealing: the atoms are in rapid motion, and
have yet to settle in a fixed configuration. Decisions made early have a vastly
disproportionate impact. So if you have strong opinions about standard library
APIs, or build systems and package managers, or security, I'd like to hear from
you.

There's a small [Discord][discord] for the language, but the best way to
communicate is probably through public [GitHub issues][issues]. Or you can ping
me on [Twitter][tw], which is the fastest way to reach me.

[discord]: https://discord.gg/8cEuAcD8pM
[issues]: https://github.com/austral/austral/issues
[tw]: https://twitter.com/zetalyrae

# Conclusion {#conclusion}

And without further ado:

```
function main(): ExitCode is
    printLn("Hello, world!");
    return ExitSuccess();
end;
```

[austral]: https://austral-lang.org/
[goals]: https://austral-lang.org/spec/spec.html#goals
[quiz]: https://wordsandbuttons.online/so_you_think_you_know_c.html
[lawyering]: http://www.catb.org/jargon/html/L/language-lawyer.html
[else]: https://en.wikipedia.org/wiki/Dangling_else
[gotofail]: https://en.wikipedia.org/wiki/Unreachable_code#goto_fail_bug
[cliche]: https://en.wikipedia.org/wiki/Thought-terminating_clich%C3%A9
[lexerhack]: https://en.wikipedia.org/wiki/Lexer_hack
[vexing]: https://en.wikipedia.org/wiki/Most_vexing_parse
[pemdas]: https://en.wikipedia.org/wiki/Order_of_operations
[concepts]: https://en.wikipedia.org/wiki/Concepts_(C%2B%2B)
[linearitycheck]: https://github.com/austral/austral/blob/32fba714b3fe900e79c39682e44b63846017f4df/lib/LinearityCheck.ml
[cap]: https://en.wikipedia.org/wiki/Capability-based_security
[supplychain]: https://en.wikipedia.org/wiki/Supply_chain_attack
[langsec]: http://langsec.org/
[clock]: https://twitter.com/robotlolita/status/1474351603008389122
[timing]: https://en.wikipedia.org/wiki/Timing_attack
[side]: https://en.wikipedia.org/wiki/Side-channel_attack
[spectre]: https://en.wikipedia.org/wiki/Spectre_(security_vulnerability)
[spec]: https://austral-lang.org/spec/spec.html
[spec-linearity]: https://austral-lang.org/spec/spec.html#linearity
[sum]: https://en.wikipedia.org/wiki/Algebraic_data_type
[typeclass]: https://austral-lang.org/spec/spec.html#type-classes
[borrowing]: https://austral-lang.org/tutorial/borrowing
[copying-c]: https://eev.ee/blog/2016/12/01/lets-stop-copying-c/

# Footnotes

[^python]:
    Because of dynamic typing, the absurd scoping rules, runtime reflection,
    decorators, constant leakage of implementation details, context-sensitive
    syntax, _etc_.

[^cuts]:
    [Here][copying-c] is a great article on the long litany of C design flaws
    that have been copied into other languages.

[^dotdot]:
    Special paths like `..` have to be handled specially.

[^async]:
    There are two ways to build a general-purpose programming language:

    1. Add features to specialize the language to _every_ domain.
    2. Don't specialize to any one domain.

    Only the latter approach is scalable. Async is a very specific feature, and
    every way of doing concurrency other than kernel threads has come and gone
    out of fashion (think Scala actors and Goroutines, two very admirable
    features). So when async goes out of fashion, it will be a big problem if
    Austral had async built right in the core language.

[^overload]:
    Because the semantics are different. Is `a * b` commutative? Depends on
    whether they're floats are matrices. Just be explicit and write out the
    function calls.
