---
title: Introducing Austral
summary: Introducing my programming language.
tags: []
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
4. [Linear Types](#linear)
   1. [Motivation](#linear-motivation)
   2. [What are Linear Types?](#linear-what)
   3. [Universes](#linear-universes)
   4. [The Use-Once Rule](#linear-once)
   5. [Linear Types and Safety](#linear-safety)
   6. [Borrowing](#linear-borrowing)
5. [Capability-Based Security](#cap)
   1. [Linear Capabilities](#cap-linear)
   2. [A Capability-Secure Filesystem API](#cap-fs)
   3. [The Root Capability](#cap-root)
6. [Status and Future Work](#status)
7. [Conclusion](#conclusion)

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

Austral is simple. Short spec, thin runtime, small compiler.

To give a concrete example: the linear type system was designed with brutal
simplicity in mind. Consequently, Austral's equivalent of a borrow checker is
[less than 600 lines of code][linearitycheck], including the implementation of
borrowing and other ergonomic features.

The goal here is that the entire programming language should fit in your head,
that you should be able to read the [specification][spec] from beginning to end
and know all there is to know about the language.

There's this famous [quiz][quiz] of the C language, where you have all these
strange-looking programs and have to decide what they output. And people who
have been working with the language for years struggle to answer correctly
because the questions refer to subtle and obscure features of the C
specification.

If you think figuring out what the program does is a fun puzzle, Austral is not
for you. [Language lawyering][lawyering] is a design flaw: if two people can
look at the same basic program and disagree about its behaviour, that's a
problem.

_Strictness_ is half language features, and half a change in mindset.

If planes were flown like we write code, we'd have daily crashes, of course, but
beyond that, the response to every plane crash would be: "only a bad pilot
blames their plane! If they'd read subparagraph 71 of section 7.1.5.5 of the
C++, er, 737 spec, they'd know that at 13:51 PM on the vernal equinox the wings
fall off the plane."

This doesn't happen in aviation, because in aviation we have decided, correctly,
that _human error is an intrinsic and inseparable part of human activity_. And
so we have built concentric layers of mechanical checks and balances around
pilots, to take on part of the load of flying.

Because humans are tired, they are burned out, they have limited focus, limited
working memory, they are traumatized by writing executable YAML, _etc_.

An example: there is a design flaw in the C programming language where, for
terseness, you can write `if` statements without braces. This introduces a
syntactic ambiguity: it's called the ["dangling else"][else] problem. This isn't
some abstract ivory tower concern: it has caused [real-world security
vulnerabilities][gotofail]. If you suggest that this is a flaw,
Stockholm-syndromed programmers will invoke the old [thought-terminating
cliche][cliche]: "only a bad craftsman blames his tools!". But the tradeoff here
is obvious: you save a few bytes and a few milliseconds of typing, but you
introduce potentially billions of dollars of harm from security
vulnerabilities. It's self-evidently a design flaw. But if you suggest to
programmers that they should add the braces, they will kick and scream as if
you're taking away some fundamental freedom.

Austral's syntax was designed with [langsec][langsec] in mind: it is
context-free, it can be parsed from a grammar, no ["lexer hack"][lexerhack] or
[strange ad-hoc ambiguity-resolution mechanisms][vexing] are needed. The
pragmatics of the syntax are designed to minimize confusion and
ambiguity. Anyone can remember [PEMDAS][pemdas], but programming languages have
many categories of binary operators --- arithmetic, comparison, bitwise, Boolean
--- and mixing them together creates room for error (what does `x ^ y && z / w`
evaluate to?). So in Austral there is simply no operator precedence: any binary
expression deeper than one level is fully parenthesized. You have to type more,
but we are not typists, we are programmers, and our task is to communicate _to
others_ what we want computers to do. When in doubt: simplify.

This isn't for everyone. But it is for me, because after ten years in the
industry, the last thing I want is power, what I want are fewer nightmares.

# Anti-Features {#anti-features}

Before going over the language features, I'd like to list the
_anti-features_. Here are the things Austral proudly doesn't have:

1. There are no pervasive `NULL`s, and therefore no null pointer dereference
   errors. You have to use an explicit `Option` type.

2. There is no garbage collection.

3. Therefore, there is no fat runtime.

4. There are no exceptions and no stack unwinding and no destructors.

5. Therefore, there is no surprise control flow.

6. There are no implicit type conversions anywhere.

7. More generally: _there are no implicit function calls_. If it's not on the
   page, it's not happening, and you're not paying for it.

8. There is no global state.

9. There is no runtime reflection.

10. There are no macros.

11. There are no Java or Python-style `@Annotations`.

12. There is no type inference: type information flows in only one direction,
    and function parameters, local variables, _etc._ have to have their types
    annotated.

13. There is no first-class async.

14. Function overloading is very restricted through typeclasses (think [C++
    concepts][concepts]).

15. There is no syntactic ambiguity: no [dangling else][else] (and, therefore,
    no [`gotofail`][gotofail]), no arithmetic precedence. All binary operations
    beyond one level have to be fully parenthesized.

# Features {#features}

What Austral _does_ have:

1. A strong, static type system that's not too big-brained.

2. Linear types, which allow resources to be handled correctly and safely
   without runtime overhead. "Resource" here means memory, but also anything
   that has an explicit lifecycle of create-use-destroy: file handles, sockets,
   database handles.

3. [Capability-based security][cap], which prevents [supply chain
   attacks][supplychain]. Your left-pad dependency can't be compromised to
   upload the contents of your disk to a remote server, because code that
   performs network access needs a network capability, code that performs
   filesystem access needs a filesystem capability, and so on, while a
   string-padding function that claims to need network access instantly stands
   out. Capabilities are unforgeable: they cannot be acquired arbitrarily, they
   have to be passed in by the client.

4. A strong, Ada-inspired module system which is not tied to filesystem
   structure and which separates module interfaces from implementations.

5. [Sum types][sum] with pattern matching and exhaustiveness checking.

6. [Type classes][typeclass], as in Haskell, for restricted function
   overloading, and type parameters can be constrained to only accept types that
   implement a given typeclass.

7. A strict, context-free, unambiguous syntax, informed by [langsec][langsec]
   ideas.

# Linear Types {#linear}

It is difficult to advertise a language as being "simple" and then start talking
about "linear types" and "type universes", but it is only the words that are
new. The concepts are simple. Austral's entire linear type system [fits in a
page][spec-linearity]. So, this isn't some abstract ivory tower feature that you need a
PhD in category theory to understand.

Linear types let us have manual memory management, without runtime overhead, and
without security vulnerabilities: they prevent memory leaks, use-after-free, and
double-free errors.

This extends beyond memory to anything that has a lifecycle, where we have to
create it, use it, and destroy it. File handles, network sockets, database
handles, locks and mutexes: the correct usage of these objects can be enforced
at compile time.

First, I'll explain the motivation: why do we need linear types? Then I'll
explain what they are, and how they provide safety.

## Motivation {#linear-motivation}

Consider a file handling API, in C++ syntax:

```c++
struct File;

File openFile(string path);

File writeString(File file, string content);

void closeFile(File file);
```

An experienced programmer understands the _implicit lifecycle_ of the `File`
object:

1. We create a `File` handle by calling `openFile`.
2. We write to the handle zero or more times.
3. We close the file handle by calling `closeFile`.

We can depict this graphically like this:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'.](/assets/content/introducing-austral/file-api.png)

But, crucially: this lifecycle is _not enforced by the compiler_. There are a
number of erroneous transitions that we don't consider, but which are
technically possible:

![The graph from the previous figure, with a new node labeled 'leak', and with four new arrows in red: one from 'closeFile' to itself labeled 'double close', one from 'closeFile' to 'writeString' labeled 'use after close', one from 'openFile' to 'leak' labeled 'forgot to close', and one from 'writeString' to 'leak' also labeled 'forgot to close'.](/assets/content/introducing-austral/file-api-errors.png)

These fall into two categories:

1. **Leaks:** we can forget to call `closeFile`, e.g.:

    ```c++
    auto file = openFile("hello.txt");
    writeString(file, "Hello, world!");
    // Forgot to close
    ```

2. **Use-After-Close:** and we can call `writeString` on a `File` object that
   has already been closed:

   ```c++
   closeFile(file);
   writeString(file, "Goodbye, world!");
   ```

   And we can close the file handle after it has been closed:

   ```c++
   closeFile(file);
   closeFile(file);
   ```

In a short linear program like this, we aren't likely to make these
mistakes. But when handles are stored in data structures and shuffled around,
and the lifecycle calls are separated across time and space, these errors become
more common.

And they don't just apply to files. Consider a database access API:

```c++
struct Db;

Db connect(string host);

Rows query(Db db, string query);

void close(Db db);
```

Again: after calling `close` we can still call `query` and `close`. And we can
also forget to call `close` at all.

And --- crucially --- consider this memory management API:

```c++
template <typename T>
*T allocate(T value);

template <typename T>
T load(*T ptr);

template <typename T>
void store(*T ptr, T value)

template <tyename T>
void free(*T ptr)
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

## What are Linear Types? {#linear-what}

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

To understand what "using" a linear value means, let's look at some examples. We
will use C syntax, rather than Austral syntax, so as to communicate just the
concepts rather than the language syntax. With one minor change: linear types
will be denoted by adding an exclamation mark after the name. So, `A` is a free
type, but `B!` is a linear type.

Suppose you have a function `f` that returns a value of a linear type `L!`.

Then, the following code:

```c
{
    L! x = f();
}
```

is incorrect. `x` is a variable of a linear type, and it is used zero
times. The compiler will complain that `x` is being silently discarded.

Similarly, if you have:

```c
{
    f();
}
```

The compiler will complain that the return value of `f` is being silently
discarded, which you can't do to a linear type.

If you have:

```c
{
    L! x = f();
    g(x);
    h(x);
}
```

The compiler will complain that `x` is being used twice: it is passed into
`g`, at which point is it said to be _consumed_, but then it is passed into
`h`, and that's not allowed.

This code, however, passes: `x` is used once and exactly once:

```c
{
    L! x = f();
    g(x);
}
```

"Used" does not, however, mean "appears once in the code". Consider how `if`
statements work. The compiler will complain about the following code, because
even though `x` appears only once in the source code, it is not being "used
once", rather it's being used --- how shall I put it? 0.5 times?:

```c
{
    L! x = f();
    if (cond) {
        g(x);
    } else {
        // Do nothing.
    }
}
```

`x` is consumed in one branch but not the other, and the compiler isn't
happy. If we change the code to this:

```c
{
    L! x = f();
    if (cond) {
        g(x);
    } else {
        h(x);
    }
}
```

Then we're good. The rule here is that a variable of a linear type, defined
outside an `if` statement, must be used either zero times in that statement,
or exactly once in each branch.

A similar restriction applies to loops. We can't do this:

```c
{
    L! x = f();
    while cond() {
        g(x);
    }
}
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

Let's consider a linear file system API. Again, we will use the modified C++ syntax.

The API looks like this:

```c++
struct File!;

File! openFile(string path);

File! writeString(File! file, string content);

void closeFile(File! file);
```

The `openFile` function is fairly normal: takes a path and returns a linear
`File!` object.

`writeString` is where things are different: it takes a linear `File!` object
(and consumes it), and a string, and it returns a "new" linear `File!`
object. "New" is in quotes because it is a fresh linear value only from the
perspective of the type system: it is still a handle to the same file. But don't
think about the implementation too much: we'll look into how this is implemented
later.

`closeFile` is the destructor for the `File!` type, and is the terminus of the
lifecycle graph: a `File!` enters and does not leave, and the object is disposed
of. Let's see how linear types help us write safe code.

Can we leak a `File!` object? No:

```c++
File! file = openFile("sonnets.txt");
// Do nothing.
```

The compiler will complain: the variable `file` is used zero
times. Alternatively:

```c++
File! file = openFile("sonnets.txt");
writeString(file, "Devouring Time, blunt thou the lion’s paws, ...");
```

The return value of `writeString` is a linear `File!` object, and it is being
silently discarded. The compiler will whine at us.

We can strike the "leak" transitions from the lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four black arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'. There are two red arrows: one from 'closeFile' to 'writeString' labeled 'use after close', and one from 'closeFile' to itself labeled 'double close'.](/assets/content/introducing-austral/file-api-without-leaks.png)

Can we close a file twice? No:

```c++
File! file = openFile("test.txt");
closeFile(file);
closeFile(file);
```

The compiler will complain that you're trying to consume a linear variable
twice. So we can strike the "double close" erroneous transition from the
lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four black arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'. There is one red arrow: from 'closeFile' to 'writeString' labeled 'use after close'.](/assets/content/introducing-austral/file-api-without-leaks-and-double-close.png)

And you can see where this is going. Can we write to a file after closing it?
No:

```c++
File! file = openFile("test.txt");
closeFile(file);
File! file2 = writeString(file, "Doing some mischief.");
```

The compiler will, again, complain that we're consuming `file` twice. So we can strike the "use after close" transition from the lifecycle graph:

![A graph with three nodes labeled 'openFile', 'writeString', and 'close File'. There are four arrows: from 'openFile' to 'writeString', from 'openFile' to 'closeFile', from 'writeString' to itself, and from 'writeString' to 'closeFile'.](/assets/content/introducing-austral//file-api.png)

And we have come full circle: the lifecycle that the compiler enforces is
exactly, one-to-one, the lifecycle that we intended.

There is, ultimately, one and only one way to use this API such that the
compiler doesn't complain:

```c++
File! f = openFile("rilke.txt");
File! f_1= writeString(f, "We cannot know his legendary head\n");
File! f_2= writeString(f_1, "with eyes like ripening fruit. And yet his torso\n");
...
File! f_15 = writeString(f_14, "You must change your life.");
closeFile(f_15);
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

And does this solution generalize? Let's consider a linear database API:

```c++
struct Db!;

Db! connect(string host);

std::pair<Db!, Rows> query(Db! db, string query);

void close(Db! db);
```

This one's a bit more involved: the `query` function has to return a tuple
containing both the new `Db!` handle, and the result set.

Again: we can't leak a database handle:

```c++
Db! db = connect("localhost");
// Do nothing.
```

Because the compiler will point out that `db` is never consumed. We can't `close` a database handle twice:

```c++
Db! db = connect("localhost");
close(db);
close(db); // error: `db` consumed again.
```

Because `db` is used twice. Analogously, we can't query a database once it's closed:

```c++
Db! db = connect("localhost");
close(db);
// auto [x,y] is tuple unpacking notation.
auto [db1, rows] = query(db, "SELECT ...");
close(db); // error: `db` consumed again.
```

For the same reason. The only way to use the database correctly is:

```c++
let db: Db! := connect("localhost");
auto [db1, rows] = query(db, "SELECT ...");
// Iterate over the rows or some such.
close(db1);
```

What about manual memory management? Can we make it safe? Let's consider a
linear pointer API.

For this example, we will switch to actual Austral syntax. This is because Austral lets us specify what universe type parameters belong to.

When you have a generic type with generic type parameters, in a normal language
(where all types belong to the same universe) you might declare it like:

```c++
type T<A, B, C>
```

In Austral, we have to specify what universe type parameters belong to, and what
universe the resulting type belongs to. For example, here's a generic `Pair`
type that takes two types in the `Free` universe, and is itself a `Free` type:

```austral
record Pair[A: Free, B: Free]: Free is
    first: A;
    second: B;
end;
```

Sometimes we want a generic type to accept type arguments from any universe. In
that case, instead of `Free` or `Linear`, we use `Type`:

```
record Pair[A: Type, B: Type]: Type is
    first: A;
    second: B;
end;
```

This basically means: the type parameters `A` and `B` can be filled with types
from either universe, and the universe that `Pair` belongs to is determined by
said arguments:

1. If `A` and `B` are both `Free`, then `Pair` is `Free`.
2. If either one of `A` and `B` are `Linear`, then `Pair` is `Linear`.

So, if we only pass `Free` types, the `Pair` type will be `Free`. But if we pass
a single `Linear` type, `Pair` becomes `Linear`, because `Linear` types are
viral.

Here's the linear pointer API, in Austral syntax:

```austral
module LinearPointer is
    type Pointer[T: Type]: Linear;

    generic [T: Type]
    function allocate(value: T): Pointer[T];

    generic [T: Type]
    function deallocate(ptr: Pointer[T]): T;

    generic [T: Free]
    function load(ptr: Pointer[T]): LoadResult[T];

    record LoadResult[T: Free]: Linear is
        pointer: Pointer[T];
        value: T;
    end;

    generic [T: Free]
    function store(ptr: Pointer[T] ptr, value: T): Pointer[T];
end module.
```

This is more involved than previous examples, and uses new notation, so let's
break it down declaration by declaration.

1. First, we declare the `Pointer` type as a generic type that takes a parameter
   from any universe, and belongs to the `Linear` universe by fiat. That is:
   even if `T` is `Free`, `Pointer[T]` will be `Linear`.

   ```austral
   type Pointer[T: Type]: Linear;
   ```

   This syntax denotes an _opaque type_, whose implementation is provided in the
   corresponding module body.

2. Second, we define a generic function `allocate`, that takes a value from
   either universe, allocates memory for it, and returns a linear pointer to it.

   ```austral
    generic [T: Type]
    function allocate(value: T): Pointer[T];
   ```

3. Third, we define a slightly unusual `deallocate` function: rather than
   returning `void`, it takes a pointer, dereferences it, deallocates the
   memory, and returns the dereferenced value:

   ```austral
    generic [T: Type]
    function deallocate(ptr: Pointer[T]): T;
   ```

4. Fourth, we define a generic function specifically for pointers that contain
   free values: it takes a pointer, dereferences it, and returns a record with
   both the pointer and the dereferenced free value.

   ```austral
   generic [T: Free]
   function load(ptr: Pointer[T]): LoadResult[T];

   record LoadResult[T: Free]: Linear is
       pointer: Pointer[T];
       value: T;
   end;
   ```

   Why does `T` have to belong to the `Free` universe? Because otherwise we
   could write code like this:

   ```austral
   // Suppose that `L` is a linear type.
   let p: Pointer[L] := allocate(...);
   // This is record destructuring notation.
   let { pointer: Pointer[T], value: T } := load(p);
   // `pointer` is consumed below.
   let res: LoadResult[L] := load(pointer);
   ```

   Here, we've allocated a pointer to a linear value, but we've loaded it from
   memory twice, effectively duplicating it. This obviously should not be
   allowed. So the type parameter `T` is constrained to take only values of the
   `Free` universe, which can be copied freely any number of times.

5. Fifth, we define a generic function, again for pointers that contain free
   values. It takes a pointer and a free value, and stores that value in the
   memory allocated by the pointer, and returns the pointer again for reuse:

   ```austral
   generic [T: Free]
   function store(ptr: Pointer[T] ptr, value: T): Pointer[T];
   ```

   Again: why can't this function be defined for linear values? Because then we
   could write:

   ```austral
   // Suppose `L` is a linear type, and `a` and b`
   // are variables of type `L`.
   let p1: Pointer[L] := allocate(a);
   let p2: Pointer[L] := store(p1, b);
   let l: L := deallocate(p2);
   ```

   What happens to `a`? It is overwritten by `b` and lost. For values in the
   `Free` universe this is no problem: who cares if a byte is overwritten? But
   we can't overwrite linear values -- like database handles and such -- because
   the they would be leaked.

It is trivial to verify the safety properties. We can't leak memory, we can't
deallocate twice, and we can't read or write from and to a pointer after it has
been deallocated.

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

_Borrowing_ is stolen lock, stock, and barrel from Rust. It improves ergonomics
by allowing us to treat a linear value as free within a delineated context. And
it allows us to degrade permissions: functions that should only be able to read
data from a linear value can take a read-only reference, functions that should
be able to mutate (but not destroy) a linear value can take a mutable reference.

Passing the linear value itself is the highest level of permissions: it allows
the receiving function to do anything whatever with that value, by taking
complete ownership of it.

Unlike Rust, Austral's borrowing is more restricted, to make it clearer to the
programmer what the lifetime of borrows is.

A _reference_ is a free (copyable) pointer to a linear value. References come in
two kinds: read-only and read-write.

Reference types are tagged with a _region_, which you can think of as a
singleton type that exists only within a lexical scope, i.e., within a block of
code. This is to ensure that references cannot escape the scope in which they
are defined, therefore, references cannot outlive the linear values they point
to.

Given a linear type `L`, and a region name `R`, `&[L, R]` is the type of a
read-only reference to `L` in `R`, and `&![L, R]` the type of read-write
references to `L` in `R`.

In the most general case, references are made with the `borrow` statement. The
`borrow` statement works on variables, rather than expressions: this simplifies
the analysis.

If you have a variable `x` with a linear type `L`, you can borrow it like so:

```austral
let x: L := f();
borrow x as ref in rho do
    -- The type of `ref` is `&[L, rho]`, it is a read-only reference to
    -- `L` in the region `rho`.
    --
    -- Within this block, `x` cannot be consumed because it is borrowed,
    -- but `ref` can be used freely.
    let y: &[L, R] := transform(ref);
    -- etc.
end;
-- After the borrow, we can consume `x`.
destroy(x);
```

Within the body of a `borrow`, the variable being borrowed cannot be consumed or
re-borrowed. This ensures we can't read from a reference after the value it
points to has been destroyed.

The name `rho` is only defined within the block of the `borrow` statement, which
means you can't do this:

```austral
let escape: &[L, rho] := ...;
borrow x as ref do
    escape := ref;
end;
```

The compiler will complain that `rho` is an unknown type, since it only exists
in the body of the `borrow` statement. This restriction ensures that references
cannot outlive the thing they reference.

A more succint syntax is available. Where you don't care about the name of the
region, i.e., where you have a function that takes a reference but whose return
type does not include the reference region, like so:

```austral
generic [R: Region]
function stringLength(ref: &[String, R]): Index;
```

Then you can call it like so:

```austral
stringLength(&str);
```

This is the equivalent of:

```
borrow str as ref in rho do
    stringLength(ref);
end;
```

Analogously, `&!` takes a read-write reference.

# Capability-Based Security {#cap}

Code is overwhelmingly permissionless. Or, rather: all code has uniform root
permissions. The size of today's software ecosystems has introduced a new
category of security vulnerability: the [supply chain attack][supplychain]. An
attacker adds malware to an innocent library used transitively by millions. It
is downloaded and run, with the user's permissions, on the computers of hundreds
of thousands of programmers, and afterwards, on application servers.

The solution is [capability-based security][cap]. Code should be
permissioned. To access the terminal, or the filesystem, or the network,
libraries should require permission to do so. Then it is evident, from function
signatures, what each library is able to do, and what level of auditing is
required.

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

2. Capabilities can be surrended by passing them to others.

3. Capabilities cannot be duplicated.

4. Capabilities cannot be acquired out of thin air: they must be passed by the
   client.

Capabilities in Austral are implemented as linear types: they are destroyed by
being consumed, they are surrended by simply passing the value to a function
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
read the contents of `/etc/passwd`, or any file in the filesystem that the
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

This demonstrates the hieararchical nature of capabilities, and how granular we
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
proving proof that the client has access to a more powerful capability. This
recursion has to end somewhere.

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

There is a bootstrapping [compiler][austral] written in OCaml. It implements the
entire language. There is also a [specification][spec].

Things are only ever asymtotically finished. The language is at a level of
maturity where I can write simple programs and talk about it in public, however,
as of late June 2022, a few missing pieces are expected.

The compiler is a very simple, whole program compiler that outputs C. Separate
compilation, for added performance, is not yet implemented but is in the works.

The next steps are: function pointer types, a capability-secure standard
library, and a build system/package manager, and separate compilation.

Contributions are welcome.

# Conclusion {#conclusion}

And finally:

```
function Main(root: Root_Capability): Root_Capability is
    PrintLn("Hello, world!");
    return root;
end;
```

[austral]: https://github.com/austral/austral
[goals]: https://austral.github.io/spec/goals
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
[spec]: https://austral.github.io/spec/
[spec-linearity]: https://austral.github.io/spec/linearity
[sum]: https://en.wikipedia.org/wiki/Algebraic_data_type
[typeclass]: https://austral.github.io/spec/type-classes
# Footnotes

[^python]:
    Because of dynamic typing, the absurd scoping rules, runtime reflection,
    decorators, constant leakage of implementation details, context-sensitive
    syntax, _etc_.

[^dotdot]:
    Special paths like `..` should be banned, naturally.
