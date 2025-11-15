---
title: Linear Types and Exceptions
summary: On the incompatibility of linear types and traditional exception handling.
tags: [plt]
---

As part of my work on [Austral][austral], I did a lot of thinking on how linear
type systems interact with error handling strategies. This article serializes
those thoughts.

A linear type system guarantees all resources allocated by a terminating program
will be freed, and none will be used after being freed. This guarantee is lost
with the introduction of exceptions: we can throw an exception before the
consumer of a linear resource is called, leaking the resource. This article goes
through different strategies for reconciling linear types and exceptions.

# A Brief Introduction to Linear Types

In a normal type system, values can be copied arbitrarily. When you do `function
(x) { return (x, x); }`, you're copying a value, even if it's a reference. This
leads to problems: when you free a chunk of memory, there is no guarantee that
there isn't another pointer to that same chunk. Reading from or writing to that
chunk leads to a segmentation fault at best, a security vulnerability at
worst.

This is still a problem in langage with garbage collection: file handles and
sockets are analogous to memory, in that they have to be explicitly opened and
closed. Trying to write to a socket after it's been closed is an error. Trying
to write to a file from multiple threads at once can lead to subtle, impossible
to reproduce errors.

In a linear type system, types are divided into two universes: unrestricted
types are things like integers and booleans, which fit in a register and can be
copied without that leading to security vulnerabilities. Resources like memory,
file handles, etc. belong in the linear universe. The compiler checks statically
that values whose type belongs to the linear universe can only be used once.

This is simpler than it sounds, the rules basically are:

1. If you have a variable `x` of a linear type, then `x` must appear once and
   exactly once in the scope in which it is defined (excluding the statement
   that defines said variable).

2. If it appears in a branch of an `if` statement, it must appear in all other
   branches.

3. And, finally, it can't appear in the body of a loop (if it was defined
   outside that loop), because then it'd be used once every iteration. When a
   linear value is used, it is said to be _consumed_.

So, for example, the API to open, write data to, and close a file handle might be:

```
open(path: string): file!
write(file: file!, data: string): file!
close(file: file!): unit
```

The exclamation mark indicates that the type is linear. Everything here is
fairly standard, except that the `write` function takes a file object and
returns a file object. This is because files are linear. We can't write:

```
let f = open("data.txt")
write(f, "foo")
write(f, "bar")
```

because `f` is used twice (note that `let f` doesn't count as a
usage). Similarly, we can't write:

```
let f = open("data.txt")
```

And forget about `f`, the compiler will complain that `f` is never used.


But we can write:

```
let f = open("data.txt")
let f' = write(f, "foo")
close(f')
```

Because each one of `f` and `f'` is used once and only once. And this is the
value of linear types: they constrain how we can use resources and ensure we
handle their lifecycle correctly.

So the basic rules are simple. Then on top of the simple rules you dump a
truckload of convenience features like [borrowing][borrowing] to make it less
onerous for programmers to write linear code.

# Example

If you're convinced that linear types and exceptions don't work together, skip
this section. Otherwise, consider:

```c
try {
  let f = open("/etc/config");
  // `write` consumes `f`, and returns a new linear file object
  let f' = write(f, "Hello, world!");
  throw Exception("Nope");
  close(f');
} catch Exception {
  puts("Leak!");
}
```

(Note: `f'` contains the same underlying file handle as `f`. It is only a new
linear object from the perspective of the type system. The way you'd implement
this is the file access library would represent file handles as opaque linear
types containing a non-linear file pointer that clients can't access directly.)

A linear type system will accept this program: `f` and `f'` are both used
once. But this program has a resource leak: an exception is thrown before `f'`
is consumed.

If variables defined in a `try` block can be used in the scope of the associated
`catch` block, we could attempt a fix:

```c
try {
  let f = open("/etc/config");
  let f' = write(f, "Hello, world!");
  throw Exception("Nope");
  close(f');
} catch Exception {
  close(f');
}
```

Even if the type system accepts this (dubious, because `f'` is potentially used
twice, as far as the type system knows), we can construct a much worse case in
which we break linearity by [closing the file handle twice][dfree]:

```c
try {
  let f = open("/etc/config");
  let f' = write(f, "Hello, world!");
  if (randBool()) {
    puts("True! Closing the file...");
    close(f');
  } else {
    puts("False! Closing then throwing");
    close(f');
    throw Exception("Nope");
  }
} catch Exception {
  close(f');
}
```

Assume for the sake of argument that the type system accepts this (the `try`
block is well-typed). If `randBool()` evaluates to `true`, the file is closed
correctly. If it evaluates to `false`, we close the file twice.

# Why Exceptions?

Exceptions involve implicit control flow, and require sophisticated runtime
support, so we could decide to build a language without any exception handling
mechanisms, and require the programmer to represent errors as values
throughout. The problem with this solution is many ordinary functions are
partial: integer division when the denominator is zero, or any arithmetic
operation when it results in [overflow][overflow], or out-of-bounds array
access. Handling these cases using option types is unnecessarily complicated:
simple arithmetic expressions would turn into a tree of nested pattern matching
expressions to catch intermediate overflow errors, for example.

# Unsound Approaches

These are solutions to the problem of exceptions in a linear type system that
break the guarantees linear types provide.

## Terminate the Process

Instead of handling the exception, [terminate the process][abort].

This is the simplest and coarsest approach. It treats the operating system as a
garbage collector: memory not freed, file handles not closed, etc. will be dealt
with by the operating system. The obvious drawback is calling a
potentially-throwing function without risking termination (e.g., in a unit test
framework) requires spawning a separate process, and communicating through RPC.

## Terminate the Task

Instead of handling the exception, [terminate the task][pthread_cancel], but not
the whole process.

This reduces the cost of safely calling potentially-throwing code: spawning a
thread is cheaper than forking a process, and since caller and callee share an
address space, inter-thread communication can be fast. Resources allocated by
the task, however, will not be freed when the task is terminated. In
long-running servers where individual tasks are terminated occasionally, this
can lead to resource -- memory, file descriptor, socket -- exhaustion.

At the same time, "terminate the process" and "terminate the task" are perfectly
valid solutions for error cases that are truly impossible, but where the
language is not expressive enough to express their impossibility.

Consider a type to represent positive integers:

```sml
signature POSITIVE_INT = sig
  type pos
  val fromInt : int -> pos option
  val toInt : pos -> int
end

structure PositiveInt : POSITIVE_INT = struct
  type pos = int

  fun fromInt i =
    if i > 0 then
      SOME i
    else
      NONE

  fun toInt p = p
end
```

Here, the type `pos` is opaque, and the function `fromInt` takes an integer and
returns an instance of `pos` if the integer is positive, and otherwise returns
`NONE`. This is fine, but if we want to use it with integer constants:

```sml
let p: pos = PositiveInt.fromInt 12
```

This won't type check, because the type of `p` is not `pos` but `pos
option`. But we know that 12 is a positive number. A construct to terminte the
task is useful here, so we could write:

```sml
let p: pos = case (PositiveInt.fromInt 12) of
              SOME p => p
            | NONE => abort "Impossible"
```

The problem with allowing terminate-like constructs is not technical, but
social: with exceptions, programmers write empty catch blocks. With termination
semantics, programmers will be tempted to continually expand the set of
circumstances that are considered "impossible" or "too unlikely" to deal with.

# Sound Approaches

These are approaches that preserve the linearity guarantees.

## Values, not Exceptions

No exception handling. Instead, all potentially-throwing operations return
[union types][union]. This can be made less onerous through syntactic sugar. The
function (in a vaguely Rust-ish syntax):

```c
T nth(array<T> arr, size_t index) throws OutOfBounds {
  return arr[index];
}
```

Can be desugared to:

```c
Result<T, OutOfBounds> nth(array<T> arr, size_t index) {
  case arr[index] {
    Some(elem: T) => {
      return Result::ok(elem);
    }
    None => {
      return Result::error(OutOfBounds());
    }
  }
}
```

This is appealing because much of the hassle of pattern matching `Result` types
can be simplified by the compiler. But this approach is immensely limiting,
because as stated above, many fundamental operations have failure modes that
have to be handled explicitly:

```
add : (Int, Int) -> Result<Int, Overflow>
sub : (Int, Int) -> Result<Int, Overflow>
mul : (Int, Int) -> Result<Int, Overflow>
div : (Int, Int) -> Result<Int, Overflow | DivisionByZero>

nth : (Array<T>, Nat) -> Result<T, OutOfBounds>
```

As an example, consider a data structure implementation that uses arrays under
the hood. The implementation has been thoroughly tested and you can easily
convince yourself that it never accesses an array with an invalid index. But if
the array indexing primitive returns an option type to indicate out-of-bounds
access, the implementation has to handle this explicitly, and the option type
will "leak" into client code, up an arbitrarily deep call stack.

The problem is that an ML-style type system considers all cases in a union type
to be equiprobable, the normal path and the abnormal path have to be given equal
consideration in the code. Exception handling systems let us conveniently
differentiate between normal and abnormal cases.

## Use Integrated Theorem Provers

No exceptions. Division by zero, overflow, and array index out of bounds errors
are prevented at compile time by proving they don't happen.

A full treatment of [abstract interpretation][absint] is beyond the scope of
this article. The usual tradeoff applies: the tractable static analysis methods
prohibit many ordinary constructions, while the methods sophisticated enough to
prove most code correct are extremely difficult to implement completely and
correctly.

## Capture the Linear Environment in the Exception

I call this the "PacLang++ solution".

[PacLang][paclang] is an interesting language: it's an imperative linearly-typed
programming language specifically designed to write packet-processing algorithms
for [network procesors][np]. The paper is worth reading.

Its authors describe the language as:

>a simple, first order, call by value language, intended for constructing
>network packet processing programs. It resembles and behaves like C in most
>respects. The distinctive feature of PacLang is its type system, treating the
>datatypes that correspond to network packets within the program as linear
>types. The target platforms are application-specific network processor (NP)
>architectures such as the Intel IXP range and the IBM PowerNP.

The type system is straightforward: `bool`, `int`, and a linear `packet` type. A
limited form of borrowing is supported, with the usual semantics:

>In PacLang, the only linear reference is a `packet`. An _alias_ to a reference
>of this type, a `!packet`, can be created in a limited scope, by casting a
>`packet` into a `!packet` if used as a function argument whose signature
>requires a `!packet`. An alias may never exist without an owning reference, and
>cannot be created from scratch. In the scope of that function, and other
>functions applied to the same `!packet`, the alias can behave as a normal
>non-linear value, but is not allowed to co-exist in the same scope as the
>owning reference `packet`. This is enforced with constraints in the type
>system:
>
>- A `!packet` may not be returned from a function, as otherwise it would be
> possible for it to co-exist inscope with the owning `packet`
>
>- A `!packet` may not be passed into a function as an argument where the
> owning `packet` is also being used as an argument, for the same reason
>
>Any function taking a `!packet` cannot presume to "own" the value it aliases,
>so is not permitted to deallocate it or pass it to another a thread; this is
>enforced by the signatures of the relevant primitive functions. The constraints
>on the `packet` and `!packet` reference types combined with the primitives for
>inter-thread communication give a _uniqueness guarantee_ that only one thread
>will ever have reference to a packet.

An interesting restriction is that much of the language has to be written in
[A-normal form][anf] to simplify type checking. This is sound: extending a
linear type system to implement convenience features like borrowing is made
simpler by working with variables rather than arbitrary expressions.

The original language has no exception handling system. PacLang++, a successor
with exception handling support, is introduced in the paper _Memory safety with
exceptions and linear types_. The paper is difficult to find, so I will quote
from it often. The authors first describe their motivation in adding exception
handling:

>In our experience of practical PacLang programming, an issue commonly arising
>is that of functions returning error values. The usual solution has been to
>return an unused integer value (C libraries commonly use -1 for this practice)
>where the function returns an integer, or to add a boolean to the return tuple
>signalling the presence of an error or other unusual situation. This quickly
>becomes awkward and ugly, especially when the error condition needs to be
>passed up several levels in the call graph. Additionally, it is far easier for
>a programmer to unintentionally ignore errors using this method, resulting in
>less obvious errors later in the program, for example a programmer takes the
>return value as valid data, complacently ignoring the possibility of an error,
>and using that error value where valid data is expected later in the program.

The linear type system of PacLang:

>A linearly typed reference (in PacLang this is known as the owning reference,
>though other kinds of non-linear packet references will be covered later) is
>one that can be used _only once_ along each execution path, making subsequent
>uses a type error; the type system supports this by removing a reference
>(_consuming_ it) from the environment after use. As copying a linear reference
>_consumes_ it, only one reference (the _owning_ reference) to the packet may
>exist at any point in the program’s runtime. Furthermore, a linear reference
>_must_ be used once and only once, guaranteeing that any linearly referenced
>value in a type safe program that halts will be consumed eventually.

The authors first discuss the exceptions as values approach, discarding it
because it doesn't support intra-function exception handling, and requires all
functions to deallocate live linear values before throwing. The second attempt
is described by the authors:

>At the time an exception is raised, any packets in scope must be consumed
>through being used as an argument to the exception constructor, being "carried"
>by the exception and coming into the scope of the block that catches the
>exception.

This is also rejected, because:

>this method does not account for live packets that are not in scope at the time
>an exception is raised. An exception can pass arbitrarily far up the call graph
>through multiple scopes that may contain live packets until it is caught.

The third and final approach:

>We create an enhanced version of the original PacLang type system, which brings
>linear references into the environment implicitly wherever an exception is
>caught. Our type system ensures that the environment starting a catch block
>will contain a set of references (that are in the scope of the exception
>handling block) to the _exact same_ linear references that were live at the
>instant the exception was thrown.

To illustrate I adapted the following example from the paper, adding a few more
comments:

```c
packet x = recv();
packet y = recv();

// At this point, the environment is {x, y}

try {
  consume(x);
  // At this point, the environment is just {y}
  if (check(!y)) {
    consume(y);
    // Here, the environment is {}
  } else {
    throw Error; // Consumes y implicitly
    // Here, the environment is {}
  }
  // Both branches end with the same environment
} catch Error(packet y) {
  log_error();
  consume(y);
}
```

The authors go on to explain a limitation of this scheme: if two different
`throw` sites have a different environment, the program won't type check. For
example:

```c
packet x = recv();

// Environment is {x}
if (check(!x)) {
  packet y = recv();

  // Environment is {x, y}
  if (checkBoth(!x, !y)) {
    consume(x);
    consume(y);
    // Environment is {}
  } else {
    throw Error; // Consumes x and y
    // Environment is {}
  }
} else {
  throw Error; // Consumes x
  // Enviroment is {}
}
```

While the code is locally sound, one `throw` site captures `x` alone while one
captures `x` and `y`.

Suppose the language we're working with requires functions to be annotated with
an exception signature, along the lines of [checked exceptions][checked]. Then,
if all throw sites in a function `f` implicitly capture a single linear packet
variable, we can annotate the function this way:

```c
void f() throws Error(packet x)
```

But in the above code example, the exception annotation is ambiguous, because
different throw sites capture different environments:

```c
void f() throws Error(packet x)
// Or
void f() throws Error(packet x, packet y)
```

Choosing the former leaks `y`, and choosing the latter means the value of `y`
will be undefined in some cases.

## The PacLang++ Solution, Extended

The PacLang++ solution can be made more sensible with the use of option types:
because environments form a partially ordered set, we can use option types to
represent bindings which are not available at every `throw` site. In the code
example above, we have:

```
{} < {x} < {x, y}
```

So the signature for this function is simply:

```c
void f() throws Error(packet x, option<packet> y)
```

## Use Affine Types

Affine types are a weakening of linear types, essentially linear types with
implicit destructors. In a linear type system, values of a linear type must be
used exactly once. In an affine type system, values of an affine type can be
used at most once. If they are unused, the compiler automatically inserts a
destructor call.

[Rust][rust] does this, and there are good reasons to prefer affine types over
linear types:

1. Less typing, since there is no need to explicitly call destructors.

2. Often, compilation fails because programmers make trivial mistakes, such as
   forgetting a semicolon. A similar mistake is forgetting to insert destructor
   calls. This isn't possible with affine types, where the compiler handles
   object destruction for the programmer.

3. Destruction order is consistent and well-defined.

4. Most linear types have a single natural destructor function: pointers are
   deallocated, file handles are closed, database connections are closed,
   etc. Affine types formalize this practice: instead of having a constellation
   of ad-hoc destructor functions (`deallocate`, `closeFile`, `closeDatabase`,
   `hangUp`), all destructors are presented behind a uniform interface: a
   generic function of type `T! -> Unit`.

The drawbacks of affine types are the same as those of destructors in languages
like C++ and [Ada][finalization], that is:

1. The use of destructors requires compiler insertion of implicit function
   calls, which have an invisible cost in runtime and code size, whereas linear
   types make this cost visible.

2. Destruction order has to be well-specified. For stack-allocated variables,
   this is straghtforward: destructors are called in inverse declaration
   order. For temporaries, this is complicated.

Additionally, destroying values we don't do anything with could lead to bugs if
the programmer simply forgets about a value they were supposed to use, and
instead of warning them, the compiler cleans it up.

But there is a benefit to using affine types with destructors: exception
handling integrates perfectly well. Again, Rust does this: [`panic`][rustpanic]
and [`catch_unwind`][rustcatch] are similar to `try` and `catch`, and
destructors are called by unwinding the stack and calling `drop` on every affine
object. The result is that exceptions are safe: in the happy path, destructors
are called by the compiler. In the throwing path, the compiler ensures the
destructors are called anyways.

The implementation strategy is simple:

1. When the compiler sees a `throw` expression, it emits calls to the destructor
   of every (live) affine variable in the environment before emitting the
   unwinding code.

   That is, given an expression `throw(...)`, where the affine environment up to
   that expression is `{x, y, z}`, the expression is transformed into:

   ```
   free(x);
   free(y);
   free(z);
   throw(...);
   ```

2. When the compiler sees a call to a potentially-throwing function (as
   determined by an [effect system][effect]), it emits a `try`/`catch` block:
   normal excecution proceeds normally, but if an exception is caught, the the
   destructors of all live affine variables are called, and the exception is
   re-thrown.

   Suppose we have a function call `f()`, where the affine environment up to the
   point of that call is `{x, y}`. If the function potentially throws an
   exception, the function call gets rewritten to something like this:

   ```
   let result = try {
     f();
   } catch Exception as e {
     free(x);
     free(y);
     throw(e);
   }
   ```

For a more complete example, a program like this:

```c
void f() throws {
  let x: string* = allocate("A fresh affine variable");
  // Environment is {x}
  g(x); // Environment is {}
}

void g(string* ptr) throws {
  let y: string* = allocate("Another heap-allocated string");
  // Environment is {ptr, y}
  h(ptr); // Environment is {y}
}

void h(string* ptr) throws {
  let z = allocate(1234);
  if (randBool()) {
    throw "Halt";
  }
}
```

Would transform to something like this:

```c
void f() {
  let x: string* = _allocate_str("A fresh affine variable");
  try {
    g(x);
  } catch {
    rethrow;
  }
}

void g(string* ptr) {
  let y: string* = _allocate_str("Another heap-allocated string");
  try {
    h(ptr);
  } catch {
    _free_str(y);
    rethrow;
  }
  _free_str(y);
}

void h(string* ptr) {
  let z = allocate(1234);
  if (randBool()) {
    _free_intptr(z);
    _free_str(ptr);
    throw "Halt";
  }
  _free_intptr(z);
  _free_str(ptr);
}
```

## Two Disjoint Approaches

We don't have to use values to represent any kind of error. Pervasive errors,
such as integer overflow, integer division by zero, and array index out of
bounds errors can simply terminate the program, allowing the operating system to
recover. Higher-level errors can be represented as values using a `Result` type.

This is essentially the [Herbceptions][sutter] approach:

>An alternate result is never an “error” (it is success, so report it using
>return). This includes “partial suc- cess” such as that a buffer was too small
>for the entire request but was filled to capacity so more can be read on the
>next call.
>
>[...]
>
>A programming bug or abstract machine corruption is never an “error” (both are
>not programmatically re- coverable, so report them to a human, by default using
>fail-fast). Programming bugs (e.g., out-of-bounds ac- cess, null dereference)
>and abstract machine corruption (e.g., stack overflow) cause a corrupted state
>that can- not be recovered from programmatically, and so they should never be
>reported to the calling code as errors that code could somehow handle.

In the same vein, the [Midori approach][midori]:

>A recoverable error is usually the result of programmatic data validation. Some
>code has examined the state of the world and deemed the situation unacceptable
>for progress. Maybe it’s some markup text being parsed, user input from a
>website, or a transient network connection failure. In these cases, programs
>are expected to recover. The developer who wrote this code must think about
>what to do in the event of failure because it will happen in well-constructed
>programs no matter what you do. The response might be to communicate the
>situation to an end-user, retry, or abandon the operation entirely, however it
>is a predictable and, frequently, planned situation, despite being called an
>“error.”
>
>A bug is a kind of error the programmer didn’t expect. Inputs weren’t validated
>correctly, logic was written wrong, or any host of problems have arisen. Such
>problems often aren’t even detected promptly; it takes a while until “secondary
>effects” are observed indirectly, at which point significant damage to the
>program’s state might have occurred. Because the developer didn’t expect this
>to happen, all bets are off. All data structures reachable by this code are now
>suspect. And because these problems aren’t necessarily detected promptly, in
>fact, a whole lot more is suspect. Depending on the isolation guarantees of
>your language, perhaps the entire process is tainted.

# Conclusion

There are effectively two solutions:

1. If you absolutely need traditional throw-and-catch exception handling, use
   affine types like Rust.

2. If you want linear types, take the [Herb Sutter][sutter] approach: divide
   errors into two categories, and use different approaches for both:

   1. Integer overflow, array index out of bounds, and division by zero are
      terminating errors: the correct approach is to stop the program in its
      tracks and let the operating system recover.

   2. "File not found", "username is too short" are high-level errors that
      should be represented as values.

# Bibliography

Not much, surprisingly:

1. Thrippleton, Richard, and Alan Mycroft. "Memory safety with exceptions and
   linear types."

2. Tov, Jesse A., and Riccardo Pucella. ["A theory of substructural types and
   control."][subst] ACM SIGPLAN Notices. Vol. 46. No. 10. ACM, 2011.

[austral]: https://github.com/austral/austral
[borrowing]: https://doc.rust-lang.org/beta/rust-by-example/scope/borrow.html
[overflow]: https://en.wikipedia.org/wiki/Integer_overflow
[abort]: https://www.gnu.org/software/libc/manual/html_node/Aborting-a-Program.html
[pthread_cancel]: https://man7.org/linux/man-pages/man3/pthread_cancel.3.html
[dfree]: https://www.owasp.org/index.php/Doubly_freeing_memory
[union]: https://en.wikipedia.org/wiki/Tagged_union
[rust]: https://rust-lang.org/
[rustpanic]: https://doc.rust-lang.org/std/macro.panic.html
[rustcatch]: https://doc.rust-lang.org/std/panic/fn.catch_unwind.html
[absint]: https://en.wikipedia.org/wiki/Abstract_interpretation
[paclang]: https://link.springer.com/chapter/10.1007/978-3-540-24725-8_15
[np]: https://en.wikipedia.org/wiki/Network_processor
[anf]: https://en.wikipedia.org/wiki/A-normal_form
[checked]: https://en.wikipedia.org/wiki/Exception_handling#Checked_exceptions
[effect]: https://en.wikipedia.org/wiki/Effect_system
[finalization]: https://www.adaic.org/resources/add_content/docs/95style/html/sec_9/9-2-3.html
[subst]: https://dl.acm.org/citation.cfm?doid=2048066.2048115
[sutter]: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p0709r4.pdf
[midori]: https://joeduffyblog.com/2016/02/07/the-error-model/
