---
title: "Type Systems for Memory Safety"
summary: A survey of type systems for memory safety.
card: type-systems-memory-safety.jpg
card_source: |
    [_The Signing of Peace in the Hall of Mirrors_][link], William Orpen, oil on canvas, 1919.

    [link]: https://en.wikipedia.org/wiki/The_Signing_of_Peace_in_the_Hall_of_Mirrors
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
    1. [Borrowing](#borrow)
    1. [First-Class References](#ref1)
    1. [Second-Class References](#ref2)
1. [Languages](#languages)
    1. [Ada](#ada)
    1. [Austral](#austral)
    1. [Cyclone](#cyclone)
    1. [LinearML](#linearml)
    1. [Rust](#rust)
    1. [Val](#val)
    1. [Vale](#vale)
    1. [Vault](#vault)
    1. [Verona](#verona)
1. [Conclusion](#conclusion)
1. [See Also](#see)

# Memory Safety {#safety}

Memory safety is a bundle of things:

1. **Null Safety:** dereferencing a `NULL` pointer is bad. This causes a
   segfault or a `NullPointerException` or `undefined is not a function`,
   depending on your language. This is the easiest one to solve and arguably
   isn't about memory at all.

1. **Buffer Overflow:** indexing past the end of a contiguous chunk of
   memory. This is solved by storing the length of arrays and checking it.

1. **No Use-After-Free:** using a chunk of memory after it has been deallocated,
   e.g.:

    ```c
    free(ptr);
    f(ptr);
    ```

    This is a source of too many security vulnerabilities to count.

1. **Leak Freedom:** all memory that is allocated is freed.

1. **Data Race Freedom:** memory can be used by multiple threads without complex
   runtime access checks (locks, mutexes etc.)

Null safety and buffer overflows are solved by quotidian solutions: option types
and range checks. Use-after-free and leak freedom are harder to enforce, and
require potentially a lot more compile time machinery.

# Resource Safety {#resource}

Use-after-free and leak freedom generalize beyond memory. Some values are also a
_resource_: something that has a lifetime and has to be acquired, used, and
disposed in a particular way.

Bytes and floats are not resources. Heap-allocated memory, file handles, network
sockets, database handles, locks, mutexes, etc. are resources. They have a
contract: the API functions have to be used in a particular order, in a
particular number of times.

# Approaches {#approaches}

This section describes the different approaches to solving memory safety.

## Option Types {#option}

Option types solve null safety. These can be hardcoded into the language
(e.g. Dart, Kotlin) or implemented as a library (OCaml, Haskell, Rust).

Typically, a type `Option<T>` has the size of `T` plus the size of the tag that
says if it's empty or not. If pointers in the language are required to be
non-null, then there is an opportunity for an optimization: types like
`Option<Pointer<T>>` can be the same size as ordinary pointers, because `NULL`
can represent the empty case.

For this reason, option types do not incur performance penalties. In fact, they
can increase performance, by reducing the need to defensively check for `NULL`.

## Region-Based Memory Management {#region}

Region-based memory management is like [arena allocation][arena] at compile
time. It addresses use-after-free errors and memory leaks. Pointers are tagged
with a compile-time tag called a _region_, which is lexically scoped, like so:

[arena]: https://en.wikipedia.org/wiki/Region-based_memory_management

```c
// Declare a lexically-scoped region `R`.
region R {
  // Allocate some data in the region `R`.
  Pointer<int, R> ptr = allocate(10, R);
};
// When the region ends, its memory is deallocated.
```

Leak freedom is guaranteed because when a region's scope ends, its memory is
deallocated. When you reach the end of the program's entrypoint, all regions
have ended and been deallocated.

Use-after-free bugs are solved by the fact that pointers in different regions
are different types. That is, `Pointer<int, R>` is a different type than
`Pointer<int, S>`. You can't do this:

```c
Pointer<int, R> escape;
region S {
  Pointer<int, S> foo = ...;
  escape = foo;
};
```

Because the types of `escape` and `foo` are different. Therefore, pointers
cannot escape the region they are defined in, and therefore their memory cannot
outlive the region.

Regions are both a compile-time tag and a runtime object. The region object is
basically a [slab allocator][slab]. This can improve performance, because most
allocation happens from userspace rather than actually going to `malloc`.

[slab]: https://en.wikipedia.org/wiki/Slab_allocation

Pros:

- Simple to understand.
- Simple to implement.
- Can improve performance by minimizing syscalls.

Cons:

- Does not address concurrency and data race freedom.
- Does not generalize to resource safety.
- If a type has to hold a pointer, the region has to be moved up to a generic
  type parameter, e.g.:

    ```c
    struct<region R> Foo {
      ptr: Pointer<Bar, R>;
    };
    ```

    This is similar to lifetime parameters in Rust.

## Linear Types {#linear}

Linear types are types whose values must be used once, and exactly once.

I've written a lot about this, so in the interest of not repeating myself:

1. [_Introducing Austral_][auintro] explains linear types from the perspective
   of a programmer.
1. [_How Austral’s Linear Type Checker Works_][aucheck] explains linear types
   from the perspective of a compiler.

[auintro]: /article/introducing-austral#linear
[aucheck]: /article/how-australs-linear-type-checker-works

But the basic idea is what it says on the tin: linear values must be used
once. Using a linear value is called _consuming_ it. Ensuring that all linear
values are consumed can be done at compile time.

For example, let `Foo` be some linear type. Then the following doesn't work:

```rust
let x: Foo = f();
// Error: x not consumed
```

And neither does this:

```rust
let x: Foo = f();
g(x);
g(x); // Error: x consumed twice.
```

But this is just right:

```rust
let x: Foo = f();
g(x);
```

We can't do this, for example:

```rust
let x: Foo = f();
if whatever() {
  g(x);
} else {
  // Nothing
}
```

Because the variable `x` is consumed inconsistently: it's consumed in one
branch, but not another. Analogously:

```rust
let x: Foo = f();
while whatever () {
  g(x); // Error: consumed in loop
}
```

Here `x` is potentially consumed infinitely many times. The rules are short,
simple, and easily enforced: the linearity checker in the Austral compiler is
[~700 lines of OCaml][checker].

[checker]: https://github.com/austral/austral/blob/master/lib/LinearityCheck.ml

Leak freedom is solved, trivially: values must be consumed, which ultimately
means deallocated. Use-after-free is solved, trivially: you literally can't use
values twice. And this generalizes to resource safety: linear types need not be
pointers, but can be file handles, sockets, etc. And since every value can only
have one owner, concurrency is solved, trivially.

Oh, and you get [capability-based security][cap] for free.

[cap]: /article/how-capabilities-work-austral

The tradeoff is linear types are very onerous to write. For example, consider a
function that returns the length of a (linear) array. In Rust we might write:

```rust
fn length<T>(array: Array<T>) -> usize { ... }
```

But this doesn't work: because calling this function consumes (and ultimately
deallocates) the array:

```rust
let len = length(arr);
do_something_else(arr); // Error: `arr` already consumed
```

We would have to change the function to instead return the array alongside the
result:

```rust
fn length<T>(array: Array<T>) -> (Array<T>, usize) { ... }
```

And use it like this:

```rust
let (arr2, len) = length(arr);
do_something_else(arr2);
```

Needless to say: this is insane. Nobody wants to write code like this, where the
logic is buried under repeated threading of use-once variables through function
calls. Most languages that use linear types add rules on top that improve
ergonomics while preserving safety. In fact, these relaxations of the linearity
rules constitute most of the semantics. There's a reason Rust's borrow checker
is called a borrow checker and not an ownership checker.

Furthermore, ownership is equivalent to root permissions on a value, so passing
a value to a function lets that function do anything. Borrowing can be thought
of as not just a mechanism for better ergonomics but for graduated permissions:
immutable references, for example, don't allow you to mutate or deallocate data
since they don't own that data.

Another limitation of linear types is they can only represent trees rooted at
variables: interconnected, graph or DAG-like data structures are not directly
representable. More on this below.

Pros:

- Simple semantics.
- Easy to implement.
- Solves general resource safety.
- Improved performance by allowing in-place mutation.
- Solved data races and concurrency.

Cons:

- Linear types [are incompatible with exception handling][exception], at least
  the C++ or Java-style of exception handling.
- Linear types solve resource safety the way a gamma ray burst solves
  hospital-acquired infections. They are not really usable alone: you need extra
  rules to soften the restrictions while preserving safety. More on this below.

[exception]: /article/linear-types-exceptions

## Affine Types {#affine}

Affine types are a slight twist on linear types. Instead of "used exactly once"
it's "used _at most_ once". That is: values can be silently dropped. When a
linear value goes out of scope, the compiler inserts calls to the
destructor. This requires a way to associate a linear type with a destructor
function.

Rust has a sophisticated ownership-tracking system that resembles affine types
if you squint. In Rust, the `Drop` trait is used to tie types to their
destructors. Mostly this is implemented automatically by the compiler, you
usually implement this yourself when dropping a value involves calling a foreign
function (e.g. closing a socket).

The main benefit of affine types is they are more compatible with traditional
exception handling, since the compiler knows how to dispose of values, and
therefore how to call destructors when unwinding the stack.

Pros:
- Less code.
- Exception handling.

Cons:
- Sometimes you do want the compiler to tell you when you've forgotten a value
  (in Rust, the `must_use` attribute exists for this).
- The double-throw problem still exists in Rust: a destructor that fails
  (because e.g. closing the file handle failed, or closing a socket failed) will
  simply abort the program.

## Borrowing {#borrow}

The core idea of borrowing is to suspend linear/affine type rules for some
delimited time. It's kind of a combination of region-based memory management
with linear/affine types.

There's two ways to do this: first-class and second-class references.

## First-Class References {#ref1}

First-class references are "first-class" because they are values like everything
else: they can be passed to functions, returned from functions, stored in
structures etc.

A reference is like a pointer in region-based memory management: a generic type
with two components, the pointed-to type and the lifetime. The lifetime is just
a compile-time tag, references compile down to just plain old fashioned
pointers.

There's usually two kinds of references: read-only (immutable) and read-write
(mutable) references.

In the above example, the function that returns the length of an array would
instead be written:

```rust
fn length<T, 'L>(arr: ReadRef<T, L>) -> usize { ... }
```

(This isn't actual Rust syntax but I'm desugaring things somewhat to make it
easier to understand.)

Here, `'L` is a lifetime parameter. Lifetimes are analogous to regions, except
that, unlike in region-based memory management, where regions are both a
compile-time tag and a runtime storage pool, here lifetimes are just a
compile-time tag. `ReadRef<T, L>` is a read-only (immutable) reference to a type
`T` in the generic lifetime `L`. You would call this like so:

```rust
let arr = make_array(1, 2, 3);
let len = length(&arr);
```

Where `&arr` means "take a read-only reference to the variable
`arr`". Desugaring further for clarity (again not valid Rust):

```rust
let arr: Array<int> = make_array(1, 2, 3);
{
    // The lifetime `L` is valid only in this scope.
    letlifetime L;
    let ref0: ReadRef<int, L> = &arr;
    let len: usize = length(ref0);
}
```

So, rather than write code like this:

```rust
let foo = make_foo();
let (foo1, bar) = do_something(foo);
let (foo2, baz) = do_something_else(foo1);
let (foo3, quux) = do_whatever(foo2);
dispose(foo3);
```

We can write code like this:

```rust
let foo = make_foo();
let bar = do_something(&foo);
let baz = do_something_else(&mut foo);
let quux = do_whatever(&foo);
dispose(foo);
```

The code is cleaner and easier to read. Another difference is the functions get
just enough permissions to get by, and no more: in the before example,
`do_something` took ownership of `foo` and could do anything with it. In the
after example, `do_something` takes an immutable reference to `foo`, so it can
only read data from it, or call functions that take an immutable reference to
the same type. Analogously, `do_something_else` takes a mutable reference to
`foo`, so it can read or write to `foo` (or, transitively, call functions that
take an immutable or mutable reference to the type of `foo`) but it can't call
`dispose` because it doesn't have full ownership.

In case this helps understand, let's desugar the code:

```rust
let foo: Foo = make_foo();
{
    letlifetime L0;
    // `ref0` can't leak because the lifetime `L`
    // is only valid in this scope.
    let ref0: ReadRef<Foo, L0> = &foo;
    let bar = do_something(ref0);
}
{
    letlifetime L1;
    let ref1: MutRef<Foo, L1> = &mut foo;
    let baz = do_something_else(ref1);
}
{
    letlifetime L2;
    let ref2: ReadRef<Foo, L2> = &foo;
    let quux = do_whatever(ref2);
}
dispose(foo);
```

We can also visually annotate the lifetimes:

```rust
let foo: Foo = make_foo();                   // ----------\
{                                            //           |
    letlifetime L0;                          //  ---\     |
    let ref0: ReadRef<Foo, L0> = &foo;       //     | L1  |
    let bar = do_something(ref0);            //  ---/     |
}                                            //           |
{                                            //           |
    letlifetime L1;                          // ---\      |
    let ref1: MutRef<Foo, L1> = &mut foo;    //    | L2   | Foo
    let baz = do_something_else(ref1);       // ---/      |
}                                            //           |
{                                            //           |
    letlifetime L2;                          // ---\      |
    let ref2: ReadRef<Foo, L2> = &foo;       //    | L3   |
    let quux = do_whatever(ref2);            // ---/      |
}                                            //           |
dispose(foo);                                // ----------/
```

So, borrowing improves ergonomics and gives us more granular access control. But
how can we convince ourselves that it preserves the safety properties?

Borrowing preserves safety in three ways:

1. As in region-based memory management, references are lexically scoped and
   have a lifetime. The compiler ensures the lifetime of the reference does not
   exceed the lifetime of the value. This means no use-after-free bugs: the
   reference cannot outlive the thing it points to.
2. Again as in region-based memory management, the fact that lifetimes are part
   of the reference means references to different lifetimes are considered
   different types. This means you can't leak references by storing them in a
   data structure or variable that is out of the lexical scope where the
   reference is defined because the lifetimes don't match (usually however there
   are subtyping rules between lifetimes, to make things more convenient).
3. Finally, for references to be sound they must implement the law of
   exclusivity.

The **Law of Exclusivity** is this: at all times, a value is either:

1. Not borrowed (owned).
2. Borrowed _immutably_, with any number of immutable references live at the
   same time.
3. Borrowed _mutably_, with one and only one mutable reference.

Mutable and immutable references are mutually exclusive: you can't have an
immutable and mutable reference to the same value live at the same time. To
understand why, imagine a type like this:

```
enum MaybeBox {
  Empty,
  Full(Box<String>),
};
```

That is: an `EmptyBox` is either a pointer to a string, or nothing. Suppose you
have both an immutable reference `readref` and a mutable reference `mutref` to
the box at the same time, and that the box has contents:

<img src="/assets/content/type-systems-memory-safety/box1.svg" style="margin-left: auto; margin-right: auto;"/>

Then you transform the read-reference to the box to a reference to its contents:

<img src="/assets/content/type-systems-memory-safety/box2.svg" style="margin-left: auto; margin-right: auto;"/>

Then you use the mutable reference to empty the box. Now the read reference is dangling:

<img src="/assets/content/type-systems-memory-safety/box3.svg" style="margin-left: auto; margin-right: auto;"/>

And now you have unsoundness: your read reference is invalid. Mutable references
are mutually exclusive with each other for the same reason.

One more point. Functions that take references as parameters must make the
lifetimes into generic function parameters, like so:

```rust
fn concatenate_strings<'a, 'b, 'c>(s1: &'a str, s2: &'b str, s3: &'c str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

Rust has a feature called lifetime elision that simplifies writing code like
this. Because most functions that take references don't return a reference type,
the compiler lets you write the function signature without lifetime annotations,
and adds them for you. The following:

```rust
fn concatenate_strings(s1: &str, s2: &str, s3: &str) -> String {
    format!("{}{}{}", s1, s2, s3)
}
```

Is equivalent to the function above. The compiler simply makes an implicit,
generic lifetime parameter for each reference type mentioned in the parameter
list. This is the most general solution: if different arguments have the same
lifetime it won't be a problem.

However, if you want to enforce that two parameters must come from the same
lifetime, you need to write the lifetime parameters explicitly.

Pros:

- **Ergonomics:** you can write code that reads data from linear/affine values,
  without consuming them or having to return them as tuples.
- **Permissions:** references are like permissions: a read-reference says "you
  can read, but not mutate or deallocate". A mutable reference says "you can
  read and mutate, but not deallocate". Linear ownership says "you can do
  anything".
- **Safety:** lifetimes soften the linearity rules while preserving safety, due
  to the law of exclusivity, and the fact that lifetimes are scoped.

Now references seem like a silver bullet: we get safety and ergonomics. What are
the problems?

Cons:

1. **Complexity:** lifetime analysis can be complex. The complexity is more or
   less proportional to how easy it is to write code that "does what you mean"
   and have it compile. In other words, ergonomics come at the cost of language
   complexity.

   To make code easier to write you need all sorts of heuristics and special
   cases. These complicate the analysis, making the compilers harder to
   implement, and making the language rules harder to understand.

2. **Learning Curve:** related to the above, the main problem people report with
   Rust is the learning curve. People often describe learning Rust as fighting
   the borrow checker until they develop an intuitive sense for how it
   works. It's hard to write down a small, core set of rules for Rust borrow
   checking.

   There are many things to understand: non-lexical lifetimes and implicit
   reborrowing for example.

3. **Lifetime Creep:** as in region-based memory management, if you want to
   store a lifetime in a data structure, you need to make that lifetime into a
   generic parameter. That is, if you want to write:

   ```rust
   struct Foo {
       bar: &Bar
   }
   ```

   You have to write:

   ```rust
   struct Foo<'L> {
       bar: &'L Bar,
   }
   ```

   And so, any data structure that stores references, the lifetime annotations
   never disappear. There isn't really a way around this: the compiler needs to
   know the lifetime to ensure safety.

   And so the usual advice for storing references in data structures in Rust is
   "don't". In fact, when I read Rust codebases, the main place where I see
   references stored in structures is with iterators.

   And so there's a tradeoff: you can write code where everything is deeply
   intertwined with pointers, but keeping the lifetimes straight can be a mess.

4. **Disuse:** because of the difficulty of writing code that uses lifetimes
   pervasively, many people don't use lifetimes, opting instead to use `Rc` or
   `Arc`, which lower compile-time complexity by introducing the run-time cost
   of reference counting.

   Another common pattern in Rust is to use indices into an array where you
   would otherwise use pointers. For example, if you were writing a binary tree
   in C, you might use:

   ```c
   struct Tree {
       Tree* left;
       Tree* right;
   };
   ```

   In Rust you will frequently see something like this:

   ```
   struct Tree {
       nodes: Vec<Node>,
       root: usize,
   };

   struct Node {
       left: usize,
       right: usize,
   };
   ```

   That is: the tree is a vector of nodes, and rather than use pointers or
   references, each node uses integer indices into the array to make the tree
   structure.


   There are many benefits to this pattern. For one, it is the only way to
   implement a graph-like data structure. It can improve performance, because
   allocating a node doesn't necesarilly require `malloc`, the vector acts as a
   resizable arena allocator. It also improves cache locality, because the nodes
   are not randomly distributed throughout the heap but kept close together.

   The problem is you lose safety:

   1. There is no guarantee that an index is in bounds.
   2. There is no guarantee that an index points to the right data. Any change
      to the node vector has to update the indices in all the nodes.
   3. There is no way to link indices and trees. If you use an index from one
      tree into another, you could trigger an index-out-of-bounds error or get
      wrong data.

   So we go right back to all the problems of memory-unsafe languages, but one
   level of abstraction up. These problems can be reduced by careful design: you
   can build a data structure that exposes a more-or-less safe API by having
   unsafe internals. But you can do the same thing in C or C++.

5. **Closures / Objects:** closures or objects with runtime dispatch are hard,
   because the whole point of a closure or an object is to hide the type of the
   things that it closes over. But if a closure captures a value by reference
   the compiler needs to track its lifetime.

   Rust gets around the limitations of closures in a fairly involved way that
   nevertheless preserves safety and likely is the best you're going to get in a
   language with explicit lifetime analysis.

## Second-Class References {#ref2}

If you study Rust codebases you may notice references are used in a very skewed
way:

- ~90% of references are just function arguments.
- A small number of functions return references.
- A vanishingly small number of data structures have lifetime annotations.
- Overwhelmingly, the main place where references are stored in data structures
  is in the case of iterators.

Graydon Hoare, who built the first versions of Rust and can be called its
creator, has this to say about lifetimes:

>**First-class &.** I wanted & to be a "second-class" parameter-passing mode,
>not a first-class type, and I still think this is the sweet spot for the
>feature. In other words I didn't think you should be able to return & from a
>function or put it in a structure. I think the cognitive load doesn't cover the
>benefits. Especially not when it grows to the next part.
>
>**Explicit lifetimes.** The second-class & types in early Rust were analyzed
>for aliasing relationships to ensure single-writer / multi-reader (as today's
>borrows are) but the analysis was based on type and path disjointness and (if
>necessary) a user-provided address-comparison disambiguation. It did not reason
>about lifetime compatibility nor represent lifetimes as variables, and I
>objected to that feature, and still think it doesn't really pay for
>itself. They were supposed to all be inferred, and they're not, and "if I were
>BDFL" I probably would have aborted the experiment once it was obvious they are
>not in fact all inferred. (Note this is interconnected with previous points:
>the dominant use-cases have to do with things like exterior iterators and
>library-provided containers).

In other words: drop lifetimes and simplify the borrow checker. How does this
work?

**Second-class references** are called second class because:

1. You can't return them from functions.
2. You can't store them in data structures.
3. Can only be created at function call sites, e.g. by writing `f(&x)`.

This massively simplifies borrow checking. Because of these rules, you don't
have to tag references with a compile-time tag to ensure safety. In other words:
you don't need lifetime annotations at all.

Since references can only be created at call sites, and cannot escape the
functions they are passed into, you get lifetime scoping for free: the reference
is guaranteed to not outlive the function.

Since references cannot be stored in data structures, you can't leak them or
cause them to outlive the thing they point to.

And so you get borrow checking without lifetimes. The only borrow checking you
need is, at function call sites, you need to enforce the law of
exclusivity. That is, a function call like:

```rust
f(&mut x, &mut x);
```

Will be rejected by the compiler, because you have two mutable references to the
same thing live at once. Similarly, a call like:

```rust
f(&x, &mut x);
```

Will also be rejected, because you have both an immutable and a mutable
reference live at once. Borrow checking has to extend into field access
expressions, so, for example, this is fine:

```rust
f(&mut pos.x, &mut pos.y);
```

Because these are mutable references to disjoint values.

And that's pretty much all there is. The borrow checker is simple to implement,
and, more importantly, it is easier for programmers to learn how to use the
language. Since there are no lifetimes, there are no complicated lifetime error
messages to worry about.

References can't be stored in data structures, but they can be stored in
variables, but only if they originate as parameters to functions. That is, you
can write:

```rust
fn foo(ref: &int) {
  let r: &int = ref;
}
```

This doesn't break any rules, since the reference is guaranteed to only live
within the scope of the function call.

Is this realizable? Yes, since, as stated above, most Rust code is already
written this way. The fact that lifetime elision works at all is due to the fact
that most references appear as arguments to functions.

Second-class references are implemented in the [Val programming language][val],
under the name of [Mutable Value Semantics][mvs].

[val]: https://www.val-lang.dev/
[mvs]: https://www.jot.fm/issues/issue_2022_02/article2.pdf

What are the drawbacks? Well, the rules themselves:

1. Returning references from functions is a common operation for data
   structures. For example, if you have a reference to an array, you might want
   to get a reference to the _n_-th element. If you have a reference to a `Box`,
   you might want to get a reference to its contents, and so on recursively.

2. Storing references in data structures is rarely done. But when it is needed,
   Rust gives you the tools to preserve safety via explicit lifetimes. In a
   language with second class references, you would have to turn those
   references into unsafe pointers.

   In that sense, Rust provides a nice middleground: most uses of lifetimes can
   be automatically elided away. But if you want to do something more complex,
   you don't have to drop down to writing unsafe code. You can preserve safety
   with lifetime analysis, it's just that the code becomes more complex to
   write.

3. Not being able to store references in data structures makes it hard to
   implement iterators.

An example of something second-class references can't implement safely: Rust's
HashMap has an Entry API. An Entry is a struct has has a reference to a key and
a reference to a value, this lets you iterate over key/value pairs. And this
requires both the ability to store references in data structures and the ability
to return a reference from a function.

So there's a tradeoff here: explicit lifetimes are harder to understand,
implement, and code with, but they preserve safety; second-class references give
us a simpler mental model of ownership and borrowing at the cost of expressivity
and being strictly less safe than Rust.

But just as borrowing softens linear/affine ownership while preserving safety,
maybe we can soften some of the restrictions of second-class references. The
goal is to keep the simplicity while increasing safety and expressivity.

Note that most of the below are just my own made-up musings. As far as I know
Val is the only languages with second-class references and it solves these
problems differently.

### Returning References

A safe way to return references from functions is to have a special class of
function---call it a _reference transform_---that is allowed to have a reference
in its return type. For example, to transform a reference to an array into a
reference to the _n_-th element, you might have:

```rust
transform get_nth<T>(arr: &Array<T>, idx: usize) -> &T
```

Reference transforms can only be called as arguments to other function
calls. That is, you can do this:

```
print(get_nth(&arr, n));
```

But not this:

```
let nthref = get_nth(&arr, n);
print(nthref);
```

In other words: calling a reference transform is like taking a reference to a
value, it can only appear at a function call site. Or, in yet other words:
reference transforms can only appear sandwiched between regular function calls,
and the ampersand expression.

And this applies to l-values too. If you have a function:

```rust
// Store an int at the location given by a mutable reference.
fn store_int(ref: &mut int, value: int) {
  *ref = value;
}
```

A reference transform call can also appear in the l-value position, for example:

```
// Given two references to an integer, return the one that
// points to the smallest value.
transform min(a: &mut int, b: &mut int) -> &mut int {
  if *a < *b {
    return a;
  } else {
    return b;
  }
}

fn main() {
  let mut a = 10;
  let mut b = 20;
  // Store `30` in the reference returned by min.
  min(&mut foo, &mut bar) = 30;
  // a = 30
  // b = 20
}
```

### Storing References

C# has a concept of ["ref structs"][refstruct], data structures that can hold
references but themselves inherit the limitations of C# reference types.

[refstruct]: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/ref-struct

Analogously, a language with second-class references could have a ref struct
type that can hold references, but:

1. Cannot be returned from functions (except reference transforms).
2. Cannot be stored in data structures (except other ref structs).
3. Can only be created at function call sites.

Suppose you have a function that needs a lot of dependencies to work, and you
pass them as references, like so:

```rust
f(&a, &b, &c, &mut d, foo);
```

You could define a ref struct to group them together, and make the code a bit
cleaner:

```rust
f(Context(&a, &b, &c, &mut d), foo);
```

### Iterators

The [`Iterator`][iter] trait in Rust looks like this:

[iter]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

```rust
pub trait Iterator {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
}
```

An iterator is typically implemented as a struct that holds a reference to the
collection and an index:

```rust
pub struct VecIterator<'a, T> {
    vec: &'a Vec<T>,
    index: usize,
}

impl<'a, T> VecIterator<'a, T> {
    pub fn new(vec: &'a Vec<T>) -> Self {
        VecIterator { vec, index: 0 }
    }
}

impl<'a, T> Iterator for VecIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.vec.len() {
            let result = Some(&self.vec[self.index]);
            self.index += 1;
            result
        } else {
            None
        }
    }
}
```

And you'd use it like this:

```rust
fn iter_and_print<'a>(mut iterator: VecIterator<'a, i32>) {
    while let Some(num) = iterator.next() {
        println!("{}", num);
    }
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    iter_and_print(VecIterator::new(&numbers));
}
```

This suggests how we might implement iterators in a version of Rust with
second-class references. You'd define a ref struct for the iterator:

```rust
ref struct VecIterator<T> {
    vec: &Vec<T>,
    index: usize,
}
```

The function that creates a `VecIterator` should be a reference transform, since
it has to be able to return a `ref struct`:

```rust
transform make_iter(vec: &Vec<T>) -> VecIterator<T> {
    VecIterator { vec, index: 0 }
}
```

And the trait implementation (some desugaring to make it simpler):

```rust
impl<T> Iterator for VecIterator<T> {
    transform next(iter: &mut VecIterator<T>) -> RefOption<&T> {
        if iter.index < iter.vec.len() {
            let result = Some(&self.vec[self.index]);
            self.index += 1;
            result
        } else {
            None
        }
    }
}
```

(Note that we need a separate option type, `RefOption`, to hold references. This
would be a `ref enum` rather than a `ref struct`.)


A usage like:

```rust
fn iter_and_print(mut iterator: VecIterator<i32>) {
    for elem in iterator {
        println!("{}", elem);
    }
}
```

Would desugar to somthing along the lines of:

```rust
fn iter_and_print(mut iterator: VecIterator<i32>) {
    while iterator.can_advance() {
        step(next(&mut iterator).unwrap());
    }
}

fn step(elem: &i32) {
    println!("{}", elem);
}
```

Note: throughout all of this code, not one lifetime annotation! The only
drawback here is that iteration requires breaking the code up into small
functions, since iterators can only be created at call sites.

### Closures

Closures that capture values by reference can be implemented analogously: as
`ref structs` that can only be create at call sites, cannot be stored except in
other ref structs, etc.

### Conclusion

Pros:

1. Greatly simplifies borrow checking.
2. Easier to understand.
3. Easier to implement.

Cons:
1. The problem with ref structs is it splits the type system in two: you need a
   `RefOption` type, a `RefVec`, etc.
1. Potentially, some of the things Rust can do with lifetimes require unsafe
   code in second-class references.

# Languages {#languages}

This section describes how different languages implement memory and resource
safety.

## Ada {#ada}

Ada is a lovely language that is unfairly maligned. People deride it as big,
bloated, designed-by-committee, the reality is it's been a better C++11 since
1983.

Ada's pointers are called "[access types][adaacc]". They're just pointers. Since
Ada 2005 you have `not null access` types, which provide null-safety, but the
ergonomics are very poor (you can't, for example, free a `not null` pointer).

[adaacc]: https://en.wikibooks.org/wiki/Ada_Programming/Types/access

Ada has [generics][adagen], but like in [Modula-2][mod2] these require explicit
instantiation. It's like the module system in Standard ML or OCaml. You can
write an `Option` type in Ada, but you can't write `Option<Foo>`, rather, you
have to instantiate the `Option` module with `Foo` as an argument and get a new
type `Option_Foo`. When programming in the large this leads to horrendous code
duplication.

[adagen]: https://en.wikibooks.org/wiki/Ada_Programming/Generics
[mod2]: https://en.wikipedia.org/wiki/Modula-2

Ada has lexically-scoped types. You can write something like this:

```ada
procedure Foo is
  type Integer_Access is access all Integer;
begin
  -- etc.
end Foo;
```

And the type `Integer_Access` is live only for the lexical scope of
`Foo`. Access types are associated with a "storage pool", an allocator. Since
the types are different, every pointer of type `Integer_Access` is guaranteed to
be in the same storage pool.

I _think_ the idea is that, like in region-based memory management, when a type
goes out of scope its storage pool is deallocated, but this doesn't happen
unless you specify the maximum size of the storage pool like so:

```ada
for Integer_Access'Storage_Size use 123;
```

In discussions of Ada you often hear this strange slogan: that the Ada spec
_allows_ but does not _require_ a garbage collector. Which isn't helpful at all.

Ada also has "[limited types][adalim]", which are kind of an eighties
approximation of linear types. Limited types disallow assignment, equality, and
composition (putting them inside other types). That is, they can only be
operated on using the API provided by the module which defines them. By
implementing a finalizer procedure you can ensure deallocation (Ada has
destructors and automatically inserts calls to those destructors).

[adalim]: https://en.wikibooks.org/wiki/Ada_Programming/Types/limited

Links:

- [Ada 2012 Language Reference Manual](http://www.ada-auth.org/standards/ada12.html)
- [Rationale for Ada 2012](http://www.ada-auth.org/standards/rationale12.html)
- Papers:
  - [Safe and Leakproof Resource Management using Ada83 Limited Types](https://www.plover.com/~mjd/misc/hbaker-archive/LimitedRoots.html)
  - [How to Steal from a Limited Private Account](https://www.plover.com/~mjd/misc/hbaker-archive/LimitedRobbery.html)
  - [Structured Programming with Limited Private Types in Ada](https://www.plover.com/~mjd/misc/hbaker-archive/LPprogram.html)

## Austral {#austral}

[Austral][austral] is the programming language I designed and implemented. It
uses plain linear types with explicit destructors for resource safety and
capability based security, and a simple borrowing model that's essentially just
a simplified version of Rust's borrow checker (lexical lifetimes and an explicit
`borrow` construct that the ampersand operator desugars to).

[austral]: https://austral-lang.org/

## Cyclone {#cyclone}

- a research language
  - safer version of C
  - type safe
  - safe memory
  - null checking
- superficially, cyclone looks like c
- extends c with new features
  - non-null pointers
    - `foo@` is the non-null version of `foo*`
  - fat pointers (pointer + size)
    - `foo?` is the fat pointer version of `foo*`
    - so strlen etc. work on `char?` instead of `char*`
  - compile-time region based memory management
    - pointers can be tagged with regions
    - syntax resembles rust
    - `char?'r` is a fat pointer to the region `r`
    - regions = lifetimes
    - `region` statement starts a new named region: `region r { ... ])`
    - `rmalloc` takes a region and a size and returns a region-tracked pointer

Links:

- [Cyclone](https://en.wikipedia.org/wiki/Cyclone_(programming_language)) on
  Wikipedia.
- [Cyclone home page](https://cyclone.thelanguage.org/)
  - [Rationale](https://cyclone.thelanguage.org/wiki/Why%20Cyclone/)
  - [Documentation](https://cyclone.thelanguage.org/wiki/User%20Manual/)
- Papers:
  - [Cyclone: A safe dialect of C](http://www.cs.umd.edu/projects/cyclone/papers/cyclone-safety.pdf) (2002)
  - [Cyclone: A Type-Safe Dialect of C](http://www.cs.umd.edu/~mwh/papers/cyclone-cuj.pdf) (2005)
  - [Region-Based Memory Management in Cyclone](http://www.cs.umd.edu/projects/cyclone/papers/cyclone-regions.pdf)
  - [Linear Regions Are All You Need](https://www.cs.cornell.edu/people/fluet/research/substruct-regions/ESOP06/esop06.pdf)

## LinearML {#linearml}

[LinearML][linearml] is a small prototype programming language, along the lines
of Standard ML or OCaml, featuring linear types. It also implements [linear
observers][linearml-obs], which are like a very lightweight version of references.

[linearml]: https://github.com/pikatchu/LinearML
[linearml-obs]: https://github.com/pikatchu/LinearML/wiki/Tutorial#observed-values

## Rust {#rust}

- the elephant in the room
- first practical language with ownership types
- originally was gonna be like a linearly-typed erlang
- gradually become a safer C++
- ownership and borrow checking

### Move Semantics

- move is the default

### Drop

When a value goes out of scope, it is said to be _dropped_. Rust inserts calls
to the appropriate destructor. Dropping can be customized by implementing the
`Drop` trait, this allows you to do RAII and e.g. close a network socket in the
destructor:

```rust
struct Foo {}

impl Drop for Foo {
    fn drop(&mut self) {
        println!("drop(Foo)");
    }
}

fn main() {
    let _ = Foo {};
}
```

This will print:

```
drop(Foo)
```

### Borrow Checking

- rust does something very practical
  - it says what safety properties it will enforce
  - but it doesn't say how
  - so the borrow checker is free to evolve, in the direction of accepting more programs over time
- tradeoff
  - rust becomes more ergonomic over time
  - you can use rust while hardly understanding how it works
    - i use rust professionaly and hardly think about lifetimes
    - writing rust doesn't feel like a puzzle or a chore of fitting the problem to the language
    - rather, you just write code, and generally the compiler will accept it
  - on the other hand, there's no specification of the borrow checker
    - that means you can't have multiple rust compilers, because the spec of the borrow checker is "whatever rustc does at present"
    - people say having multiple compilers is bad because it wastes effort
    - i think there is merit to this argument
    - but there is value in stability, and in a language being describable by spec rather than implementation

### Lexical Lifetimes

- rust's first lifetimes were lexical
  - tied to lexical scope
  - this is the simplest way to implement lifetimes
- lifetimes stretched to the end of a lexical scope
- so you couldn't do this
  - example
- because the lifetime ofthe reference stretches to the lifetime of the value you're borrowing

### Non-Lexical Lifetimes

- lexical lifetimes are exactly rectangles in the source text
- they are convex, don't have hole
- this is a problem with e.g. conditional code, where a ref is only used in one branch

### Exception Handling

- rust has panic
- panic unwinds the stack, and calls destructors
- the unwinding code is inserted by the compiler

### Links

- [Rust for "modern" C++ devs][rust-slides]: slides from a 2022 talk by Graydon
  Hoare.
- [The Rust I Wanted Had No Future][rust-future]: retrospective by Graydon Hoare.
  - [HN comments][rust-future-hn]
  - [Lobste.rs comments][rust-future-lb]
  - [Reddit comments][rust-future-reddit]
- Papers:
  - [RustBelt: Securing the Foundations of the Rust Programming Language](https://dl.acm.org/doi/pdf/10.1145/3158154)

[rust-slides]: http://venge.net/graydon/talks/RustForModernCPPDevs.pdf
[rust-future]: https://graydon2.dreamwidth.org/307291.html
[rust-future-hn]: https://news.ycombinator.com/item?id=36193326
[rust-future-lb]: https://lobste.rs/s/47amaq/rust_i_wanted_had_no_future
[rust-future-reddit]: https://www.reddit.com/r/rust/comments/1415is1/the_rust_i_wanted_had_no_futur/e

## Val {#val}

- new programming language
- high level systems programming
- based on mutable value semantics
- kind of a combination of swift and rust
  - compiler written in swift
- val has a semantics that aims to let you write code without thinking about
  references at all
  - rather, references are kind of like an optimization that happens under the
    hood
  - and instead you can think in higher-level concepts
- val has an essentially linear type system
  - all values are trees, rooted at program variables
- as in essentially all linear type systems, references (borrowing) is
  introduced to relax some of the rules and improve the ergonomics
- in val, references are second-class, which means:
  - they're not quite a type, but more like a second-class parameter passing mode
- concretely, the (simplified) semantics are:
  - references can only be created at function calls
  - references cannot be returned from functions
  - references cannot be stored in structures
  - references follow the law of exclusivity
  - references don't have lifetime annotations
  - "borrow checking" is just checking that references at call sites are disjoint
- this is a simplified version, as a starting point. I will elaborate the actual
  semantics in the following sections

### Local References

- i lied
- references can be created outside function boundaries
- `let y = &x`
- a lightweight analysis pass ensures these are used correctly
- kind of like a lighter rust
- so to use references you don't have to split everything up into a thousand
  tiny functions
- lifetime annotations are not needed since all the information of lifetimes is
  in the control flow graph

### Subscripts

- subscripts are kind of like a special kind of function that is allowed to return references
- it's not quite returning the reference, rather, it's like a coroutine
- it takes its inputs, and a callback
- and "returns" the reference by calling the callback with the reference
- subscripts are basically used in place of what, in rust, would be a function
  that takes a reference to an object and returns a reference to an interior
  value

### Remotes

- some structures can have "remote parts"
- this relaxes the restriction on not being able to store references in data
  structures
- nevertheless remotes don't have to be annotated with lifetimes
- soundness is maintained, i think, by the fact that structs with remotes are
  not `Moveable` or `Copyable`, so the lifetime analysis pass can detect
  unsoundness without lifetime annotations
  - im not sure exactly how it works

### Closures

- closures can capture things by reference
- a closure works exactly like a struct with a remote part

### Links

- [Home page](https://www.val-lang.dev/)
- [Language tour](https://tour.val-lang.dev/)
- [Discussions](https://github.com/orgs/val-lang/discussions)
  - [How do remote parts and closure captures work?](https://github.com/orgs/val-lang/discussions/754), where yours truly tries to suss out the semantics.
- Papers:
  - [Implementation Strategies for Mutable Value Semantics](https://www.jot.fm/issues/issue_2022_02/article2.pdf)
  - [Native Implementation of Mutable Value Semantics](https://arxiv.org/pdf/2106.12678.pdf)
  - [The Val Object Model](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2676r0.pdf)
- [Value Oriented Programming Needs Implicits?](https://matklad.github.io/2023/05/02/implicits-for-mvs.html#Value-Oriented-Programming-Needs-Implicits)
  - [Comments on lobste.rs](https://lobste.rs/s/uptebu/value_oriented_programming_needs)

## Vale {#vale}

[Vale][vale] is a new programming language for safe systems programming. It uses
a combination of compile-time and run-time checks to provide
safety. Specifically, it provides [generational references][genref] and the
language semantics allow the compiler to elide many of the runtime checks
through [region borrowing][regborrow]. I don't have a clear mental model of how
it works. But one interesting feature is [Fearless FFI][fearlessffi], which uses
a separate stack to isolate FFI calls and create a language that doesn't need
unsafe escape hatches to interact with external code.

[vale]: https://vale.dev/
[genref]: https://verdagon.dev/blog/generational-references
[regborrow]: https://verdagon.dev/blog/first-regions-prototype
[fearlessffi]: https://verdagon.dev/blog/fearless-ffi

Links:

- [Vale website](https://vale.dev/)
  - [Vale's Memory Safety Strategy: Generational References and Regions](https://verdagon.dev/blog/generational-references)

## Vault {#vault}

Vault was a research programming language from Microsoft Research. There's
little left on the Internet about it, the best description I found of it was a
paper titled [_Enforcing High-Level Protocols in Low-Level Software_][hlp]. It
provides memory safety using an interesting and unique compile-time approach
that is vaguely similar to region-based memory management. What follows is
mostly paraphrased straight from the paper.

From the abstract:

>The Vault programming language allows a programmer to describe resource
>management protocols that the compiler can statically enforce. Such a protocol
>can specify that operations must be performed in a certain order and that
>certain operations must be performed before accessing a given data
>object. Furthermore, Vault enforces statically that resources cannot be
>leaked. We validate the utility of our approach by enforcing protocols present
>in the interface between the Windows 2000 kernel and its device drivers.

Vault tracks resources using _keys_, which are compile-time tokens. The compiler
maintains a _held-key set_. Types can be parameterized by keys to ensure that
values of those types are only accessed when the given key is in the held-key
set. In turn, functions can be annotated with an expression with describes how
calling that function modifies the held-key set, essentially an effect system.

Keys are like linear values in that they cannot be duplicated or discarded, and
every key represents one distinct resource. Tracked types are types
parameterized by keys:

```c
Tracked<Point, K> point = new Point { x = 10, y = 20 };
```

I've changed the syntax to make this more intelligible. Here we're constructing
an instance of `Point`, and the `new` operator allocates memory for it and
stores it on the heap. `Tracked<T, K>` is the type of tracked (keyed) pointers,
the key is created by the `new` operator and given the name `K`. After this
call, the held-key set is `{K}`.

For the remainder of the lifetime of `K`, the compiler knows that every type
`Tracked<Point, K>` refers to the same `Point` object.

Just as `new` adds a key to the held-key set, `free` removes it:

```c
// Key set: {K}
free(point);
// Key set: {}
```

Functions can be annotated with an effect clause that describes how they change
the held-key set. For example:

```c
void fclose(Tracked<File, K> f) [-F];
```

Here, the effect clause `[-F]` indicates that the function removes `F` from the
key set.

There's an extra dimension of expressivity in that keys can be in one of some
number of states, and the effect clause of a function can describe key
preconditions like "`K` must be in state `S` before call" and postconditions
like "`K` will be in state `S'` after call". This is a key part of protocol
enforcement: you can, force example, specify that the key guarding a file
pointer could be in various states like `unopened`, `opened`, `closed`, and that
closing a file requires the key be in the `open` state and transition to the
`closed` state.

The main limitations the authors identify with this approach are the linearity
of keys, and the fact that keys track scalar resources, creates difficulties
when dealing with collections. So this is essentially region-based memory
management with "type-level linear types" to enforce transitions between
protocol states.

Links:

- [Enforcing High-Level Protocols in Low-Level Software][hlp]

[hlp]: http://people.eecs.berkeley.edu/~necula/Papers/vault01.pdf

## Verona {#verona}

[Verona][verona] is a new research programming language from Microsoft
Research. The main idea seems to be to "lift" linearity from the single-object
level to the collection level (linear regions) in order to make the language
safer by compartmentalizing memory.

[verona]: https://en.wikipedia.org/wiki/Project_Verona

Links:

- [Wikipedia](https://en.wikipedia.org/wiki/Project_Verona)
- [GitHub repository](https://github.com/microsoft/verona)
- [Microsoft: We're creating a new Rust-like programming language for secure coding](https://www.zdnet.com/article/microsoft-were-creating-a-new-rust-based-programming-language-for-secure-coding/)
  - "If we want compartments, and to carve up the legacy bits of our code so
    [attackers'] exploit code can't get out, what do we need in the language
    design that can help with that?"
  - "The ownership model in Verona is based on groups of objects, not like in
    Rust where it's based on a single object. In C++ you get pointers and it's
    based on objects and it's pretty much per object. But that isn't how I think
    about data and grammar. I think about a data structure as a collection of
    objects. And that collection of objects as a lifetime."
  - "So by taking ownership at the level of ownership of objects, then we get
    much closer to the level of abstraction that people are using and it gives
    us the ability to build data structures without going outside of safety."

# Conclusion {#conclusion}

# See Also {#see}

- Papers:
  - [Ownership Types for Safe Programming: Preventing Data Racesand Deadlocks](https://www.cse.msu.edu/~cse914/Readings/ownershipTypesForSafeProgramming-oopsla02.pdf)
