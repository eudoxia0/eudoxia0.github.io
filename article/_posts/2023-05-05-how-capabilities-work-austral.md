---
title: How Capabilities Work in Austral
summary: A walkthrough of Austral's capability-based security features.
card: how-capabilities-work-austral.jpg
card_source: |
    “Labyrinth, Giovanni Battista Piranesi, 1750, engraving, from the British Museum”, DALL-E, June 2022.
---

Code is permissionless by default.

Rather, all code within the same address space runs with _uniform permissions_,
despite different modules having different degrees of trustworthiness. If you
download a `left-pad` library, it might implement leftpad, but it might contain
malware that hoovers up your home directory and sends it to a server
somewhere. This is called a supply chain attack.

And there is nothing in the semantics of most programming languages that lets
you prevent this. Anything can access the FFI, the filesystem, the network,
etc. Even clock access is dangerous, since timing information is useful in
carrying out timing attacks.

The transitive closure of dependencies in modern applications is huge. The
`node_modules` directory on a humble React app I have on my laptop is 460 MiB,
and that's just React and ProseMirror. Most applications are a thin layer of
business logic sitting atop a huge pile of library code, out of sight and out of
mind. Nobody can audit all of it (though LLMs might make a dent here), and
malware can be hidden in very subtle edge cases of the semantics of a language.

What's the solution? The graybeard will blame the programmer, say we need fewer
dependencies and that any competent dev can re-write Figma in vanilla.js over a
weekend. The HN commenter will blame the programmer for failing to audit the
quarter-million lines of code in their dependencies.

But, as I wrote in the [Austral intro post][intro], blaming the programmer will
change nothing:

[intro]: /article/introducing-austral

>If planes were flown like we write code, we’d have daily crashes, of course,
>but beyond that, the response to every plane crash would be: “only a bad pilot
>blames their plane! If they’d read subparagraph 71 of section 7.1.5.5 of the
>C++, er, 737 spec, they’d know that at 13:51 PM on the vernal equinox the wings
>fall off the plane.”
>
>This doesn’t happen in aviation, because in aviation we have decided,
>correctly, that human error is an intrinsic and inseparable part of human
>activity. And so we have built concentric layers of mechanical checks and
>balances around pilots, to take on part of the load of flying. Because humans
>are tired, they are burned out, they have limited focus, limited working
>memory, they are traumatized by writing executable YAML, etc.

Discipline doesn't fix type errors: type systems do. Discipline doesn't fix
memory leaks and buffer overflow: ownership types do. Similarly, security
vulnerabilities will not be fixed by demanding superhuman discipline but by
building languages with safer semantics.

# Contents

1. [The Solution](#solution)
1. [Capabilities in Austral](#austral)
   1. [Example](#example)
   1. [Capabilities vs. Values](#values)
   1. [The Capability Hierarchy](#hierarchy)
   1. [The Root Capability](#root)
1. [Limitations](#limitations)
   1. [Irrevocability](#irrevocability)
   1. [Global Uniqueness](#unique)
   1. [Unsafe FFI](#ffi)
   1. [Summary](#summary)
1. [Future Work](#future)
   1. [Auditing](#audit)
   1. [A Stricter Model](#strict)

# The Solution {#solution}

The solution is capability-based security. A capability is an unforgeable token
that grants access to some permissioned resource, like the filesystem or the
network or an accurate clock. Anything that should be locked down should require
a capability to access.

And, ideally, capabilities should be arbitrarily granular: requiring a
capability to access the filesystem as a whole, read and write, removes a good
chunk of security vulnerabilities. But we can go further: we can constraint
access to a directory and its contents, or to a specific file, or to a specific
file in read-only mode, and so on.

Capabilities at the process and operating system level are widely implemented:
Capscisum, Fuchsia, pledge, seccomp. These are typically more coarse-grained
than what you can do with language-level support, but they're easier to
implement, because you can implement capability security around a completely
untrusted, unaudited codebase, written in any language, runtime, or era.

Language-level capabilities are harder. The language's semantics have to be
designed with capabilities in mind, trying to slap capability-security on a
language ex post is like trying to slap a type system on a dynamically-typed
language. It might work, but you will have soundness issues, and you will cope
and say it "doesn't matter in practice".

The reason it's hard is, you'd typically represent capabilities as types, and
most programming languages don't give hard guarantees about the provenance of
types. In C and C++ you can in principle cast anything into anything. In Python
or Common Lisp or other dynamic languages, you can dynamically search for a
class by name and instantiate it anywhere. Unsafe operations---precise what
capability-based security is meant to constraint---let you get around that very
security.

# Capabilities in Austral {#austral}

Austral "supports" capability-based security. Supports in quotes because it is
not a first-class feature: capability security is simply a consequence of linear
types (except for the `RootCapability`---more on this later). Which makes me
proud of the language design, since it's a good sign when good things naturally
fall out of a design by logical necessity.

Capabilities are represented as linear types. For an introduction to linear
types in Austral, see X or Y.

Because they are linear, they are not copyable. A piece of code in possession of
a capability can destroy it, or surrender it to someone else, but not send a
copy to someone else and keep theirs. And because there is no global mutable
state, capabilities cannot surreptitiously be stored in a global variable for
acquisition by other code.

Because of Austral's strict module system and encapsulation, capabilities cannot
be created _ex nihilo_. To create a capability, you must prove that you have
access to a higher, more powerful capability. This satisfies all the security
properties we want.

## Example {#example}

The following is the API for a network socket library that is capability-secure:

```austral
module Network is
    type NetworkCapability: Linear;

    generic [R: Region]
    function acquire(root: &![RootCapability, R]): NetworkCapability;

    function surrender(netcap: NetworkCapability): Unit;

    type Socket: Linear;

    generic [R: Region]
    function open(
        netcap: &![NetworkCapability, R],
        host: String,
        port: Nat16
    ): Socket;

    function close(socket: Socket): Unit);

    -- ... the rest of the socket API ...
end module.
```

The `Network` module exports a `NetworkCapability` type, and two lifecycle
functions: `acquire` and `surrender`. The `acquire` function takes a reference
to the `RootCapability`, which is the equivalent of global God-mode permissions,
and returns a `NetworkCapability`.

The `NetworkCapability` type is _opaque_: it is declared, but not defined, in
the module API file. Which means it can be imported and mentioned by other
modules, but it cannot be constructed by code outside the `Network`
module. Austral is absolutely strict about this. The only way to create a
`NetworkCapability` outside this module, therefore, is via the `acquire`
function.

Similarly, `Socket` is a linear value that wraps some internal, unsafe socket
handle. Since `Socket` is a linear type, you can also think of it as a
capability: having a value of type `Socket` gives you the capability to read or
write from that socket or `close` it.

Analogously, `Socket` has two lifecycle functions: `open` takes a reference to
the network capability, a host and a port and returns a `Socket` (error handling
is elided for clarity), and `close` takes a `Socket`, closes it, and consumes
it.

The way you'd use this is:

```austral
-- Assuming we have a variable `root` holding the RootCapability.
let netcap: NetworkCapability := acquire(&!root);
let socket: Socket := open(&!netcap, "example.com", 80);
-- We can surrender the capability immediately after opening the socket, since
-- we don't need it for anything else.
surrender(netcap);
-- Do something with the socket.
close(socket);
```

## Capabilities vs. Values {#values}

There isn't a sharp distinction between capabilities and linear types.

Capabilities can have be empty records, holding no values. In that case they are
pure type-level permission slips. Typically such capabilities are "broad": a
network capability, or a filesystem capability likely wouldn't have a pointer to
anything.

But linear types that have values---like a linear `File` type that wraps an
unsafe file handle, or a network socket, or a database handle, etc.---can also
be thought of as capabilities, especially if the API is designed such that those
types can only be constructed by proving you have a capability to use that API.

## The Capability Hierarchy {#hierarchy}

Capabilities form a hierarchy. Again, this hierarchy isn't an inheritance
hierarchy, and it's not built into the language. It's a hierarchy that is
implicit in the functions that let you create a capability from another, more
powerful capability.

For example, in the above code example, the hierarchy looks like this:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/how-capabilities-work-austral/hierarchy.svg"/>

Analogously, a capability-security filesystem API with granular permissions
might look like this:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/how-capabilities-work-austral/fs-hierarchy.svg"/>

With each capability type providing functions to constraint it further, until we
get to the leaf nodes like `FileAttrsRead` (e.g. read a file's modification
time)

## The Root Capability {#root}

The root capability is the only part of Austral's capability-security model that
is built into the language.

As mentioned above, capabilities cannot be created out of thin air, you have to
pass a proof (a reference) that you own a broader, more powerful capability. The
root capability is the base case of the recursion: it represents the highest
level of permissions.

Values of type `RootCapability` cannot be created in userspace. The root
capability is only available as the first argument of the entrypoint
function. Where in C an entrypoint might look like this:

```c
int main(int argc, char** argv) {
    printf("Hello, world!\n");
    return 0;
}
```

In Austral the entrypoint function looks like this:

```austral
function main(root: RootCapability): ExitCode is
    -- Some code here.
    surrenderRoot(root);
    return ExitSuccess();
end;
```

The entrypoint is all-powerful. The design pattern here is that the entrypoint
should acquire the capabilities that it needs (e.g. filesystem access, network
access), then surrender the root, and call some other function with those
capabilities:

```
function main(root: RootCapability): ExitCode is
    -- Acquire some capabilities.
    let netcap: NetworkCapability := acquireNetwork(&!root);
    let fscap: FileSystemCapability := acquireFileSystem(&!root);
    let termcap: TerminalCapability := acquirTerminale(&!root);
    -- Surrender the root.
    surrenderRoot(root);
    -- Pass our capabilities to some other function.
    mainInner(netcap, fscap, termcap);
    -- Finally, exit.
    return ExitSuccess();
end;
```

But also, a program can immediately surrender the root and acquire no
capabilities. With such an entrypoint, you are guaranteed that the program is
useless: that it does nothing but warm the CPU and exit.

# Limitations {#limitations}

This section describes the limitations in Austral's current capability security
model.

## Irrevocability {#irrevocability}

Capabilites are, by default, irrevocable. Code that owns a capability can
surrender it (by consuming the linear value), but there's no built-in mechanism
for the owner of a higher-level capability to revoke it.

This _can_ be implemented, however, using pointers and access control
lists. E.g. a `File` capability might hold an internal, opaque ID and a
reference to the `FileSystem` capability that created it. Then all operations
that use `File` follow that reference and check the `File` ID against an access
control table in the `FileSystem` capability. This can be implemented and
_would_ work, but it has to be implemented explicitly, every time you want to do
this, for each capability type. There is no "automatic revoke".

It also has the drawback that, by holding a reference, the `File` type is now
tied to the lifetime of the `FileSystem` capability.

## Global Uniqueness {#unique}

In the above example, you can do this:

```austral
function main(root: RootCapability): ExitCode is
    -- Acquire the network capability twice.
    let netcap1: NetworkCapability := acquireNetwork(&!root);
    let netcap2: NetworkCapability := acquireNetwork(&!root);
    -- ...
```

And this is fine: different subsystems might each require a separate network
capability, e.g. an HTTP server and a database client.

But what about the terminal? Terminal access should _probably_ be globally
unique: once you acquire a terminal capability, you shouldn't be able to do so
again until the original capability has been surrendered.

Otherwise, you could hand a terminal capability to two separate threads, each of
which logs its work to the terminal, and the output will appear interleaved in a
non-deterministic way.

In the current model, the only way to implement the global uniqueness of
capabilities something like this:

```austral
module Unique is
    type UniqueCapability: Linear;

    function acquire(root: RootCapability): UniqueCapability;

    function surrender(cap: UniqueCapability): RootCapability;
end module.
```

That is: the `UniqueCapability` _contains_ the `RootCapability`, and returns it
to the client at the end of its existence. And this is inconvenient, because we
can only have _one_ globally unique resource live at any one time.

So this is an area where, at present, the only solution is programmer
discipline: the programmer should be informed that some capabilities should only
be acquired once for the duration of the program, and they take the
responsibility when they fail to do this.

## Unsafe FFI {#ffi}

Every programming language needs a escape hatch. When interacting with the
outside world, you have to do unsafe things. The question is: can you draw a
boundary around unsafe code, so that is is clearly visible and demarcated, and
can you help programmers identify and audit the unsafe parts of their code?

Austral's FFI is unsafe. The only limit to using the FFI, calling foreign
functions, and doing unsafe pointer arithmetic is that any module that does this
must be marked unsafe. The idea is that we can then separate safe and unsafe
modules and thus reduce the auditing burden on end-users.

## Summary {#summary}

The model is good---but not good enough. You can still have supply-chain attacks
in Austral, the only thing that is reduced is the scope of auditing: rather than
having to audit every line of code, you only have to audit unsafe modules, and
verify that 1) they don't do anything nasty and 2) they wrap their unsafe
internals in a safe, capability-secure API.

So the improvement is quantitative and partial, rather than qualitative and
absolute.

# Future Work {#future}

This section describes different ways in which Austral's capability-based
security might evolve to support greater safety.

## Auditing {#audit}

One solution is to force the user to audit new packages. When you build your
project for the first time, the [dependency solver][solver] will create a
lockfile with the exact version of every package to build against. Initially,
all of those as marked as unaudited.

[solver]: /article/dependency-resolution-made-simple

Then the build system makes you go through the unsafe modules of each
dependency, you read the code and accept or reject each module. If you build
against unaudited packages you get a warning, if everything is audited, you get
no warnings.

The pro is that this is tractable: it can be implemented using the existing
capability model, using ordinary technology, and only unsafe modules have to be
audited for safety, unlike most languages where you'd have to audit every single
line.

The drawback is that is it still requires a lot of elbow grease from the
programmer. Maybe a web-of-trust, collective (paid?) auditing solution can
replace manual auditing by the end user.

## A Stricter Model {#strict}

The main safety limitation in Austral's current capability security model is the
FFI.

FFI code is completely unsafe, code that imports `Austral.Memory` is completely
unsafe. The only safety check is that modules that do this must be marked unsafe
with `pragma Unsafe_Module;`. But there is no obligation that the programmer
audit these modules. So there's a faultline here: capabilities are a userspace
construct, and unsafe modules are a language-level construct.

Maybe we can unify them, and achieve greater safety?

The approach described here is much more tedious to use, but it is safer and
more precise.

The basic idea is to introduce a new capability called `Unsafe`, that
is acquired from a root capability:

```austral
module Austral.Unsafe is
    type Unsafe: Linear;

    generic [R: Region]
    function acquire(root: &![RootCapability, R]): Unsafe;

    function surrender(unsafe: Unsafe): Unit;
end module.
```

All unsafe operations (pointer arithmetic, pointer casting, calling an FFI
function) require passing an FFI capability. So, while the `Austral.Memory`
module currently looks like this:

```austral
module Austral.Memory is
    type Address[T: Type]: Free;
    type Pointer[T: Type]: Free;

    generic [T: Type]
    function allocate(): Address[T];

    generic [T: Type]
    function load(pointer: Pointer[T]): T;

    generic [T: Type]
    function positiveOffset(pointer: Pointer[T], offset: Index): Pointer[T];

    -- ...
```

Under this model it would look something like:

```austral
import Austral.Unsafe ( Unsafe );

module Austral.Memory is
    type Address[T: Type]: Free;
    type Pointer[T: Type]: Free;

    generic [T: Type, R: Region]
    function allocate(unsafe: &![Unsafe, R]): Address[T];

    generic [T: Type, R: Region]
    function load(unsafe: &![Unsafe, R, pointer: Pointer[T]): T;

    generic [T: Type, R: Region]
    function positiveOffset(unsafe: &![Unsafe, R, pointer: Pointer[T], offset: Index): Pointer[T];

    -- ...
```

Similarly, every FFI function that is defined has an implicit parameter added to
at the beginning of its parameter list: it needs to take a reference to the
`Unsafe` capability.

This has a number of advantages:

1. **Symmetry:** the concept of unsafe modules is no longer needed. So we can
   get rid of the `Unsafe_Module` pragma. The `Austral.Memory` module can be
   imported by any other module, but its functions cannot be used unless the
   client has an `Unsafe` capability.

1. **Granularity:** the scope of "unsafe" is made a lot more granular: no it is
   no longer that some modules are unsafe, and others are safe, and we have to
   audit the _entirety_ of an unsafe module. Rather: anything that takes an
   `Unsafe` capability is potentially unsafe and becomes an auditing target.

And some disadvantages:

1. **False Positives:** some foreign functions are side-effect free. Do we have
   to pass an `Unsafe` capability to calculate the `sin` of a number? That's not
   very convenient.

1. **Allocation:** any data structure that does memory allocation needs to take
   and hold on to an `Unsafe` capability, or maybe something more specific.

   Alternatively, we could have allocator objects that take and hold on to the
   `Unsafe` capability, and can dispense blocks of memory using it. Then
   anything that needs memory allocation takes a value that implements the
   `Allocator` type class or something along those lines.

Since `Unsafe` is very broad it might be sub-capabilities: a `ForeignCapability`
type to call a foreign function, an `AllocationCapability` type to call the
`allocate` function in `Austral.Memory`, etc.

So if you're building an API that wraps an unsafe, foreign library, the API
might look something like this:

```austral
import Austral.Unsafe ( Unsafe );

module LibFoo is
    type FooCapability: Linear;

    function acquire(unsafe: Unsafe): FooCapability;

    function surrender(cap: FooCapability): Unsafe;

    -- Other functions that use `FooCapability`.
    -- ...
end module.
```

Internally, `FooCapability` would be a record that holds on to the `Unsafe`
capability:

```austral
import Austral.Unsafe ( Unsafe );

module body LibFoo is
    record FooCapability: Linear is
        unsafe: Unsafe;
    end;

    function acquire(unsafe: Unsafe): FooCapability is
        return FooCapability(unsafe => unsafe);
    end;

    function surrender(cap: FooCapability): Unsafe is
        let { unsafe: Unsafe } := cap;
        return unsafe;
    end;

    -- etc.
end module body.
```

And the rest of the API would take references to the `FooCapability`
capability. Internally, each of those functions would then transform that into a
reference to the inner `Unsafe` value, and pass that to any FFI functions that
have to be called.

# Conclusion

The current model works, but might benefit from being made stricter. I'll have
to experiment with this to see what works in practice.

# See Also:

- [RFC: Safer Capabilities](https://github.com/austral/austral/discussions/438)
