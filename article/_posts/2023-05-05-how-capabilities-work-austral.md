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

Language-level capabilities are harder. The language's semantics have to be designed with capabilities in mind, trying to slap capability-security on a language ex post is like trying to slap a type system on a dynamically-typed language. It might work, but you will have soundness issues, and you will cope and say it "doesn't matter in practice".

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
types. Which makes me proud of the language design, since it's a good sign when
good things naturally fall out of a design by logical necessity.

Capabilities are represented as linear types. For an introduction to linear
types in Austral, see X or Y.

Because they are linear, they are not copyable. A piece of code in posession of
a capability can destroy it, or surrender it to someone else, but not send a
copy to someone else and keep theirs. And because there is no global mutable
state, capabilities cannot surreptitiously be stored in a global variable for
acquisition by other code.

Because of Austral's strict module system and encapsulation, capabilities cannot
be created _ex nihilo_. To create a capability, you must prove that you have
access to a higher, more powerful capability. This satisfies all the security
properties we want.

## Example {#example}
- example
  - network sockets
    - interface
    - usage

## Capabilities vs. Values {#values}

- capabilities can:
  - have a value
    - file
    - db handle
    - socket
  - no value: pure type-level permission slip
  - no sharp distinction between caps and linear values

## The Capability Hierarchy {#hierarchy}

- capability hierarchy

## The Root Capability {#root}

- the root capability
  - capabilities cannot be obtained from nowhere
    - you need smth that represents a higher-level capability, conceptually
    - root cap is the base case of this recursion
  - cannot be created within the language
  - exists only at the program entrypoint, which has root cap
  - root can be surrendered immediately at the start of the program
    - that program can't do anything

# Limitations {#limitations}

This section describes the limitations in Austral's current capability security
model.

## Global Uniqueness {#unique}

- global uniqueness
  - e.g. stdio
  - rootcapability type has no contents
  - can't "mark" a capability as acquired and not possible to acquire again
  - two solutions
    - consume the root capability
    - have a larger intermediate capability, that the user has to use with discipline

## Unsafe FFI {#ffi}

- ffi is unsafe
  - unsafe ffi is needed
    - every language has a escape hatch
    - every time you interact with the outside world you need to do unsafe things
      - caveat: vale
    - the question is:
      - how can you bound the dangers of unsafe code?
      - how can you provide a safe interface to unsafe internals?\
  - to use ffi, have to mark module unsafe
  - it's not about complete safety
  - it's about minimizing audit surface area

## Summary {#summary}

- not good enough
  - you can have the same supply chain attacks in austral
  - unless you audit all unsafe modules
    - this is more tractable than having to audit all code
    - it's also still hard

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

- unsafe capability
  - built from rootcap
  - every unsafe operation takes an unsafe cap
    - allocating
    - deallocating
    - pointer arith
    - pointer casting
    - dereferencing a pointer
    - storing to a pointer
  - data structures that do allocation keep a reference to the allocator
    - allocator provides the unsafe cap for use
