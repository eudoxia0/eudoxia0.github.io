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

But, as I wrote in the Austral intro post, blaming the programmer will change
nothing:

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

- capabilities
  - unforgeable permission slips
  - grant permission to access some resource, like the filesystem or network
  - ideally arbitrarily granular
    - filesystem access
    - directory access
    - file access
    - file-read access
    - file attributes access
- process-level capabilities widely implemented
  - capscicum
  - pledge
  - etc.
  - backwards compatible
  - operating system can evolve independently of applications written in different languages, runtimes, and eras
- language level
  - harder
  - E language
  - needs semantics
    - encapsulation
      - in highly-dynamic languages like python you can dynamically find the type of a cap and instantiate it
    - no type casting
      - casting `void*` to `foo*` should require a capability
    - more generally, unsafe operations should require a capability

# Capabilities in Austral {#austral}

- austral has capability based security
  - capabilities are represented as linear types
  - because they are linear, they are not copyable
  - they can be surrendered by their owners, but not duplicated
  - or stored in mutable global state
- not a feature
  - a consequence of linear types
  - except for the root capability, more on this later

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

- when building against a version of a package for the first time, audit
- the lockfile which specifies exact versions of packages to reproducibly build against also tracks whether smth has been audited
- puts a lot of pressure on the user
- doable

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
