---
title: How Capabilities Work in Austral
summary: A walkthrough of Austral's capability-based security features.
card: how-capabilities-work-austral.jpg
card_source: |
    “Labyrinth, Giovanni Battista Piranesi, 1750, engraving, from the British Museum”, DALL-E, June 2022.
---

- the problem
- code is permissionless by default
- all code within an address space has uniform permissions
  - to call any other code
  - to FFI
- e.g. leftpad
  - can be leftpad and nothing else
  - can hoover up your disk
  - send it to a remote server, etc.
- more generally, many things can be abused:
  - filesystem access
  - network access
  - clock access can be used to do timing attacks
- this is called a supply chain attacks
  - the transitive closure of the dependencies of a modern application is huge
  - react hello-world node_modules
    - 460MiB
- what is the solution?
  - the graybeard
    - blame the programmer
    - say we need fewer dependencies
    - says any competent dev can write a modern web app in vanilla js over a weekend
  - a differnet type of HN commenter will:
    - blame the programmer for failing to audit 500 MiB of node_modules
    - says they should have read the manual^H source
  - blaming human error isn't good enough
  - as i wrote in the article introducing austral
    - human error is inescapable
  - we need an automated, mechanical solution
    - just as type systems fix type errors
    - ownership and borrowing fixes memory errors
    - capability-based security fixes permission errors

# Contents

1. [The Solution](#solution)
1. [Capabilities in Austral](#austral)
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
- example
  - network sockets
    - interface
    - usage
- capabilities can:
  - have a value
    - file
    - db handle
    - socket
  - no value: pure type-level permission slip
  - no sharp distinction between caps and linear values
- capability hierarchy
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
