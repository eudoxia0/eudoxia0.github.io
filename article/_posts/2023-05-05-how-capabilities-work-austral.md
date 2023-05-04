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
- supply chain attacks
- e.g. leftpad
  - can be leftpad and nothing else
  - can hoover up your disk
  - send it to a remote server, etc.
  - clock access can be used to do timing attacks

# Contents

1. [The Solution](#solution)
1. [Capabilities in Austral](#austral)
1. [Limitations](#limitations)
1. [Auditing](#audit)
1. [A Stricter Model](#strict)

# The Solution {#solution}

- capabilities: unforgeable permission slips
- process-level
  - capscicum
  - pledge
  - etc.
- language level
  - harder
  - needs semantics
    - encapsulation
      - in highly-dynamic languages like python you can dynamically find the type of a cap and instantiate it
    - no type casting
      - casting `void*` to `foo*` should require a capability
    - more generally, unsafe operations should require a capability

# Capabilities in Austral {#austral}

  - not a feature
    - a consequence of linear types
    - except for the root capability, more on this later
  - example
    - network sockets
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

- global uniqueness
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
- not good enough
  - you can have the same supply chain attacks in austral
  - unless you audit all unsafe modules
    - this is more tractable than having to audit all code
    - it's also still hard

# Auditing {#audit}

- when building against a version of a package for the first time, audit
- the lockfile which specifies exact versions of packages to reproducibly build against also tracks whether smth has been audited
- puts a lot of pressure on the user
- doable

# A Stricter Model {#strict}

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
