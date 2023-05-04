---
title: How Capabilities Work in Austral
summary: A walkthrough of Austral's capability-based security features.
---

- problem
  - code is permissionless by default
  - all code within an address space has uniform permissions
    - to call any other code
    - to FFI
  - e.g. leftpad
    - can be leftpad and nothing else
    - can hoover up your disk
    - send it to a remote server, etc.
    - clock access can be used to do timing attacks
- solution
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
