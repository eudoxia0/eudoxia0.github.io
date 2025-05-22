---
title: Look for Cartesian Products
summary: TODO
card: TODO
math: yes
---

A decision process to find opportunities for innovation: look for $n \times k$ problems that can be factored into $n+k$ problems by introducing an intermediate layer.

# Compilers

We have $n$ languages and $m$ instruction set architectures. Naively, we might have: a C compiler that targets x86, another that targets ARM, and so on:

|       | C         | C++ | Fortran | Scala |
| x86   | `c2x86`   | ... | ...     | ...   |
| amd64 | `c2amd64`  | ... | ...     | ...   |
| arm64 | `c2arm64` | ... | ...     | ...   |
