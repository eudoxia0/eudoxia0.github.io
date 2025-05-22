---
title: Look for Cartesian Products
summary: TODO
card: TODO
math: yes
---

A decision process to find opportunities for innovation: look for $n \times m$ problems that can be factored into $n+m$ problems by introducing an intermediate layer.

# Compilers

We have $n$ languages and $m$ instruction set architectures. Naively, we might have $nm$ compilers: a C compiler that targets x86, another that targets ARM, and so on:

|       | C         | Ada | Fortran | Scala |
| x86   | `c2x86`   | ... | ...     | ...   |
| amd64 | `c2amd64` | ... | ...     | ...   |
| arm64 | `c2arm64` | ... | ...     | ...   |

But we don't. Instead we have [LLVM]: a portable assembly language that can any language can be compiled to, and which can in turn compile to any instruction set architecture:
