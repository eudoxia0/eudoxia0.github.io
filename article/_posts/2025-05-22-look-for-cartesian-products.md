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

<img src="/assets/content/look-for-cartesian-products/llvm.svg" alt="A graph with a central node labeled LLVM. Nodes representing C, Ada, Fortran, and Scala have arrows pointing to LLVM. Nodes representing x86, amd64, and arm64 have arrows pointed-to by LLVM." style="margin: auto;" />

Instead of writing $nm$ compilers, we need only write $n$ frontends plus $m$ backends.

# LSP

We have $n$ languages and $m$ editors. For a _very_ long time, we just accepted that if you wanted to use e.g. Python in Emacs, someone had to have written `python-mode`. Or you just accepted a worse experience. And if you're building a new programming language, you need to write the Emacs mode, the Vim plugin, the VS Code plugin, and so on.

The [Language Server Protocol][lsp] (LSP) solves this by creating an intermediate layer: rather than writing $nm$ editor plugins, we can instead write $n$ language-specific LSP servers plus $m$ editor-specific LSP clients.

It is remarkable how much time passed between this $nm$ problem becoming evident (I think it would have been the late 90s, early 2000s) and the introduction of LSP in ~2016. c.f.: https://matklad.github.io/2025/05/20/open-source-cant-coordinate.html

# Format Conversion

TODO

# Object Store APIs

TODO

# The Web

TODO

[lsp]: https://en.wikipedia.org/wiki/Language_Server_Protocol
[LLVM]: https://llvm.org/
