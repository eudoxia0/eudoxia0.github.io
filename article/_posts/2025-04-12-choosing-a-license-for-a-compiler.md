---
title: Choosing a License for a Compiler
summary: How to license a compiler without inconveniencing users.
card: choosing-a-license-for-a-compiler.webp
---

Choosing a license for a software project is usually easy: people know whether they prefer [permissive] or [copyleft] licenses. Compilers are unique, however, in that parts of the compiler's own source text might end up in the compiled output, which complicates the licensing situation.

[permissive]: https://en.wikipedia.org/wiki/Permissive_software_license
[copyleft]: https://en.wikipedia.org/wiki/Copyleft

I'm not a lawyer, and this is not legal advice, just my best effort to elucidate the problem.

# Why Compilers Are Different

A compiler takes source text in one language and emits object code or source text in another language. From the user's perspective, the compiler's license should not affect the compiled output. But in practice compilers don't just _transform_ code, they _add_ to it. Often the compiler emits a block of code, called a "prelude" or "runtime", alongside the compiled output.

Sometimes the runtime code is embedded directly in the compiler's source text (and therefore subject to the compiler's license) and is emitted directly into the resulting object code. In the case of [GCC], the runtime is a library that is built separately and dynamically linked to the compiled output, but which is part of the GCC source tree and subject to the same license as GCC.

[GCC]: https://en.wikipedia.org/wiki/GNU_Compiler_Collection

For example: suppose the language you're compiling requires arithmetic to trap on overflow. What you likely do is implement a function in the runtime that abstracts the logic of checking for overflow and aborting, e.g.:

```c
int32_t trapping_add(int32_t lhs, int32_t rhs) {
    int32_t result;
    if (__builtin_add_overflow(lhs, rhs, &result)) {
        fprintf(stderr, "Integer overflow.\n");
        abort();
    }
    return result;
}
```

Then, to compile an arithmetic operation like `a+b`, you emit a call to `trapping_add(a, b)`.

Now the compiled output includes both the user's code and the runtime. So what is the license of the whole? It depends: on the license of both the compiler and the runtime; and whether the runtime is emitted into the same object file as the user's code, or linked statically, or linked dynamically.

For example, if the compiler and the runtime are copyleft, with no linking exceptions, then compiled output is likely also copyleft. This is bad since the user should be able to use the compiler without worrying that it will impose legal obligations on them.

So the choice of license is both complicated and consequential. However, there's a few well-trod paths we can follow.

# What To Do

This section explains my licensing preferences, conditional on whether you want the compiler to be permissively licensed or copyleft.

## Permissive

[Apache 2.0][ap] with the [LLVM Exception][ex]. This is the license used by the [LLVM] project. Apache 2.0 is a good default choice for a permissive license, and if the wording of the exception is good enough for LLVM it's good enough for me.

[ap]: https://spdx.org/licenses/Apache-2.0.html
[ex]: https://spdx.org/licenses/LLVM-exception.html
[LLVM]: https://en.wikipedia.org/wiki/LLVM

## Copyleft

[GPLv3] with the [GCC Runtime Library Exception][rle]. People have been using GCC for 38 years without any license issues.

[GPLv3]: https://en.wikipedia.org/wiki/GNU_General_Public_License
[rle]: https://www.gnu.org/licenses/gcc-exception-3.1

# What Not To Do

This section describes a couple licensing situations that create problems.

## Copyleft without Runtime Exceptions

Do not do this unless you understand the consequences. This risks making all compiled output subject to the same license as the compiler itself.

## Copyleft Compiler, Permissive Runtime

This seems reasonable enough: make the compiler GPL with a linking exception, and the runtime permissively licensed. But there's a subtle complication.

The compiler and the runtime are intimately linked. As the project evolves you want to have the freedom to shunt code across them, so what was once the responsibility of the compiler becomes the responsibility of the runtime, and vice-versa. But if the compiler is copyleft and the runtime is permissively licensed, you can't do that. Or, rather: you can move code from the runtime to the compiler, but if you move code from the compiler to the runtime, the runtime is now GPL, unless the necessary rites of re-licensing and copyright assignment have been performed.
