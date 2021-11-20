---
title: Finding Malloc
summary: A tour through glibc.
---

Today I thought: I'd like to find where [`malloc`][malloc] is defined in
[glibc][glibc]. There's a [mirror][mirror] on GitHub, which makes it easy to
navigate the code from the browser. It's easy enough to find: the root has a
`malloc` directory, with many implementation and test files. As expected,
there's `malloc.h` and there you find:

```c
/* Allocate SIZE bytes of memory.  */
extern void *malloc (size_t __size) __THROW __attribute_malloc__
     __attribute_alloc_size__ ((1)) __wur;
```

A bit of GCC attribute noise, but otherwise understandable. `malloc.c` opens
with an extensive documentation comment, which is very good. Then this is the
very first line of code:

```c
#ifndef void
#define void      void
#endif /*void*/
```

This sets the tone, because all GNU code is like this. Inscrutable,
inexplicable, endlessly-nested macros and GCC builtins. If you dig through
mailing list archives to find the explanation for constructs like this you find
it's a load-bearing hack for a PA-RISC server running GCC 2.1 somewhere in a
warehouse under a freeway in Taipei, it does payroll for TSMC and if you yank
this macro the world economy collapses.

Every actual function is hidden behind layers of declaration macros,
underscore-prefixed internal versions, and weird linker script renames. If you
find a function defined like its prototype in the header file it's like an
accomplishment. And every macro you have to expand ten times to find the actual,
underlying definition. And then you read `#define void void` and you're made to
feel like you've gone insane.

What's wrong with the following?

```c
/* malloc.h */

/** Interface documentation. */
void* malloc(size_t size);

/* malloc.c */

/** Implementation documentation. */
void* malloc(size_t size) {
  ...
}
```

I guess it's not "portable" enough. So, so much of the horror in GNU codebases
is justified through vague allusions to "portability". This is absurd. Your
resources are not infinite. If someone wants modern glibc to build on the 90's
[SPARC laptop][sparc] they use to evade government surveillance from their
prepper hideout: tell them to fork the project.

Further down the same file, you find gems like this:

```c
#define largebin_index_32(sz)                                                \
  (((((unsigned long) (sz)) >> 6) <= 38) ?  56 + (((unsigned long) (sz)) >> 6) :\
   ((((unsigned long) (sz)) >> 9) <= 20) ?  91 + (((unsigned long) (sz)) >> 9) :\
   ((((unsigned long) (sz)) >> 12) <= 10) ? 110 + (((unsigned long) (sz)) >> 12) :\
   ((((unsigned long) (sz)) >> 15) <= 4) ? 119 + (((unsigned long) (sz)) >> 15) :\
   ((((unsigned long) (sz)) >> 18) <= 2) ? 124 + (((unsigned long) (sz)) >> 18) :\
   126)
```

Macros containing both ternary if expressions and unexplained bit-shifting
operations with made up constants explained nowhere. Every time I see a bitwise
operator I have to pull out pen and paper and draw the bitfields, so this isn't
great.

Some things are standard for C programs: multi-thousand-line source files, huge
conditional compilation blocks, twelve levels of indentation. C programmers find
that tolerable. I have a much shallower threshold of complexity: at a couple
hundred lines of code I start thinking about how to split the module up further.

It doesn't have to be this way. The [musl][musl] C library has a [much
simpler][musl-malloc] implementation: 118 lines of code is all there is to
it. It's trivial to find which function implements `malloc`. Maybe it's a sign
of maturity: 30 year old C code is supposed to have a fine patina of superfluous
GCC builtins and indirection, like mussles (ironically) on the keel of a ship.

Again: where's `malloc`? I'm not sure. I think it's a function called
`__libc_malloc`, which is defined 3100 lines into the file (inside a conditional
compilation block) and possibly linked to `malloc` via linker
scripts. Immediately there's [an `if` statement without braces][if]. This is,
objectively, a design error in C syntax that's kept around for compatibility and
because programmers have a weird love of terseness. Not only does it make the
grammar [even _more_ ambiguous][ambig] and thus worse from a [langsec][langsec]
perspective, it has already caused at least [one major bug][gotofail]. But it
saves three bytes (so it's [suckless][suckless] and not bloat) out of ~5800
lines in this file.

I browsed this through the GitHub mirror, but the actual repository is
elsewhere. There's a [gitweb] frontend for browsing, but for contributing it's
the standard GNU approach: mailing lists and patch emails.

Nobody is required to accomodate my ignorance. But I will offer one datapoint: I
was born in 1994 and got the Internet in 2008. By then, forums -- the last
redoubt of the decentralized Internet we had and threw away -- were already on
the way out. I got my first job in 2012. By then, GitHub was universal. I've
never sent a patch file through email, never used `git send-mail`, and I don't
know how mailing lists work (how does [Mailman][mailman] know how to assign
emails to threads?).

Eventually the graybeards who know how all this works are going to retire. Who's
going to pick up the maintenance of GCC, a program whose source code was
[_intentionally obfuscated_][obfus] to prevent GPL violations? Zoomers aren't
hacking Perl. Nobody under 35 knows how [autoconf][autoconf] works. And yet this
is the foundation of the libre software ecosystem.

This saddens me because I genuinely love [Richard Stallman][stallman]'s
vision. The GPL and AGPL are like a peace treaty from a freer world, where
software is post-scarce and is built lovingly for the benefit of all. But the
actual implementation of GNU projects is a pre-modern nightmare and is long-term
unsustainable.

---

I should try to explain the macro business.

What's going on here, formally, is that if `void` is unknown to the preprocessor
you define a preprocessor macro called `void` that evaluates to the _real_
`void`. Why this is necessary is beyond me.

StackOverflow [claims][so] this is from the time before ANSI C, when the `void`
keyword did not exist. ANSI C was released in 1989, 32 years ago. But that
doesn't make sense: if the compiler doesn't know `void` why does the macro
expand to `void`?

What I suspect has happened here is the code used to be something like:

```c
#ifndef void
#define void      char
#endif /*void*/
```

So if the compiler doesn't know `void` you'd define it as `char` so `void*`
macroexpands to `char*` and it's all good. Then eventually someone changed it to
`void` because all compilers know about it and it's still there for some reason.

Again, this is the _first_ bit of code in `malloc.c`, last modified October 29,
2021.

[malloc]: https://man7.org/linux/man-pages/man3/malloc.3.html
[glibc]: https://www.gnu.org/software/libc/
[mirror]: https://github.com/bminor/glibc
[so]: https://stackoverflow.com/questions/11980125/glibc-define-void
[mailman]: https://list.org/
[sparc]: https://en.wikipedia.org/wiki/Tadpole_Computer
[if]: https://wiki.sei.cmu.edu/confluence/display/c/EXP19-C.+Use+braces+for+the+body+of+an+if%2C+for%2C+or+while+statement
[ambig]: https://en.wikipedia.org/wiki/Dangling_else
[langsec]: http://langsec.org/
[gotofail]: https://annexi-strayline.com/blog/posts/1
[gitweb]: https://git-scm.com/book/en/v2/Git-on-the-Server-GitWeb
[obfus]: https://lwn.net/Articles/582697/
[autoconf]: https://www.gnu.org/software/autoconf/
[stallman]: https://stallman.org/
[musl]: https://www.musl-libc.org/
[musl-malloc]: https://git.musl-libc.org/cgit/musl/tree/src/malloc/lite_malloc.c
[suckless]: https://suckless.org/
