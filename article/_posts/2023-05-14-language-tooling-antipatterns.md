---
title: Language Tooling Antipatterns
summary: We will build a tower of Webpack wrappers to reach heaven.
---

Earlier today I tried to write a small project starter template for
[OCaml][ocaml], since tooling is the weakest link to getting started with that
language. After a thousand little sources of friction I was defeated, so I
decided to write this post instead.

[ocaml]: /article/two-years-ocaml

# Contents

1. [Not Being Like Cargo](#cargo)
1. [Environment Variables](#envvar)
1. [Shell Dotfiles](#shell)
1. [Explicit Environments](#env)
1. [Using a DSL](#dsl)
1. [Local Vendoring](#vendor)
1. [Shotgun Tooling](#shotgun)
1. [No Pinning](#pinning)

# Not Being Like Cargo {#cargo}

[`cargo`][cargo] is the gold standard of language tooling.

[cargo]: https://doc.rust-lang.org/book/ch01-03-hello-cargo.html

I don't ask for much. I don't need a language server or a time-traveling
debugger. All I need is a build system and a package manager and a project
skeleton generator, ideally rolled up into one. I want the `cargo` workflow:

```bash
$ foo new hello
$ cd hello
$ foo build
$ foo run
./Hello, world!
$ foo test
19/19 tests passed.
```

That's all I want. Because I want to _use the language_. If I have to learn how
the tooling works, the DX is automatically bad, because we know how tooling
should work. It should work like `cargo`.

Sometimes, it may be permissible to deviate from the `cargo` workflow, e.g. if
your language is [very alien][lisp] and merits a different workflow.

[lisp]: https://lisp-lang.org/

# Environment Variables {#envvar}

If your build system requires a `LANGUAGEPATH` environment variable it's already
over. Just take a sledgehammer to the whole stack and start over. Environment
variables are the worst of global mutable state with the worst of
[stringly-typed][string] systems.

[string]: https://wiki.c2.com/?StringlyTyped

Language tooling should be as stateless as possible. It should store whatever
global data it needs in user-local [XDG directories][xdg]. It should refer only
to its XDG data and to the contents of the current directory.[^env]

[xdg]: https://wiki.archlinux.org/title/XDG_Base_Directory

Cargo doesn't need me to manage environment variables. Why do you?

# Using a DSL {#dsl}

"Build files are programs written in TwingoScript, you see---" Nope. Not
learning another language. 99% of the time, I want build descriptions to be
declarative, static data, and not programs. For the 1% of cases, accidentally
Turing complete JSON is still better than having a full-blown language all of
the time.

This also applies, but less so, to using some other fanciful configuration
language. I can't even spell [Dhall][dh] most of the time. I'd rather write dumb
JSON.

[dh]: https://dhall-lang.org/

# Shell Dotfiles {#shell}

Stop asking me to put things in my `.bashrc`. It is not for you. There is never
any sound engineering reason for this. See the above point: tooling should be as
stateless as possible.

Cargo doesn't require me to modify my shell config. Why do you?

# Explicit Environments {#env}

Like virtualenv and its offspring. Explicit as in: the user has to create a
named environment (usually specifying a compiler version), and manually enter
it. This is never really necessary, from an engineering perspective.

The fact that in many languages, you cannot have multiple versions of the same
library installed at once does _not_ mean you need explicitly managed
environments: the build system can simply decide which one to link to at build
time. "Environments", if they must exist, should be implicit, managed by the
build system, and _unseen_ by the user.

# Local Vendoring {#vendor}

This hardly needs mentioning but having a `node_modules` directory in every
project is a mistake. Dependencies should be downloaded once to a global cache
in the appropriate XDG directory. This is to save bandwidth and storage. Nobody
really disputes this so I won't argue it.

# Shotgun Tooling {#shotgun}

Shotgun as in [shotgun parsers][shotgun].

[shotgun]: http://spw16.langsec.org/slides/katie-underwood-slides.pdf

"How do I build foolang projects?"

"Easy, just `sudo apt-get install bar` which is the build system."

"Ok great. I can get hello world going. How do I add a dependency?"

"Ah, you need `baz` to manage dependencies."

"Ok, does `baz.json` create `bar.sexp` or the other way around?"

"Well, nobody uses them directly. You need `derp` as a swiss army knife."

"Alright."

"And `flurpy` to manage `derp` environments."

"I think I am a fictional character in a Socratic dialogue about OCaml build
tooling."

"You first use `sdkman` to install `yog-sothoth`, who is the key and the gate,
with knowledge of forbidden and abhorrent lore you `mkvirtualenv` a named
environment and update your `.zshrc`, then you `eval $(rlyeh env)` and `dagon
install requirements.docx --with-backdoors` to get your dependencies. We deploy
on ZIRP.js, a cloudless codeless deployment startup that will take down the
world economy when it folds six weeks from now. No you cannot test anything
locally."

"I wish I was using Haskell."

# No Pinning {#pinning}

When I run the `build` command, the build system should:

1. [Resolve dependencies][resolve] into a consistent set of assignments.
1. **Pin the exact version of all dependencies in a lockfile so the build is
   reproducible.**
1. Download any dependencies not present in the machine.
1. Build all the dependencies, if needed.
1. Build my code.

[resolve]: /article/dependency-resolution-made-simple

Most modern package managers have figured this out so it's astounding this has
to be said.

As far as I can tell [`opam`][opam] doesn't have lockfiles. It also doesn't
install dependencies automatically. So your build might fail tomorrow because
the package manager is resolving a different transitive dependency. The usual
advice is to "just use [Nix][nix]", which I think is code for "go fuck
yourself".[^nix]

[opam]: https://opam.ocaml.org/
[nix]: https://nixos.org/

# Footnotes

[^env]:
    You think you can wrap it up, hide them behind more layers of tooling, but
    there is nothing build on environment variables that is not a leaky
    abstraction. Some day your users will have to read the manual (or worse, the
    source code) to find they had to set the `SNOBOLPATH` environment variable
    or something.

[^nix]:
    I know this is offered as actionable advice, and I know Nix is great, I
    think this is attacking the problem at the wrong level of abstraction. Nix
    is very coarse-grained: you can't, for example, pin specific versions of
    packages, you can only pin a specific revision of the Nix packages
    store. Language-level package managers are not duplicating Nix's
    functionality, they're providing a smaller and more specialized subset.
