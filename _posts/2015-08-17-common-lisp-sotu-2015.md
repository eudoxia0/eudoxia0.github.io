---
title: State of the Common Lisp Ecosystem, 2015
summary: >
  The state of Common Lisp's library ecosystem, from the perspective of a
  contributor.
tags: [lisp]
layout: sotu
description: |

  This is a description of the Common Lisp ecosystem, as of August 2015, from the
  perspective of a user and contributor.

  The purpose of this article is both to give an overview of the ecosystem, and
  to help drive [consolidation][fare] in each domain.

  Each application domain has recommendations for consolidating that part of the
  ecosystem, and pointers for interesting future work.

  [sotu]: https://github.com/Gabriel439/post-rfc/blob/master/sotu.md
  [sotu-hn]: https://news.ycombinator.com/item?id=10071535
  [fare]: http://fare.livejournal.com/169346.html

---

<div class="extender"></div>

# Application Domains

## Web Development

### Backend

[Clack][clack], the equivalent of WSGI/Rack has existed since 2009, and is
throughly tested and battle-tested. Thre web frameworks -- [Caveman2][caveman],
[Ningle][ningle], and [Lucerne][lucerne] -- are built on top of it.

Clack is an HTTP server abstraction, that allows the user to write web
applications (or, more reasonably, web application *frameworks*) without
depending on a particular server.

The importance of using Clack cannot be understated: If you build an application
directly on, say, Hunchentoot, you're tied to Hunchentoot, and if a new, faster
server -- like [Woo][woo] -- comes out, you have to rewrite the entire
application to use it. If you write a plugin for Clack -- like
[clack-errors][clack-errors] -- it is automatically usable by all applications,
regardless of framework, that are built on Clack, reducing useless duplication
of code.

With Clack, switching from Hunchentoot to Woo, and enjoying the incredible
speedup, is a simple matter of installing [libev][libev] and changing a keyword
argument.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

**Stop using Hunchentoot directly**. Use Clack, or even better, one of the
frameworks built on it.

  </div>
  <div class="future">

**Future Work:**

The foundation is finished, now it's time to write higher-level layers. An
extensible administration framework for Clack applications, like Django's Admin,
would be a good example.

  </div>
</div>

### Frontend

This is limited by Common Lisp's ability to compile to JavaScript. Common Lisp
being rather a sizeable language, the options are:

* [Parenscript][parenscript]: A DSL that compiles a subset of Common Lisp to
  idiomatic JavaScript.

* [JSCL][jscl]: A CL-to-JS compiler designed to be self-hosting from day
  one. Lacks CLOS, `format` and `loop`.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

The best way to help consolidation is to drive one of the existing CL-to-JS
implementations forward.

  </div>
  <div class="future">

**Future Work:**

Something like CFFI, but for CL-to-JavaScript implementations, so bindings to
JavaScript libraries don't have to be rewritten if a new implementation comes
out.

A tool for compiling Common Lisp to JavaScript files, independent of the actual
implementation, to make it easier to go from Parenscript to JSCL or to some
other implementation.

  </div>
</div>

## Command Line

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Kill cl-launch, use [Roswell][roswell].

  </div>
  <div class="future">

**Future Work:**

More Roswell scripts.

  </div>
</div>

## GUI

For quite a while, a major concern was the lack of a complete GUI
solution. Well, now we have it: [CommonQt][commonqt] plus [Qtools][qtools]. The
former has years of use in real-life applications, and the latter is a layer to
make everything simpler.

The biggest problem when using CommonQt is it requires [Smoke][smoke] to run,
and getting the libraries can be difficult, especially on systems other than
Linux. This is solved by Qtools, which depends on the [qt-libs][qt-libs]
library. It downloads the Smoke libraries for whatever platform you're on, and
this makes setting it up and deploying applications easier.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Focus on CommonQt, and help improve [cl-cffi-gtk][gtk], but other libraries
should be considered deprecated.

CLIM is interesting and was the last attempt to do any kind of research on how
user interfaces should be built, but is not a viable option in 2015.

  </div>
  <div class="future">

**Future Work:**

More tutorials and examples of using CommonQt and Qtools.

  </div>
</div>

## Machine Learning

[CLML][clml] is a fairly extensive solution. It was developed by
[Mathematical Systems Inc.][msi], a Japanese company. [Mike Maul][mmaul] then
took it to GitHub and cleaned it up a little. A [tutorial][clml-tutorial] on
time series is available.

A library I've always though was interesting in this domain is [Antik][antik],
but sadly it depends on the [GNU Scientific Library][gsl], making it GPL.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Work should focus on CLML.

  </div>
  <div class="future">

**Future Work:**

CLML probably has a lot of numerical code that can be excised and released as a
separate library, along the lines of [SciPy][scipy] and [NumPy][numpy].

  </div>
</div>

## Databases

[cl-dbi][dbi] provides a uniform interface to the various database
server-specific libraries (cl-postgres, cl-mysql, etc.). [SxQL][sxql] provides a
DSL for building safe, automatically [parameterized][sql-parameter] SQL queries.

There are two fairly complete ORMs: [Crane][crane], by yours truly, and
[Integral][integral], by the author of [cl-dbi][dbi].

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Discourage using anything other than [cl-dbi][dbi].

  </div>
  <div class="future">

**Future Work:**

Bindings for other database systems, e.g. Oracle, exist. Writing drivers for
[cl-dbi][dbi] would be the best course of action and help consolidation.

  </div>
</div>

## Graphics

There's [CEPL][cepl], and its sister project, [Varjo][varjo].

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Promote the use of CEPL.

  </div>
  <div class="future">

**Future Work:**

A high-level OpenGL library, like [pg][pg], would be great.

More libraries in this area, especially for manipulating different mesh or other
3D formats.

  </div>
</div>

## System

UIOP, ASDF's portable compatiblity layer, contains a large set of tools for
portably doing everything from querying the hostname to running external
programs to manipulating environment variables.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Use UIOP for everything. Don't use [cl-fad][fad]. Don't use [OSICAT][osicat].

  </div>
  <div class="future">

**Future Work:**

An interesting project to develop in this area would be a DSL
  for building LLVM IR, either as a string or using the LLVM API bindings. This
  would allow you to use Common Lisp as an assembly language with a very
  powerful macro system. A use case: JIT-compiling highly-optimized,
  SIMD-vectorized numerical code.

  </div>
</div>

## Other

Benchmarking
: Building a framework like Haskell's [Criterion][criterion] would be a good
step forward.

Parsing
: My go-to library in this area is [esrap][esrap], which generates parsers from
  Lispy grammar definitions. More diversity in this area would be
  welcome. Recently, [proc-parse][proc-parse], a fast vector parsing library,
  was released.

Logging
: There is not much to say about this, except that Common Lisp has good tools in
this area, like [log4cl][log4cl] and [vom][vom].

# Development

## Type System

There's not much to say here, except that Common Lisp has a pretty great type
system that is not exploited nearly enough.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Not applicable. CLOS has no competition, e.g. from revived versions of Flavors, and is basically
the only OOP system there is in Common Lisp.

  </div>
  <div class="future">

**Future Work:**

I often reach for [trivial-types][t-types], a library that contains a bunch of
simple type specifiers. A larger library, e.g. including
[this CDR][interval-types], would be good.

  </div>
</div>

## Testing

There are many existing test frameworks, the main ones being [FiveAM][5am] and
the much newer [Prove][prove].

Of the existing libraries, Prove is a good candidate for the one to settle
on. Extensible test reporters are a great idea, and the general approach to
extensibility makes it a good solution to the problem of test frameworks.

Other, older libraries depend on test frameworks like `rt` or `lisp-unit`, but
these aren't relevant anymore.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Discourage using testing frameworks other than FiveAM and Prove.

  </div>
  <div class="future">

**Future Work:**

Writing any new test frameworks will be counter-productive at this point. Work
should focus on consolidating the existing ones.

  </div>
</div>

## Concurrency

[cl-async][async] is probably the most complete solution to anything concurrency
related. And it's built on [libuv][uv], the library that powers [Node.js][node].

Other libraries of interest in this area are,

* [STMX][stmx]: Provides support for software transactional memory, which is
  pretty impressive.

* [lparallel][lpara]: A very eframework for parallel programming.

Libraries like [legion][legion] simplify concurrency for specific use cases.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**



  </div>
  <div class="future">

**Future Work:**

There's plenty of room for new ideas in this area.

  </div>
</div>

## Documentation

For online, automatically updated documentation, in the style of
[Read the Docs][rtd], we have [Quickdocs][quickdocs].

There aren't many documentation generators, surprisingly. I use [Codex][codex]
to generate my documentation. It's written on [CommonDoc][commondoc], a library
that provides a format-agnostic internal representation for documents. Codex is
designed along the lines of Sphinx: documentation is written in prose, and you
insert automatically-extracted API documentation (functions, macros, classes,
etc.) into the text using macros.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Promote the use of Quickdocs by linking to it in library READMEs, to new users,
etc.

  </div>
  <div class="future">

**Future Work:**

I'll continue working on Codex on CommonDoc, and any improvements to CommonDoc
(mainly, more output formats and more macros) will be reflected in Codex.

  </div>
</div>

## Unicode

This is essentially a solved problem: Characters in Common Lisp are Unicode
glyphs, not bytes. Sometimes you can get tripped up if an implementation's
encoding format is set to something like Latin-1 instead of UTF-8, but those are
easily fixed, e.g. in SBCL:

~~~lisp
(setf sb-impl::*default-external-format* :utf-8)
~~~

Other than that, I've never run into any Unicode problems, even when interfacing
with C libraries.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Not applicable.

  </div>
  <div class="future">

**Future Work:**

None, to the best of my knowledge.

  </div>
</div>

## File Formats

There exist Common Lisp libraries for all the major file formats:

* XML: [Plump][plump]
* JSON: [Yason][yason]
* YAML: [cl-yaml][cl-yaml]
* CSV: [cl-csv][cl-csv]

A new player in the field of JSON libraries is [Jonathan][jonathan], a very fast
JSON encoder and decoder.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

There are too many XML and JSON libraries, this leads to choice paralysis.

  </div>
  <div class="future">

A YAML parser so that [cl-yaml][cl-yaml] doesn't depend on the `libyaml` library
would make distribution far simpler.

  </div>
</div>

## Package Management

[Quicklisp][ql] is the de-facto package manager for Common Lisp.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Not applicable.

  </div>
  <div class="future">

**Future Work:**

Tools built on Quicklisp, like [Quickdocs][quickdocs] and [qlot][qlot].

  </div>
</div>

## Build System

ASDF is the de-facto build system of Common Lisp. Everyone uses it.

Every project `.asd` file, called a system definition file, which defines
project metadata (author, maintainer, homepage, etc.) and the components.

This, to me, is one of the major selling points of Common Lisp. With languages
like Python, every file imports whatever it needs, and your project becomes a
massive graph of interdependent files.

In ASDF, you basically list the files in your project in the order in which they
are defined. Or, you can specify the dependencies between the files, and let
ASDF figure out a linear ordering. The point is that dependencies are explicit,
and clearly spelled out.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Not applicable.

  </div>
  <div class="future">

**Future Work:**

More ASDF components, e.g. for buiding C/C++ files. A platform-independent
package manager for downloading the C libraries needed by a library would be
amazingly useful.

  </div>
</div>

## IDEs

[SLIME][slime] is a Common Lisp IDE built on Emacs, and the most widely-used CL
IDE. The fact that it's built on Emacs is a problem, as telling a new user they
have to learn Emacs before using SLIME (which is not strictly true) is a
significant artificial barrier to entry.

<div class="recommendations">
  <div class="consolidation">

**Consolidation:**

Reduce the barrier to entry to using SLIME.

  </div>
  <div class="future">

**Future Work:**

Non-Emacs IDEs can't hurt. An [Atom][atom] plugin that interfaces with a Swank
server, for instance, would be pretty great.

  </div>
</div>

# Conclusion

## Specific Problems

### Communication

The number of people building web applications in Common Lisp who don't know
about Clack is too high. Obviously, there's a problem in reaching potential
users with good library choices.

### Choice Paralysis

When someone asks what library to use to write code in a given domain, only the
*best* library in that domain should be recommended. Saying "you also have X, Y
and Z" only worsens the paradox of choice.

If someone asks what GUI toolkit to use, answer "[CommonQt][commonqt] with
[Qtools][qtools]", don't add "but you can also use [cl-cffi-gtk][gtk] or
[LTK][ltk]..."

If someone asks what to use to build web applications, link them to one of the
frameworks built on Clack. Don't talk about Hunchentoot or Wookie or Woo or some
other server.

If someone asks what IDE to use, point them to [SLIME][slime]. Don't tell them
about some obscure abandoned project from 2005 that only runs [CLISP][clisp].

And, most importantly: when someone asks which implementation to use, just say
[SBCL][sbcl] or [CCL][ccl]. Don't tell them, "well, you can use [ECL][ecl] to
embed Lisp or [ABCL][abcl] to run on the JVM". Those are for more niche users,
not new users asking how to get started.

## Growth

I am subscribed to [this feed][github-repos] of new Common Lisp repos on
GitHub. I wrote some code to query my [Newsbeuter][news] database:

~~~lisp
{% include code/newsbeuter.lisp %}
~~~

<div class="plot" id="projects-per-day">
  <div class="text">
  From <span class="value" id="earliest"></span> to <span class="value" id="latest"></span>,
  a total of <span class="value" id="number"></span> repos were created
  </div>
  <svg></svg>
</div>

<div class="extender"></div>

[clack]: http://clacklisp.org/
[caveman]: http://8arrow.org/caveman/
[ningle]: http://8arrow.org/ningle/
[lucerne]: http://eudoxia.me/lucerne/
[woo]: https://github.com/fukamachi/woo
[hunchentoot]: http://weitz.de/hunchentoot/
[libev]: http://software.schmorp.de/pkg/libev.html
[clack-errors]: https://github.com/eudoxia0/clack-errors

[parenscript]: https://github.com/vsedach/Parenscript
[jscl]: https://github.com/davazp/jscl

[cl-launch]: https://github.com/luismbo/cl-launch
[roswell]: https://github.com/snmsts/roswell

[commonqt]: https://github.com/commonqt/commonqt
[qtools]: http://shinmera.github.io/qtools/
[smoke]: https://techbase.kde.org/Development/Languages/Smoke
[qt-libs]: https://github.com/Shinmera/qt-libs
[gtk]: https://github.com/crategus/cl-cffi-gtk

[antik]: https://www.common-lisp.net/project/antik/
[gsl]: http://www.gnu.org/software/gsl/

[msi]: http://www.msi.co.jp/english/
[mmaul]: https://github.com/mmaul
[clml]: https://github.com/mmaul/clml
[clml-tutorial]: https://mmaul.github.io/clml.tutorials//2015/08/08/CLML-Time-Series-Part-1.html
[scipy]: http://www.scipy.org/
[numpy]: http://www.numpy.org/

[fad]: http://weitz.de/cl-fad/
[osicat]: https://common-lisp.net/project/osicat/

[log4cl]: http://quickdocs.org/log4cl/
[vom]: https://github.com/orthecreedence/vom/

[t-types]: https://github.com/m2ym/trivial-types
[interval-types]: https://common-lisp.net/project/cdr/document/5/extra-num-types.html

[5am]: http://quickdocs.org/fiveam/
[prove]: https://github.com/fukamachi/prove

[async]: http://orthecreedence.github.io/cl-async/
[uv]: https://github.com/joyent/libuv
[node]: https://nodejs.org/
[stmx]: http://stmx.org/
[lpara]: http://lparallel.org/
[legion]: https://github.com/fukamachi/legion

[rtd]: https://readthedocs.org/
[quickdocs]: http://quickdocs.org/
[codex]: https://github.com/CommonDoc/codex
[commondoc]: http://commondoc.github.io/

[criterion]: https://hackage.haskell.org/package/criterion
[esrap]: http://quickdocs.org/esrap/
[proc-parse]: https://github.com/fukamachi/proc-parse

[jonathan]: https://github.com/Rudolph-Miller/jonathan
[plump]: https://github.com/Shinmera/plump
[yason]: http://quickdocs.org/yason
[cl-yaml]: https://github.com/eudoxia0/cl-yaml
[cl-csv]: http://quickdocs.org/cl-csv

[ql]: https://www.quicklisp.org
[qlot]: https://github.com/fukamachi/qlot

[dbi]: https://github.com/fukamachi/cl-dbi
[sxql]: https://github.com/fukamachi/sxql
[sql-parameter]: http://blog.codinghorror.com/give-me-parameterized-sql-or-give-me-death/
[crane]: https://github.com/eudoxia0/crane
[integral]: https://github.com/fukamachi/integral

[cepl]: https://github.com/cbaggers/cepl
[varjo]: https://github.com/cbaggers/varjo
[pg]: https://github.com/fogleman/pg

[slime]: https://common-lisp.net/project/slime/
[sly]: https://github.com/capitaomorte/sly
[atom]: https://atom.io/

[github-repos]: http://planet.lisp.org/github.atom
[news]: http://www.newsbeuter.org/

[ltk]: http://www.peter-herth.de/ltk/
[clisp]: http://www.clisp.org/
[sbcl]: http://www.sbcl.org/
[ccl]: http://ccl.clozure.com/
[ecl]: https://common-lisp.net/project/ecl/
[abcl]: https://common-lisp.net/project/ecl/
