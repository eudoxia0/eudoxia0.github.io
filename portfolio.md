---
title: Portfolio
layout: common
permalink: /portfolio/
cl_projects:
    - name: cl-virtualbox
      url: https://github.com/eudoxia0/cl-virtualbox
      description: A few functions that map to `vboxmanage` commands.
    - name: trivial-ssh
      url: https://github.com/eudoxia0/trivial-ssh
      description: A simple abstraction layer over `cl-libssh2`, a binding to the `libssh2` library.
    - name: trivial-extract
      url: https://github.com/eudoxia0/trivial-extract
      description: For extracting `.tar`, .`tar.gz` and `.zip` files simply.
    - name: trivial-download
      url: https://github.com/eudoxia0/trivial-download
      description: It downloads files.
    - name: cl-base58
      url: https://github.com/eudoxia0/cl-base58
      description: An implementation of Bitcoin’s base58 encoding and decoding.
    - name: eco
      url: https://github.com/eudoxia0/eco
      description: A fast and designer-friendly template engine, inspired by the syntax of eRuby.
    - name: cl-pass
      url: https://github.com/eudoxia0/cl-pass
      description: Password hashing and verification with reasonable, secure defaults.
    - name: hermetic
      url: https://github.com/eudoxia0/hermetic
      description: An authentication library for Clack web applications.
    - name: clack-errors
      url: https://github.com/eudoxia0/clack-errors
      description: A clone of [better_errors][be] for Clack.
    - name: asdf-linguist
      url: https://github.com/eudoxia0/asdf-linguist
      description: A collection of extensions to ASDF, the de-facto build system for Common Lisp.
    - name:
      url: https://github.com/eudoxia0/
      description:
    - name:
      url: https://github.com/eudoxia0/
      description:
    - name: x
      url: https://github.com/eudoxia0/
      description: x
---

<article>

[I write small libraries][libs]. If a project implements something that may
potentially be useful somewhere else, I'll factor it out.

I've written a lot of [Common Lisp][lisp] libraries. Other than libraries, I
mostly write compilers or small utilities.

[libs]: https://gist.github.com/substack/5075355
[lisp]: https://lisp-lang.org/

# Austral

<div class="two-columns">
<div class="column">

My current main project is the [Austral][austral] programming language: a
systems programming language with a linear type system. The language is designed
to be simple to understand and implement.

The key features are: memory safety, data race freedom, and capability-based security.

</div>
<div class="column">

![A screenshot of some Austral coode.]({{ site.post_images }}/portfolio/austral.svg)

</div>
</div>

[austral]: https://github.com/austral/austral

# Common Lisp Website

<div class="two-columns">
<div class="column">

I built [a website](https://lisp-lang.org/) to promote Common Lisp, including a gallery of Lisp [success
stories][lisp-success], [a wiki][lisp-wiki], and [a tutorial][lisp-tutorial].

</div>
<div class="column">

![A screenshot of the lisp-lang.org website.]({{ site.post_images }}/portfolio/lisp-lang.png)

</div>
</div>

[lisp-success]: https://lisp-lang.org/success/
[lisp-wiki]: https://lisp-lang.org/wiki/
[lisp-tutorial]: https://lisp-lang.org/learn/

# Crane

<div class="two-columns">
<div class="column">

Crane is an ORM for Common Lisp. It uses CLOS's amazing MetaObject
Protocol to map Common Lisp objects to SQL records and back, and
provides a [Django][django]-inspired simple interface and automatic
migrations.

Internally, Crane is very similar in structure to
[SQLAlchemy][sqlalchemy], only most of the components SQLAlchemy
implements internally are external libraries that Crane depends
on. Crane is built mostly on top of [fukamachiware][fukamachi]:
[cl-dbi][dbi] is used to provide a backend-agnostic database
abstraction, and [SxQL][sxql] is used as the DSL for generating SQL.

</div>
<div class="column">

![A screenshot of a code example of using Crane.]({{ site.post_images }}/portfolio/crane.svg)

</div>
</div>

[django]: https://www.djangoproject.com/
[sqlalchemy]: http://www.sqlalchemy.org/
[fukamachi]: https://github.com/fukamachi
[dbi]: https://github.com/fukamachi/cl-dbi
[sxql]: https://github.com/fukamachi/sxql

# cmacro

<div class="two-columns">
<div class="column">

cmacro is like [sweet.js][sjs] for C. It implements a simple, pattern-matching
based macro language that can be used to extend C.

cmacro is written in Common Lisp, and is compiled to a native binary, so
you can drop it right into your Makefile between a file and clang
without changing anything.

The companion project, [Magma][magma], is a collection of cmacro extensions that
shows the scope of the macro system.

</div>
<div class="column">

![A screenshot of a code example of using cmacro.]({{ site.post_images }}/portfolio/cmacro.svg)

</div>
</div>

[sjs]: http://sweetjs.org/
[magma]: https://github.com/eudoxia0/magma

# Corona

<div class="two-columns">
<div class="column">

Corona is basically a [Vagrant][vagrant] clone. You define virtual machines
(Their name, a base system to build from, amount of RAM, IP address, etc.) and
Corona sets it up. It uses VirtualBox to run the machines and [Vagrant
Cloud][vcloud] as a source of base images to build machines from.

The advantage it has over Vagrant is simply that it's pure Lisp, so it can be
used as a dependency in a Common Lisp system, without the user having to set up
the Ruby ecosystem to install Vagrant. It's also pretty nice to use.

</div>
<div class="column">

![A screenshot of a code example of using Corona.]({{ site.post_images }}/portfolio/corona.svg)

</div>
</div>

[vagrant]: https://www.vagrantup.com/
[vcloud]: https://vagrantcloud.com/

# Rock

<div class="two-columns">
<div class="column">

Rock is an asset manager for Common Lisp, basically a combination of
[Bower][bower] plus an asset pipeline. It automatically downloads web
development libraries (JavaScript libraries like jQuery, JS/CSS libraries like
Bootstrap, etc.), manages their versions, and allows you to merge their files
together so you only serve a single CSS file and a single JS one.

</div>
<div class="column">

![A screenshot of a code example of using Rock.]({{ site.post_images }}/portfolio/rock.svg)

</div>
</div>

[bower]: http://bower.io/

# Other Projects

Common Lisp projects:

{% assign cl_projects_sorted = page.cl_projects | sort: 'name' %}
<dl>
{% for project in cl_projects_sorted %}
<dt>
<a href="{{ project.url }}">{{ project.name }}</a>
</dt>
<dd>
{{ project.description }}
</dd>
{% endfor %}
</dl>

[be]: https://github.com/BetterErrors/better_errors

</article>
