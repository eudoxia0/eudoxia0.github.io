---
title: 'Libraries: The Database Engineering Perspective'
summary: Evolving a schema for libraries one counterexample at a time.
tags: [design, sql, database]
---

This post grows the basics of a database schema for a library management system,
and is somewhat in the style
of [_Gay marriage: the database engineering perspective_][qntm].

Suppose you want to build a [Calibre][calibre] replacement. Where do you start?
With [representation][repr]. So let's start with a table to hold the two most
basic aspects of a book, its title and author:

~~~sql
create table books (
  id integer,
  title text,
  author text
)
~~~

This works fine for most fiction, but there are even fiction books with multiple
authors: [_The Mote in God's Eye_][mote] or [_The Mongoliad_][mongoliad] are two
examples.

The counterexamples are even more evident in textbooks and conference
proceedings, which more often than not have multiple authors. A quick fix:

~~~sql
create table books (
  id integer,
  title text
)

create table authors (
  id integer,
  name text,

  book integer,
  foreign key (book) references books(id)
)
~~~

This is an obvious dead end: either writers can only write a single book, or we
have to create duplicate authors for each book. Authors and books form a
many-to-many relation:

~~~sql
create table books (
  id integer,
  title text
)

create table authors (
  id integer,
  name text
)

create table authorship (
  id integer,
  author integer,
  book integer,

  foreign key (author) references authors(id),
  foreign key (book) references books(id)
)
~~~

But we're not quite done with authors. Rather, we shouldn't deal with authors,
but with the more general role of creators: textbooks and conference proceedings
have authors and editors, works of fiction and non-fiction have authors and
editors and translators, books of all kinds have illustrators, etc.

Luckily for us the Library of Congress has a standardized list,
called [MARC][marc], of creator types and their code names, so we can decide to
reference that (who doesn't like standardization?) and update our schema thusly:

~~~sql
create table books (
  id integer,
  title text
)

create table creators (
  id integer,
  name text
)

create table creatorship (
  id integer,
  creator integer,
  book integer,
  marc_role text,

  foreign key (creator) references creators(id),
  foreign key (book) references books(id)
)
~~~

If you wanted to be really relational, you could have a table of MARC relator
codes and have the `marc_role` column be a foreign key, but a `text` column is
good enough for this post.

A further problem: there isn't really a single book. Textbooks often have many
different editions, for instance. For some purposes it's useful to be able to
store multiple editions of the same book in the database, under the same entry,
with distinct edition-specific information (_e.g._, cover image, copyright
year), but without duplicating common information (_e.g._, title, publisher).

This problem has already been solved but I can't remember the link, so here is
my humble attempt:

~~~sql
create table books (
  id integer,
  title text
)

create table editions (
  id integer,
  book integer,
  edition text,

  foreign key (book) references books(id)
)

create table creators (
  id integer,
  name text
)

create table creatorship (
  id integer,
  creator integer,
  book integer,
  marc_role text,

  foreign key (creator) references creators(id),
  foreign key (book) references books(id)
)
~~~

It is debatable whether the book's title should be stored in the `books` table
or the `editions` table (the counterexample would be a book that changes title
across editions), and whether the `creatorship` table should reference a book or
a specific edition (a partial counterexample would be an author who only
collaborated for a single edition of a work). The latter problem is not as
serious as the first, since when searching we would almost certainly be
interested in the union of all sets of creators who have contributed to the
work.

Yet another problem arises when this library is not just a collection of PDF and
EPUB files on your computer, but is the metadata store of a _physical_ library
of books. Then we need a further layer of abstraction: the `item`. A library can
have multiple copies of the same book, and a different number of copies of
different editions, checked out by different people in non-overlapping
intervals, _etc_.

~~~sql
create table books (
  id integer,
  title text
)

create table editions (
  id integer,
  book integer,
  edition text,

  foreign key (book) references books(id)
)

create table creators (
  id integer,
  name text
)

create table creatorship (
  id integer,
  creator integer,
  book integer,
  marc_role text,

  foreign key (creator) references creators(id),
  foreign key (book) references books(id)
)

create table items (
  id integer,
  edition integer,

  foreign key (edition) references editions(id)
)
~~~

This example conveniently leaves out what a physical library would need: where
each copy is stored in the shelves, who has checked out what, and that imports a
giant, orthogonal problem: that of representing people.

[qntm]: https://qntm.org/gay
[calibre]: https://calibre-ebook.com/
[repr]: https://en.wikipedia.org/wiki/Fred_Brooks
[mote]: https://en.wikipedia.org/wiki/The_Mote_in_God%27s_Eye
[mongoliad]: https://en.wikipedia.org/wiki/The_Mongoliad
[marc]: https://www.loc.gov/marc/relators/relaterm.html
