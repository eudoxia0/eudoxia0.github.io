---
title: Composable SQL
summary: Better SQL through typed, composable query fragments.
---

SQL could be improved somewhat by introducing composable query fragments with statically-typed interfaces. I begin by explaining two areas (testing and reusing business logic) where SQL does very poorly. Then I explain my solution, and how it addresses the problems.

# Contents

TODO

# Motivation

This section explains two big pain points of SQL.

## Testing

Testing SQL is impossible. Consider the simplest possible query:

```sql
select title from books;
```

What does this query depend on? It depends on the `title` column from the `books` table. In an ideal world, the smallest complete test dataset for this query would be:

```sql
insert into books (title) values ('The King in Yellow');
```

But it's not enough to populate the columns a query depends on. You have to populate _every column_ in the row:

```sql
insert into books (book_id, author_id, title, isbn, year, language, ...
```

The values of these columns are _completely causally disconnected_ from the query. They cannot influence the output. But you must populate them.

And the process is explosive: to insert a row you have to insert every row that it points to, on and on recursively until you hit a root object (typically a user). And each of those rows must have _every one_ of its columns populated.

Testing the simplest query requires building up a massive object graph, just to test an infinitesimal slice of it. Reams and reams of code have to be written to insert test data, of which only a few lines are causally relevant. More of your time will be spent writing test fixtures, factories, test helpers. Tests become too long to write _ab initio_, so a test suite becomes a giant file of copy-pasted functions, each of which differ in only a few lines.

Shallow queries that retrieve objects near the root of the foreign key DAG are easy to test. Queries that involve joins across many tables, or which retrieve objects that are deep in the DAG, are catastrophically expensive to write tests for. So your test coverage is uneven: the queries that don't need tests have them, the queries that need them don't.

And the performance of the test suite is really bad, which starts to hurt even in medium-sized projects.

There are no good solutions to this:

- You can make every single FK in the database deferred, so it's checked at the end of a transaction rather than during an insert, but that solves half the problem (the non-null columns still need to be populated) and requires updating every FK in the schema.
- You can make all your columns nullable, ruining the data model.
- You can write all your tables in sixth normal form, which is the same as making everything nullable.
