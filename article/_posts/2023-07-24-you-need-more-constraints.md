---
title: You Need More Constraints
summary: A checklist of useful SQL constraints.
---

If you use a relational database, you are almost certainly underusing
constraints. This post is a checklist of useful database constraints you should
consider when designing a schema.

# Contents

1. [Invariants](#inv)
1. [Defense in Depth](#depth)
1. [Checklist: Scalar Constraints](#scalar)
    1. [Non-Null](#null)
    1. [Non-Empty Strings](#string)
    1. [String Normalization](#string-norm)
    1. [Uniqueness](#unique)
    1. [Numeric Ranges](#numeric)
    1. [Allowed Values](#enums)
1. [Checklist: Multi-Column Constraints](#multi)
    1. [Unique Together](#together)
    1. [Implication](#impl)
    1. [Multiple Boolean Columns](#bool)
    1. [Timestamp Relationships](#timestamp)
    1. [State Fields and Nulls](#state)
1. [Checklist: Before/After Constraints](#before)
    1. [Immutable Columns](#imm)
    1. [State Transitions](#state-trans)
    1. [Numeric Changes](#numeric-change)

# Invariants {#inv}

Every data model has invariants: statements about the data which must always be
true.

Most invariants are implicit: things that people reasonably expect but wouldn't
list off the top of their head.

Good design is about making invariants explicit rather than explicit.

This is related to [Primitive Obsession][prim]: you should use domain-specific
types like `Email`, `Username`, rather than generic types like `String`.

For example: usernames as stored as strings. But what is a string? Anything from
the empty string to the complete works of William Shakespeare and beyond. But
when you focus on it, usernames are an infinitesimal subset of all
strings. Usernames usually have the following properties:

1. First and foremost they are non-empty.
1. Usernames are globally unique, often with the added constraint of being
   unique in a case-insensitive way.
1. A single line of text: newlines are not permitted.
1. Usually limited to some alphabet (e.g. alphanumeric characters and
   underscores, usually in ASCII).
1. No spaces allowed.
1. Obscure characters like non-printable characters, non-breaking spaces, and
   weird Unicode characters (see homoglyph attack) should not be allowed
   (_display names_ are a different matter than _usernames_. It would be
   impermissible to prevent users from writing their name in their own script.)

But most of these invariants are never enforced. The ones that are enforced are
usually enforced in ad-hoc places.

Invariants can be ranked by how hard they are to enforce:

1. *Scalar* invariants involve a single value, and are easy to enforce both in a
   database and in a programming language.
1. *Multi-column* invariants, involving a relationship between values in a
   single row are also easy to enforce.
1. *Before/after* invariants between the pre-update and post-update values of a
   row are harder to enforce, but are still enforceable in SQL using triggers.
1. *Relational* invariants spanning multiple database tables are very hard to
   enforce in SQL. They have to be enforced at the app level, usually when
   converting the results of an SQL query involving joins into an object in the
   programming language of the API server.

In general you should enforce every invariant you can tractably enforce.

Most of these you can come up with while designing the database.

Following a checklist (such as this post) can help.

Some of them you will only come up with after the fact, looking at the data, and
often wonder why you didn't think of them.

In either case, adding a constraint is not terribly time consuming.

It's just a matter of creating a migration and writing the DDL.

The only tedious part is the verbosity of SQL, but GitHub
Copilot is really good at this. You can write a comment explaining what you want
and it usually gets the constraint right.

You should start with the strictest possible data model.

Because it's trivial to go from strict to lax: you just drop the
constraints. But it is very, very hard to go from lax to strict, often it
involves days or weeks or months of planning and data migrations.

# Defense in Depth {#depth}

Where should invariants be enforced?

1. In the frontend?
2. In the API?
3. In the backend models?
4. In the database?

The answer is: at least in the API and the database. Ideally everywhere.

In practice you will miss some.

So practice defense in depth: if you forget enforcement in one layer, you get
enforcement in the next layer. A 500 error is better than bad data: one is
transient, the other is a thorn on the side of your data model that eventually
leads to the death by a thousand cuts.

The database is special: it's the last line of defense. Ideally, all of your
invariants are enforced at higher layers, and database constraints are never
violated. A database constraint violation is a sign of a bug in the code.

# Checklist: Scalar Constraints {#scalar}

This section lists useful scalar constraints: these are constraints applying to
a single column in a database row.

## Non-Null {#null}

This goes without saying. Most things should be non-null.

The best reason to avoid null values is that in SQL, null is two things:

1. The `None` case of an `Option` type.
1. A three valued logic that propagates across the entire language.

SQL becomes harder to reason about where nulls are involved.

If you have columns that are initially null, but become non-null after some
transformation, you should consider having another table for those values and a
foreign key. Then the nulls move from the column to the join, where they are
more expected. Alternatively, you should enforce constraints, see below.

## Non-Empty Strings {#string}

- almost all strings we use in databases are intended to be non-empty strings
- this is hardly ever enforced
- add a constraint to make the string non-empty
- if a string is nullable, consider make it non-null and using the mpty string as the empty case
- you might want to have server-side types to represent non-empty strings
- you might also want to enforce the string is whitespace trimmed

## String Normalization {#string-norm}

- emails should be unique
    - goes without saying
- emails should be lowercase
    - you dont want to debug the case where multiple users have john@doe and John@doe.

## Uniqueness {#unique}

- rule: unique
    - when making a column, always briefly ask yourself: should this be unique?
    - should this foreign key be unique?

## Numeric Ranges {#numeric}

- rule: numeric ranges
    - think about your numbers
    - are your numbers:
        - integers?
        - naturals?
        - positive?
    - check constraint: column > 0
    - check constraint: column > min and column < max

## Allowed Values {#enums}

- rule: enum allowed values

# Checklist: Multi-Column Constraints {#multi}

## Unique Together {#together}

- when designing a table, always ask yourself: are there any pairs of fields that should be unique together?
- examples:
    - two foreign keys
    - a foreign key and a position/order field

## Implication {#impl}

- you can implement if-then constraints

## Multiple Boolean Columns {#bool}

- if you have multiple boolean toggles in a table, think about the relationship between them: are some of them mutually exclusive? does one toggle imply the other?
- enforce those relationships
- think about instead using a state field

## Timestamp Relationships {#timestamp}

- started at < ended at

## State Fields and Nulls {#state}

- if state = foo, then column is non-null
- if state = bar, then column is null
- if x is null, then the other must be non-null

# Checklist: Before/After Constraints {#before}

## Immutable Columns {#imm}

- if a field should be immutable, add a trigger to make it so
- if old.foo <> new.foo: error

## State Transitions {#state-trans}

- if you have a state column, you should have a trigger that checks that the column can't be changed
- if old.state = foo and new.state = bar: error

## Numeric Changes {#numeric-change}

- rule: non-decreasing fields
- rule: monotonically increasing fields
