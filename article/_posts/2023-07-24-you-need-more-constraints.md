---
title: You Need More Constraints
summary: A checklist of useful SQL constraints.
math: yes
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

Almost everywhere you use a string, you _almost certainly_ want a non-empty string.

This is hardly ever enforced.

It is easy to add a constraint to enforce non-empty values:

```sql
alter table foo add constraint username_non_empty check (length(username) > 0);
```

If a string is nullable, consider making it non-null and using the empty string
as the null value. That way you avoid both the pitfalls of nulls and the problem
of having two distinct values to represent the empty case.

You should also have server-side types to represent non-empty strings:

```python
example
```

## String Normalization {#string-norm}

- emails should be unique
    - goes without saying
- emails should be lowercase
    - you dont want to debug the case where multiple users have john@doe and
      John@doe.

## Uniqueness {#unique}

When making a column, always ask yourself: should this be unique? Especially
with foreign keys.

## Numeric Ranges {#numeric}

Most numeric columns should usually be constrained to some range:

```sql
-- Login count is at least zero.
constraint login_count_is_natural check (login_count >= 0);

-- Weight must be greater than zero.
constraint weight_is_positive check (weight > 0);

-- Percentages are between 0.0 and 1.0.
constraint percentage_in_range check ((percentage >= 0.0) and (percentage <= 1.0));
```

## Allowed Values {#enums}

Columns that represent enumerations should have constraints on their allowed values.

Postgres has native support for enum types, with some limitations.

Alternatively you can have strings and a check constraint:

```sql
constraint allowed_states is check (state in ('not_started', 'started', 'finished'));
```

# Checklist: Multi-Column Constraints {#multi}

This section describes constraints involving multiple values within the same
row.

## Unique Together {#together}

When designing a table, always ask yourself: is there some pair of columns that
should be unique together? This is often a pair of foreign keys, or a pair of a
foreign key and some other value, like for example position in a list.

In this schema:

```sql
create table book (
  book_id uuid primary key
);

create table chapter (
  chapter_id uuid primary key,
  book_id uuid not null references book(book_id),
  position integer not null
);
```

The `(book_id, position)` pair should be unique together:

```sql
constraint unique_book_and_position unique (book_id, position);
```

And naturally the position should have a range check:

```
constraint position_is_natural check (position >= 0);
```

## Conditional Nulls {#cond}

Consider this schema:

```sql
create table user (
    user_id uuid primary key,
    is_verified boolean not null,
    verified_at timestamp
);
```

There is an implicit relationship between `is_verified` and `verified_at` here:

1. If `is_verified` is true, `verified_at` must be non-null.
1. If `is_verified` is false, `verified_at` must be null.

And dually:

1. If `verified_at` is non-null, `is_verified` must be true.
1. If `verified_at` is null, `is_verified` must be false.

Constraints like these are hardly ever explicitly enforced, but should be. And
they can be enforced very simply:

```sql
check (
    (is_verified = true and verified_at is not null) or
    (is_verified = false and verified_at is null)
)
```

This is a bad example because the `is_verified` field is wholly redundant, but a
more common example is when you have multiple foreign keys are some are
conditionally null. For example:

```sql
-- Users are either students or teachers.
create table user (
  user_id uuid primary key,
  user_type string not null,
  student_id uuid references student(student_id),
  teacher_id uuid references teacher(teacher_id),

  constraint user_type_allowed_values check (user_type in ('student', 'teacher'));
);
```

The constraints here are:

1. User type is `student` iff `student_id` is non-null and `teacher_id` is null.
1. User type is `teacher` iff `student_id` is null and `teacher_id` is non-null.

We can enforce them like this:

```sql
check (
    (user_type = 'student' and student_id is not null and teacher_id is null)
    or
    (user_type = 'teacher' and student_id is null and teacher_id is not null)
);
```

## Implication {#impl}

Many constraints have the form "if _condition_ then _something_". SQL doesn't
have native support for writing if-then constraints, but you can exploit logic
to do this: an implication $P \implies Q$ is equivalent to $\neg P \lor Q$, and
this you can write in SQL.

For example:

```sql
-- state = finished implies finished_at is non-null
check ((state <> finished) or (finished_at is not null))
```

Note that, when you write down all the constraints, you usually find that most
implication relationships are usually bi-directional, and enforcing them as
if-then constraints creates a combinatorial explosion of constraints. Therefore,
for things that are logically related, you should try to write one constraint
with one big logical expression. That way, when evolving your data model, you
only have to change one constraint rather than an implicitly-connected group of
constraints.

## Multiple Boolean Columns {#bool}

A common anti-pattern is to have multiple boolean toggles in the same row, with
some implicit exclusion relationship between them. Ideally you should replace
these with an enum field of some kind, as an interim solution, you can add
constraints about which values are allowed at the same time.

For example:

```
create table user (
    user_id uuid primary key,
    is_admin boolean not null,
    is_teacher boolean not null,
    is_student boolean not null
);
```

We can enforce that all of these are mutually exclusive:

```sql
check (
    (is_admin and (not is_teacher) and (not is_student))
    or
    ((not is_admin) and is_teacher and (not is_student))
    or
    ((not is_admin) and (not is_teacher) and is_student)
)
```

## Timestamp Relationships {#timestamp}

These are usually missed, but if you have columns like `started_at`, `ended_at`
and such, you should enforce their temporal relationship:

```
check (started_at < ended_at);
```

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
