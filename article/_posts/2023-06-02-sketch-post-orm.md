---
title: Sketch of a Post-ORM
summary: A better way to interact with databases.
card: sketch-post-orm.jpg
card_source: |
    Screenshot of the [Connection Machine][cm] document retrieval system.

    [cm]: https://en.wikipedia.org/wiki/Connection_Machine
---

I've been writing a lot of database access code as of late. It's frustrating
that in 2023, my choices are still to either write all of the boilerplate by
hand, or hand all database access over to some inscrutable "agile" ORM that will
become a crippling liability in the 2-3y timescale.

This post is about how I want to use databases, from the perspective of an
application server developer---not a DBA or a BI guy or whatever. In a past life
I wrote a [Django-inspired ORM for Common Lisp][crane], which taught me a few
things about the failure modes of ORMs specifically and software engineering
generally, but mostly this is a reflection on my experience in industry.

# Preamble: What is an ORM?

By ORM I mean a tool that lets you write code that looks like this:

```python
class User(Model):
    class Meta:
        tablename = "users"
        pk = "id"

    id: BigSerial
    name: str
    email: Unique(str)
    age: Nullable(int)
    freemium: bool

user: User = User(name="Fernando", age=28).insert()

for user in User.filter(freemium=False).select(["email"]):
    foo(user)
```

That is: a tool where you can write your database access code "language-first"
rather than SQL first, where you define a record type and sprinkle some
annotations and you can start writing queries using ordinary code. The tool then
handles creating and altering the schema and converting your method calls to SQL
dynamically. Typically the goal of an ORM is to have the most succint (or agile,
if you want) possible way to use the database.

It doesn't actually require that you map classes to tables, or even that your
language support OOP at all.

# The State of the Art

How do programmers use databases? The state of things is bimodal: you either
write raw SQL, or you use an ORM.

## Case: Use Raw SQL

"Everyone" knows ORMs are bad, because of the ["object-relational impedance
mismatch"][impedance] or that they're ["the Vietnam of computer
science"][vietnam] or whatever. "Everyone" knows you should write raw SQL
instead.

But in most languages going from an ORM to raw SQL is like going from
[OCaml][ocaml] top Java: a three-line type definition in OCaml becomes four Java
files, each of which is tens of lines of code written in quadruplicate, with
[IntelliJ][ij]-generated boilerplate.

[ocaml]: http://localhost:4000/article/two-years-ocaml

So why do people use raw SQL?

1. Queries written in SQL can be optimized endlessly, you're not upper-bounded
   by the ORM's query generator.
2. You can use the power-user features specific to your RDBMS, while many ORMs
   provide a lowest-common-denominator interface.
3. It's easy to know where queries are happening, so you can centralize database
   access to specific modules ([data access objects][dao]). This, in turn, has
   many benefits: there's a clear separation between the interface to a query
   and its implementation, which allows it to be optimized separately. You have
   a central place to add features like pre or post-save checks, or database
   access logging.

And why wouldn't people write raw SQL? The main problem is it looks like this:

```python
def get_users_in_group(
    db: Db,
    group_id: GroupId,
    is_active: bool = True,
    limit: int = 100
) -> list[User]:
    query: str = """
        SELECT
            id,
            email,
            display_name,
            joined_at,
            deleted_at
        FROM
            users
        WHERE
            group_id = $1
            AND is_active = $2
        LIMIT $3;
    """
    q: Query = db.prepare(query).bind([group_id.to_uuid(), is_active])
    rs: ResultSet = q.fetch_all()
    users: list[User] = []
    for row in rs:
        user: User = User(
            id=UserId.from_db(row["id"]),
            email=Email.from_db(row["email"]),
            display_name=DisplayName.from_db(row["display_name"]),
            joined_at=parse_db_datetime_(row["joined_at"]),
            deleted_at=parse_db_datetime_or_null(row["deleted_at"]),
        )
        users.add(user)
    return users
```

The next problem is type-checking disappears at the query boundary. When you
pull from a database, you get a dynamically-typed [result set][rs]. Parsing that
into your domain objects is boilerplate. If you change the query, but not the
code around it, you get errors the compiler won't catch.

Writing raw SQL is like writing bindings to a foreign C library, except the
types are slightly richer and you're defining said library inline, inside
untyped string literals.

And SQL itself has problems(more on which later).

## Case: Use an ORM

Raw SQL is tedious. So people use ORMs because they're faster and more
expedient: they let you hit the ground running and keep running for a very long
time.

In fact, for building a throwaway prototype, ORMs are a great choice. The
problem is that modern software engineering has no discipline around throwing
away throwaway prototypes, instead insisting on gradually-evolving them into
production-quality codebases (which never happens).

The problem with ORMs are:

1. **Performance:** the generated SQL is often badly optimized.

2. **Fundamental Performance:** sometimes it's not even badly optimized, it's
   just that the best queries you can write are intrinsically slow and involve
   eighteen joins because of the way the ORM has set up the schema. And because
   you don't see the generated SQL, you don't notice this until it starts
   showing up in timeouts and production error logs.

   And people will tell you to just optimize the hot loops. But let's be
   realistic: how many of us are measuring query performance uniformly? Thorough
   instrumentation is a "nice to have" that doesn't fit neatly into a user
   stories, so your incompetent PM will kick that down to the bottom of the
   backlog. The path of least resistance here is to just use the ORM
   exclusively, and that is what gets done in practice.

3. **Pointless Portability:** ORMs often emphasize portability (across RDBMSs)
   at the cost of performance and specificity, giving you a portable
   lowest-common-denominator interface when what you usually want is database
   access that leverages the RDBMS' feature set.

4. **Knowledge Doesn't Transfer:** knowing how to write good queries in the
   Django ORM doesn't tell you how to write good SQL by hand.

5. **Sunk Costs:** While ORMs make it easier to swap e.g. Postgres for MySQL,
   which you will never do, they make it hard to swap the ORM itself for
   something else.

6. **Query Smearing:** ORMs make it easy to access the database. The problem is
   they make it easy to access the database: queries are smeared across the
   application. It's very hard to statically determine where, in your codebase,
   the database access is actually happening, and there is no pressure to
   centralize database access in a DAO for easy instrumenting and encapsulation.

   Sometimes you want features along the lines of "when saving a model, do X",
   this is hard to implement with many ORMs since updates could potentially
   happen anywhere. "Just use database triggers lol" doesn't fix this, because
   the whole problem with ORMs is they either don't play well with native RDBMS
   features, or they make it painful to use them.

So we get agility, at the cost of many other things.

## Comparison

When do you choose one over the other? You can think of it as being about fixed
vs. marginal costs.

With raw SQL, the fixed cost is high: you have to write a lot! But the actual
time spent writing boilerplate is not much. It just feels like a lot because
it's tedious. And it's the kind of tedium that LLMs can handle
wonderfully. Migrations can also be a problem if you don't have something like
[Liquibase].

The marginal cost is low. Each new query is an infinitesimal fraction of the
total database access code. Every query is a function, so it can be tested
independently. You can easily find where that function is being called, and
where the tests for it are, by saerching for usages or even simply searching the
function name. Database access is easy to centralize and instrument.

The performance ceiling can be as high as you want: within the query function
you can refactor and optimize the query endlessly.

With ORMs the fixed cost is low: you hit the ground running fast. But the
marginal cost is high.

A few years into the project, in the maintenance phase, the schema changes
rarely, so the schema management and auto-magic-migration tools are less
useful. You have omnipresent performance problems. There are n+1 queries
everywhere, but where specifically? You don't know. It's impossible to
statically determine where a specific query is happening. You have to instrument
at runtime, which is rarely done rigorously or uniformly, tracing every call and
staring at logs until you find your performance problems.

There is a missing middle: something with a bit more convenience and less
boilerplate than writing all the query boilerplate by hand, but without the
problems of ORMs that try to reinvent the whole universe and do so poorly.

# The Sketch

- how i want to use databases from code
- how i want a database interface to be in the late 2020's
- sketch in brief
  - i don't want to write anything uninteresting
    - binding paramters to prepared statements
    - pulling stuff from ResultSets into domain objects
  - migrations first
  - migrations declaratively specified
    - written in json or something
    - not in sql
    - why?
      - because sql is write-only
      - it can't be parsed
      - you can write a parser for ISO SQL, but that's not the language anyone uses
      - everyone uses their db's dialect
      - you can't write a parser for Postgres' SQL dialect since that's a moving target
      - so there's an assymetry:
        - it's hard to go from SQL to first-class data
        - but it's _trivial_ to go from a first-class record to SQL
      - therefore, rather than write SQL migrations and make a Herculean effort
        to bring them up to the level of first-class objects, you should write
        migrations using a standard format like JSON with a specific schema, and
        a limited set of schema-manipulation actions you can take
      - these may still use postgres (or whatever) specific features
      - in fact the only way to "parse" a Postgres `ALTER TABLE` is to query the
        schema from the database, run the statement, query the schema after, and
        diff the schemas. this isn't great.
  - specific to the database
  - language-portable
    - same interface generates code for
      - a microservice in rust
      - whatever else
  - i want a language that's better than sql. specifically, i want a statically typed language with a sane composable syntax
  - i want sum types

## Migrations First

- "just write raw sql lol" isn't practical advice much of the time
- doesn't solve the problem of migrations
- you can either
  - use liquidbase
  - roll your own migration architecture
- orms are schema first
  - write the schema
  - generate migrations by diffing against the last known good version of the schema
  - convenient
    - do what i mean
    - i give you the schema i want, you figure out how to get there
  - problem is it underemphasizes migrations
  - i'm not sure why, i dont think i can argue this rigorously, but i think migrations should come first
  - you write your migrations
    - either as code, or as a declarative format like json
    - but ideally not as SQL
    - the problem with writing raw SQL is it's very hard to bring it up to the level where it can be manipulated programmatically
    - you want to be able to handle migrations as first class objects, which means: parse them, compare them, serialize them, turn them to documentation
  - then a tool runs those migrations virtually, starting with an empty schema, applying one migration at a time, and dumps the resulting schema to a file where it can be visualized
  - also can generate schema docs
  - this is similar to how code-first graphql libraries let you define your graphql schema as code and them dump a schema.gql file that the frontend can pick up

## Database-Specific

Many ORMs and database access tools advertise portability as a feature. For
certain tools (e.g. [DBeaver][dbeaver] or Java's `java.sql`) this makes
sense. For more involved tools, portability is an anti-feature.

The problem is that SQL, in practice, is never portable.

For example, SQLite vs. everything else is a completely different
universe. Until a recent version, SQLite's `INSERT` statement didn't support a
`RETURNING` clause.

But even putting SQLite aside, even within the big iron databases (Postgres,
MySQL and the various proprietary ones) the differences are immense. The set of
data types is different. The performance characteristics are different. Even the
SQL itself has a different syntax.

The choice of database is unlike, say, the choice of filesystem. If your
computer uses `ext4` or `zfs`, there exists a functional level above which the
differences disappear. `ls` works the same regardless. `fopen` works the
same. As long as you're not using the more advanced features, you can swap one
filesystem for another without caring at all which one it is.

The choice of Postgres vs. MySQL is more like the choice of C and C++: two
programming languages that are syntactically and semantically kind of similar,
such that people often write "C/C++", but which are in completely different
universes.

And the problem with portability is it comes at the cost of specificity. I don't
want a database access tool, I want a Postgres access tool. I want it to expose
Postgres' power user features as first-class features, not remote extensions
enabled at my own peril.

## Portable Across Languages

- not really a hard requirement but an example of what is possible
- like openapi spec
- your migrations are json
- your queries are written in some separate language
- code generator creates bindings to whatever language you're using to
  - apply the migrations
  - run those queries with type checking
  - a query like [example] gets compiled to code like [example]
  - a struct is automatically defined for the return type of each query

## Relational

Ten years ago or so [NoSQL][nosql] took off. The movement promised two things:

1. Schemaless databases for agility. See, for example, this article in the
   MongoDB blog from 2009: [Databases Should be Dynamically Typed][dynamicdb].

2. Schemaless databases for performance.

[dynamicdb]: https://www.mongodb.com/blog/post/databases-should-be-dynamically-typed

Lots of people whose data would fit in SQLite and whose workloads could run off
an Apple Watch switched to MongoDB and similar.

Gradually reality sets in.

There is no such thing as a schemaless database, only a database with an
implicit schema, a schema defined piecemeal at a thousand different points in
the codebase. Types are good. Dynamic typing is bad. Slow is fast. You are not
Google. Most everyone isn't [the LHC][lhc] and doesn't need a non-relational
key-value or blob store for their core With regards to agility.

[lhc]: https://www.mongodb.com/blog/post/holy-large-hadron-collider-batman

ACID guarantees are _extremely_ valuable. Explicit schemas are great. I don't
want less schemas, I want more. I want stricter schemas than Postgres
provides. I want strong and static types. I want queries I can typecheck
statically, before executing them.

## Post-SQL

- sql is bad
  - syntax is highly irregular
    - hard to parse
    - hard to learn
    - hard to remember
    - select statement grammar has like 7 holes for subexpressions
    - you can argue back and forth about whether this is or isn't good or beginner-friendly
    - i'm explicitly ignoring the point of view of beginners programmers or business analysts
    - my point of view is: programmers who want to access a database
    - they want type checking and they want a sane, manipulable, decently functional syntax
    - they want neither anti-intellectal just git er done dynamic typing tarpits or trans-dimensional monad optic stacks
  - type checking is absent
    - matters less if youre using the database interactiely, like a business analyst
    - again, irrelevant: as long as the underlying database is SQL, you can use SQL if you want
    - but
    - from the point of view of an _application_, your queries are fixed, it's just the parameters that have different values (but usually fixed types)
  - i want a query language with
    - sane syntax
    - type checking
    - compiles to Postgres SQL
  - this is easier said than done
  - the challenge has three components
    - design a sane query language with sane syntax and type checking
    - make it feature-complete to native postgres
    - make it compile to efficient sql

## Sum Types

We need sum types. [Java has sum types now][javasum]. Humanity cannot survive
this century without sum types in relational databases.

Typically this is implemented in userspace (in SQL), in one of two ways:

1. Having a whole slew of nullable columns, with complicated constraints.
2. Using multiple tables with foreign keys.

Both of these are a problem. But with a typed query language the latter can be
implemented very simply. A schema like:

```rust
enum PersonKind {
  Student;
  Teacher;
}

table Person {
  key id: BigSerial;
  name: String;
  case kind: PersonKind {
    when Student {
      grade: String;
    }
    when Teacher {
      subject: String;
    }
  };
};
```

Can be compiled to:

```sql
CREATE TYPE person_kind AS ENUM ('Student', 'Teacher');

CREATE TABLE person (
  id bigserial primary key,
  name text not null,
  kind person_kind not null,
  student__grade text,
  teacher__subject text,

  -- Student implies grade is non-null, subject is null.
  CHECK ((kind <> 'student') or ((student__grade <> null) and (teacher__subject = null)));
  -- Teacher implies grade is null, subject is non-null.
  CHECK ((kind <> 'teacher') or ((student__grade = null) and (teacher__subject <> null)));
);
```

Which is a hell to do by hand. Also a reason why SQL should have an implication
logical statement.

## Stored Procedures

Maybe someone can work out a way to do stored procedures that isn't a huge
liability with regards to migration and deployment.

# The Workflow

- the complete sketch
  - walkthrough of how it would work
  - start a project
  - initial migration
  - make a table
  - add some changes
  - write a query
  - run the codegen
    - functions that call the queries
    - types for the return types
    - helper function for tests, that initialize a separate database for each test, and run the whole migration history against it
  - linter
    - warn: adding a non-null column without a default is a problem
