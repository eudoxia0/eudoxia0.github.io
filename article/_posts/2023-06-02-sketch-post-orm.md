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
application server developer---not a DBA or a BI guy or whatever.

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

```java
TODO
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

- pros:
  - very quick to write
  - looks like this: [pic of a django orm code]
  - expedient
- cons:
  - queries can be badly optimized
  - orms often emphasize portability, at the cost of specificity
  - "just optimize your hot loops lol"
    - let's be realistic
    - how many of you are measuring your query performance?
    - how many of you are actually going to be rewriting your queries to use pgsql prepared statements?
    - the path of least resistance is to use the orm exclusively, so you have the orm's performance bedrock
  - knowledge doesn't transfer
    - knowing how to optimize a django orm query doesn't transfer to optimizing a raw sql query
  - hard to go from one orm to another
  - query smearing
    - shotgun queries: database access is smeared across the application
    - easy to take a queryset, pass it to another function that then does something else with it (which in turn triggers a query)
    - very hard to statically determine where your database code is
    - this in turn makes it hard to do things like: whenever we update a model X, run this job or whatever.
      - "just use triggers" doesn't solve this because native rdbms features may not play well with the orm!

## Comparison

- you can think of this as fixed vs. marginal cost
- raw sql:
  - fixed cost is high
    - you have to write a lot!
  - marginal cost is low:
    - adding a new query is a small fraction of your existing database code
    - each query is a function
      - therefore each query can be separately tested
      - it's trivial to find where the query is made (find usages)
      - and where it's tested (again, find usages)
  - performance ceiling is high
    - can go as high as you want
- orm:
  - fixed cost is low
    - hit the ground running
    - fast
  - marginal cost is high
    - years into a project
    - schema changes rarely
      - so automigate stuff is less of a boon
    - performance problems
    - n+1 queries everywhere
    - where? i don't fucking know
    - impossible to find queries statically
    - have to instrument at runtime, which is rarely done uniformly, which means you just have to put tracing calls everywhere in the codebase until you find your perf problems in the logs
    - with dynamic languages, often you find that once youve optimized everything, perf is still bad
- there is a missing middle
  - something more convenient than the jdbc example
  - has less problems than orms

# Background

- background
  - what license do i have to speak of this?
  - not much
  - most of my experience is using the django orm
  - in a past life i wrote my own orm inspired by the django orm
    - for common lisp
    - with automatic migrations using schema diffing
    - i started a rewrite that I never ended
  - not a dba
  - don't know enough about raw sql
  - but actively working on learning

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

- many orms and db access libraries advertise portability as a feature
- in reality: sql is never portable
  - sqlite vs. everything else is completely differnet universes
  - sqlite aside, within big iron (pg, mysql, microsoft, oracle) the differences are huge
    - sql has different syntax
    - different types
    - differnet features
    - different perf characteristics
  - the choice of postgres vs. mysql is not like ext4 vs zfs.
    - with that choice, there exists a semantic level above which the differences disappear. dd and du work the same.
    - it's more like choosing between Python and C.
- abandon the whole concept of database portability
  - just use postgres/mysql/whatever
    - build the tool for what you're gonna be using it for
  - shamelessly exploit native features
  - switching from one db to another rarely happens

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

## Post-SQL

- not no-sql but post-sql
  - why nosql?
    - schemaless for agility
    - schemaless for performance
    - there is no such thing as schemaless
    - there is only an implicit schema
    - schemas are agile
    - dynamic types are not
    - again: slow is fast
    - none of these coping morons have data at the scale where le schemaless le column store makes sense
    - "i need a mongodb cluster for my 4TiB postgres database"
    - you need urgent psychiatric help
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

- we need sum types
- java has sum types now
- the world needs sum types
- humanity cannot survive this century without sum types

## Stored Procedures

- maybe we can use stored procedures

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
