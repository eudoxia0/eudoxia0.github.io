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

[crane]: https://github.com/eudoxia0/crane

# Contents

1. [Preamble: What is an ORM?](#preamble)
1. [The State of the Art](#sota)
    1. [Case: Use Raw SQL](#case1)
    1. [Case: Use an ORM](#case2)
    1. [Comparison](#comp)
1. [The Sketch](#sketch)
    1. [Migrations First](#mig-first)
    1. [Declarative Migrations](#mig-decl)
    1. [Database-Specific](#specific)
    1. [Portable Across Languages](#portable)
    1. [Relational](#relational)
    1. [Post-SQL](#postsql)
    1. [Sum Types](#sum)
    1. [Stored Procedures](#stored)
1. [The Workflow](#workflow)
1. [Prior Art](#prior)

# Preamble: What is an ORM? {#preamble}

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

# The State of the Art {#sota}

How do programmers use databases? The state of things is bimodal: you either
write raw SQL, or you use an ORM.

## Case: Use Raw SQL {#case1}

"Everyone" knows ORMs are bad, because of the ["object-relational impedance
mismatch"][impedance] or that they're ["the Vietnam of computer
science"][vietnam] or whatever. "Everyone" knows you should write raw SQL
instead.

[impedance]: https://en.wikipedia.org/wiki/Object%E2%80%93relational_impedance_mismatch
[vietnam]: https://www.odbms.org/wp-content/uploads/2013/11/031.01-Neward-The-Vietnam-of-Computer-Science-June-2006.pdf

But in most languages going from an ORM to raw SQL is like going from
[OCaml][ocaml] to Java: a three-line type definition in OCaml becomes four Java
files, each of which is tens of lines of code written in quadruplicate, with
[IntelliJ][ij]-generated boilerplate.

[ocaml]: /article/two-years-ocaml
[ij]: https://www.jetbrains.com/idea/

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

[dao]: https://en.wikipedia.org/wiki/Data_access_object

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

[rs]: https://docs.oracle.com/javase/8/docs/api/java/sql/ResultSet.html

Writing raw SQL is like writing bindings to a foreign C library, except the
types are slightly richer and you're defining said library inline, inside
untyped string literals.

And SQL itself has problems(more on which later).

## Case: Use an ORM {#case2}

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

## Comparison {#comp}

When do you choose one over the other? You can think of it as being about fixed
vs. marginal costs.

With raw SQL, the fixed cost is high: you have to write a lot! But the actual
time spent writing boilerplate is not much. It just feels like a lot because
it's tedious. And it's the kind of tedium that LLMs can handle
wonderfully. Migrations can also be a problem if you don't have something like
[Liquibase][lq].

[lq]: https://www.liquibase.org/

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

# The Sketch {#sketch}

This section describes how I want to use databases. In brief:

1. I don't want to write anything uninteresting, i.e. mapping result sets to
   native language types.
2. Migrations first, not schema first.
3. Migrations are specified in a declarative format, not in SQL.
4. The tool is specific to the database, not portable, and exposes features
   specific to that database.
5. The tool can generate database access bindings for multiple programming languages.
6. I want a query language that's better than SQL. Specifically, it has to be 1)
   composable, 2) statically typed and with 3) a sane syntax.
7. I want sum types.

## Migrations First {#mig-first}

ORMs are typically schema-first: you write your schema (as Python classes), and the ORM automatically generates a schema. When you modify your classes, the ORM diffs the resulting schema against the previous one and makes a migration.

This is convenient: it does what you mean and is declarative.

The problem is it underemphasizes migrations and makes them into an implementation detail.

I don't think I can argue this very effectively or rigorously, but I think
migrations should come first. You should write migrations as separate files,
then the database access tool should run those migrations virtually against an
empty schema, to recover the current state of the schema. Then it can print this
schema out as an SQL file or as HTML documentation for the current state of the
database.

This is similar to code-first GraphQL libraries where you define your GraphQL
schema in code, and then the library dumps a `schema.gql` file for the frontend
to pick up.

## Declarative Migrations {#mig-decl}

Migrations should not be written in SQL but in some parseable, declarative
format like JSON or YAML.

Why? Because to typecheck queries, you need to know what the schema looks
like. To know what the schema looks like, you can either query the live database
(it's arguable whether this is good), or build a virtual model of that schema. I
prefer static solutions.

If your migrations are a bunch of YAML files like:

```yaml
comment: Add a `deleted_at` column for the user table.
actions:
  - type: add_column
    table: users
    column: deleted_at
    type: "Timestamp"
  - type: add_constraint
    table: users
    name: deleted_at_is_greater_than_joined_at
    check: "(deleted_at is null) or (deleted_at > joined_at)"
```

Then you can parse those migrations into objects, and run those migration
objects against a virtual schema, starting from the empty schema, to see what
the resulting schema looks like.

The problem with raw SQL is that it's impossible to parse SQL into objects that
can be manipulated programmatically. SQL is, in a sense, a write-only language.

You can write a parser for ISO SQL, but that's not the language you're
using. You're using your RDBMS' dialect of SQL. You can write a parser for
e.g. Postgres' dialect, but that's a moving target. Really the only way to parse
a Postgres `ALTER TABLE` statement is:

1. Query the schema from the live database.
2. Run the statement.
3. Query the schema again.
4. Diff the old and new versions.

Which is not ideal.

So there's an assymetry, where it's trivial to go from first-class objects to
SQL, but it's basically impossible to go from textual SQL to a first-class
object.

Therefore, rather than write migrations as SQL and make a Herculean effort to
parse them, we should simply write some in some declarative format that's easy
to parse.

## Database-Specific {#specific}

Many ORMs and database access tools advertise portability as a feature. For
certain tools (e.g. [DBeaver][dbeaver] or Java's `java.sql`) this makes
sense. For more involved tools, portability is an anti-feature.

[dbeaver]: https://dbeaver.io/

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

## Portable Across Languages {#portable}

This isn't really a hard requirement, but an example of what is possible.

With OpenAPI you define your schema in some declarative language, and a code
generator spits out client code and server stubs in many languages. Why not the
same for databases? If the database schema/migrations are defined in a
declarative format, and the query language is some type-checked compile-to-SQL
language, the database access tool can easily typecheck the queries separately
from your codebase, and generate code to:

1. Apply the migrations.
2. Run the generated SQL queries.
3. Map the query result set to a type defined from the schema.

## Relational {#relational}

Ten years ago or so [NoSQL][nosql] took off. The movement promised two things:

[nosql]: https://en.wikipedia.org/wiki/NoSQL

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
key-value or blob store for their core database.

[lhc]: https://www.mongodb.com/blog/post/holy-large-hadron-collider-batman

ACID guarantees are _extremely_ valuable. Explicit schemas are great. I don't
want less schemas, I want more. I want stricter schemas than Postgres
provides. I want strong and static types. I want queries I can typecheck
statically, before executing them.

## Post-SQL {#postsql}

SQL is bad, for two reasons:

1. The syntax is bad.
2. Type checking is absent.

On the first point, here's the grammar for the `SELECT` statement in Postgres:

```
[ WITH [ RECURSIVE ] with_query [, ...] ]
SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
    [ * | expression [ [ AS ] output_name ] [, ...] ]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY [ ALL | DISTINCT ] grouping_element [, ...] ]
    [ HAVING condition ]
    [ WINDOW window_name AS ( window_definition ) [, ...] ]
    [ { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] select ]
    [ ORDER BY expression [ ASC | DESC | USING operator ] [ NULLS { FIRST | LAST } ] [, ...] ]
    [ LIMIT { count | ALL } ]
    [ OFFSET start [ ROW | ROWS ] ]
    [ FETCH { FIRST | NEXT } [ count ] { ROW | ROWS } { ONLY | WITH TIES } ]
    [ FOR { UPDATE | NO KEY UPDATE | SHARE | KEY SHARE } [ OF table_name [, ...] ] [ NOWAIT | SKIP LOCKED ] [...] ]

where from_item can be one of:

    [ ONLY ] table_name [ * ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
                [ TABLESAMPLE sampling_method ( argument [, ...] ) [ REPEATABLE ( seed ) ] ]
    [ LATERAL ] ( select ) [ AS ] alias [ ( column_alias [, ...] ) ]
    with_query_name [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    [ LATERAL ] function_name ( [ argument [, ...] ] ) [ AS ] alias ( column_definition [, ...] )
    [ LATERAL ] function_name ( [ argument [, ...] ] ) AS ( column_definition [, ...] )
    [ LATERAL ] ROWS FROM( function_name ( [ argument [, ...] ] ) [ AS ( column_definition [, ...] ) ] [, ...] )
                [ WITH ORDINALITY ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    from_item join_type from_item { ON join_condition | USING ( join_column [, ...] ) [ AS join_using_alias ] }
    from_item NATURAL join_type from_item
    from_item CROSS JOIN from_item

and grouping_element can be one of:

    ( )
    expression
    ( expression [, ...] )
    ROLLUP ( { expression | ( expression [, ...] ) } [, ...] )
    CUBE ( { expression | ( expression [, ...] ) } [, ...] )
    GROUPING SETS ( grouping_element [, ...] )

and with_query is:

    with_query_name [ ( column_name [, ...] ) ] AS [ [ NOT ] MATERIALIZED ] ( select | values | insert | update | delete )
        [ SEARCH { BREADTH | DEPTH } FIRST BY column_name [, ...] SET search_seq_col_name ]
        [ CYCLE column_name [, ...] SET cycle_mark_col_name [ TO cycle_mark_value DEFAULT cycle_mark_default ] USING cycle_path_col_name ]

TABLE [ ONLY ] table_name [ * ]
```

It's bad that something so basic has like twenty eight structural variants and
fifty subexpression holes.

You can imagine syntax on a spectrum of uniformity (I'd normally say regularity
but this gets confused with regular languages), with Lisp and XML at one end
representing extreme syntactic uniformity, and SQL at the other end representing
extreme syntactic specificity. Generally you want a happy medium: where the
syntax is regular enough to be composable and easily remembered, but specific
enough you can read code effectively.

SQL being so non-uniform makes it hard to parse, hard to learn, and hard to remember.

You may argue this is "good for beginners and non-programmers", and I would
dispute that. But I am not beginner and I am not a non-programmer. They can have
SQL, if they want. I want something better.

On the second point about type checking, this matters less if you're using the
database interactively, like a DBA or a business analyst, and it matters greatly
if you're using the database from an application server, where queries are fixed
and only parameters are different.

I want a better SQL. A better SQL is a language with:

1. Sane, composable syntax.
2. Static type checking.
3. That compiles to efficient Postgres SQL.

This is easier said than done. The challenge has three stages:

1. Design a sane query language.
2. Make it feature-complete to Postgres' SQL.
3. Compile it to efficient SQL.

## Sum Types {#sum}

We need sum types. [Java has sum types now][javasum]. Humanity cannot survive
this century without sum types in relational databases.

[javasum]: https://openjdk.org/jeps/409

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

## Stored Procedures {#stored}

Maybe someone can work out a way to do stored procedures that isn't a huge
liability with regards to migration and deployment.

# The Workflow {#workflow}

Here's what using a post-ORM would look like. I've started a new project that
needs database access, so I `cd` to its directory and run:

```bash
$ postorm init
```

And this creates some directories:

```
postorm/
    config.json
    migrations/
    queries/
```

Then I create my first migration:

```bash
$ postorm migrations new
```

This brings up my editor on a temporary YAML file, and I write:

```yaml
comment: Initial migration.
actions:
  - type: create_table
    name: users
    columns:
      - name: id
        type: bigserial
        pk: true
      - name: email
        type: string
        unique: true
      # etc.
```

I save the file and quit and this is saved to.

```
postorm/migrations/2029-01-23-06-51-47-initial.yaml
```

Migrations (sans comments) are hashed to prevent editing them after the fact. Then:

```bash
$ postorm queries new getUsersByGroup
```

This creates a file `postorm/queries/getUsersByGroup.fql` (where `fql` stands
for "future query language"), and I open it and write:

```
query findUsersInCohort(cohort_id: Cohort, limit: Nat = 100): Stream[User] =
  select(users)
  | filter(active = true)
  | filter(%.cohort_id = cohort_id)
  | project id, email, display_name
  | sort joined_at
  | reverse
  | take limit
```

Which compiles to something along the lines of:

```sql
SELECT
    id, email, display_name
FROM
    users AS u
WHERE
    u.active = true
    AND u.cohort_id = $1
ORDER BY
    joined_at DESC
LIMIT
    $2
```

Then `postorm generate python` generates Python bindings to the above, along the lines of:

```python
@dataclass(frozen=True)
class UserIdEmailDisplayName:
    id: UserId
    email: Email
    display_name: DisplayName

def get_users_in_cohort(
    db: Db,
    cohort_id: CohortId,
    limit: int = 100
) -> Stream[UserIdEmailDisplayName]:
    query: str = """
        SELECT
            id, email, display_name
        FROM
            users AS u
        WHERE
            u.active = true
            AND u.cohort_id = $1
        ORDER BY
            joined_at DESC
        LIMIT
            $2
    """
    q: Query = db.prepare(query).bind([cohort_id.to_int(), limit])
    rs: ResultSet = q.fetch_all()

    def _map_row(row: Row) -> User:
        return UserIdEmailDisplayName(
            id=UserId.from_db(row["id"]),
            email=Email.from_db(row["email"]),
            display_name=DisplayName.from_db(row["display_name"]),
        )

    return rs.map(_map_row).stream()
```

Alongside code to run the migrations, and maybe linting code and such.

# Prior Art {#prior}

[PRQL][prql] is the closest thing that exists today to the query language I want
to use.

[prql]: https://prql-lang.org/

[SQLx][sqlx] an asynchronous database access library for Rust. It supports typechecking queries. The [FAQ][faq] explains how they implemented this:

[sqlx]: https://github.com/launchbadge/sqlx
[faq]: https://github.com/launchbadge/sqlx/blob/253d8c9f696a3a2c7aa837b04cc93605a1376694/FAQ.md

> The macros work by talking to the database at compile time. When a(n) SQL client asks to create a prepared statement
> from a query string, the response from the server typically includes information about the following:
>
> * the number of bind parameters, and their expected types if the database is capable of inferring that
> * the number, names and types of result columns, as well as the original table and columns names before aliasing
>
> In MySQL/MariaDB, we also get boolean flag signaling if a column is `NOT NULL`, however
> in Postgres and SQLite, we need to do a bit more work to determine whether a column can be `NULL` or not.
>
> After preparing, the Postgres driver will first look up the result columns in their source table and check if they have
> a `NOT NULL` constraint. Then, it will execute `EXPLAIN (VERBOSE, FORMAT JSON) <your query>` to determine which columns
> come from half-open joins (LEFT and RIGHT joins), which makes a normally `NOT NULL` column nullable. Since the
> `EXPLAIN VERBOSE` format is not stable or completely documented, this inference isn't perfect. However, it does err on
> the side of producing false-positives (marking a column nullable when it's `NOT NULL`) to avoid errors at runtime.
>
> If you do encounter false-positives please feel free to open an issue; make sure to include your query, any relevant
> schema as well as the output of `EXPLAIN (VERBOSE, FORMAT JSON) <your query>` which will make this easier to debug.
>
> The SQLite driver will pull the bytecode of the prepared statement and step through it to find any instructions
> that produce a null value for any column in the output.

So it works, but it is supremely complicated. The FAQ also has this to say about the difficulty of doing any kind of semantic analysis on SQL:

> **Why can't SQLx just look at my database schema/migrations and parse the SQL itself?**
>
> Take a moment and think of the effort that would be required to do that.
>
> To implement this for a single database driver, SQLx would need to:
>
> - know how to parse SQL, and not just standard SQL but the specific dialect of that particular database
> - know how to analyze and typecheck SQL queries in the context of the original schema
> - if inferring schema from migrations it would need to simulate all the schema-changing effects of those migrations
>
> This is effectively reimplementing a good chunk of the database server's frontend,
>
> and maintaining and ensuring correctness of that reimplementation,
>
> including bugs and idiosyncrasies,
>
> for the foreseeable future,
>
> for every database we intend to support.
>
> Even Sisyphus would pity us.
