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

## Business Logic

"Business logic" is usually thought of as imperative: in response to an event, we do X, Y, and Z. But if you have a fully-normalized database, a lot of your business logic is going to be implemented at read-time. This generally falls into two categories:

- The state of an object is determined dynamically from the state of its constituents, e.g. your score on an exam is the sum of the questions you got right.
- Reporting features that require OLAP queries specifically, and which query properties of the data which are computed dynamically.

Imagine you're working on a logistics system. We have boxes, which have mass, and boxes can go on pallets:

```sql
create table boxes (
    box_id uuid primary key,
    mass decimal not null,
    pallet_id uuid not null references pallets(pallet_id)
);
```

Pallets have a dry mass, a maximum payload mass they can support, and they go on containers:

```sql
create table pallets (
    pallet_id uuid primary key,
    dry_mass decimal not null,
    max_payload_mass decimal not null,
    container_id uuid not null references containers(container_id)
);
```

Pallets also have a number of computed properties:

1. Payload mass: the sum of the masses of all the boxes on the pallet.
2. Clearance: if the pallet's payload mass is less than the maximum, the pallet is cleared to be moved.
3. Wet mass: the sum of the dry mass and the payload mass.

Containers are analogous to pallets, but one level up:

```sql
create table containers (
    container_id uuid primary key,
    dry_mass decimal not null,
    max_payload_mass decimal not null
);
```

With just three tables, there are many possible questions we could ask about our data:

1. Is this container cleared to load?
1. How much does this container mass?
1. What's the total mass of this set of containers (e.g., those being loaded onto a ship)?
1. Are we packing boxes efficiently? What's the average percent utilization on our pallets?
1. Which pallet should this box be packed into?
1. Are any pallets in this container in excess of their maximum payload mass?

Each question corresponds to a different query. Each query depends on computed properties of the data, such as the clearance state of a container. On top of that, logic builds on logic: the logic for "is this pallet cleared?" depends on the logic for "what is the payload mass of this pallet?".

We want to write our queries in a way that satisfies these properties:

1. Performance: queries should be fast.
2. Testability: queries should be testable.
3. Comprehensibility: queries should be readable and understandable through local reasoning.
4. Reusability: logic should be defined once, tested once, and used in many places.

Because SQL has such limited means of abstraction, we have only a choice of bad options:

1. We can duplicate the business logic across queries, or
2. We can give up on normalization and cache the computed properties in response to events, or
3. We can implement the business logic in views.

The next sections explain why each option is bad.

### Duplication

Write out the logic for computed properties in every query. Hope that changes to the business logic affect every place where the logic is defined. Testing would help here, but as discussed above, testing (especially for deep OLAP-type queries) is intractable because of the combinatorial explosion.

Worse, if you duplicate the logic, but tailor it to the specifics of the query, it becomes much harder to actually find other instances. There is a single, abstract concept of a relation that e.g. maps pallet IDs to payload masses sale counts, but the implementations are varied and can't easily be identified.

While the logic here isn't too complex, there are enough degrees of freedom that, if the logic is duplicated, we will have drift.

Ideally, the logic for the definition of "how heavy is this pallet?" and "is this container ready to be loaded?" should be defined once, and tested once, but used in many places.

### Denormalization

We can denormalized the computed properties: adding a `payload_mass` and `cleared` column to both the `pallets` and `containers` tables. Whenever an event enters the system which affects these properties, they are recomputed. The logic can be implemented in one place, at the application layer (where it is easier to test).

The costs of denormalization are well-known, but it boils down to:

- There are now two definitions of the same concept: the declarative one and the imperative one.
- There is an implicit invariant: for every row in the `pallets` table, the value of `payload_mass` must equal the result of the declarative query on the normalized data model. Detecting violations of this invariant is both computationally expensive and requires building custom infrastructure.
- When writing a new mutation, you have to very carefully consider all the places in the database where denormalization is happening, to ensure the mutation doesn't violate implicit invariants.
- Dually, when _introducing_ denormalization, you have to consider all existing mutations to patch the ones that relate to the denormalized data.
- Bugs in the code require identifying all affected data (potentially impossible!) and running a data migration (incredibly tiresome).
- Finally, there is the cost of physical storage. While storage is cheap, IaaS providers love to charge extra for database disks, as if only the finest iron oxides are fit for your Postgres cluster.

### Views

This is an approach I experimented with. I call it the "tree of views". You write a view for each of these read-time properties, and then your queries can read from those views. It's a tree because views can query other views, since logic builds upon logic (e.g. the logic for how well a product line is selling depends on the logic for how well each product is selling). The result is that each view is a very focused, very atomic piece of business logic, and the top-level queries can read from the views as if they were reading denormalized data, so they are usually very short.

Concretely, for this case, you would write views like this:

```sql
-- Map a pallet ID to its payload mass.
create view pallet_payload_mass as
    select
        p.pallet_id,
        coalesce(sum(b.mass), 0) as payload_mass
    from
        pallets p
    left outer join
        boxes b on p.pallet_id = b.pallet_id
    group by
        p.pallet_id;

-- Map a pallet ID to its clearance state.
create view pallet_clearance as
    select
        p.pallet_id,
        (ppm.payload_mass < p.max_payload_mass) as cleared
    from
        pallets p
    inner join
        pallet_payload_mass ppm on p.pallet_id = ppm.pallet_id;
```

And so on. With the views having the following data dependencies:

![](/assets/content/composable-sql/view.svg)

What you hope happens is that Postgres will recursively inline every view, merge them all together into a gigaquery, and shuffle the predicates up and down to maximally optimize the query. That was the conceit. And of course that never happens.

The central problem is that views have to be written for the general case, and then you filter on the view's output. Views can't take parameters. And the optimizer is very conservative about pushing predicates down into the view. The `explain analyze` output shows these massive sequential scans, where only a tiny fraction of the data is ever needed, meaning Postgres is materializing the view and then filtering on it.

An analogous situation is if you've ever written a query with lots of very general CTEs, and with filtering at the end:

```sql
with foo as ( ... ),
     bar as ( ... ),
     baz as ( ... )
select derp from baz where ...;
```

These are often slow. Moving the predicates into the CTEs:

```sql
with foo as ( ... where ... ),
     bar as ( ... where ... ),
     baz as ( ... where ... )
select derp from baz;
```

Improves performance by forcing Postgres to do the filtering at earlier stages. But the fact that Postgres won't push predicates into the CTEs on its own means CTEs and views are a minefield of pessimization, and there's a performance upper bound to using them[^perf].

If the query planner were [sufficiently smart][sm], this wouldn't be a problem. But the sufficiently smart query planner is always just one more heuristic away.

[sm]: https://wiki.c2.com/?SufficientlySmartCompiler

# The Solution

Imagine a programming language without functions. You can only write code that operates on concrete values, i.e. variables or literals. So instead of writing a function an calling it anywhere you have to write these little code templates as comments and every time you want to "call" the "function" you copy the template and do a search/replace.

This would be tiresome. But that's what SQL is. The concrete values are the table names. The code is the queries. And the function templates you have to search replace are your business logic, which must be inlined everywhere it is used.

This formulation suggests the solution: we need something like functions, for SQL. That is, we need a way to define composable query fragments with statically-typed interfaces. I'm calling these **functors**.

## Functors

The parameters to a functor are tables satisfying some interface, the return type is the return type of the body query. For example, this[^syn]:

```sql
create functor author_books(
    a table (author_id uuid, name text),
    b table (title text, author_id uuid)
) returns table (author text, title text) as
    select
       	a.name as author,
       	b.title,
    from
    	   a
    inner join
    	   b on b.author_id = a.author_id;
```

Declares a functor `author_books`. The parameter `a` is any table that has _at least_ a column `author_id` of type `uuid` and a column `name` of type text [^null]. The functor's return type is the type of the rows returned by the query.

Table types form a [subtyping](https://en.wikipedia.org/wiki/Subtyping) relationship, so any table with a `title` column of type `text` can be passed as an argument. This is the same as to [row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism) in [TypeScript](https://www.typescriptlang.org/docs/handbook/type-compatibility.html).

## Functors for Testing

The reason testing is hard is SQL queries depend on _concrete_ tables. But functors can depend on _interfaces_ instead.

The business logic for "payload mass of a pallet" is implemented by this query:

```sql
-- Maps a pallet ID to its payload mass.
select
    p.pallet_id,
    coalesce(sum(b.mass), 0) as payload_mass
from
    pallets p
left outer join
    boxes b on p.pallet_id = b.pallet_id
group by
    p.pallet_id;
```

Can be parameterized like so:

```sql
create functor pallet_payload_mass(
    p table (pallet_id uuid),
    b table (pallet_id uuid, mass decimal)
) returns table (pallet_id uuid, payload_mass decimal) as
    select
        p.pallet_id,
        coalesce(sum(b.mass), 0) as payload_mass
    from
        p
    left outer join
        b on b.pallet_id = p.pallet_id
    group by
        p.pallet_id;
```

We can test this query against fake tables satisfying the interface, e.g.:

```sql
create table test_boxes (pallet_id uuid, mass decimal);
create table test_pallets (pallet_id uuid);

insert into ...;

select payload_mass from pallet_payload_mass(test_pallets, test_boxes);
```

While `pallets` point to `containers`, for this test, we don't need to create a container. We also don't need to come up with a value `max_payload_pass` for the pallet. If a value is causally independent of the query, it doesn't need to be provided.

Postgres also supports [table literals][lit], so there is actually a way to write things without doing a single `insert` whatever. Test data can be loaded into a table literal like so:

```sql
with test_boxes as (
    select * from (
        values ('...', 1.0),
               ('...', 2.0),
               ('...', 3.0),
    ) as t (pallet_id, mass)
)
```

The `test_books` CTE has type `(title text)`, and therefore satisfies the interface.

## Functors for Business Logic

We have a functor `pallet_payload_mass` that maps pallet IDs to their payload mass. The pallet clearance state depends on the pallet's maximum payload mass, and the pallet's actual payload mass, so we can implement it as a functor like so:

```sql
create functor pallet_clearance(
    p   table (pallet_id uuid, max_payload_mass decimal)
    ppm table (pallet_id uuid, payload_mass decimal)
) returns table (pallet_id uuid, cleared boolean) as
    select
   	    p.pallet_id,
       	(ppm.payload_mass <= p.max_payload_mass) as cleared
    from
        p
    inner join
        ppm on ppm.pallet_id = p.pallet_id;
```

Even though logic builds on logic, the `pallet_clearance` functor doesn't need to be aware of the `pallet_payload_mass` functor. The results of the latter can just be passed in. This makes the functors more testable (since testing one functor won't call another) but also keeps the interfaces small.

Say we want to query the clearance state of a specific pallet. How do we write this? We can try this:

```sql
select
    pc.cleared
from
    pallet_clearance(pallets, pallet_payload_mass(pallets, boxes)) pc
where
    pc.pallet_id = '...';
```

This query macroexpands into:

```sql
select
    pc.cleared
from
    (
        select
            p.pallet_id,
            (ppm.payload_mass <= p.max_payload_mass) as cleared
        from
            pallets p
        inner join
            pallet_payload_mass(pallets, boxes)
            on
            ppm.pallet_id = p.pallet_id
    ) pc
where
    pc.pallet_id = '...';
```

Which in turn expands into:

```sql
select
    pc.cleared
from
    (
        select
            p.pallet_id,
            (ppm.payload_mass <= p.max_payload_mass) as cleared
        from
            pallets p
        inner join
            (
                select
                    p.pallet_id,
                    coalesce(sum(b.mass), 0) as payload_mass
                from
                    pallets p
                left outer join
                    boxes b on b.pallet_id = p.pallet_id
                group by
                    p.pallet_id
            ) ppm
            on
            ppm.pallet_id = p.pallet_id
    ) pc
where
    pc.pallet_id = '...';
```

This is not satisfactory. It has the same problem of views: we're doing the filtering at the end, and relying on the optimizer to push the predicate down as far as it will go. Anecdotally, Postgres is more aggressive about optimizing subqueries than views, but relying on query planner arcana does not inspire confidence. We want to be able to write queries that expand into what we would write by hand.

Can we do better? Yes. We can just do this:

```sql
with pallets as (
    select * from pallets where pallet_id = '...'
)
select
    cleared
from
    pallet_clearance(pallets, pallet_payload_mass(pallets, boxes));
```

And that's it. If we want to filter a table early, we just filter it early, and pass the result to the functor. The functor can be applied to any table that satisfies the interface, including CTEs.

You can't do this with native SQL, because SQL does not compose. The closest you could implement is copying the business logic query manually into a CTE, renaming the table references (and hoping you didn't forget any), and now you have one more query duplicating business logic that has to be kept in sync with everything else.
