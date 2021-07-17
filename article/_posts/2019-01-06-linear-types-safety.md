---
title: Linear Types and Safety
summary: An example of hardening an API using linear types.
tags: [plt]
---

A simple type system like that of C helps us by pointing out errors concerning
the shape of data: trying to fit a square peg in a round hole. More advanced
type systems help us express the semantic role of data in our types.

Linear types prevent errors concerning *when* data is used rather than *how*:
any error equivalent to a use-after-`free` in C or similar languages is
eliminated.

Consider the following database API, using SML notation:

```sml
val connect : string -> database
val query : database -> string -> result list
val close : database -> unit
```

Briefly, the `connect` function takes a connection URI and returns a `database`
instance, the `query` function takes a database instance and a query string and
returns the result list of that query, and `close` closes a database connection.

The correct usage pattern is: we open a database connection, make zero or more
queries, then close it. Visually, we can represent it like this:

![Database lifecycle diagram]({{ site.post_images }}/linear-types-safety/usage.png)

But SML's type system does not allow us to enforce this.

For instance, we can use the database after it's been closed:

```sml
close db;
val results = query db "SELECT ...";
```

Or we can close the database twice in a row:

```sml
close db;
close db;
```

These errors are similar to use-after-`free` and double-`free` errors in memory
management, respectively. Linear types can help us eliminate this category of
errors entirely. Looking at the graph above, what we want is a way to prevent
the programmer from drawing an arrow from `close` to `query`.

Consider the same API, but in a slightly different type system where prefixing a
type name with `!` denotes a linear type. Then:

```sml
val connect : string -> !database
val query : !database -> string -> (result list, database)
val close : !database -> unit
```

Now these errors disappear. Use-after-`close` is impossible, because `close`
returns `unit`:

```sml
let val db = connect "my_database"
in
  close db;
  do_something_with db; (* This line is an error because `db` has already been used in the line above *)
end
```

And double-`close` errors are also impossible for the same reason:

```sml
let val db = connect "my_database"
in
  close db;
  close db; (* `db` is used twice *)
end
```

But we can still make multiple queries with the linearly-typed API, because the
`query` function returns a tuple of the result set and the new database
object. In SML notation:

```sml
let val db = connect "my_database"
in
  let val (results, db') = query "INSERT ..." db
  in
    let val (results, db'') = query "SELECT ..." db'
    in
      close db''
    end
  end
end
```

(You can imagine using something like Haskell's `do` notation to remove the
nesting from this example.)
