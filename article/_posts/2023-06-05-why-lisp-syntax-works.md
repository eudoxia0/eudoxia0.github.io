---
title: Why Lisp Syntax Works
summary: On uniform vs. specific syntax.
card: why-lisp-syntax-works.jpg
card_source: |
    Screenshot of the source code of [_Kandria_][kandria], an action RPG
    platformer written in Common Lisp.

    [kandria]: https://store.steampowered.com/app/1261430/Kandria/
---

[Lisp][lisp]'s unusual syntax is connected to its expressive power. How? Not
because of "homoiconicity", a word that has [no meaning][homo] but leaves people
somewhat impressed, because it sounds fanciful and mathematical. It's because of
_uniformity_.

[lisp]: https://lisp-lang.org/
[homo]: https://www.expressionsofchange.org/dont-say-homoiconic/

# Uniform

Language syntax exists on a spectrum from uniform to specific. Lisp (and XML)
have a _uniform syntax_[^uni]. The same syntactic construction of

```
(<operator> <arg1> <arg2> ... <argn>)
```

Is used for:

1. Expressions.
2. Expressions that would be statements in other languages (i.e. control flow).
3. Special operators like `if` and `let`.
4. Declarations, like function or class declarations.

This means that macros apply everywhere. The exact same `defmacro` facility lets
you write macros that operate on expressions, special operators, and
declarations.

You can write macros that make writing expressions easier, like
[`infix-math`][infix]:

[infix]: https://github.com/ruricolist/infix-math

```lisp
($ (x * 2) / (x * 3))

;; expands to (/ (* x 2) (* x 3))
```

You can write macros that operate on would-be statements, like [`for`][for]:

[for]: https://github.com/Shinmera/for

```lisp
(for:for ((li in (list 1 2 3 4))
          (vi across #(a b c d)))
  (format T "~&~a ~a" li vi))

;; expands to
;;
;; (loop for li in (list 1 2 3 4)
;;       for vi across #(a b c d)
;;       do (format T "~&~a ~a" li vi))
```

You can write macros that operate on declarations, like `deftable` in
[`crane`][crane]:

[crane]: https://github.com/eudoxia0/crane

```lisp
(deftable user ()
  (name :type text :uniquep t :nullp nil)
  (age :type integer :nullp t :initform 18)
  (description :type text))

;; expands to a whole bunch of ORM stuff
```

And macros can be defined succinctly, e.g.:

```lisp
(defmacro if-let ((var cond) &body consequent alternate)
  `(let ((,var ,cond))
     (if ,var
       ,consequent
       ,alternate)))
```

Defining a macro does not require some ceremonious process of writing an
AST-manipulating program, registering it with the build system or whatever, it
can be done inline, in the source. The language is extensible from within,
without requiring you to string together some build-time Rube Goldberg
preprocessing pipeline.

And macros are Lisp code, so they can use whatever toolkit of list and
tree-manipulation code you bring in. You can even define [lexically-scoped
macros][macrolet].

[macrolet]: https://www.lispworks.com/documentation/lw70/CLHS/Body/s_flet_.htm

Syntactic uniformity also means there is a lot less room for syntax
bikeshedding. Really the only thing you can bikeshed is the naming convention
for identifiers, and how S-expressions should be indented.

Common Lisp syntax works because of the conjunction of:

1. Syntactic uniformity, allowing macros to be applied everywhere.
2. Turing-complete macros that can execute arbitrary code at compile-time, and
   leverage libraries.
3. An image-based development model that blurs the lines between compile-time
   and run-time.

# Specific

At the other end of the spectrum you have languages with a highly _specific
syntax_. The best example is plain old fashioned SQL. This is the grammar for
the humble [`ALTER TABLE`][alter] statement in Postgres:

[alter]: https://www.postgresql.org/docs/current/sql-altertable.html

```sql
ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    action [, ... ]
ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    RENAME [ COLUMN ] column_name TO new_column_name
ALTER TABLE [ IF EXISTS ] [ ONLY ] name [ * ]
    RENAME CONSTRAINT constraint_name TO new_constraint_name
ALTER TABLE [ IF EXISTS ] name
    RENAME TO new_name
ALTER TABLE [ IF EXISTS ] name
    SET SCHEMA new_schema
ALTER TABLE ALL IN TABLESPACE name [ OWNED BY role_name [, ... ] ]
    SET TABLESPACE new_tablespace [ NOWAIT ]
ALTER TABLE [ IF EXISTS ] name
    ATTACH PARTITION partition_name { FOR VALUES partition_bound_spec | DEFAULT }
ALTER TABLE [ IF EXISTS ] name
    DETACH PARTITION partition_name [ CONCURRENTLY | FINALIZE ]

where action is one of:

    ADD [ COLUMN ] [ IF NOT EXISTS ] column_name data_type [ COLLATE collation ] [ column_constraint [ ... ] ]
    DROP [ COLUMN ] [ IF EXISTS ] column_name [ RESTRICT | CASCADE ]
    ALTER [ COLUMN ] column_name [ SET DATA ] TYPE data_type [ COLLATE collation ] [ USING expression ]
    ALTER [ COLUMN ] column_name SET DEFAULT expression
    ALTER [ COLUMN ] column_name DROP DEFAULT
    ALTER [ COLUMN ] column_name { SET | DROP } NOT NULL
    ALTER [ COLUMN ] column_name DROP EXPRESSION [ IF EXISTS ]
    ALTER [ COLUMN ] column_name ADD GENERATED { ALWAYS | BY DEFAULT } AS IDENTITY [ ( sequence_options ) ]
    ALTER [ COLUMN ] column_name { SET GENERATED { ALWAYS | BY DEFAULT } | SET sequence_option | RESTART [ [ WITH ] restart ] } [...]
    ALTER [ COLUMN ] column_name DROP IDENTITY [ IF EXISTS ]
    ALTER [ COLUMN ] column_name SET STATISTICS integer
    ALTER [ COLUMN ] column_name SET ( attribute_option = value [, ... ] )
    ALTER [ COLUMN ] column_name RESET ( attribute_option [, ... ] )
    ALTER [ COLUMN ] column_name SET STORAGE { PLAIN | EXTERNAL | EXTENDED | MAIN }
    ALTER [ COLUMN ] column_name SET COMPRESSION compression_method
    ADD table_constraint [ NOT VALID ]
    ADD table_constraint_using_index
    ALTER CONSTRAINT constraint_name [ DEFERRABLE | NOT DEFERRABLE ] [ INITIALLY DEFERRED | INITIALLY IMMEDIATE ]
    VALIDATE CONSTRAINT constraint_name
    DROP CONSTRAINT [ IF EXISTS ]  constraint_name [ RESTRICT | CASCADE ]
    DISABLE TRIGGER [ trigger_name | ALL | USER ]
    ENABLE TRIGGER [ trigger_name | ALL | USER ]
    ENABLE REPLICA TRIGGER trigger_name
    ENABLE ALWAYS TRIGGER trigger_name
    DISABLE RULE rewrite_rule_name
    ENABLE RULE rewrite_rule_name
    ENABLE REPLICA RULE rewrite_rule_name
    ENABLE ALWAYS RULE rewrite_rule_name
    DISABLE ROW LEVEL SECURITY
    ENABLE ROW LEVEL SECURITY
    FORCE ROW LEVEL SECURITY
    NO FORCE ROW LEVEL SECURITY
    CLUSTER ON index_name
    SET WITHOUT CLUSTER
    SET WITHOUT OIDS
    SET ACCESS METHOD new_access_method
    SET TABLESPACE new_tablespace
    SET { LOGGED | UNLOGGED }
    SET ( storage_parameter [= value] [, ... ] )
    RESET ( storage_parameter [, ... ] )
    INHERIT parent_table
    NO INHERIT parent_table
    OF type_name
    NOT OF
    OWNER TO { new_owner | CURRENT_ROLE | CURRENT_USER | SESSION_USER }
    REPLICA IDENTITY { DEFAULT | USING INDEX index_name | FULL | NOTHING }
```

That's half the grammar. I trimmed the second half because the point is made.

SQL syntax isn't composable, the way Lisp is. Composability is about having a
large number of distinct operators with a small number of expression holes,
while SQL is all about having a small number of operators with fifty different
syntactic variants.

And you can't write macros for SQL. You only options are C-style
text-manipulation macros, cleanly staged and separated from the SQL itself, or
writing a program that parses the SQL into an AST (good luck) and transforms it
at the AST level.

# The Mean

Most languages inhabit the middle ground. Generally, syntax at the same level
will have more or less the same form. Declarations all look kind of the same,
they start with a keyword (`struct`, `fn`, `union`) and use brackets to contain
their inner declarations (record fields, function bodies). Statements may all
end in semicolons. That kind of thing.

But for most languages, writing even a simple template match-and-replace macro
system is hard because the syntax is too specific and irregular. [I tried it
with C][cmacro] and the results were mixed.

[cmacro]: https://github.com/eudoxia0/cmacro

So for most languages, the only way to do macros is typed AST-manipulation code,
like OCaml's [PPX][ppx].

[ppx]: https://ocaml.org/docs/metaprogramming

I maintain that Common Lisp is the gold standard of DX for macros, but it works
only because Common Lisp exists at a _very narrow_ conjunction of tradeoffs. In
particular, image-based development is a rarity nowadays, a [Galápagos island
feature][galapagos] that is undesirable in many contexts, but it's the thing
that makes it possible to have Turing-complete macros that are defined in the
same place as the code, without needing to involve a build system.

[galapagos]: https://en.wikipedia.org/wiki/Gal%C3%A1pagos_syndrome

And languages that don't share this evolutionary niche should simply do away
with macros. The complexity-benefit tradeoff is not worth it.

# Footnotes

[^uni]:
    _Regular_ would be a better adjective, but that confuses it with [regular languages][lang].

[lang]: https://en.wikipedia.org/wiki/Regular_language
