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

- every data model has invariants
  - statements that must always be true
- most invariants are implicit
    - things that people expect but wouldn't be able to list off the top of their head
    - good design is making invariants explicit rather than implicit
- for example: usernames
  - stored as strings
  - but what's a string?
  - anything from the empty strings to the complete works of william shakespeare
  - when you focus on it, usernames are a very small subset of strings
  - properties:
    - non-empty
    - short, some reasonable length, say, less than 100 characters
    - not including newlines
    - usually limited to some alphabet like alphanumeric characters plus underscores
      - no spaces, or, at least, not starting or ending with spaces
      - no weird symbols
      - no weird unicode homoglyph attacks
    - but where are such invariants enforced?
      - most are not enforced
      - some are enforced in various scattershot ways
- invariants can be ranked by how hard they are to enforce
    - at the bottom you have scalar invariants: that apply to a single column in a row.
        - they are the easiest to enforce.
    - then you have invariants that involve relations between two columns in the same row.
        - these are again easy to enforce.
    - then you have invariants that are about changes to a row:
        - relations between the pre-update and post-update (old and new) versions of the row
        - these are a bit harder to enforce: they require triggers.
    - larger-scale constraints are much harder to enforce
        - constraints that span multiple tables
        - i generally don't enforce these, because i don't know how
    - you should saturate all constraints that are easy to enforce
    - many of these you can come up with while designing the database, it's a matter of going through a checklist
    - some of these you'll only come up with after the fact, looking at the data, and wonder why you didn't think of them
    - when you run into that case, you should add those to your checklist
    - you should start with the strictest possible data model
    - it's easy, brutally easy, to go from strict to lax
    - it is very hard, often involving days or weeks of planning and work, to go from lax to strict

# Defense in Depth {#depth}

- where is this constraint enforced?
    - in the frontend?
    - in the backend?
    - in the database?
- answer: all of them
    - defense in depth
    - you will miss one
    - a 500 error is better than bad data
        - the former is transient
        - the latter is a thorn on the side, one of the tiny papercuts that lead to the death by a thousand cuts

# Checklist: Scalar Constraints {#scalar}

## Non-Null {#null}

- almost everything should be non-null
- if you have a null field that's iniitally null, you might consider having a different table instead
- one reason to avoid null is that null is two things
    - a way to make fields optional
    - a three-valued logic that propagates across the language
    - sql becomes harder to predict where nulls are involved

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
