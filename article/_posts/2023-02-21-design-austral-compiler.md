---
title: Design of the Austral Compiler
summary: A high-level walkthrough of the Austral bootstrapping compiler.
---

This post describes the design of the bootstrapping compiler for [Austral][austral].

[austral]: https://austral-lang.org/

# Contents

1. [Overview](#overview)
2. [Requirements](#requirements)
3. [Limitations](#limitations)
4. [High-Level View](#high-level-view)
5. [Frontend](#frontend)
    1. [The Concrete Syntax Tree](#cst)
    2. [Lexing](#lexing)
    3. [Parsing](#parsing)
    4. [Combining Pass](#combining)
6. [Core](#core)
    1. [Type Representation](#type)
    2. [The Environment](#env)
    3. [Import Resolution](#imports)
    4. [The Abstract Syntax Tree](#ast)
    5. [Abstraction Pass](#abst)
    6. [Extraction Pass](#extract)
    7. [Linked Representation](#linked)
    8. [Typing Pass](#typing)
    9. [Linearity Checking](#linearity)
    10. [Monomorphization](#mono)
7. [Backend](#backend)
    1. [C Representation](#crepr)
    2. [Code Generation](#codegen)
    3. [C Rendering](#crender)
8. [Built-In Modules](#builtins)
9. [The Entrypoint](#entrypoint)
10. [Testing](#testing)
11. [Future Work](#future)

# Overview {#overview}

- austral
  - features
    - linear types
    - capability-based security
    - a good module system
  - design goals
    - simplicity
    - short implementation
- compiler
  - written in ocaml
  - few lines of code
  - braindead code style: readable by anyone
  - bootstrapping: can't be written in austral because nothing is written in austral
- why ocaml
  - good language for writing compilers
  - algebraic data types
  - pattern matching
- link to "lessons from writing a compiler" post

# Requirements {#requirements}

- bootstrapping compiler
    - readable
    - maintanable
    - hackable
    - evolvable
    - not necessarily performant
- batch
- whole program
- good diagnostics

# Limitations {#limitations}

- batch
- very functional
    - poor performance

# High-Level View {#high-level-view}

- parsing
- combining
- import resolution
- abstraction
- extraction
- tast
- monomorphization
- code generation
- gcc

# Frontend {#frontend}

- cst
- parser
    - parser is written in menhir
    - lexer is ocamllex

## The Concrete Syntax Tree {#cst}

- simplest representation

## Lexing {#lexing}

- ocamllex
- give example

## Parsing {#parsing}

- menhir
- example
    - production rule
    - input
    - example

## Combining Pass {#combining}

- example
    - interface
    - body
    - combined representation

# Core {#core}

- environment
  - decls are stored
- typing
- linearity
- monomorphization

## Type Representation {#type}

- ty adt

## The Environment {#env}

- env interface
- env impl
- ids
- env manipulation is functional

## Import Resolution {#imports}

- identifiers are qualified
- rules

## The Abstract Syntax Tree {#ast}

- repr
- differences from cst

## Abstraction Pass {#abst}

- import resolution
- abstraction

## Extraction Pass {#extract}

- describe rule checking

## Linked Representation {#linked}

- adt

## Typing Pass {#typing}

- code quality: 5 out of 5 cthulhus
- big mess

## Linearity Checking {#linearity}

Describe [here](/article/how-australs-linear-type-checker-works).

## Monomorphization {#mono}

- mono ty
- mono expr
- monomorphization example

# Backend {#backend}

- very simple
- mono language is essentially equivalent to C
- c ast
- used to be c++ to take advantage of templates
    - that had problems

## C Representation {#crepr}

- simple c ast

## Code Generation {#codegen}

- code generation is a straightforward map
- a few things worth pointing out
    - compilation of structs
    - compilation of unions
    - compilation of case statements
- future work
    - context comments to identify what a monomorph is

## C Rendering {#crender}

- codegen is just string concatenation

# Built-In Modules {#builtin}

# The Entrypoint {#entrypoint}

# Testing {#testing}

# Future Work {#future}
