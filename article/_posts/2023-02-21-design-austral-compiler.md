---
title: Design of the Austral Compiler
summary: A high-level walkthrough of the Austral bootstrapping compiler.
---

This post describes the design of the bootstrapping compiler for [Austral][austral].

[austral]: https://austral-lang.org/

# Requirements

- bootstrapping compiler
    - readable
    - maintanable
    - hackable
    - evolvable
    - not necessarily performant
- batch
- whole program
- good diagnostics

# Limitations

- batch
- very functional
    - poor performance

# High-Level View

- parsing
- combining
- import resolution
- abstraction
- extraction
- tast
- monomorphization
- code generation
- gcc

# Frontend

- cst
- parser
    - parser is written in menhir
    - lexer is ocamllex

## The Concrete Syntax Tree

- simplest representation

## Lexing

- ocamllex
- give example

## Parsing

- menhir
- example
    - production rule
    - input
    - example

## Combining Pass

- example
    - interface
    - body
    - combined representation

# Core

## Type Representation

## The Environment

## Import Resolution

## The Abstract Syntax Tree

## Abstraction Pass

## Extraction Pass

## Linked Representation

## Representation of Types

## Typing Pass

## Monomorphization

# Backend

- very simple
- mono language is essentially equivalent to C
- c ast
- used to be c++ to take advantage of templates
    - that had problems

## C Representation

- simple c ast

## Code Generation

- code generation is a straightforward map
- a few things worth pointing out
    - compilation of structs
    - compilation of unions
    - compilation of case statements
- future work
    - context comments to identify what a monomorph is 

## C Rendering

- codegen is just string concatenation
