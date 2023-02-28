---
title: Design of the Austral Compiler
summary: A high-level walkthrough of the Austral bootstrapping compiler.
---

[Austral][austral] is a new systems programming language. It features [linear types][linear] for safe, compile-time memory and resource management; [capability-based security][cap] to prevent [supply chain attacks][chain]; and strong modularity. It is designed with fits-in-head simplicity as the goal.

[austral]: https://austral-lang.org/
[linear]: https://austral-lang.org/spec/spec.html#rationale-linear-types
[cap]: https://austral-lang.org/spec/spec.html#rationale-cap
[chain]: https://en.wikipedia.org/wiki/Supply_chain_attack

A while back I wrote a post on the [lessons I learnt writing the Austral compiler][lessons]. This post is a more detailed walkthrough of the [bootstrapping compiler][compiler] for Austral.

[lessons]: /article/lessons-writing-compiler
[compiler]: https://github.com/austral/austral

# Contents

1. [Why OCaml?](#why)
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

# Why OCaml? {#why}

[OCaml][ocaml] is a great language for writing compilers. Like Haskell and other
languages in the ML family, its support for [sum types][sum] and pattern
matching makes it very easy to write tree transformations, which is what most of
a compiler is. Like Haskell, you can write functional code; unlike Haskell, you
can mutate and perform side effects pervasively. And OCaml has [Menhir][menhir],
a really outstanding parser generator on par with Java's [ANTLR][antlr].

[ocaml]: https://ocaml.org/
[sum]: https://ocaml.org/docs/data-types#a-simple-custom-type
[menhir]: http://gallium.inria.fr/~fpottier/menhir/
[antlr]: https://www.antlr.org/

# Requirements {#requirements}

1. **Short Time to MVP:** it's a bootstrapping compiler, not a production
   compiler, so the goal is to be able to get to "Hello, world!" as early as
   possible. Consequently the implementation should be as simple and quick to
   implement as possible. As a result the compiler is just 12,000 lines of
   straightforward grugbrained OCaml.

1. **Readable:** the compiler is written in the least fanciful style of OCaml
   imaginable. There's no transdimensional optical profunctors or whatever
   Haskellers are up to.

1. **Hackable:** languages evolve most in their early stages. Writing a
   highly-optimized production compiler would be premature if you end up having
   to throw out most of it because you changed your mind about some central
   language feature. So the compiler is designed to be hackable and gradually
   evolvable, at the expensive of semantics-specific optimizations and thus
   performance.

1. **Correctness:** the compiler is a reference implementation, so correctness
   matters above most other things.

# Limitations {#limitations}

The above requirements, which prioritize certain dimensions over others, have
some consequences:

1. **No Separate Compilation:** for simplicity, the compiler is a whole-program
   compiler. It compiles all source files, in order, from the first one to the
   last one. Separate compilation is not supported.
1. **Batch Compilation:** also for simplicity, the compiler is a batch compiler,
   so there's no incremental compilation or external symbol database. Everything
   happens in one long pipeline.
1. **Performance:** much of the compiler is written in an easily-testable
   functional style, potentially at the expense of performance.
1. **Backend:** the backend is just C, which is in turn compiled by a C compiler
   (though it would not be too much work to "upgrade" this to compile to LLVM
   IR).

# High-Level View {#high-level-view}

The following diagram shows the flow of data in the compiler. Blue nodes are
source representations, green nodes are compiler passes:

<a href="/assets/content/design-austral-compiler/pipeline.png"><img style="max-width: 30%; margin-left: auto; margin-right: auto;" src="/assets/content/design-austral-compiler/pipeline.png" alt="Graphviz diagram of the compiler pipeline."></a>

# Frontend {#frontend}

The frontend is the parser, which takes source code and turns it into the
earliest tree representation: the concrete syntax tree. After that there's the
combining pass, where the separate module interface and module body are combined
into a single object. The parser is written in [Menhir][menhir], a powerful
parser generator for OCaml, and the lexer is writting in [ocamllex][lex].

[lex]: https://v2.ocaml.org/manual/lexyacc.html

## The Concrete Syntax Tree {#cst}

- simplest representation
- just types
`concrete_decl`
`concrete_def`

```ocaml
and cexpr =
  | CNilConstant of span
  | CBoolConstant of span * bool
  | CIntConstant of span * string
  | CFloatConstant of span * string
  | CStringConstant of span * string
  | CVariable of span * identifier
  | CArith of span * arithmetic_operator * cexpr * cexpr
  | CFuncall of span * identifier * concrete_arglist
  | CComparison of span * comparison_operator * cexpr * cexpr
  | CConjunction of span * cexpr * cexpr
  | CDisjunction of span * cexpr * cexpr
  | CNegation of span * cexpr
  | CIfExpression of span * cexpr * cexpr * cexpr
  | CPath of span * cexpr * concrete_path_elem list
  | CEmbed of span * typespec * string * cexpr list
  | CDeref of span * cexpr
  | CTypecast of span * cexpr * typespec
  | CSizeOf of span * typespec
  | CBorrowExpr of span * borrowing_mode * identifier
```

```ocaml
and cstmt =
  | CSkip of span
  | CLet of span * identifier * typespec * cexpr
  | CDestructure of span * concrete_binding list * cexpr
  | CAssign of span * concrete_lvalue * cexpr
  | CIf of span * cexpr * cstmt * cstmt
  | CCase of span * cexpr * concrete_when list
  | CWhile of span * cexpr * cstmt
  | CFor of span * identifier * cexpr * cexpr * cstmt
  | CBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      body: cstmt;
      mode: borrowing_mode
    }
  | CBlock of span * cstmt list
  | CDiscarding of span * cexpr
  | CReturn of span * cexpr
```

```ocaml
and typespec =
  | TypeSpecifier of identifier * typespec list
  | ConcreteReadRef of typespec * typespec
  | ConcreteWriteRef of typespec * typespec
```

## Lexing {#lexing}

- ocamllex
- give example
- `token` rule

```ocaml
rule token = parse
  (* Comments *)
  | comment { advance_line lexbuf; token lexbuf }
  (* Brackets *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LCURLY }
  | "}" { RCURLY }
  (* Arithmetic operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  (* ... *)
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline { advance_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ {err ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'") }
```

`if x > 0` gets turned into the token stream `IF, IDENTIFIER "x", GREATER_THAN, INT "0"`.

## Parsing {#parsing}

- menhir
- example
    - production rule
    - input
    - example

```ocaml
/* Module interfaces and bodies */

module_int:
  | doc=docstringopt imports=import_stmt* MODULE
    name=module_name IS decls=interface_decl*
    END MODULE PERIOD EOF
    { ConcreteModuleInterface (name, doc, imports, decls) }
  ;

module_body:
  | doc=docstringopt imports=import_stmt* MODULE BODY
    name=module_name IS pragmas=pragma* decls=body_decl*
    END MODULE BODY PERIOD EOF
    { make_module_body name imports pragmas decls doc }
  ;
```

```ocaml
atomic_expression:
  | NIL { CNilConstant (from_loc $loc) }
  | TRUE { CBoolConstant (from_loc $loc, true) }
  | FALSE { CBoolConstant (from_loc $loc, false) }
  | int_constant { $1 }
  | float_constant { $1 }
  | string_constant { $1 }
  | path { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  | intrinsic { $1 }
  | SIZEOF LPAREN typespec RPAREN { CSizeOf (from_loc $loc, $3) }
  | BORROW_READ identifier { CBorrowExpr (from_loc $loc, ReadBorrow, $2) }
  | BORROW_WRITE identifier { CBorrowExpr (from_loc $loc, WriteBorrow, $2) }
  | DEREF atomic_expression { CDeref (from_loc $loc, $2) }
  ;
```

```ocaml
funcall:
  | identifier argument_list { CFuncall (from_loc $loc, $1, $2) }
  ;

parenthesized_expr:
  | LPAREN expression RPAREN { $2 }
  ;

argument_list:
  | LPAREN positional_arglist RPAREN { ConcretePositionalArgs $2 }
  | LPAREN named_arglist RPAREN { ConcreteNamedArgs $2 }
  | LPAREN RPAREN { ConcretePositionalArgs [] }
  ;
```

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

Described [here](/article/how-australs-linear-type-checker-works).

## Monomorphization {#mono}

- mono ty
- mono expr
- monomorphization example

# Backend {#backend}

The backend is very simple: it spits out C code. There's a group of types to
represent C ASTs, the monomorphic AST gets transformed into a C AST by the code
generation step. The codegen is very simple given that monomorphized Austral is
very close to C, pretty much the only feature Austral has that C doesn't is
tagged unions.

It used to be the backend was C++, and generic Austral functions and generic
typeclass instances were compiled to templated C++ functions. That meant I
didn't have to implement monomorphization myself (which was a huge pain in the
ass) but it created these inscrutable bugs due to the subtle differences between
the two type systems. It also covered up bugs in type-checking and typeclass
resolution that would have shown up had I implemented monomorphization. So I
ended up changing the backend to straightforward C and implementing
monomorphization in OCaml.

## C Representation {#crepr}

The C AST is defined by the types in the `CRepr` module. This is more or less
what you'd expect, for example, the type of C expressions is:

```ocaml
type c_expr =
  | CBool of bool
  | CInt of string
  | CFloat of string
  | CString of escaped_string
  | CVar of string
  | CFuncall of string * c_expr list
  | CFptrCall of c_expr * c_ty * c_ty list * c_expr list
  | CCast of c_expr * c_ty
  | CArithmetic of arithmetic_operator * c_expr * c_expr
  | CComparison of comparison_operator * c_expr * c_expr
  | CConjunction of c_expr * c_expr
  | CDisjunction of c_expr * c_expr
  | CNegation of c_expr
  | CIfExpression of c_expr * c_expr * c_expr
  | CStructInitializer of (string * c_expr) list
  | CStructAccessor of c_expr * string
  | CPointerStructAccessor of c_expr * string
  | CIndex of c_expr * c_expr
  | CAddressOf of c_expr
  | CEmbed of c_ty * string * c_expr list
  | CDeref of c_expr
  | CSizeOf of c_ty
```

The types for C type specifiers, statements, and declarations are defined
analogously.

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
