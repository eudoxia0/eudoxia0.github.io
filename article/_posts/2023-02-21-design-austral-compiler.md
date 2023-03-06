---
title: Design of the Austral Compiler
summary: A high-level walkthrough of the Austral bootstrapping compiler.
---

[Austral][austral] is a new systems programming language. It features [linear
types][linear] for safe, compile-time memory and resource management;
[capability-based security][cap] to prevent [supply chain attacks][chain]; and
strong modularity. It is designed with fits-in-head simplicity as the goal.

[austral]: https://austral-lang.org/
[linear]: https://austral-lang.org/spec/spec.html#rationale-linear-types
[cap]: https://austral-lang.org/spec/spec.html#rationale-cap
[chain]: https://en.wikipedia.org/wiki/Supply_chain_attack

A while back I wrote a post on the [lessons I learnt writing the Austral
compiler][lessons]. This post is a more detailed walkthrough of the
[bootstrapping compiler][compiler] for Austral.

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
    2. [Errors](#errors)
    3. [The Environment](#env)
    4. [Import Resolution](#imports)
    5. [The Abstract Syntax Tree](#ast)
    6. [Abstraction Pass](#abst)
    7. [Extraction Pass](#extract)
    8. [Linked Representation](#linked)
    9. [Typing Pass](#typing)
    10. [Linearity Checking](#linearity)
    11. [Monomorphization](#mono)
7. [Backend](#backend)
    1. [C Representation](#crepr)
    2. [Code Generation](#codegen)
    3. [C Rendering](#crender)
8. [Testing](#testing)
9. [Future Work](#future)

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

The concrete syntax tree is the first tree representation in the compiler, and
the one closest to the original source code. It's defined in the Cst module and
it's just a set of types. The main four types represent:

- Module interface declarations.
- Module body declarations.
- Statements.
- Expressions.

And there's other auxiliary types. As an example, the sum type that represents
expressions looks like this:

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

Every case has a `span`, which carries source position information, and
optionally other data. For example, an arithmetic expression `a+b` is
represented by an instance of `CArith`, which has four values: the source span,
the arithmetic operator `+`, and the expressions `a` and `b`.

Analogously, `cstmt` is the type of concrete statements:

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

## Lexing {#lexing}

Lexing turns a string into a stream of tokens. For example, a code fragment like
`if x > 0` gets turned into the token stream `IF, IDENTIFIER "x", GREATER_THAN,
DEC_CONSTANT "0"`.

Tokens are defined, strangely, in the parser. Tokens that carry no information
(like symbols are language keywords) are defined like this:

```c
/* Brackets */
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
```

Tokens that carry a value (like integer literals or identifiers) are defined like this:

```c
/* Strings and docstrings */
%token <string> STRING_CONSTANT
%token <string> TRIPLE_STRING_CONSTANT
/* Identifiers and constants */
...
%token <string> DEC_CONSTANT
%token <string> HEX_CONSTANT
%token <string> BIN_CONSTANT
%token <string> OCT_CONSTANT
%token <string> CHAR_CONSTANT
%token <string> FLOAT_CONSTANT
%token <string> IDENTIFIER
```

Every `%token` declaration essentially corresponds to the constructors of a
`token` sum type (which you don't define, but the parser defines it internally):

```ocaml
type token =
  | LPAREN
  | RPAREN
  ...
  | IDENTIFIER of string
  | STRING_CONSTANT of string
```

The main part of the lexer is the `token` rule, which associates regular
expressions with code that evaluates to a token:

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

## Parsing {#parsing}

Parsing turns a sequence of tokens into a tree, specifically into the concrete syntax tree. The parser is written in Menhir, which is simple to use although the documentation is lacking: you just write a BNF grammar, and tell it how the right-hand side of each production rule should map to a CST instance.

For example: in the `cexpr` type (the CST type for Austral expressions), the constructor for function calls looks like this:

```ocaml
type cexpr =
  (* ... *)
  | CFuncall of span * identifier * concrete_arglist
```

That is, it has a code span, the function name, and the argument list, which in turn is defined:

```ocaml
type concrete_arglist =
  | ConcretePositionalArgs of cexpr list
  | ConcreteNamedArgs of (identifier * cexpr) list
```

With one case for positional argument lists like `(x,y,z)` and another for named
argument lists like `(foo => a, bar => b)`.

The parser rule for function calls is:

```ocaml
funcall:
  | identifier argument_list { CFuncall (from_loc $loc, $1, $2) }
  ;
```

There are two parts to this: the (E)BNF part and and the OCaml code in curly braces. The BNF says the `funcall` non-terminal is an `identifier` followed by an `argument_list`.

When this is encountered, this parser will execute the code in the curly
braces. This creates a `cexpr` value through the `CFuncall` constructor. `$loc`
is a special variable that has the parser's current location in the input
stream, `from_loc` turns this value into a `span`. `$1` and `$2` refer
respectively to `identifier` and `argument` list (it's also possible to name
them, when that increases clarity).

The rules for argument lists work the same way:

```
argument_list:
  | LPAREN positional_arglist RPAREN { ConcretePositionalArgs $2 }
  | LPAREN named_arglist RPAREN { ConcreteNamedArgs $2 }
  | LPAREN RPAREN { ConcretePositionalArgs [] }
  ;

positional_arglist:
  | separated_list(COMMA, expression) { $1 }
  ;

named_arglist:
  | separated_list(COMMA, named_arg) { $1 }
  ;
```

The last two show a useful feature of Menhir: parameterized rules. In this case
the pattern of rules for parsing comma-separated lists has been abstracted away
and we can just use `separated_list(COMMA, <rule>)`.

## Combining Pass {#combining}

In the combining pass, the separate CST representations for the module interface
and module body are combined into one object, that has one collection of
declarations, augmented with visibility information.

For example, given the following interface:

```austral
module Foo is
    constant Pi: Float32;

    record Pair: Free is
        x: Int32;
        y: Int32;
    end;

    type Quux: Free;

    function zero(): Nat64;
end module.
```

And the following body:

```
module body Foo is
    constant Pi: Float32 := 3.14;

    union Quux: Free is
        case Up;
        case Down;
    end;

    function zero(): Nat64 is
        return 0;
    end;

    function infinite(): Unit is
        return infinite();
    end;
end module body.
```

The combined representation, if rendered as code, might look something like
this:


```austral
combined module Foo is
    public constant Pi: Float32 := 3.14;

    public record Pair: Free is
        x: Int32;
        y: Int32;
    end;

    opaque union Quux: Free is
        case Up;
        case Down;
    end;

    public function zero(): Nat64 is
        return 0;
    end;

    private function infinite(): Unit is
        return infinite();
    end;
end combined module.
```

# Core {#core}

- environment
  - decls are stored
- typing
- linearity
- monomorphization

## Type Representation {#type}

- ty adt

## Errors {#errors}

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

Code generation is mostly a one-to-one map from the monomorphic AST to the C
AST. The function that codegens expressions makes this clear:

```ocaml
let rec gen_exp (mn: module_name) (e: mexpr): c_expr =
  let g = gen_exp mn in
  match e with
  | MNilConstant ->
     CBool false
  | MBoolConstant b ->
     CBool b
  | MIntConstant i ->
     CInt i
  | MFloatConstant f ->
     CFloat f
  | MStringConstant s ->
     CFuncall (
         "au_make_array_from_string",
         [
           CString s;
           CInt (string_of_int (String.length (escaped_to_string s)))
         ]
       )
  | MConstVar (n, _) ->
     CVar (gen_qident n)
  | MParamVar (n, _) ->
     CVar (gen_ident n)
  | MLocalVar (n, _) ->
     CVar (gen_ident n)
  | MGenericFunVar (id, _) ->
     CCast (CVar (gen_mono_id id), fn_type)
  | MConcreteFunVar (id, _) ->
     CCast (CVar (gen_decl_id id), fn_type)
  | MConcreteFuncall (id, _, args, _) ->
     CFuncall (gen_decl_id id, List.map g args)
  | MGenericFuncall (id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)
  | MConcreteMethodCall (id, _, args, _) ->
     CFuncall (gen_ins_meth_id id, List.map g args)
  | MGenericMethodCall (_, id, args, _) ->
     CFuncall (gen_mono_id id, List.map g args)

  ...
```

The only thing that is different is the one feature monomorphic Austral (i.e.,
Austral with all generic functions and typeclass instances monomorphized) has
that C does not: tagged unions.

An Austral union like this:

```austral
union OptionalInt: Free is
    case Present is
        value: Int32;
    case Absent;
end;
```

Gets compiled into something like this:

```c
enum OptionalInt_tag {
   PRESENT,
   ABSENT
}

typedef struct {
   OptionalInt_tag tag;
   union {
       struct { int32_t value; } Present;
       struct {} Absent;
   } data;
} OptionalInt;
```

Then, a case statement like:

```austral
let opt: OptionalInt := Present(value => 123);
case opt of
    when Present(value: Int32) do
        -- Do something with `value`.
    when Absent do
        -- Do something else.
end case;
```

Gets compiled into:

```c
switch (opt.tag) {
   case OptionalInt_tag.PRESENT:
        int32_t value = opt.data.Present.value;
        /* Do something with `value`. */
        break;
   case OptionalInt_tag.ABSENT:
        /* Do something else. */
        break;
}
```

Something I want to improve in the future: right now each monomorph gets
compiled to a function with the monomorph ID as the name, e.g.:

```
int mono_12() {
  ...
}
```

I'd like to add some more metadata to the compiled code, so there's at least a
comment above the function that says which function or typeclass method this
monomorph points to, and the type arguments it was called with.

## C Rendering {#crender}

And as if the compiler wasn't braindead enough, the last step of code generation
is literally string concatenation: the C AST gets turned into a string. This process is called rendering.

Ideally the compiled C code should be readable, which means it has to be
indented. Rather than handle the string padding when rendering the C AST, the
rendering process returns a list of `line` values, which have an indentation
count and a string of the actual contents of that line, e.g.:

```ocaml
and render_stmt (i: indentation) (stmt: c_stmt): line list =
  match stmt with
  | CLet (name, ty, value) ->
     let s = (render_type ty) ^ " " ^ name ^ " = " ^ (e value) ^ ";" in
     [Line (i, s)]
  | CAssign (lvalue, value) ->
     let s = (e lvalue) ^ " = " ^ (e value) ^ ";" in
     [Line (i, s)]
  | CDiscarding value ->
     [Line (i, (e value) ^ ";")]
  | CIf (c, t, f) ->
     List.concat [
         [Line (i, "if (" ^ (e c) ^ ") {")];
         render_stmt (indent i) t;
         [Line (i, "} else {")];
         render_stmt (indent i) f;
         [Line (i, "}")]
       ]
```

Each line then gets turned into a string by turning its identation count `i`
into a string of with `i` space characters and prefixing it to the code:

```ocaml
let render_line (Line (Indentation i, s)) =
  (String.make i ' ') ^ s
```

And then all the lines get concatenated together:

```ocaml
let rec render_unit (CUnit (name, decls)): string =
  let rd d =
    String.concat "\n" (List.map render_line (render_decl zero_indent d))
  in
  "/* --- BEGIN translation unit for module '" ^ name ^ "' --- */\n"
  ^ (String.concat "\n\n" (List.map rd decls))
  ^ "\n/* --- END translation unit for module '" ^ name ^ "' --- */\n"
```

# Testing {#testing}

The compiler has some OCaml tests that test the individual components
directly. But because this is a bootstrapping compiler that is meant to evolve
rapidly there's few of these: they'd mostly be destroyed by large-scale
refactoring.

Instead, most of the tests are end-to-end tests, where the compiler is treated
as a black box.

Tests are stored in the [`test-programs/suites/`][suites] directory, each test
is a folder with some Austral code, and the test is essentially defined by the
files in that folder:

[suites]: https://github.com/austral/austral/tree/96f007fd9bad0ad8070669acb9757231d785b4b8/test-programs/suites

- If the code is meant to compile and run successfully, you just need a
  `Test.aum` file.
- If the code is meant to compile, and run, and emit some specific output, you
  need a `program-stdout.txt` file with the expected output.
- If the code is meant to _fail_ to compile, you need an `austral-stderr.txt`
  file with the expected compiler error message.

The [test runner][runner] is a simple Python program that walks the test folder and executes every test it finds.

[runner]: https://github.com/austral/austral/blob/96f007fd9bad0ad8070669acb9757231d785b4b8/test-programs/runner.py

# Future Work {#future}

In addition to many bug fixes, these are some things I plan to do with the compiler in the future:

1. **HTML Error Reporting:** because terminals are incredibly anemic. I got the
   idea, of all things, from [Inform 7][i7], which is far from a normal
   programming language but their error output has always been lovely.

1. **JSON Error Reporting:** this is for the end-to-end tests. Right now, tests
   of programs that are expected to fail to compile just assert that the
   compiler produces a particular message. This is not robust to changes in the
   error reporting format. So I'd like to add JSON error reporting that just
   produces a reliably structured message for the end-to-end tests or for
   clients to parse the errors if they want to.

1. **Separate Compilation:** this will likely involve significant refactoring,
   but it will allow large programs to be compiled effectively by allowing
   compilation to happen in parallel and to pick up where it left off.

4. **Literate Programming:** I'm not sure whether this is a great idea or a
   terrible idea, but part of me wants to refactor the compiler into a literate
   program. This would help onboard more people, but it would also make the
   compiler's source code into the most useful compilers textbook ever written,
   since there are so few textbooks that actually focus on the frontend (type
   checking, diagnostics, etc.) parts of a compiler, and so few that actually
   show you how to build a production-quality compiler.

[i7]: https://ganelson.github.io/inform-website/
