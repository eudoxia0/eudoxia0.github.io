---
title: 'Parsing with Menhir, Part I: Forth'
summary: A tutorial on parsing Forth with Menhir.
tags: [parsing, ocaml, forth]
---

This is the first post on a series on parsing using the Menhir parser generator
for [OCaml][ocaml]. I found the existing tutorials insufficient, so I wrote my
own.

[Menhir][menhir] is the standard parser generator for
OCaml. [ocamllex][ocamllex] is the standard lexer generator. They are built to
work together. We're using OCaml because it's a great language for writing compilers.

# Preliminaries

Install [`opam`][opam], the package manager for OCaml, then install
[`dune`][dune], the OCaml build system, by running:

```bash
$ opam install dune
```

# Source Code

The code for this tutorial is in [this repository][repo], in the `forth`
directory.

# Project Structure

The project structure is:

```
forth/
  bin/
    dune
    menhir-tutorial.ml
  lib/
    dune
    Ast.ml
    Lexer.mll
    Parser.mly
    ParserInterface.ml
  test/
    dune
    ParserTest.ml
  dune-project
```

Much of this is build system boilerplate. The important files are:

`bin/menhir-tutorial.ml`
: The entrypoint for the interactive executable.

`lib/Ast.ml`
: Types to represent Forth programs.

`lib/Lexer.mll`
: The lexer source code.

`lib/Parser.mly`
: The parser source code.

`lib/ParserInterface.ml`
: An abstraction to simplify calling the parser.

`test/ParserTest.ml`
: Tests of the parser.

# Forth

[Forth][forth] is a simple language and parsing it is little more than lexing: a
Forth program is a string of _atoms_, where each atom is one of:

- An integer constant, like `123` or `-10`.
- A floating point constant, like `3.14` or `6.1e23`.
- A _word_, which is a basically a function call, like `ADD` or `PRIMEP`.

# Representing Forth Programs

Menhir won't write type definitions for us. We have to first define the types to
represent Forth programs. Since Forth is very simple, the types are a one-to-one
translation of the definition above.

First, an `atom` is either an integer constant, or a float constant, or a word:

```ocaml
type atom =
  | Int of int
  | Float of float
  | Word of string
```

Second, a program is a sequence of `atoms`:

```ocaml
type program = Program of atom list
```

That's it.

For interactivity, let's add a couple of functions to turn atoms and programs
into human-readable strings:

```ocaml
let dump_atom = function
  | Int i ->
     "Int " ^ string_of_int i
  | Float f ->
     "Float " ^ string_of_float f
  | Word s ->
     "Word \"" ^ s ^ "\""

let dump_program (Program atoms) =
  "[" ^ (String.concat ", " (List.map dump_atom atoms)) ^ "]"
```

# The Lexer

Lexing means turning a stream of bytes into a stream of tokens. The lexer
essentially applies a series of regular expressions to the input until one
matches, and returns the token associated to that regular expression. But it's
still a flat list of tokens. Later, the parser will turn that flat list of
tokens into a tree.

The full source code of the lexer is:

```ocaml
let digit = ['0'-'9']
let sign = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+

let exponent = ['e' 'E']
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+

(* Rules *)

rule token = parse
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { FLOAT_CONSTANT (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { WORD (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
```

The lexer is divided into two parts: `let` definitions and rules. A simple lexer
like this has one rule, generally strings like string parsing are implemented as
other rules. But we're not doing string literals in this example yet.

Regular expressions can get very complicated, so we can use `let` to define
regular expression fragments. For example, `digit` is a regular expression
fragment that matches any digit from 0 to 9:

```ocaml
let digit = ['0'-'9']
```

`sign` matches the plus or minus characters:

```ocaml
let sign = ['-' '+']
```

And `alpha` matches any letter in the alphabet, lowercase or uppercase:

```ocaml
let alpha = ['a'-'z' 'A'-'Z']
```

The regular expression for integer constants is then:

```ocaml
let int_constant = sign? digit+
```

Which is simpler, and easier to read, than its equivalent expanded form:

```ocaml
let int_constant = ['-' '+']? ['0'-'9']+
```

The regular expression for float constants is more complicated:


```ocaml
let exponent = ['e' 'E']
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
```

Finally, the regex for identifiers: a letter followed by zero or more letters, digits, and dashes.

```ocaml
let identifier = alpha (alpha | digit | '-')*
```

We also need a regex to match whitespace:

```
let whitespace = [' ' '\t']+
```

Next, we define the rule for parsing a token. We have three types of tokens in
the language: integer constants, floating point constants, and words. After that
we have to handle three special cases.

The general structure of a rule is:

```
  | <regex> { <expression> }
```

When `<regex>` matches the input stream, evaluate `<expression>`. When writing
rules, the expression `Lexing.lexeme lexbuf` extracts the text matched by the
regex on the left hand side of the rule.

The rules for parsing atoms are simple:

```ocaml
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { FLOAT_CONSTANT (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { WORD (Lexing.lexeme lexbuf) }
```

Translated:

1. If the `int_constant` regex matches the input, take the string matched by the
   regex, convert the string to an integer, and return the `INT_CONSTANT` token
   with said integer as the argument.

   Here, `INT_CONSTANT` is a function that takes an int and returns a
   token. This is defined, in a somewhat strange-looping manner, by the parser,
   which complicates the presentation. Just hold on and we'll get to it in the
   next section.

2. If the `float_constant` regex matches the input, take the matched string,
   convert it to a float, and return a `FLOAT_CONSTANT` token with the float
   value. The type of `FLOAT_CONSTANT` is `float -> token`. Again, see below.

3. If the `identifier` regex is satisfied, return a `WORD` token with just the
   matched text.

The last two rules are universal: `eof` produces the `EOF` token (we need this
for the parser, the start symbol needs to end with an EOF to signal to the
parser it need not consume anything else), and any character not matched by any
regular expression produces a lexer error:

```ocaml
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
```

# The Parser

Parsers are defined in `.mly` files. A parser has two sections, separated by a
line with two percent signs:

```
<declarations>

%%

<rules>
```

The `declarations` section allows you to define tokens and the start symbols of
the grammar, the start symbol being the rule where the parser begins. You can
have multiple start symbols, this can be useful to test different parts of the
parser in isolation.

First, we have to import the `Ast` module so we can access the constructors of
the `program` and `atom` types to build up our abstract syntax tree.

```ocaml
%{
open Ast
%}
```

The percentage sign and curly brace are for embedding OCaml code in the
parser. This can be, for example, to define functions that will be used in the
rules. But our parser is very simple so we don't need anything other than an
import.

Then, we define our tokens. Tokens that carry no data (like language keywords,
symbols like braces, or the `EOF` token) are defined with `%token [name]`.

We define the `EOF` token as follows:

```ocaml
%token EOF
```

Tokens can also carry information. For example, a string constant token would
carry the text of the string. We define this with the syntax `%token <[type]>
[name]`. Note that the angle brackets are literal here.

We define the `INT_CONSTANT`, `FLOAT_CONSTANT`, and `WORD` tokens we used
earlier in the parser:

```ocaml
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> WORD
```

Finally, we specify the start rule, which will be called `program`:

```ocaml
%type <Ast.program> program
%start program
```

Menhir needs to know the type of all start symbols. Additionally, the type has
to be a fully qualified name, even though we've imported the `Ast` module
earlier.

Now we move on to the rules.

The rule for parsing a program is very simple: a program is a non-empty sequence
of atoms, followed by `EOF`:

```ocaml
%%

program:
  | atom* EOF { Program $1 }
  ;
```

`atom*` produces a potentially empty list of `atom` instances.

The `EOF` token is very important: all start symbols have to end with
`EOF`. Otherwise, the parser will error because it expects nothing, but
encounters an `EOF` token.

The rule for parsing atoms is a one-to-one match with the definition of atoms
above: an atom is either an integer constant, a float constant, or a word.

```ocaml
atom:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float i }
  | s=WORD { Word s }
  ;
```

In the first case, the expression `i` is of type `int`, since the `INT_CONSTANT`
token carries an `int` value. Analogously, in the second case the type of `f` is
`float` since `FLOAT_CONSTANT` carries a `float`, and in the third case the type
of `s` is `string` since the `WORD` token carries a `string`.

Note how the grammar definition matches the inductive definition of the types:

```ocaml
type program = Program of atom list

type atom =
  | Int of int
  | Float of float
  | Word of string
```

The full contents of `lib/Parser.mly`:

```ocaml
%{
open Ast
%}

%token EOF
%token <int> INT_CONSTANT
%token <float> FLOAT_CONSTANT
%token <string> WORD

/* Types */

%type <Ast.program> program
%start program

%%

program:
  | atom* EOF { Program $1 }
  ;

atom:
  | i=INT_CONSTANT { Int i }
  | f=FLOAT_CONSTANT { Float f }
  | s=WORD { Word s }
  ;
```

# Parser Interface

This is just a bit of boilerplate: we create a function `parse_program: string
-> program`. Parse errors throw the `Failure` exception with line and column
information in the error message.

```ocaml
open Lexing

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try
    f Lexer.token lexbuf
  with Parser.Error ->
    raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))

let parse_program s =
  parse' Parser.program s
```

# Unit Tests

We use the [OUnit2][ounit2] unit test library.

First, we import the unit test library, and we import `Ast` to have access to the constructors for the `program` and `atom` types, and we import `ParserInterface` to have the `parse_program` function.

```ocaml
open OUnit2
open Menhir_tutorial_core.Ast
open Menhir_tutorial_core.ParserInterface
```

`peq` (for "parse equals") is a brief utility function to check that the given
string parses to the given value:

```ocaml
let peq (s: string) (v: 'a) =
  assert_equal v (parse_program s)
```

Unit tests of parsing integer constants:

```ocaml
let test_parse_int_constants _ =
  peq "0" (Program [Int 0]);
  peq "123" (Program [Int 123]);
  peq "+123" (Program [Int 123]);
  peq "1000" (Program [Int 1000]);
  peq "-0" (Program [Int 0]);
  peq "-123" (Program [Int (-123)])
```

Unit tests of parsing float constants:


```ocaml
let test_parse_float_constants _ =
  peq "0.0" (Program [Float 0.0]);
  peq "123.0" (Program [Float 123.0]);
  peq "+123.0" (Program [Float 123.0]);
  peq "1000.0" (Program [Float 1000.0]);
  peq "-123.0" (Program [Float (-123.0)]);
  peq "123.0e6" (Program [Float 123.0e6]);
  peq "123.0e-6" (Program [Float 123.0e-6]);
  peq "-123.0e6" (Program [Float (-123.0e6)]);
  peq "-123.0e-6" (Program [Float (-123.0e-6)])
```

Unit tests of parsing words:

```ocaml
let test_parse_words _ =
  peq "TEST" (Program [Word "TEST"]);
  peq "TEST-PROGRAM" (Program [Word "TEST-PROGRAM"]);
  peq "TEST-123" (Program [Word "TEST-123"])
```

Unit tests of parsing atom sequences:

```ocaml
let test_parse_programs _ =
  peq "TEST 3.14   123 -3.4" (Program [Word "TEST"; Float 3.14; Int 123; Float (-3.4)]);
  peq "A 1 B" (Program [Word "A"; Int 1; Word "B"])
```

Finally, we put it all together into a test suite and run it:

```ocaml
let suite =
  "Parser tests" >::: [
      "Integer constants" >:: test_parse_int_constants;
      "Float constants" >:: test_parse_float_constants;
      "Words" >:: test_parse_words;
      "Programs" >:: test_parse_programs
    ]

let _ = run_test_tt_main suite
```

The full contents of `test/ParserTest.ml` file:

```
open OUnit2
open Menhir_tutorial_core.Ast
open Menhir_tutorial_core.ParserInterface

let peq (s: string) (v: 'a) =
  assert_equal v (parse_program s)

let test_parse_int_constants _ =
  peq "0" (Program [Int 0]);
  peq "123" (Program [Int 123]);
  peq "+123" (Program [Int 123]);
  peq "1000" (Program [Int 1000]);
  peq "-0" (Program [Int 0]);
  peq "-123" (Program [Int (-123)])

let test_parse_float_constants _ =
  peq "0.0" (Program [Float 0.0]);
  peq "123.0" (Program [Float 123.0]);
  peq "+123.0" (Program [Float 123.0]);
  peq "1000.0" (Program [Float 1000.0]);
  peq "-123.0" (Program [Float (-123.0)]);
  peq "123.0e6" (Program [Float 123.0e6]);
  peq "123.0e-6" (Program [Float 123.0e-6]);
  peq "-123.0e6" (Program [Float (-123.0e6)]);
  peq "-123.0e-6" (Program [Float (-123.0e-6)])

let test_parse_words _ =
  peq "TEST" (Program [Word "TEST"]);
  peq "TEST-PROGRAM" (Program [Word "TEST-PROGRAM"]);
  peq "TEST-123" (Program [Word "TEST-123"])

let test_parse_programs _ =
  peq "TEST 3.14   123 -3.4" (Program [Word "TEST"; Float 3.14; Int 123; Float (-3.4)]);
  peq "A 1 B" (Program [Word "A"; Int 1; Word "B"])

let suite =
  "Parser tests" >::: [
      "Integer constants" >:: test_parse_int_constants;
      "Float constants" >:: test_parse_float_constants;
      "Words" >:: test_parse_words;
      "Programs" >:: test_parse_programs
    ]

let _ = run_test_tt_main suite
```

# Bonus: An RPN Calculator

I would be remiss in my duties if I didn't do this:

```
(* bin/menhir_tutorial.ml *)
open Menhir_tutorial_core.Ast
open Menhir_tutorial_core.ParserInterface

let pop = function
  | [] -> raise (Failure "Stack is empty")
  | first::rest -> (first, rest)

let push v s = v :: s

let rec eval_program program stack =
  match program with
  | [] ->
     stack
  | head::rest ->
     let stack = (match head with
                  (* Constants are self-evaluating *)
                  | Int i -> push (Int i) stack
                  | Float i -> push (Float i) stack
                  | Word s ->
                     (match s with
                      | "ADD" -> oper (+.) stack
                      | "SUB" -> oper (-.) stack
                      | "MUL" -> oper ( *. )  stack
                      | "DIV" -> oper (/.) stack
                      | _ -> raise (Failure ("Unknown word: " ^ s))))
     in
     eval_program rest stack

and oper f stack =
  let (rhs, stack) = pop stack in
  let (lhs, stack) = pop stack in
  let lhs = as_float lhs
  and rhs = as_float rhs in
  let res = Float (f lhs rhs) in
  push res stack

and as_float = function
  | Int i -> float_of_int i
  | Float f -> f
  | Word _ -> raise (Failure "Not a number.")

let rec repl _ =
  print_string "> ";
  let input = read_line () in
  let (Program program) = parse_program input in
  try
    let result = eval_program program [] in
    let output = dump_program (Program result) in
    print_endline output;
    print_endline "";
    repl ()
  with (Failure msg) ->
    print_endline msg;
    repl()

let _ = repl ()
```

# Building and Running

You can clone this example, build it, and run the interpreter by running:

```
$ git clone git@github.com:eudoxia0/parsing-menhir.git
$ cd parsing-menhir/forth
$ dune build
$ ./_build/default/forth/bin/menhir_tutorial.exe
> 123
[Int 123]

> 3.14
[Float 3.14]

> 3.0 1.0 ADD
[Float 4.]

> 10 30 MUL
[Float 300.]

> 100 5 DIV
[Float 20.]
```

[ocaml]: https://ocaml.org/
[menhir]: http://gallium.inria.fr/~fpottier/menhir/
[ocamllex]: https://ocaml.org/manual/lexyacc.html
[opam]: https://opam.ocaml.org/
[dune]: https://dune.readthedocs.io/en/stable/
[repo]: https://github.com/eudoxia0/parsing-menhir
[ounit2]: https://opam.ocaml.org/packages/ounit2/
[forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
