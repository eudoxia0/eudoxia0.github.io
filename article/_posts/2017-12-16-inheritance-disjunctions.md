---
title: Inheritance and Disjunctions
summary: A case study in porting a compiler.
tags: [plt, sml]
---

I recently began porting a compiler from Common Lisp to Standard ML. This was
the class hierarchy representing types in the original compiler:

~~~common-lisp
(defclass base-type ()
  ()
  (:documentation "Base class of types."))

(defclass scalar (base-type)
  ()
  (:documentation "Base class of scalar types."))

(defclass unit (scalar)
  ()
  (:documentation "The unit type."))

(defclass boolean-type (scalar)
  ()
  (:documentation "Boolean type."))

(defclass integer-type (scalar)
  ((signed :reader integer-signed-p
           :initarg :signedness
           :type boolean
           :documentation "Whether the integer is signed or not.")
   (width :reader integer-width
          :initarg :width
          :type (integer 8 64)
          :documentation "The integer bit width."))
  (:documentation "Integer types."))

(defclass floating-point-type (scalar)
  ()
  (:documentation "The base class of IEEE 754 floating-point types."))

(defclass float-32 (floating-point-type)
  ()
  (:documentation "32-bit wide binary floating point type."))

(defclass float-64 (floating-point-type)
  ()
  (:documentation "64-bit wide binary floating point type."))

(defclass pointer (scalar)
  ((base :reader pointer-base
         :initarg :base
         :type base-type
         :documentation "The pointed-to type.")
   (region :reader pointer-region
           :initarg :region
           :type region
           :documentation "The pointer's region."))
  (:documentation "A pointer."))

(defclass region ()
  ((name :reader region-name
         :initarg :name
         :type string
         :documentation "The lexical name of the region."))
  (:documentation "The base class of regions."))

(defclass region-parameter (region)
  ()
  (:documentation "A region parameter appearing in a generic type specifier."))

(defclass lexical-region (region)
  ((id :reader region-id
       :initarg :id
       :type integer
       :documentation "The unique ID of the region."))
  (:documentation "A region created with the letregion construct."))

(defclass composite (base-type)
  ()
  (:documentation "Base class of composite types."))

(defclass fixed-array (composite)
  ((type :reader array-type
         :initarg :type
         :type base-type
         :documentation "The type of the array elements.")
   (length :reader array-length
           :initarg :length
           :type (integer 0)
           :documentation "The length of the array."))
  (:documentation "Fixed-size arrays types."))

(defclass tuple (composite)
  ((types :reader tuple-types
          :initarg :types
          :type (vector base-type)
          :documentation "The types of the tuple's elements."))
  (:documentation "Tuple types."))
~~~

This was my initial translation to Standard ML:

~~~sml
datatype ty = Unit
            | Boolean
            | Integer of signedness * bit_width
            | Float of float_size
            | Pointer of ty * string
            | FixedArray of ty * int
            | Tuple of ty list
and      signedness = Unsigned | Signed
and      bit_width = Bit8 | Bit16 | Bit32 | Bit64
and      float_size = F32 | F64
~~~

From ninety lines of code to a seven line-long [sum type][sum], and some
auxiliary definitions. This is a marked improvement, although much of it is
attributable to my obsessively filling the `:documentation` options in both slot
and class definitions.

A number of things other than size have improved: the use of a Boolean in the
definition of the `integer-type` class was replaced with the `signedness`
datatype (see [Boolean blindness][blindness] for a justification), and the
integer bit-width is now restricted to four symbolic choices, as opposed to the
coarser type [`(integer 8 64)`][cl-integer]. The use of more domain-specific
types obviates the need for documentation: there is no need to write a docstring
saying the bit-width of an integer is a power of two between eight and
sixty-four if the types don't allow anything else.

Languages in the ML family make it so easy to define types that they encourage
you to do so, whereas object-oriented languages encourage[^scala], due to the
syntactic cost of class definition, a one-size-fits-all approach to data
representation. The problem with this is the resulting class hierarchy begins to
accumulate extraneous subclasses which are only useful in very particular
contexts, but must be handled universally.

By way of example in the same problem domain: consider a compiler for some
hypothetical language with higher-order types, and a function `monomorphize`,
best explained with a (pseudocode) type signature:

~~~sml
monomorphize :: (t: PolymorphicType, r: Map<String, MonomorphicType>) -> Option<MonomorphicType>
~~~

In others: it takes a polymorphic type expression (one containing type
parameters), a map of type parameter names to their corresponding arguments, and
returns a fresh monomorphic type expression where all type parameters have been
replaced with their arguments. If a type parameter is not mentioned in the map,
it fails by returning the empty case of the `Option` type.

In a Common Lisp implementation of this compiler, there would be no question of
writing a completely new class hierarchy of types for each individual purpose,
because the definitions would've dwarfed the rest of the code.[^graham] The
`monomorphize` function would have taken an instance of a subclass of
`base-type` and a hash table as arguments, and returned another instance of a
subclass of `base-type`. This lack of specificity leaves the code open to
errors: there is no way to statically guarantee that the resulting type doesn't
have left-over, unexpanded type parameters, for example.

Another unfavourable effect of using a single class hierarchy throughout
different transformations is that subsets of the hierarchy's subclasses often
have an implicit lifetime throughout the data's lifetime. Using many different
fine-grained datatypes, it becomes evident when in the compiler's chain of
passes a particular case (subclass) of the data ceases to be. In an
object-oriented program, this information is usually implicit, or at best
documented in comments or docstrings.

Whereas in languages of the ML family, where type definitions are cheap, it's
feasible to have different datatypes for slightly different purposes. In a
Standard ML version of this compiler, there could be a `type_spec` datatype to
represent type specifiers (expressions that denote types), a `poly_type`
datatype to represent types with type parameters, a `mono_type` datatype to
represent monomorphic types, and yet another `exp_type` datatype to represent
the immediate type of expressions.[^mono]

These types would have essentially the same cases except for very small deltas
(_e.g._, a case for type parameters in `poly_type` but not in `mono_type`), but
it's in those deltas where type safety lies. For example, in a Standard ML
implementation of this compiler, the `monomorphize` function would look like
this:

~~~sml
fun monomorphize (t: poly_type, r: replacements): mono_type result
~~~

There is no possibility, for instance, of forgetting to recur in the subterms of
some case of `poly_type` and ending up with a result that contains unexpanded
type parameters, because the result is an instance of `mono_type`, which
contains no case for a type parameters. In a Common Lisp implementation of this
compiler, with a sole omnipresent class hierarchy of types, such a defect would
have to be explicitly tested for.

These benefits stem from the ease with which sum types can be defined. And when
type definition is expensive, you see what litters so many object-oriented
codebases: `raise Error("Impossible")`, whenever a method is called with an
instance of a subclass which should have disappeared many passes ago. With
static types, and with cheap types, whole categories of errors are erased by
making huge volumes of the state space unrepresentable.

Another example, again from my compiler port: in the Common Lisp compiler I had
a function to parse a type specifier in the form of an S-expression into an
instance of `base-type`. The parser function took both an S-expression and an
instance of the type environment (the map of type names to their definitions) as
parameters, and looked like this:

~~~common-lisp
;; Notes: the type `node` is `(or list identifier integer float)`

(declaim (ftype (function (node environment) base-type)
                parse-type-specifier))
(defun parse-type-specifier (form environment)
  (if (atom form)
      ;; We're dealing with a named type, which is either built in, or defined
      ;; in the environment
      (parse-named-type form environment)
      ;; Type constructor. Since we have no higher-order types, this must be
      ;; built-in.
      (parse-type-constructor form environment)))

(declaim (ftype (function (node environment) base-type)
                parse-named-type))
(defun parse-named-type (form environment)
  (unless (identifier-p form)
    (error "This form is not a valid type specifier."))
  (let ((name (identifier-name form)))
    (values ; Avoids a 'type assertion too complex to check' warning
     (switch (name :test #'string=)
       ("unit"
        (make-instance 'unit))
       ("boolean"
        (make-instance 'boolean-type))
       ("u8"
        (make-instance 'integer-type :signedness nil :width 8))
       ("i8"
        (make-instance 'integer-type :signedness t   :width 8))
       ("u16"
        (make-instance 'integer-type :signedness nil :width 16))
       ("i16"
        (make-instance 'integer-type :signedness t   :width 16))
       ("u32"
        (make-instance 'integer-type :signedness nil :width 32))
       ("i32"
        (make-instance 'integer-type :signedness t   :width 32))
       ("u64"
        (make-instance 'integer-type :signedness nil :width 64))
       ("i64"
        (make-instance 'integer-type :signedness t   :width 64))
       ("f32"
        (make-instance 'float-32))
       ("f64"
        (make-instance 'float-64))
       (t
        ;; Not a built in type, so look it up
        (or (get-type name environment)
            (error "No type with name ~A is defined." name)))))))

(declaim (ftype (function (node environment) base-type)
                parse-type-constructor))
(defun parse-type-constructor (form environment)
  (let ((fn (first form))) ;; We know it's non-empty since (atom nil) => t
    (unless (identifier-p fn)
      (error "The first element of a type constructor must be an identifier."))
    (let ((name (identifier-name fn))
          (args (rest form)))
      (switch (name :test #'string=)
        ("pointer"
         (destructuring-bind (base-type region) args
           (values (make-instance 'pointer
                                  :base (parse-type-specifier base-type environment)
                                  :region (parse-region region)))))
        ("tuple"
         (values (make-instance 'tuple
                                :tupes (mapcar #'(lambda (form)
                                                   (parse-type-constructor form environment))
                                               args))))
        (t
         (error "No type constructor with this name ~A." name))))))

(declaim (ftype (function (node) region)
                parse-region))
(defun parse-region (form)
  (if (identifier-p form)
      (values (make-instance 'region-parameter
                             :name (identifier-name form)))
      (error "Invalid region name.")))
~~~

Note:

1. The vain attempts at adding type safety through the use
   of [`declaim`][declaim]. [SBCL][sbcl] has a very good type inference engine,
   especially when aided by type declarations, but even moderately complex code
   results in warnings that a type assertion is "too complex to check". The
   liberal use of [`values`][values] in the code is a folk remedy for
   suppressing the warnings.[^checker]

2. The "notes to self" along the lines of "we know this expression must be true
   because an earlier test, _etc._" These are attempts to convince oneself of
   the code's soundness without mechanical aid.

3. The use of exceptions, and this is key: in languages of the ML family, it's
   nothing at all to define a new datatype specifically to represent the output
   of a function doing tricky input validation.[^algebra-blindness] It would be
   a sum type along the lines of:

   ~~~sml
   datatype type_result = Success of ty
                        | NoSuchNamedType of string
                        | InvalidForm of sexp
   ~~~

    In object-oriented code, however, defining a class hierarchy with a base
    class, a subclass whose sole purpose is to contain another object, and a
    slew of subclasses carrying error information is never idiomatic. Instead,
    the idiomatic approach is to define a class hierarchy of _exceptions_, raise
    them as needed, and catch them when appropriate. But exceptions escape the
    type system[^smlex], so to know which volumes of the code are
    exception-free, and what kind of exceptions you might encounter at any
    point, you have to rely on your own memory, or an IDE that's deeply
    integrated with the implementation.

Here's roughly the same code in the Standard ML port of the compiler:

~~~sml
fun parseTypeSpecifier (Identifier name) = SOME SNamedType s
  | parseTypeSpecifier (SList (tycon::args)) = parseTypeConstructor (func, args)
  | parseTypeSpecifier _ = NONE
and parseTypeConstructor (Identifier "pointer", base::(StringLiteral s)::nil) =
    (case (parseTypeSpecifier base) of SOME typespec => SOME (SPointer (typespec, s))
                                     | _ => NONE)
  | parseTypeConstructor (Identifier "tuple", args) =
    let val parsed = map parseTypeSpecifier args
    in
      if (List.all Option.isSome parsed) then
        SOME (Tuple List.mapPartial (fn x => x) parsed)
      else
        NONE
      end
    end
  | parseTypeConstructor _ = NONE
~~~

For completeness, the definitions of the types involved:

~~~sml
datatype cst = Identifier of string
             | IntegerLiteral of string
             | FloatLiteral of string
             | StringLiteral of string
             | SList of cst list

datatype type_specifier = SNamedType of name
                        | SPointer of type_specifier * string
                        | SNamedType of name
~~~

The code is far more compact. The parsing function no longer references an
environment because it no longer needs to: rather than parse an expression to a
type directly, the function parses an expression to a datatype representing a
type _specifier_, not a type.[^stretch] Named types are not searched in the
environment and are simply turned into `SNamedType` instances. Another function,
further down the line, takes the tree representing the type specifier and the
environment and turns it into a proper type expression, in turn checking whether
named types actually exist.[^typespec]

# Concluding Remarks

The conjunction of unceremonious type definition, sum types with exhaustiveness
checking, and monadic error handling is a tremendously powerful multiplier of
programmer ability. Taken together, they make it straightforward to write total
functions which statically erase large classes of errors.

Exhaustiveness checking is often presented as a benefit of pattern matching but
has more to do with the nature of sum types. In a language like Common Lisp,
which solves the expression problem by completely separating classes and
methods, either can be defined at any time and independently of each other. But
when extending some software object by defining new classes, the programmer has
to know ahead of time which methods they have to implement for it, which ones
have sensible defaults for either any value or the superclass they're inheriting
from, _etc._ Languages where methods are bound to classes are strictly less
powerful, but the statically-typed variety warn the programmer when a subclass
hasn't defined a necessary method.

A limitation of sum types is that, unlike classes, new cases cannot be defined
after the initial definition. This induces different design decisions, but is
not a showstopper: if need be, dictionary passing and generic types provide for
data extensibility.[^dataext]

A follow-up post will expand on the idea of using precise sum types to make
illegal states unrepresentable.

# Acknowledgements

[Tim Herd][eqdw] helpfully proofread this post.

# Footnotes

[^scala]:
    Scala is somewhat exempted from this criticism.

[^graham]:
    As a digression, I think most "OOP vs. FP" debates are Bible salespeople
    talking past each other, but there is an argument from Paul Graham which is
    underdiscussed and fits this case study perfectly:

    >Object-oriented programming generates a lot of what looks like work. Back in
    >the days of fanfold, there was a type of programmer who would only put five or
    >ten lines of code on a page, preceded by twenty lines of elaborately formatted
    >comments. Object-oriented programming is like crack for these people: it lets
    >you incorporate all this scaffolding right into your source code. Something
    >that a Lisp hacker might handle by pushing a symbol onto a list becomes a whole
    >file of classes and methods. So it is a good tool if you want to convince
    >yourself, or someone else, that you are doing a lot of work.
    >
    >&mdash; [Why Arc Isn't Especially Object-Oriented][no-oop]

[^mono]:
    This extreme differentiation can make it easier to incorporate subtleties of
    the type system: we would ideally like `5 + n`, where `n` is any integer
    type, to compile. We don't want the compiler to decide that all positive
    integer literals are an instance of `uint64_t` and refuse to compile the
    addition where `n` is anything other. This is typically handled at the
    type-inference layer, but it's a useful thing to have even in a first-order
    language.

    You could define a datatype `exp_type` with a case for "the type of an
    integer literal" and "the type of a floating-point literal", and have
    an explicit compilation step that resolves these ambiguous types by
    inserting necessary disambiguation and conversion expressions into the
    AST, and have a separate datatype to represent the types of
    known-unambigious expressions.

    The ambiguity-resolving layer is a good place to handle errors that might
    make code non-portable, _e.g._, something like `var a = 5` might require
    the programmer to insert a type disambiguation.

[^checker]:
    My experience with type declarations and CLOS has been very poor: type
    inference appears to stop at the boundary of the object system. This
    limitation is probably an instrinsic consequence of Common Lisp's dynamic
    nature, where classes and more specific methods can be defined at any point
    in time, the identity of classes can change, and fundamental behaviour can
    be overridden through the metaobject protocol.

[^algebra-blindness]:
    See [Algebraic blindness][a-blindness].

[^smlex]:
    Incidentally this is a minor pain point of Standard
    ML. Haskell's [`do` notation][haskell-do] makes exception-free error
    handling syntactically cheap. You can
    have [monadic error handling][sml-monad] in Standard ML, but not the `do`
    notation, due to the lack of a macro system.

[^stretch]:
    When I port object-oriented code to ML family languages, I find that it
    _stretches_: intermediate representations that more accurately model the
    data and its constraints fill the gaps.


[^typespec]:
    I admit to having modified the function just now. I realized as I wrote the
    paragraph referencing this footnote that it could be simplified further by
    having the environment object provide the definition of built-in named
    types. The original function looked like this:

    ~~~sml
    fun parseTypeSpecifier (Identifier name) = SOME (parseNamedType name)
      | parseTypeSpecifier (SList (func::args)) = parseTypeConstructor (func, args)
      | parseTypeSpecifier _ = NONE
    and parseNamedType "unit" = SUnit
      | parseNamedType "boolean" = SBoolean
      | parseNamedType "u8" = SInteger (Unsigned, Bit8)
      | parseNamedType "i8" = SInteger (Signed, Bit8)
      | parseNamedType "u16" = SInteger (Unsigned, Bit16)
      | parseNamedType "i16" = SInteger (Signed, Bit16)
      | parseNamedType "u32" = SInteger (Unsigned, Bit32)
      | parseNamedType "i32" = SInteger (Signed, Bit32)
      | parseNamedType "u64" = SInteger (Unsigned, Bit64)
      | parseNamedType "i64" = SInteger (Signed, Bit64)
      | parseNamedType "f32" = SFloat F32
      | parseNamedType "f64" = SFloat F64
      | parseNamedType s = SNamedType s
    and parseTypeConstructor (Identifier "pointer", base::(StringLiteral s)::nil) =
        let val parsed = parseTypeSpecifier base
        in
            case parsed of SOME typespec => SOME (SPointer (typespec, s))
                         | _ => NONE
        end
      | parseTypeConstructor (Identifier "tuple", args) =
        let val parsed = map parseTypeSpecifier args
        in
          if (List.all Option.isSome parsed) then
            SOME (Tuple List.mapPartial (fn x => x) parsed)
          else
            NONE
          end
        end
      | parseTypeConstructor _ = NONE
    ~~~

     And since I'm in a sharing mood I should clarify that the Common Lisp code
     in this article is not from one compiler but several (I have a whole graveyard in `~/code/old/`).

[^dataext]:
    I should explain this with a concrete example. Suppose you have a GUI
    toolkit with the following widgets:

    ~~~sml
    type pos = int * int

    datatype widget = Label of string
                    | LineInput
                    | Form of component list
    and component = Component of pos * widget
    ~~~

     Admittedly very spartan. In addition there is a `draw` function that draws
     a widget by calling widget-specific, lower-level drawing functions:

     ~~~sml
    fun draw (Component (pos, Label s)) = paintText pos s
      | draw (Component (pos, LineInput)) = paintLineInputBox pos
      | draw (Component (pos, Form elems)) = (List.map (draw) elems; ())
     ~~~

     One way to make this extensible, in the object-oriented sense, is to add a
     case to the `widget` datatype  which carries a function pointer to its own drawing
     function (think of a [vtable][vtable]):

    ~~~sml
    datatype widget = Label of string
                    | LineInput
                    | Form of component list
                    | Custom of pos -> unit
    ~~~

    And modify the `draw` function thusly:

    ~~~sml
    fun draw (Component (pos, Label s)) = paintText pos s
      | draw (Component (pos, LineInput)) = paintLineInputBox pos
      | draw (Component (pos, Form elems)) = (List.map (draw) elems; ())
      | draw (Component (pos, Custom func)) = func pos
    ~~~

    This is a common pattern in functional programming and is also, essentially,
    how object-orientation is implemented in plain C.

    [React][react] works somewhat like this, althought in an object-oriented, dynamically-typed
    language: components in React are functions that take an input and return
    a tree of other components or GUI primitives. By parameterizing components over the
    types of data they take as input, you can build a statically-typed React-like
    framework using only sum types.

    An alternative solution is [trait objects][tobjects], as in Rust.

[sum]: https://en.wikipedia.org/wiki/Tagged_union
[blindness]: https://shreevatsa.wordpress.com/2015/01/31/boolean-blindness/
[cl-integer]: https://clhs.lisp.se/Body/t_intege.htm
[no-oop]: https://www.paulgraham.com/noop.html
[declaim]: https://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm
[sbcl]: https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp
[values]: https://www.lispworks.com/documentation/HyperSpec/Body/f_values.htm
[a-blindness]: https://github.com/quchen/articles/blob/master/algebraic-blindness.md
[haskell-do]: https://en.wikibooks.org/wiki/Haskell/do_notation
[sml-monad]: https://existentialtype.wordpress.com/2011/05/01/of-course-ml-has-monads/
[react]: https://reactjs.org/
[tobjects]: https://doc.rust-lang.org/book/second-edition/ch17-02-trait-objects.html
[eqdw]: http://eqdw.net/
[vtable]: https://en.wikipedia.org/wiki/Virtual_method_table
