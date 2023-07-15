---
title: Why Checked Exceptions Failed
summary: Programming language features are never orthogonal.
card: why-checked-exceptions-failed.jpg
card_source: |
    Diagram of the Joint European Torus. From _Tokamaks_, John Wesson, 2004, Oxford University Press.
---

Java has this feature called checked exceptions, why lets you [annotate][spec] a
method with the set of exceptions it may throw, like so:

[spec]: https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.4.6

```java
private void foo() throws FooException, BarException {
  // ...
}
```

Methods that call `foo()` must either explicitly catch `FooException` and
`BarException`, or otherwise annotate themselves with the set of exceptions they
don't handle and which propagate up from them.

Nobody uses this. Why? Because the rest of the language is missing.

The central concepts of most programming languages are values and types. All of
the machinery of the language is, in one way or another, about transforming
values into other values, or transforming types into other types. We can do a
lot of things with values and types.

Functions and methods have a _signature_: the types of the parameters, and the
return type. Java added extra information to method signatures, orthogonal to
the types: the set of exceptions the method can throw. But the rest of the
machinery is missing.

Say I have an interface with a method `foo`:

```java
public interface ExampleInterface {
    void foo();
}
```

How does this interact with checked exceptions? Can I write this?

```java
public class ExampleClass implements ExampleInterface {
    public void foo() throws IOException {
        throw new IOException("Oh no!");
    }
}
```

Obviously not:

```
error: foo() in ExampleClass cannot implement foo() in ExampleInterface
    public void foo() throws IOException {
                ^
overridden method does not throw IOException
```

Because the code that uses the interface, rather than the class, has no way of
seeing this.

Can I specify that implementations of `foo` can throw no, some, or all
exceptions? What would it even mean to write something like `throws *`?
Analogously, if I have a function that takes a method as an argument, like a
callback, how do I specify what set of exceptions in can throw? Can I have
generic "exception set parameters"? Can I write a type like "a function that
takes two ints and returns an int and throws any subclass of `FooException`"?

It starts with a very simple and laudable goal: let's add extra information to
the type signature, orthogonal to the types, so we can track exceptions,
computational effects, maybe even things like stack usage or whether the
function is recursive. And before long you're overwhelmed with subtyping
relationships between effects, lattice theory, covariance and contravariance of
effects.

Concretely, checked exceptions in Java failed because Java lacks "throwingness
polymorphism", if you will.

More broadly: in programming languages, _everything impinges on everything
else_. Which is why you can't bolt a type system or an ownership system on a
language after the fact. At least, not without massive holes you will cope about
and deny exist.

Was Java wrong to add checked exceptions? No. They took a risk and it didn't pay
off. The design flaws in programming languages are not necessarily evident until
you write tens of thousands, or hundreds of thousands, or millions of lines of
code. They can only be evaluated in the large.
