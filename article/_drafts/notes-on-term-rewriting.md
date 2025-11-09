---
title: Notes on Term Rewriting
summary: Notes from <i>Term Rewriting and All That</i>.
math: yes
---

$$
\gdef\join{ \downarrow }
\gdef\starpath{\stackrel{*}{\to}}
\gdef\pluspath{\stackrel{+}{\to}}
\gdef\conv{\stackrel{*}{\leftrightarrow}}
\gdef\BeginKB{\begin{align}}
\gdef\EndKB{\end{align}}
\gdef\Fact#1#2{\tag{#1} & #2 \\}
\gdef\FFact#1#2{\tag{#1} &\qquad #2 \\}
\gdef\FFFact#1#2{\tag{#1} &\qquad\qquad #2 \\}
\gdef\FFFFact#1#2{\tag{#1} &\qquad\qquad\qquad #2 \\}
\gdef\CA#1#2#3{#1 \stackrel{*}{\leftarrow} #2 \stackrel{*}{\rightarrow} #3}
\gdef\pos{ \mathcal{Pos} }
\gdef\subterm#1#2{ #1 \vert_{#2} }
\gdef\p#1{ \left( #1 \right) }
$$

Human language can be expressed with [formal grammars][gram], which are just
term rewriting systems. Parsing is term rewriting in reverse: finding derivation
trees that create a given sentence. In [equational logic][eqlog], most of a
proof is just applying rewrite rules. Lambda calculus evaluation can be
expressed using term rewriting. According to Stephen Wolfram, [the universe is a
graph rewriting system][wolfram].

[gram]: https://en.wikipedia.org/wiki/Formal_grammar
[eqlog]: https://en.wikipedia.org/wiki/Equational_logic
[wolfram]: https://www.wolframphysics.org/index.php.en

So term rewriting is a formalism with very broad applicability and worth
learning.

These notes are from [_Term Rewriting and All That_][traat] by [Franz
Baader][franz] and [Tobias Nipkow][tobias]. First-level headings correspond to
chapters in the book, everything else is my own organization.

[traat]: https://www.cambridge.org/core/books/term-rewriting-and-all-that/71768055278D0DEF4FFC74722DE0D707
[franz]: https://en.wikipedia.org/wiki/Franz_Baader
[tobias]: http://en.wikipedia.org/wiki/Tobias_Nipkow

# Contents
{: .no_toc }

1. toc
{:toc}

# Motivating Examples

Consider the grammar:

```
Expr = 0
     | 1
     | X
     | Y
     | Expr + Expr
     | Expr Expr
     | D_X(Expr)
```

And this set of rewrite rules:

$$
\begin{align}
    \tag{R1}
	D_X(X) &\to 1 \\
    \tag{R2}
	D_X(Y) &\to 0 \\
    \tag{R3}
	D_X(u+v) &\to D_X(u) + D_X(v) \\
    \tag{R4}
	D_X(uv) &\to D_X(v)u + D_X(u)v
\end{align}
$$

(Note that we're "lifting" the algebraic variables $X$ and $Y$ into constants of
the object language)

Consider the expression

$$
D_X(XX)
$$

By applying $\text{R4}$ we can write this into:

$$
D_X(X)X + D_X(X)X
$$

Then, we can apply $\text{R1}$, _but_, we have two places where we can apply
this rule. So this expression has two possibly "outputs":

1. $1X + D_X(X)X$ if we rewrite the first occurrence of $D_X(X)$, and
2. $D_X(X)X + 1X$ if we choose to rewrite the second

Both of these expressions can be rewritten into the same expression, by applying
$\text{R1}$ on the last instance of $D_X(X)$:

$$
1X + 1X
$$

At this point, no rewrite rules apply.

Graphically:

![](/assets/content/notes-on-term-rewriting/diff.png)

This tiny example brings up a couple import concepts.

## Termination

The first is termination. A TRS **terminates** when, given any starting
expression, we can always reach an expression for which no more rules
apply. Such an expression is called a **normal form**.

More formally, a TRS is terminating if given a term $t$ there exists a
(potentially empty) finite chain of rule application that gives us a normal form
$t'$.

A dual definition: a TRS terminates if there does not exist an infinite chain of
rule applications.

## Confluence

In the graph above there's an expression where the rule applications fork, and
then they join again. They might be other systems where expressions can diverge
but not join again. Dually, there might be systems where _any_ divergence is
guaranteed to eventually join again.

A TRS is **confluent** where, if there is a fork in the tree of rule
applications, we can always find a join point. More formally: if a term $t$ can
be rewritten into $t_1$ and $t_2$ with $t_1 \neq t_2$, there exists a common
term $s$ that can be reached from both $t_1$ and $t_2$.

## Completion

Making a non-confluent TRS into a confluent one by adding rules is called
**completion**.

## Convergence

A TRS that is both terminating and confluent is called **convergent** or
**canonical**.

# Relations

A **relation** is a subset of $A \times B$. Conceptually, it's a function where
an input can have more than one output (this expressivity is needed because
multiple rewrite rules might apply to a given expression).

For a relation $R$ we can write $R(a, b)$ to mean $(a,b) \in R$. This is Prolog
notation.

Usually the relation is implicit and we just write $a \to b$ to mean $(a,b) \in
R$. This is arrow notation. If we want to make the relation explicit, we can
write $a \stackrel{R}{\to} b$.

Relations can be composed: given $R \subseteq A \times B$ and $S \subseteq B
\times C$, the composition $R \circ S \subseteq A \times C$ is defined by:

$$
\set{(x,z) \in A \times C \mid \exists y \in B . (x,y) \in R \land (y,z) \in S}
$$

That is: if $a \stackrel{R}{\to} b$ and $b \stackrel{S}{\to} c$, then $a
\stackrel{R \circ S}{\to} c$. Or, in Prolog notation: if $R(a,b)$ and $S(b,c)$
then $(R \circ S)(a, c)$.

## Identity

The relation $R^0$ is defined by:

$$
R^0 = \set{(x,x) \mid x \in A}
$$

This is the **identity relation**, which maps every term to itself.

Note that $R_0 \subseteq A \times A$, so the domain and the codomain must be the
same.

## $i$-fold Composition

Given a relation $R$ and $i \in \N^+$ we can define:

$$
R^i = R^{i-1} \circ R
$$

That is, $R^i$ is the **$i$-fold composition** of $R$ with itself. It is the
version of $R$ that "skips ahead" by $i-1$ steps.

Note that, as in arithmetic: $R^1 = R^0 \circ R = R$.

## Transitive Closure

One step further, we have:

$$
R^+ = \bigcup\limits_{i \gt 0} R^i
$$

That is:

$$
R^+ = R^1 \cup R^2 \cup \ldots
$$

This is called the **transitive closure** of $R$ and it relates every term in
$A$ to any one of its successors.

Say $R$ is the successor relation on the set $\N$, that is:

$$
R = \set{(n, n+1) \mid n \in \N}
$$

So that $0 \to 1$, $1 \to 2$, and so on.

Then $R^2$ is $R \circ R$ which relates $n \to n+2$, and $R^3$ relates $n \to
n+3$, and so on. So $R^+$ relates:

$$
\begin{align*}
0 & \to 1 \\
0 & \to 2 \\
0 & \to 3 \\
& \ldots
\end{align*}
$$

and so on forever.

Note: $R^+$ does not include a union with $R^0$. The next definition adds that.

## Reflexive Transitive Closure

 The **reflexive transitive closure** $R^*$ is defined by:

$$
R^* = R^+ \cup R^0
$$

This is simply the transitive closure plus the identity relation.

## Reflexive Closure

The **reflexive closure** $R^=$ is defined by:

$$
R^= = R \cup R^0
$$

That is: $R$ plus the identity relation.

## Inverse

The **inverse** of a relation, denoted $R^{-1}$, is defined by:

$$
R^{-1} = \set{(y,x) \mid (x,y) \in R}
$$

If $R(a,b)$ then $R^{-1}(b,a)$.

## Symmetric Closure

The **symmetric closure** $R^s$ is defined by

$$
R^s = R \cup R^{-1}
$$

That is, $R(a,b)$ implies both $R^s(a,b)$ and $R^s(b,a)$.

## Transitive Symmetric Closure

The **transitive symmetric closure** of $R$ is $R^{s+}$, i.e.:

$$
R^{s+} = \bigcup\limits_{i \gt 0} R^i \cup (R^i)^{-1}
$$

## Reflexive Transitive Symmetric Closure

Without further ado:

$$
R^{s*} = R^{s+} \cup R^0
$$

# Abstract Rewriting Systems

An **term rewriting system** (TRS) is a pair $(A, R)$ where $A$ is a set of
**terms** and the **reduction** $R$ is a binary relation on the set, i.e. $R
\subseteq A \times A$.

## Paths

A **path** is a (potentially empty, potentially infinite) chain of rule
applications. More formally, given a TRS $(A, R)$, a path $P$ is a sequence
$(a_0, a_1, a_2, ...)$ with $a_i \in A$ such that $a_i \to a_{i+1}, \forall i
\in \N$.

The notation $x \stackrel{n}{\to} y$ means there exists a path of length $n$
from $x$ to $y$. Alternatively: $R^n(x,y)$.

The notation $x \starpath y$ means there exists a finite path from $x$ to
$y$. Alternatively: $R^*(x,y)$.

The notation $x \pluspath y$ means there exists a non-empty path from $x$ to
$y$. Alternatively: $R^+(x,y)$.

The notation $x \stackrel{*}{\leftrightarrow} y$ means $R^{s*}(x, y)$. It is
_not_ an assertion that there exists a bidirectional finite path in $R$ _itself_
that joins $x$ to $y$. Rather, it means that there would be such a path if we
ignored the direction of the arrows.

In other words: what $a \stackrel{*}{\leftrightarrow} b$ really says is that $a$
and $b$ are connected in some way by $R$, but we don't know the direction of the
arrows, and we don't know how many steps of indirection/transitivity there are.

The notation $x \stackrel{*}{\leftrightarrow} y$ is typically read as
"convertible".

## Terminology

$x$ is **reducible** iff there is a $y$ such that $x \to y$. That is, if $R(x,
y)$.

$x$ is in normal form (i.e. **irreducible**) iff it is not reducible.

$y$ is a normal form of $x$ iff $x \starpath y$ and $y$ is a normal form.

If $x$ has a unique normal form, this is denoted $x \downarrow$.

If $x \to y$ then $y$ is called a **direct successor** of $x$.

If $x \pluspath y$ then $y$ is a **successor** of $x$.

$x$ and $y$ are **joinable** iff there exists a $z$ such that $x \pluspath z$
and $y \pluspath z$, which is denoted $x \join y$.

## Example: The Divisibility Relation

Let $A = \N - \set{0, 1}$ and

$$
R = \set{ (p,q) \mid p \gt q \land q \mid p   }
$$

The notation $q \mid p$ means reads "$q$ divides $p$" and means $\exists n \in
\N : p = nq$. For example: $R(10, 5)$ because $10 \gt 5$ and $5 \mid 10$
i.e. $10 = 2 \times 5$.

More abstractly: $a \to b$ when $a$ can be integer-divided by $b$.

From these definitions we can draw the following conclusions:

- $m$ is in normal form iff $m$ is prime
	- This follows immediately from the definition of primality.
- $p$ is a normal form of $m$ iff $p$ is a prime factor of $m$.
	- Obviously true.
- $R^+ = R$ because $\gt$ and divides are already transitive:
	- That is, in the context of $R$, if $a \to b$ and $b \to c$, then $a \to c$
      is also true by the transitivity of greater-than and divisibility.
- $R^{s*} = A \times A$
	- In $R^{-1}$, the graph has the same shape, but the arrows are flipped.
	- In $R^s = R \cup R^{-1}$ the two graphs are merged, so that the arrows are
      now bidirectional.
	- So between any two distinct numbers $a$ and $b$, we can find a chain that
      joins them. e.g. for $(3,2)$:
		- By the definition of $R$ we know $R(6, 3)$
		- Which implies $R^{-1}(3,6)$.
		- By the definition of $R$ we know $R(6,2)$.
		- So in the context of $R^{s*}$, which includes $R^{-1}$ we have a chain
          $3 \to 6 \to 2$. By transitivity: $3 \to 2$.
	- The reflexivity step allows one more step of generality, for the case
      where $a=b$.
	- Then any pair of numbers can be joined up.
	- This is the fundamental theorem of arithmetic in disguise.

## Word Problems

The **word problem** is: given a TRS whose rewrite rules are bidirectional, and
two terms $a$ and $b$, is it possible to transform $a$ into $b$?

That is: are $a$ and $b$ equivalent under the given set of identities?

# Properties of Relations

A reduction is called **terminating** if there is no infinite chain of reductions $a_0 \to a_1 \to ...$

A reduction is called **normalizing** if every element has a normal form.

A reduction is called **convergent** if it is both confluent and terminating.

## The Church-Rosser Property

A relation is called **Church-Rosser** iff $x \stackrel{*}{\leftrightarrow} y
\implies x \join y$. More formally:

$$
\text{ChurchRosser}(R) \iff \left( x \stackrel{*}{\leftrightarrow} y \implies x \join y \right)
$$

(The name is because Alonzo Church and J. Barkley Rosser proved the $\lambda$
calculus has this property.)

Note that the dual of the right hand side is always true. That is: it is a
theorem that $x \downarrow y \implies x \stackrel{*}{\leftrightarrow} y$. The
intuitive proof is that if you can get from $x$ or $y$ to a common normal form
$z$, then, once you erase the direction of the arrows, there is a path
connecting $x$ and $y$.

The formal proof is:

- Have: $x \join y$
- By def joinability: $\exists z : x \starpath z \land y \starpath z$.
- Therefore $x \stackrel{*}{\leftrightarrow} y$ by joining the chains at the $z$.

Because of the above, the Church-Rosser property can be restated as:

$$
\text{ChurchRosser}(R) \iff \left( x \stackrel{*}{\leftrightarrow} y \iff x \join y \right)
$$

In other words, in a CR relation, connectedness and joinability are the same
property.

## Confluence

A reduction is called **confluent** when $x \starpath y_1$ and $x \starpath y_2$
implies $y_1 \join y_2$. In English: if two terms have a common ancestor, then
they are joinable. More formally:

$$
\text{Confluent}(R) \iff \left(
	\CA{y_1}{x}{y_2} \implies y_1 \join y_2
\right)
$$

Confluence feels like a weaker property than CR: "two terms have a common
ancestor" is clearly a strict subset of "two terms are connected in some way".

However, confluence and the Church-Rosser property are the same thing! That is:

$$
\text{Confluent(R)} \iff \text{ChurchRosser(R)}
$$

The next few sections are dedicated to proving this.

## Theorem: CR implies Confluence

The CR property implies confluence. Formally:

$$
\text{ChurchRosser}(R) \implies \text{Confluence}(R)
$$

**Informal Proof:**

The CR property means: if two terms are convertible, they are joinable.

Confluence means: if two terms have a common ancestor, they are joinable.

Clearly, "having a common ancestor" implies convertibility. Because two terms
are convertible if they are connected in any way, and common ancestry is a way
of being connected.

**Formal Proof:**

Let $R$ be a CR relation:

$$
\BeginKB
	\Fact{1}{x \conv y \implies x \join y}
\EndKB
$$

Assume there exist $x, y_1, y_2$ such that $\CA{y_1}{x}{y_2}$:

$$
\BeginKB
	\Fact{1}{x \conv y \implies x \join y}
		\FFact{2}{\CA{y_1}{x}{y_2}}
\EndKB
$$

From this we can derive $y_1 \conv y_2$. Why? Because both terms have a common
ancestor at $x$, so we can "join the chains" at $x$.

$$
\BeginKB
	\Fact{1}{x \conv y \implies x \join y}
		\FFact{2}{\CA{y_1}{x}{y_2}}
		\FFFact{3}{y_1 \conv y_2}
\EndKB
$$

Modus ponens from 1 and 3:

$$
\BeginKB
	\Fact{1}{x \conv y \implies x \join y}
		\FFact{2}{\CA{y_1}{x}{y_2}}
			\FFFact{3}{y_1 \conv y_2}
			\FFFact{4}{y_1 \join y_2}
\EndKB
$$

Discharge:

$$
\BeginKB
	\Fact{1}{x \conv y \implies x \join y}
		\FFact{2}{\CA{y_1}{x}{y_2} \implies y_1 \join y_2}
\EndKB
$$

Discharge:

$$
\BeginKB
	\Fact{1}{
		\left(
			x \conv y \implies x \join y
		\right)
		\implies
		\left(
			\CA{y_1}{x}{y_2}

			\implies
			y_1 \join y_2
		\right)
	}
\EndKB
$$

Rewriting:

$$
\BeginKB
	\Fact{1}{\text{ChurchRosser}(R) \implies \text{Confluent}(R)}
\EndKB
$$

## Semi-Confluence

The proof that confluence implies the CR property is harder. To make is simpler,
we introduce an intermediate definition.

A relation is **semi-confluent** iff $x \to y$ and $x \starpath y_2$ implies
$y_1 \join y_2$. More formally:

$$
\text{Semiconfluent}(R) \iff \left(
y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2
\right)
$$

The intuitive meaning is: if $y_1$ has $x$ as a direct ancestor, and $y_2$ has
$x$ as an ancestor (direct or otherwise), then $y_1$ and $y_2$ are joinable.

## Theorem: Confluence implies SC

Confluence implies semiconfluence. More formally:

$$
\text{Confluent}(R) \implies \text{Semiconfluent}(R)
$$

**Intuitive Proof:**

Semiconfluence is a special case of confluence.

**Proof:**

$x \to y_1$ can be weakened to $x \starpath y_1$ . QED.

## Lemma: Semiconfluent Chain Extension

Let:

1. Semiconfluent $R$
2. $x \conv y$
3. $x \join y$

Then: if $y \to y'$ or $y \leftarrow y'$, then $x \join y'$.

Less formally: if $R$ is semiconfluent and we have $x$ and $y$ which are
convertible and joinable, extending the chain by one element means the new chain
is still joinable.

**Intuitive Proof:**

In the forward case we have $x \conv y \to y'$. The definition of semiconfluence
means $x \join y'$.

In the backward case we have $x \conv y \leftarrow y'$. By transitivity: $y' \to
y \starpath z$. And since $x \starpath z$ we know $x \join y'$.

**Formal Proof:**

TODO

## Theorem: SC implies CR

Semiconfluence implies the CR property. Formally:

$$
\text{Semiconfluent}(R) \implies \text{ChurchRosser}(R)
$$

**Intuitive Proof:**

Let $R$ be a semiconfluent relation. Assume a chain $x \conv y$ exists. Then,
using the fact of semiconfluence, prove that the chain is joinable.

**Proof:**

Let $R$ is be semiconfluent:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
\EndKB
$$

Assume there exists a chain $x \conv y$:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
\EndKB
$$

Let $P_n$ stand for the statement: $x \conv y$ has length $n$ and $x \join
y$. We need to prove $P$ for all $n$.

The base case, $P_0$, is trivial: in a chain of length zero, $x = y$ and $x = x
\join x$.

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_0}
\EndKB
$$

To prove the inductive case, assume $P_k$ for some $k$:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_0}
			\FFFact{4}{P_k}
\EndKB
$$

That is: we have a chain $x \conv y$ of length $k$ and we have proven there
exists a $z = x \join y$.

We now extend the chain to length $k+1$ by adding a term $y'$ on the
right. There are two ways of doing this: $x \conv y \to y'$ or $x \conv y
\leftarrow y'$.

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_0}
			\FFFact{4}{P_k}
				\FFFFact{5}{
					\left( x \conv y \to y' \right)
					\lor
					\left( x \conv y \leftarrow y' \right)
				}
\EndKB
$$

By the semiconfluent chain extension lemma, we can derive $x \join y'$.

We have proven the inductive case:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_0}
			\FFFact{4}{P_k}
			\FFFFact{5}{P_{k+1}}
\EndKB
$$

Discharge 4:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_0}
			\FFFact{4}{P_k \implies P_{k+1}}
\EndKB
$$

Induction 3, 4:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{P_n}
\EndKB
$$

Expanding the definition of $P_n$:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y}
			\FFFact{3}{x \join y}
\EndKB
$$

Discharge:

$$
\BeginKB
	\Fact{1}{y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2}
		\FFact{2}{x \conv y \implies x \join y}
\EndKB
$$

Rewrite:

$$
\BeginKB
	\Fact{1}{\text{Semiconfluent}(R)}
		\FFact{2}{\text{ChurchRosser}(R)}
\EndKB
$$

Discharge:

$$
\BeginKB
	\Fact{1}{\text{Semiconfluent}(R) \implies \text{ChurchRosser}(R)}
\EndKB
$$

## Theorem: Church-Rosser Equivalence

The following conditions are equivalent:

1. A reduction has the Church-Rosser property.
2. A reduction is confluent.
3. A reduction is semi-confluent.

From now on, we simply write $\text{Confluent}(R)$ and forget about CR.

**Proof:**

The above theorems.

## Theorem 2.1.6 {#theo_216}

If $\text{Confluent}(R)$ and $x \conv y$ then:

1. $y$ is a normal form implies $x \starpath y$
2. $x$ and $y$ being normal forms implies $x = y$.

**Intuition:**

What this means intuitively is that for a confluent relation, terms being in
normal form put constraints on how they can be connected. If we know $x \conv y$
and $y$ is a normal form, then $x$ can only be an ancestor of $y$ (or $y$
itself). If both are normal forms, then neither can be converted to the other,
since no rewrite rules apply to either. Therefore, they must be the same term.

**Proof of point 1:**

We know $x \conv y$. There are five ways in which two terms can be convertible:

1. Left-ancestry: $x \pluspath y$
1. Right-ancestry: $x \stackrel{+}{\leftarrow} y$
1. Common ancestry: $\exists z . x \stackrel{*}{\leftarrow} z \starpath y$
1. Join: $\exists z . x \starpath z \stackrel{*}{\leftarrow} y$
1. Identity: $x=y$

Since $y$ is a normal form, it can't be right-ancestry or join (i.e.: no arrows
can flow out of $y$). And it can't be common ancestry: if $x$ and $y$ had a
common ancestor, then, by confluence, they'd be joinable, but $y$ can't be
joinable because it's a normal form. This leaves two options:

1. $x \pluspath y$, or
2. $x = y$

Or, more succinctly: $x \starpath y$.

**Proof of point 2:**

Now suppose both $x$ and $y$ are normal. We can rule out left-ancestry,
right-ancestry, and join. Confluence means we can throw out common ancestry
because it would lead to a contradiction. And so we have one option left: $x=y$.

**Weakening:**

Why is confluence a requirement for this theorem? Consider the relation defined
by:

$$
b' \leftarrow b \leftarrow a \to c
$$

Or, in set notation:

$$
R = \set{(a, b), (a, c), (b, b')}
$$

Here, $c$ is in normal form, and $b \conv c$, but it is not true that $b
\starpath c$. Also, both $b'$ and $c$ are normal forms, and $b' \conv c$, but it
is not true that $b=c$.

## Theorem

If a relation $R$ is confluent, then every element has _at most_ one normal
form, which we denote $x \join$.

**Proof:**

Let $\text{Confluent}(R)$ and pick a term $x$. There are two possibilities:

1. $x$ has zero normal forms
2. $x$ has one normal form
3. $x$ has more than one normal form

The first two cases satisfy the theorem. For the third case: let $x$ have two
normal forms $y$ and $z$. Therefore: $x \starpath y$ and $x \starpath z$.

However, by the definition of confluence, $y \join z$, which means they can't be
in normal form. A contradiction.

So $x$ can have either zero or one normal forms.

## Theorem

If a relation $R$ is normalizing and confluent, every element has a unique
normal form.

**Proof:**

Normalizing means every $x$ must have a normal form $x \join$. So this is the
previous theorem, excluding the zero case.

## Theorem

If $R$ is confluent and normalizing then:

$$
x \conv y \iff \left( x \downarrow = y \downarrow \right)
$$

**Forward Proof:**

If $x \conv y$ then $x \join \conv y \join$ by reachability. Then, by [Theorem
2.1.6](#theo_216), $x \join = y \join$.

**Backward Proof:**

If $x \join = y \join$ then $x \join y$, which obviously implies $x \conv y$.

# Well-Founded Induction

The principle of **well-founded induction** is defined by the inference rule:

$$
\frac{
	\left( x \pluspath y \implies P(y) \right) \implies P(x)
}{
	P(x)
}
$$

That is: if you can prove $P(x)$ under the assumption that $P$ holds for all
successors of $x$, then $P(x)$ holds universally.

Where is the base case? For elements without a successor, there is no $y$ such
that $x \pluspath y$, so the assumption is vacuously true.

WFI doesn't hold for arbitrary $R$, only terminating $R$.

## Theorem

If $R$ is terminating then WFI holds. That is:

$$
\text{Terminating}(R) \implies \text{Valid}(\text{WFI})
$$

**Proof:**

Proof by contraposition. The goal is to prove:

$$
\neg \text{Valid}(\text{WFI}) \implies \neg \text{Terminating}(R)
$$

If WFI does not hold, that means there exists a predicate $P$ for which the
premise holds but the conclusion does not. That is, this holds.

$$
\forall x . \left( \forall y . x \pluspath y \implies P(y) \right) \implies P(x)
$$

But $\exists a_0 : \neg P(a_0)$. Instantiate the WFI premise with $x = a_0$:

$$
\left( \forall y . a_0 \pluspath y \implies P(y) \right) \implies P(a_0)
$$

So, modus tollens: since the consequent does not hold, the premise cannot hold, so:

$$
\forall y. a_0 \pluspath y \implies P(y)
$$

is **false**. That is:

$$
\exists a_1 . \neg \left( a_0 \pluspath a_1 \implies P(a_1) \right)
$$

Rewriting using $\neg \left( P \implies Q \right) = P \land \neg Q$:

$$
\exists a_1 . a_0 \pluspath a_1 \land \neg P(a_1)
$$

That is, there exists an $a_1$ such that $a_0 \to a_1$ and $\neg P(a_1)$.

Now repeat the above logic with $a_1$ and we can derive the existence of $a_2$,
$a_3$, and so on infinitely. Therefore, $R$ does not terminate.

## Theorem

If WFI holds, then $R$ terminates.

**Proof:**

Let $P(x)$ stand for "there is no infinite chain starting at $x$".

Proving the induction step is easy: if there is no infinite chain starting at
any successor of $x$, then there is no infinite chain starting from $x$. Since
WFI holds by hypothesis, we conclude $P(x)$ for all $x$. And $P(x)$ holds for
all $x$, then $R$ terminates.

## Naming

Terminating relations are also called **well-founded** or **Noetherian**. WFI is
also called **Noetherian induction**.

# Branching and Cycles

We can use WFI to study some further properties of relations that relate to
termination.

A relation is called **finitely branching** if each element has only finitely
many direct successors.

A relation is called **globally finite** of each element has only finitely many
successors. This _does not_ mean that the chains from each element are finite
(since that would be the same as termination) but rather that the number of
_distinct_ successors is finite. The cyclic relation:

$$
\begin{align*}
	a &\to b \\
	b &\to a
\end{align*}
$$

is globally finite (since the set of successors for each element is $\set{a,
b}$) but not terminating because it has a cycle.

A relation is called **acyclic** if there is no element $a$ such that $a
\pluspath a$.

## Counterexample

Are all terminating relations finitely-branching? No.

Consider the set of terms $A = \set{a, b} \cup \N$ and the relation:

$$
R = \set{ (a, n) \mid n \in \N } \cup \set{ (n, b) \mid n \in \N }
$$

That is: $a$ rewrites to any natural number (infinitely-branching), and every
natural number rewrites to $b$.

## Theorem: Global Finitude Criterion {#gfc}

A finitely branching and terminating relation is globally finite. Or,

$$
\text{FB}(R) \land \text{Terminating}(R) \implies \text{GF}(R)
$$

**Informal Proof:**

An element can have infinitely many successors in two ways:

- Depth: a chain that never terminates.
- Width: infinitely many direct successors (e.g., a relation that rewrites a
  natural number $n$ into every number $m$ with $m > n$)

If a relation is terminating, every chain is finite. And if it's
finitely-branching, every element has finitely many direct successors.

Therefore, the number of successors of an element must be finite.

**Formal Proof:**

We use WFI.

Let $R$ be a finitely-branching, terminating relation. Assume the induction hypothesis:

$$
\forall x . \forall y . x \pluspath y \implies P(y)
$$

Where $P(x)$ means "$x$ has finitely many successors". What the above says is:
for all terms $x$, the successors of $x$ all have finitely many successors.

Let $x$ be an arbitrary term with direct successors $\{y_0, y_1, \ldots,
y_n\}$. We know this set is finite because $R$ is finitely-branching. From the
induction hypothesis, we know that $y_0, \ldots, y_n$ each have finitely-many
successors. Therefore, $x$ itself has finitely many successors, since the sum of
finitely-many natural numbers is finite.

We have proven:

$$
\forall x . \left( \forall y . x \pluspath y \implies P(y) \right) \implies P(x)
$$

Since $R$ is terminating, we can invoke WFI. By WFI: $P(x)$ holds for all $x$,
i.e., $R$ is globally finite.

## Theorem

An acyclic, globally finite relation is terminating. Or:

$$
\text{Acyclic}(R) \land \text{GF}(R) \implies \text{Terminating}(R)
$$

**Informal Proof:**

If $R$ is acyclic, the chains starting from an element $a$ cannot reach back to
$a$. And if $R$ is globally finite, $a$ has finitely-many successors. So
eventually each of those chains must terminate.

**Formal Proof:**

Let $R$ be an acyclic GF relation. Assume $R$ does not terminate, that is: there
exists an infinite chain:

$$
x_0 \to x_1 \to x_2 \to \ldots
$$

Since $R$ is acyclic, $x_i \neq x_j$ for all $i \neq j$. Therefore $x_0$ has
infinitely-many distinct successors. Which contradicts the GF assumption. So we
have a contradiction. Therefore $R$ terminates.

# Proving Termination

is undecidable in general (Turing, 1936), but not impossible in every single
case. For example it is trivial to prove that $\set{a, b}$ with the relation
$\set{(a,b)}$ terminates, however uninteresting this may be. Analogously there
are proof techniques to prove termination in larger systems. This is a problem
in the design of languages for high-integrity systems: how to have the most
expressivity while retaining the ability to prove termination.

## Preliminary: Preimages

Let $f : A \to B$. The **preimage** of $S \subseteq B$ under $f$ is the set:

$$
f^{-1}[S] = \set{ x \in A \mid f(x) \in S }
$$

That is: the preimage of $S$ under $f$ is the set of $x \in A$ that $f$ maps to
$S$.

We can extend this to relations.

Let $f: A \to B$ and $R \subseteq B \times B$. The preimage of $R$ under $f$ is
the set:

$$
f^{-1}[R] = \set{ (x, x') \in A \times A \mid (f(x), f(x')) \in R }
$$

That is: the preimage of a relation $R \subseteq B \times B$ under a function
$f: A \to B$ is the set of pairs in $A \times A$ that are mapped by $f$ to pairs
which are in $R$.

## Inverse Image Construction

The most basic method for proving a rewrite system $(A, R)$ terminates is to embed
it in another rewrite system $(B, S)$ that is known to terminate, using a
monotone mapping function $\varphi : A \to B$, where **monotone** means that
$R(x, x') \implies S(\varphi(x), \varphi(x'))$.

If $R$ was non-terminating, an infinite chain $x_0 \stackrel{R}{\to}x_1
\stackrel{R}{\to} \ldots$ would map into an infinite chain $\varphi(x_0)
\stackrel{S}{\to} \varphi(x_1) \stackrel{S}{\to} \ldots$ which we know can't be
true since $S$ terminates.

The mapping $\varphi$ is called a **measure function** and the whole processis
called the **inverse image construction** because $R \subseteq \varphi^{-1}[S]$
where:

$$
\varphi^{-1}[S] = \set{ (x, x') \in A \times A \mid \varphi(x) \stackrel{S}{\to} \varphi(x') }
$$

Symbolically, "there is a monotone mapping $\varphi$" can be written:

$$
\exists \varphi : A \to B . \left( \text{Injective}(\varphi) \land \left( R(x,y) \implies S(\varphi(x), \varphi(y)) \right) \right)
$$

## Example Embedding

TODO

## Example: Embedding into $\N$

The system $(\N, \gt)$ is known to terminate. Let $A$ bet the set of strings for
some alphabet $\Sigma$.

One choice of mapping is length. Let $\varphi(w)$ be the length of the string
$w$. This proves that all length-decreasing reductions terminate.

Another choice is the number of letters. For each $a \in \Sigma$ define
$\varphi_a(w)$ as the number of occurrences of $a$ in $w$.

## Lemma

A finitely-branching rewrite system $(A,R)$ terminates iff there is a monotone
embedding into $(\N, \gt)$. More formally:

$$
\text{FB}(R) \implies \left(
\text{Terminating}(R) \iff \text{Monotone}(A, R, \N, \gt)
\right)
$$

**Informal Proof:**

The backward direction is true because if there is a monotone mapping, then $R$
terminates.  This is intuitively true but maybe should be lifted into its own
result.

The forward direction (termination implies the existence of a monotone mapping):
let $\varphi(x)$ be the number of successors of $x$. By the [global finitude
criterion](#gfc), $R$ is globally finite, therefore, $\varphi(x)$ is finite, and
$x \to x'$ implies that $x'$ has strictly fewer successors than $x$. That means
$\varphi(x) \gt \varphi(x')$, and so the mapping is monotone.

**Formal Proof:**

TODO

## Lexicographic Order

Given two strict orders $(A, R_A)$ and $(B, R_B)$, the **lexicographic product**
$R_{A \times B}$ defined on $A \times B$ is:

$$
R_{A \times B}((x, y), (x', y')) \iff R_A(x, x') \lor (x = x' \land R_B(y, y'))
$$

i.e., element-wise greater than.

## Lemma

The lexicographic product of two strict orders is a strict order.

## Lemma

The lexicographic product of two terminating relations is terminating.

# Proving Confluence

Is hard, and so we do it step by step.

## Local Confluence

A relation $R$ is **locally confluent** iff:

$$
y_1 \leftarrow x \rightarrow y_2 \implies y_1 \join y_2
$$

Local confluence is strictly weaker than confluence: that is, a relation can be
locally confluent while not being confluent.

## Newman's Lemma

A terminating relation is confluent if it is locally confluent. Formally:

$$
\left( \text{Terminating}(R) \land \text{LC}(R) \right) \implies \text{Confluent}(R)
$$

**Informal Proof:**

Since $R$ is terminating, we can use WFI. The induction predicate is the
definition of global confluence: $\CA{y}{x}{z} \implies y \join z$. We prove
that this holds under the assumption that it holds for all successors of $x$.

**Formal Proof:**

$R$ is terminating, is we can use WFI. Define the induction predicate as follows:

$$
P(x) = \forall y, z . \CA{y}{x}{z} \implies y \join z
$$

Assume that $P(t)$ holds for all $t$ such that $x \pluspath t$. That is: if $t$
is a successor of $x$, then the successors of $t$ have a join.

Now consider an arbitrary chain $\CA{y}{x}{y}$. Let's do case analysis:

1. $x = y, x = z$: the trivial chain, $x = x \join x$ and so $P(x)$ holds.
2. $x = y, x \neq z$: the chain $x \starpath z$, $P(x)$ holds since $z = x \join
   z$.
3. $x \neq y, x = z$: the chain $y \stackrel{*}{\leftarrow} x$, $P(x)$ holds
   analogously.
4. $x \neq y, x \neq z$: here we have $y \stackrel{*}{\leftarrow} y_1 \leftarrow
   x \rightarrow z_1 \starpath z$. By the local confluence assumption, $\exists
   u . u = y_1 \join z_1$. By assumption, we know $P(y_1)$ holds, that is, there
   exists a $v = y \join u$. Analogously, there exists a $w = v \join
   z$. Therefore, $P(x)$ holds.

Graphically:

![](/assets/content/notes-on-term-rewriting/newman.png)

By WFI, $P(x)$ holds universally.

## Strong Confluence

A relation is **strongly confluent** iff:

$$
y_1 \leftarrow x \rightarrow y_2 \implies \exists z . y_1 \starpath z \stackrel{=}{\leftarrow} y_2
$$

(Note: $\stackrel{=}{\leftarrow}$ is [reflexive closure](#reflexive-closure).)

Less formally: $x$ being a direct ancestor of $y_1$ and $y_2$ means there exists
an $z$ where $y_1$ is an ancestor of $z$ and $y_2$ is either a direct ancestor
of $z$ or equal to $z$. This implies: $z = y_1 \join y_2$.

Visually, strong confluence turns triangles into squares:

![](/assets/content/notes-on-term-rewriting/strong.png)

Note that $y_1$ must be swappable WLOG. That is, if we have $b \leftarrow a
\rightarrow c$ and the relation is strongly confluent, we must have both
$\exists z_1 . b \starpath z_1 \stackrel{=}{\leftarrow} c$ and $\exists z_2 . c
\starpath z_2 \stackrel{=}{\leftarrow} b$.

## Lemma

Any strongly confluent relation is confluent.

$$
\text{StronglyConfluent}(R) \implies \text{Confluent}(R)
$$

**Informal Proof:**

By [this theorem](#theorem-church-rosser-equivalence), if we prove
semiconfluence from strong confluence, we can prove confluence. Starting from
strong confluence and doing induction on a chain we can prove semiconfluence.

**Formal Proof:**

Assume $R$ is strongly confluent, that is:

$$
y_1 \leftarrow x \rightarrow y_2 \implies \exists z . y_1 \stackrel{*}{\leftarrow} z \stackrel{=}{\leftarrow} y_2
$$

And assume there exists a chain:

$$
y \leftarrow x \stackrel{n}{\rightarrow} y_n
$$

We need to prove that $y \join y_n$. We do this by induction on the length $n$
of the right side of the chain.

For $n=0$, start with the chain

$$
y \leftarrow x
$$

where $x = y_0$. Trivially $y = y \join x$.

For $n=1$ we have:

$$
y \leftarrow x \rightarrow y_1
$$

By strong confluence, there exists a $z_1$ such that $y
\stackrel{*}{\rightarrow} z_1$ and $z_1 \stackrel{=}{\leftarrow}
y_1$. Therefore: $z_1 = y \join y_1$.

For $n = k + 1$ and $k \geq 0$, we have:

$$
y \leftarrow x \rightarrow y_1 \rightarrow \ldots \rightarrow y_k \rightarrow y_{k+1}
$$

And our induction hypothesis is:

$$
y \stackrel{*}{\rightarrow} z_{k} \stackrel{=}{\leftarrow} y_k
$$

By strong confluence: $\exists z_{k+1} . y \stackrel{*}{\rightarrow} z_{k+1}
\stackrel{=}{\leftarrow} y_{k+1}$. And, therefore: $z_{k+1} = y \join y_{k+1}$.

## The Diamond Property

A relation $R$ has the **diamond property** iff

$$
y_1 \leftarrow x \rightarrow y_2 \implies \exists z . y_1 \rightarrow z \leftarrow y_2
$$

## Commutation

Let $\to_1$ and $\to_2$ be two relations. Then we say that they **commute** iff:

$$
y_1 \stackrel{*}{\leftarrow_1} x \stackrel{*}{\rightarrow_2} y_2 \implies \exists z . y_1 \stackrel{*}{\rightarrow_2} z \stackrel{*}{\leftarrow_1} y_2
$$

They they **strongly commute** iff:

$$
y_1 \leftarrow_1 x \rightarrow_2 y_2 \implies \exists z . y_1 \stackrel{=}{\rightarrow_2} z \stackrel{*}{\leftarrow_1} y_2
$$

And that they have the **commuting diamond property** iff:

$$
y_1 \leftarrow_1 x \rightarrow_2 y_2 \implies \exists z . y_1 \rightarrow_2 z \leftarrow_1 y_2
$$

Commutation is a generalization of confluence: confluence concerns one relation,
commutation two. Note that a relation $\to$ is confluent if $\to$ and $\to$
commute.

## The Commutative Union Lemma

The utility of commutation is we can apply divide-and-conquer to confluence
proofs by dividing a relation into a set of commuting relations, and proving
each one confluent. This is enabled by the commutative union lemma, which says:

If $R$ and $R'$ are confluent and commute, then $R \cup R'$ is confluent.

**Proof:**

`sorry`

## The Commutation Lemma

Two strongly commuting reductions commute. Formally:

$$
\text{StrongCommute}(R) \land \text{StrongCommute}(R') \implies \text{Commute}(R, R')
$$

Proof:

`sorry`

# Universal Algebra

## Signatures

A **signature** $\Sigma$ is a set of **function symbols** to and a mapping
$\Sigma \to \N$ that associates each symbol with an **arity**. For $n \in \N$,
$\Sigma^{(n)} \subseteq \Sigma$ is the set of $n$-ary symbols. The members of
$\Sigma^{(0)}$ are called **constant symbols**.

## Terms

Let $\Sigma$ be a signature, and $X$ a set of **variables**, such that $\Sigma
\cap X = \empty$.

The set of **$\Sigma$-terms over $X$**, denoted $T(\Sigma, X)$, is defined
inductively by:

- $\forall x \in X . x \in T(\Sigma, X)$ (every variable is a term)
- $\forall n \in \N, f \in \Sigma^{(n)}, t_1, \dots, t_n \in T(\Sigma, X)
  . f(t_1, \dots, t_n) \in T(\Sigma, X)$ (applying an $n$n-ary function to $n$
  terms yields a term)

## Positions

A **position** is a vector of natural numbers.

Let $t \in T(\Sigma, X)$. The set of positions of $t$, denoted
$\mathcal{Pos}(t)$, is defined by:

$$
\mathcal{Pos}(t) =
\begin{cases}

\set{()} & t \in X \\

\set{()} \bigcup\limits_{i=1}^n \set{ i :: p \mid p \in \mathcal{Pos}(t_1) }  & t = f(t_1, \dots, t_n) \\

\end{cases}
$$

Where $::$ is consing, i.e., a function $\N \times \N^n \to \N^{n+1}$ that
prepends an element to a vector.

The **size** of a term $t$, denoted $\vert t \vert$, is the cardinality of
$\mathcal{Pos}(t)$.

For a term $f(x, g(y))$, the set of positions is:

$$
\set{(), (1), (2), (2, 1)}
$$

## Subterm at a Position

Let $t \in T(\Sigma, X)$ and $p \in \pos(t)$. The **subterm of $t$ at position
$p$**, denoted $\subterm{t}{p}$, is:

$$
\subterm{t}{p} = \begin{cases}
t & p = () \\
\subterm{t_i}{p'} & t = f(t_1, \dots, t_n), p = i :: p'
\end{cases}
$$

For a term $t = f(x, g(y))$, the subterms are:

$$
\begin{align*}
\subterm{t}{()}     &= f(x, g(y)) \\
\subterm{t}{(1)}    &= x \\
\subterm{t}{(2)}    &= g(y) \\
\subterm{t}{(2, 1)} &= y \\
\end{align*}
$$

## Substitution of a Subterm

Let $t \in T(\Sigma, X)$ and $p \in \pos(t)$. The term created by replacing the
subterm at $p$ by $s$ is denoted $t[s]_p$, and this is defined by:

$$
t[s]_p \begin{cases}
s & p = () \\
f(t_1, \dots, t_i[s]_{p'}, \dots, t_n) & f(t_1, \dots, t_i, \dots, t_n), p = i :: p' \\
\end{cases}
$$

## Variables in a Term

The set of variables in $t$, denoted $\mathcal{Var}(t)$, is defined inductively
by:

$$
\mathcal{Var}(t) = \begin{cases}
\set{x} & t = x \in X \\
\bigcup\limits_{i=1}^n \mathcal{Var}(t_i) & t = f(t_1, \dots, t_n)
\end{cases}
$$

## Ground Terms

A term $t$ is called **ground** iff $\mathcal{Var}(t) = \empty$.

The set of ground terms over $\Sigma$ is denoted $T(\Sigma)$.

## Variable Substitution

Let $\Sigma$ be a signature, and $V$ a countably-infinite set of variables. A
**substitution** is a function $\sigma : V \to T(\Sigma, V)$ where, for finitely
many variables $x \in V$, we have $\sigma(x) \neq x$.

$\mathcal{Dom}(\sigma)$, called the **domain** of $\sigma$, is the set $\set{x
\in V \mid \sigma(x) \neq x}$.

$\mathcal{Ran}(\sigma)$, called the **range** of $\sigma$, is the set
$\set{\sigma(x) \mid x \in \mathcal{Dom}(\sigma)}$.

If $x \in \mathcal{Dom}(\sigma)$, we say $\sigma$ **instantiates** $x$.

## Term Substitution

A substitution $\sigma : V \to T(\Sigma, V)$ can be trivially **extended** into
a function $\hat{\sigma} : T(\Sigma, V) \to T(\Sigma, V)$ defined by:

$$
\hat{\sigma}(t) = \begin{cases}
\sigma(x) & t = x \in V \\
f(\hat{\sigma}(t_1), \dots, \hat{\sigma}(t_n)) & t = f(t_1, \dots, t_n)
\end{cases}
$$

To simplify notation, we often write $\sigma$ where we should technically write
$\hat\sigma$.

## Instances

A term $t$ is called an **instance** of another term $s$ iff $\exists \sigma
. \sigma(s) = t$.

## Identities

Let $\Sigma$ be a signature, and $V$ a countably-infinite set of variables. A
**$\Sigma$-identity** is a pair $(t, s) \in T(\Sigma, V) \times T(\Sigma, V)$,
which we denote $s \approx t$.

Identities can be interpreted as in algebra: as asserting the equality of two
expressions.

## The Reduction Relation

Let $E$ be a set of $\Sigma$ identities. The **reduction relation** $R_E
\subseteq T(\Sigma, V) \times T(\Sigma, V)$ is:

$$
R_E(a, b) \iff \exists (l, r) \in E, p \in \pos(a), \sigma \in \mathcal{Sub}(\Sigma, V) . \subterm{a}{p} = \sigma(l) \land b = a[\sigma(r)]_p
$$

In other words: $R_E(a,b)$ means there's a pair $(l, r)$ and a substitution by
which the subterm of $a$ at $p$ can be matched with $l$, and, replacing that
subterm with $\sigma(r)$ makes the resulting term equal to $b$.

## Closure and Compatibility

Let $R$ be a binary relation on $T(\Sigma, V)$.

$R$ is **closed under substitutions** iff:

$$
R(a,b) \implies R(\sigma(a), \sigma(b))
$$

In other words: any consistent substitution of variables to terms preserves the relation.

$R$ is **closed under $\Sigma$ operations** iff:

$$
R(a_1, b_1) \land \dots \land R(a_n, b_n) \implies R(f(a_1, \dots, a_n), f(b_1, \dots, b_n))
$$

In other words: terms built up from related arguments, in the right order and arity, are related.

$R$ is **compatible with $\Sigma$ operations** iff:

$$
R(a,b)
\implies
R(f(t_1, \dots, a, \dots t_n), f(t_1, \dots, b, \dots t_n))
$$

In other words: $a \to b$ means any term with $a$ as an immediate subterm is related to that term where $a$ has been replaced with $b$.

$R$ is **compatible with $\Sigma$ contexts** iff:

$$
R(a, b) \implies R(t[a]_p, t[b]_p)
$$

For any $t \in T(\Sigma, V)$ and $p \in \pos(t)$. In other words: any term that
contains $a$ is related to itself where $a$ has been replaced with $b$ at a
given position $p$.

## Lemma

Let $t \in T(\Sigma, X)$, $p \in \pos(t)$, and $\sigma \in \mathcal{Sub}(\Sigma, X)$. Then:

$$
\sigma(\subterm{t}{p}) = \subterm{\sigma(t)}{p}
$$

**Proof:**

Trivial. Since $\sigma$ can only replace variables, any $p$ in $\pos(t)$ must
also be in $\pos(\sigma(t))$: positions can only ever be added by a
substitution.

## Lemma

Let $E$ be a set of $\Sigma$ identities. The reduction relation $R_E$ is closed
under substitutions.

**Informal Proof:**

$R(a,b)$ means we have a $\sigma$ to make $\sigma(l) = \subterm{a}{p}$, and
$a[\sigma(r)]_p = b$. To prove closure under substitution, we need to prove
$R(\sigma'(a), \sigma'(b))$ for some arbitrary substitution $\sigma'$, that is,
we need to find an $s$ such that $\subterm{\sigma'(a)}{p} = s(l)$ and
$\sigma'(b) = \sigma'(a)[s(r)]_p$. Just let $s(t) = \sigma'(\sigma(t))$.

**Proof:**

Assume $R_E(a, b)$. By the definition of $R_E$, there exists an $(l, r) \in E$,
a $p \in \pos(s)$, and a $\sigma \in \mathcal{Sub}(\Sigma, V)$ such that:

$$
\subterm{a}{p} = \sigma(l)
$$

And:

$$
b = a[\sigma(r)]_p
$$

Let $\sigma'$ be any substitution. We want to prove $R(\sigma'(a), \sigma'(b))$.

Let $s(t) = \sigma'(\sigma(t))$. Then apply $\sigma'$ to both sides of
$\subterm{a}{p} = \sigma(l)$:

$$
\sigma'(\subterm{a}{p}) = \sigma'(\sigma(l))
$$

By the previous lemma:

$$
\subterm{\sigma'(a)}{p} = \sigma'(\sigma(l))
$$

Rewriting using the definition of $s$:

$$
\subterm{\sigma'(a)}{p} = s(l)
$$

Analogously, applying $\sigma'$ to both sides of $b = a[\sigma(r)]_p$:

$$
\sigma'(b) = \sigma'(a[\sigma(r)]_p)
$$

Clearly, $\sigma(a[b]_p) = \sigma(a)[\sigma(b)]_p$, and so:

$$
\sigma'(b) = \sigma'(a)[\sigma'(\sigma(r))]_p
$$

Rewriting using the definition of $s$:

$$
\sigma'(b) = \sigma'(a)[s(r)]_p
$$

This proves $R(\sigma'(a), \sigma'(b))$.

## Lemma

Let $E$ be a set of $\Sigma$ identities. The reduction relation $R_E$ is
compatible with $\Sigma$ operations.

**Proof:**

`sorry`

## Remark

$R_E$ need not be closed under $\Sigma$ operations, since reduction takes place
at a single position.

Example: consider $E = \set{a \approx 1, b \approx 2}$. Clearly, $f(a, b) \to_E
f(1, b)$ and $f(a,b) \to_E f(a, 2)$, but since we can't substitute in multiple
places at once, we can't assert $f(a,b) \to_E f(1,2)$.

## Lemma

Let $R$ be a binary relation on $T(\Sigma, V)$. Then $R$ is compatible with
$\Sigma$ operations iff it is compatible with $\Sigma$ contexts.

**Proof:**

`sorry`

## Lemma

Let $R$ be a binary relation on $T(\Sigma, V)$. If $R$ is reflexive and
transitive, then it is compatible with $\Sigma$ operations iff it is closed
under $\Sigma$ operations. More formally:

$$
\p{ \text{Refl}(R) \land \text{Trans}(R) }
\implies
\p{ \text{CWO}(R) \iff \text{CUO}(R) }
$$

**Proof:**

`sorry`

## Theorem

Let $E$ be a set of $\Sigma$ identities. The relation
$\stackrel{*}{\leftrightarrow}_E$ is the smallest equivalence relation on
$T(\Sigma, V)$ that contains $E$ and is closed under substitution and
$\Sigma$-operations.

**Proof:**

`sorry`

## Algebras

Algebras extend signatures with an interpretation of their function symbols.

Let $\Sigma$ be a signature. A **$\Sigma$-algebra** $\mathcal A$ is:

- a **carrier set** $A$, and
- a mapping from each function symbol $f \in \Sigma^{(n)}$ to a function
  $f^{\mathcal A} : A^n \to A$.

## Example

Consider $\Sigma_G = \set{e \to 0, i \to 1, f \to 2}$ (the G is for group). Let
$\Z$ be the carrier set, and interpret $e$ as $0$, $i$ as negation, and $f$ as
addition. The resulting algebra is the additive group of the integers.

## Subalgebras

$\mathcal B$ is a subalgebra of $\mathcal A$ iff $B \subseteq A$ and $\forall n
\in \N . \forall b_1, \dots, b_n \in B . f^{\mathcal B}(b_1, \dots, b_n) =
f^{\mathcal A}(b_1, \dots, b_n) \land f^{\mathcal B}(b_1, \dots, b_n) \in B$.

## Generators

Let $X$ be a subset of the carrier set of an algebra $\mathcal A$. The
**$\Sigma$ subalgebra of $\mathcal A$ generated by $X$** is the smallest
subalgebra of $A$ that contains $X$.

## Homomorphisms

Let $\Sigma$ be a signature and $\mathcal A$, $\mathcal B$ be $\Sigma$
algebras. A $\Sigma$ **homomorphism** $\phi: \mathcal A \to \mathcal B$ is a
mapping $A \to B$ such that:

$$
\phi(f^{\mathcal A}(a_1, \dots, a_n)) = f^{\mathcal B}(\phi(a_1), \dots, \phi(a_n))
$$

If the mapping is surjective, then $\mathcal B$ is called a **homomorphic
image** of $\mathcal A$.

If $\phi: \mathcal A \to \mathcal A$ then it's called an **endomorphism**, and a
bijective endomorphism is called an **isomorphism**.

## Congruence

Let $\mathcal A$ be a $\Sigma$ algebra. An equivalence relation $R$ on the
carrier set $A$ is called a **congruence** on $\mathcal A$ iff:

$$
\forall a_1 \equiv b_1, \dots, a_n \equiv b_n . f^{\mathcal A}(a_1, \dots, a_n) \equiv f^{\mathcal A}(b_1, \dots, b_n)
$$

## Quotient Algebras

The **quotient algebra** $\mathcal{A} /_\equiv$ has as its carrier set the set
of equivalence classes on $A$ defined by $\equiv$, and the interpretation is:

$$
f^{\mathcal{A} /_\equiv}([a_1]_\equiv, ..., [a_n]_\equiv) = [f^{\mathcal A}(a_1, ..., a_n)]_\equiv
$$

## Lemma

Let $\equiv$ be a congruence on $\mathcal A$. The quotient algebra $\mathcal A /
_ \equiv$ is the homomorphic image of $\mathcal A$ under the **canonical
homomorphism** $\pi_\equiv : A \to A /_\equiv$ defined by:

$$
\pi_\equiv(a) = [a]_\equiv
$$

**Proof:**

We need to prove that $\pi_\equiv$ is a homomorphism to $\mathcal A / _ \equiv$,
and that is it surjective. By the definition of $\pi_\equiv$:

$$
\pi_\equiv(f^{\mathcal A}(a_1, \dots, a_n)) = [f^{\mathcal A}(a_1, \dots, a_n)]_\equiv
$$

The right hand side is how $f^{\mathcal A / _ \equiv}$ is defined, so it is a
homomorphism.

Is $\pi_\equiv$ surjective? Yes: equivalence relations partition every element
in the set.

## Lemma

Let $\mathcal $ and $\mathcal B$ be $\Sigma$-algebras, with $\mathcal A$
generated by $X$. Let $\phi$ and $\psi$ be homomorphisms $\mathcal A \to
\mathcal B$. If $\forall x \in X. \phi(x) = \psi(x)$, then $\forall x \in A
. \phi(x) = \psi(x)$.

**Proof:**

`sorry`

## Free Algebras

**Informal Definition:**

> If an algebra $\mathcal A$ is generated by a subset $X$ of its carrier set,
> any homomorphism of $\mathcal A$ into $\mathcal B$ is uniquely determined by
> $X$.

In other words: free algebras are like basis sets in linear algebra.

**Formal Definition:**

Let $\Sigma$ be a signature, $X$ a set, $\mathcal K$ a class of $\Sigma$
algebras. $\mathcal A$ is called **free in $\mathcal K$ with generating set
$X$** iff the following are true:

1. $\mathcal A$ is generated by $X \subseteq A$
1. $\mathcal A \in \mathcal K$
1. $\forall \mathcal B \in \mathcal K$ every mapping $\phi : X \to B$ can be
   extended into a homomorphism $\phi : \mathcal A \to \mathcal B$.

A free algebra with an empty generating set is called an **initial algebra**.

## Term Algebras

Let $\Sigma$ be a signature and $X$ a set of variables disjoint from
$\Sigma$. The **$\Sigma$-term algebra** $\mathcal T (\Sigma, X)$ has $T(\Sigma,
X)$ as its carrier, and each function symbol $f \in \Sigma^{(n)}$ is interpreted
as a function

$$
f^{\mathcal T (\Sigma, X)}: T(\Sigma, X)^n \to T(\Sigma, X) : (t_1, \dots, t_n) \to f(t_1, \dots, t_n)
$$

## Theorem

**Informal Statement:**

Term algebras are free in the class of all $\Sigma$ algebras.

**Formal Statement:**

$\mathcal T(\Sigma, X)$ is free with generating set $X$ in the class of all
$\Sigma$ algebras.

**Proof:**

`sorry`

## Entailment

The $\Sigma$ identity $s \approx t$ **holds** in the $\Sigma$ algebra $\mathcal
A$, denoted $A \models s \approx t$) iff $\phi(s) = \phi(t)$ for all
homomorphisms $\phi: \mathcal T(\Sigma, V) \to \mathcal A$.

Intuitively, an identity holds when all possible assignments of variables to
values cause the expression to evaluate to true. The use of a homomorphism above
reflects the fact that this replacement can be thought of as a homomorphism from
terms to objects in the algebra.

## Models

Let $E$ be a set of $\Sigma$ identities. The $\Sigma$ algebra $\mathcal A$ is a
model of $E$, denoted $\mathcal A \models E$ iff every identity in $E$ holds in
$\mathcal A$.

## Varieties

The class of all models of $E$ is called the **$\Sigma$-variety** of $E$, and is
denoted $\mathcal V(E)$.

## Semantic Consequence

The identity $s \approx t$ is a **semantic consequence** of $E$, denoted $E
\models s \approx t$, iff it holds in all models of $E$. That is:

$$
\forall \mathcal A \in \mathcal V(E) . \mathcal A \models s \approx t
$$

## Equational Theories

The relation:

$$
\approx_E = \set{(s,t) \in T(\Sigma, V) \times T(\Sigma, V) \mid E \models s \approx t}
$$

is called the **equational theory** induced by $E$.
