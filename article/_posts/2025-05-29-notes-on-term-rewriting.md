---
title: Notes on Term Rewriting
summary: Notes from _Term Rewriting and All That_.
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
$$

Human language can be expressed with [formal grammars][gram], which are just term rewriting systems. Parsing is term rewriting in reverse: finding derivation trees that create a given sentence. In [equational logic][eqlog], most of a proof is just applying rewrite rules. Lambda calculus evaluation can be expressed using term rewriting. According to Stephen Wolfram, [the universe is a graph rewriting system][wolfram].

[gram]: https://en.wikipedia.org/wiki/Formal_grammar
[eqlog]: https://en.wikipedia.org/wiki/Equational_logic
[wolfram]: https://www.wolframphysics.org/index.php.en

So term rewriting is a formalism with very broad applicability and worth learning.

# Contents
{: .no_toc }

1. toc
{:toc}

# Example: Symbolic Differentiation

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

(Note that we're "lifting" the algebraic variables $X$ and $Y$ into constants of the object language)

Consider the expression

$$
D_X(XX)
$$

By applying $\text{R4}$ we can write this into:

$$
D_X(X)X + D_X(X)X
$$

Then, we can apply $\text{R1}$, _but_, we have two places where we can apply this rule. So this expression has two possibly "outputs":

1. $1X + D_X(X)X$ if we rewrite the first occurrence of $D_X(X)$, and
2. $D_X(X)X + 1X$ if we choose to rewrite the second

Both of these expressions can be rewritten into the same expression, by applying $\text{R1}$ on the last instance of $D_X(X)$:

$$
1X + 1X
$$

At this point, no rewrite rules apply.

Graphically:

![](/assets/content/notes-on-term-rewriting/diff.png)

This tiny example brings up a couple import concepts.

## Termination

The first is termination. A TRS **terminates** when, given any starting expression, we can always reach an expression for which no more rules apply. Such an expression is called a **normal form**.

More formally, a TRS is terminating if given a term $t$ there exists a (potentially empty) finite chain of rule application that gives us a normal form $t'$.

A dual definition: a TRS terminates if there does not exist an infinite chain of rule applications.

## Confluence

In the graph above there's an expression where the rule applications fork, and then they join again. They might be other systems where expressions can diverge but not join again. Dually, there might be systems where _any_ divergence is guaranteed to eventually join again.

A TRS is **confluent** where, if there is a fork in the tree of rule applications, we can always find a join point. More formally: if a term $t$ can be rewritten into $t_1$ and $t_2$ with $t_1 \neq t_2$, there exists a common term $s$ that can be reached from both $t_1$ and $t_2$.

## Completion

Making a non-confluent TRS into a confluent one by adding rules is called **completion**.

## Convergence

A TRS that is both terminating and confluent is called **convergent** or **canonical**.

# Relations

A **relation** is a subset of $A \times B$. Conceptually, it's a function where an input can have more than one output (this expressivity is needed because multiple rewrite rules might apply to a given expression).

For a relation $R$ we can write $R(a, b)$ to mean $(a,b) \in R$. This is Prolog notation.

Usually the relation is implicit and we just write $a \to b$ to mean $(a,b) \in R$. This is arrow notation. If we want to make the relation explicit, we can write $a \stackrel{R}{\to} b$.

Relations can be composed: given $R \subseteq A \times B$ and $S \subseteq B \times C$, the composition $R \circ S \subseteq A \times C$ is defined by:

$$
\set{(x,z) \in A \times C \mid \exists y \in B . (x,y) \in R \land (y,z) \in S}
$$

That is: if $a \stackrel{R}{\to} b$ and $b \stackrel{S}{\to} c$, then $a \stackrel{R \circ S}{\to} c$. Or, in Prolog notation: if $R(a,b)$ and $S(b,c)$ then $(R \circ S)(a, c)$.

## Identity

The relation $R^0$ is defined by:

$$
R^0 = \set{(x,x) \mid x \in A}
$$

This is the **identity relation**, which maps every term to itself.

Note that $R_0 \subseteq A \times A$, so the domain and the codomain must be the same.

## $i$-fold Composition

Given a relation $R$ and $i \in \N^+$ we can define:

$$
R^i = R^{i-1} \circ R
$$

That is, $R^i$ is the **$i$-fold composition** of $R$ with itself. It is the version of $R$ that "skips ahead" by $i-1$ steps.

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

This is called the **transitive closure** of $R$ and it relates every term in $A$ to any one of its successors.

Say $R$ is the successor relation on the set $\N$, that is:

$$
R = \set{(n, n+1) \mid n \in \N}
$$

So that $0 \to 1$, $1 \to 2$, and so on.

Then $R^2$ is $R \circ R$ which relates $n \to n+2$, and $R^3$ relates $n \to n+3$, and so on. So $R^+$ relates:

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

An **term rewriting system** (TRS) is a pair $(A, R)$ where $A$ is a set of **terms** and the **reduction** $R$ is a binary relation on the set, i.e. $R \subseteq A \times A$.

## Paths

A **path** is a (potentially empty, potentially infinite) chain of rule applications. More formally, given a TRS $(A, R)$, a path $P$ is a sequence $(a_0, a_1, a_2, ...)$ with $a_i \in A$ such that $a_i \to a_{i+1}, \forall i \in \N$.

The notation $x \stackrel{n}{\to} y$ means there exists a path of length $n$ from $x$ to $y$. Alternatively: $R^n(x,y)$.

The notation $x \starpath y$ means there exists a finite path from $x$ to $y$. Alternatively: $R^*(x,y)$.

The notation $x \pluspath y$ means there exists a non-empty path from $x$ to $y$. Alternatively: $R^+(x,y)$.

The notation $x \stackrel{*}{\leftrightarrow} y$ means $R^{s*}(x, y)$. It is _not_ an assertion that there exists a bidirectional finite path in $R$ _itself_ that joins $x$ to $y$. Rather, it means that there would be such a path if we ignored the direction of the arrows.

In other words: what $a \stackrel{*}{\leftrightarrow} b$ really says is that $a$ and $b$ are connected in some way by $R$, but we don't know the direction of the arrows, and we don't know how many steps of indirection/transitivity there are.

The notation $x \stackrel{*}{\leftrightarrow} y$ is typically read as "convertible".

## Terminology

$x$ is **reducible** iff there is a $y$ such that $x \to y$. That is, if $R(x, y)$.

$x$ is in normal form (i.e. **irreducible**) iff it is not reducible.

$y$ is a normal form of $x$ iff $x \starpath y$ and $y$ is a normal form.

If $x$ has a unique normal form, this is denoted $x \downarrow$.

If $x \to y$ then $y$ is called a **direct successor** of $x$.

If $x \pluspath y$ then $y$ is a **successor** of $x$.

$x$ and $y$ are **joinable** iff there exists a $z$ such that $x \pluspath z$ and $y \pluspath z$, which is denoted $x \join y$.

## Example: The Divisibility Relation

Let $A = \N - \set{0, 1}$ and

$$
R = \set{ (p,q) \mid p \gt q \land q \mid p   }
$$

The notation $q \mid p$ means reads "$q$ divides $p$" and means $\exists n \in \N : p = nq$. For example: $R(10, 5)$ because $10 \gt 5$ and $5 \mid 10$ i.e. $10 = 2 \times 5$.

More abstractly: $a \to b$ when $a$ can be integer-divided by $b$.

From these definitions we can draw the following conclusions:

- $m$ is in normal form iff $m$ is prime
	- This follows immediately from the definition of primality.
- $p$ is a normal form of $m$ iff $p$ is a prime factor of $m$.
	- Obviously true.
- $R^+ = R$ because $\gt$ and divides are already transitive:
	- That is, in the context of $R$, if $a \to b$ and $b \to c$, then $a \to c$ is also true by the transitivity of greater-than and divisibility.
- $R^{s*} = A \times A$
	- In $R^{-1}$, the graph has the same shape, but the arrows are flipped.
	- In $R^s = R \cup R^{-1}$ the two graphs are merged, so that the arrows are now bidirectional.
	- So between any two distinct numbers $a$ and $b$, we can find a chain that joins them. e.g. for $(3,2)$:
		- By the definition of $R$ we know $R(6, 3)$
		- Which implies $R^{-1}(3,6)$.
		- By the definition of $R$ we know $R(6,2)$.
		- So in the context of $R^{s*}$, which includes $R^{-1}$ we have a chain $3 \to 6 \to 2$. By transitivity: $3 \to 2$.
	- The reflexivity step allows one more step of generality, for the case where $a=b$.
	- Then any pair of numbers can be joined up.
	- This is the fundamental theorem of arithmetic in disguise.

## Word Problems

The **word problem** is: given a TRS whose rewrite rules are bidirectional, and two terms $a$ and $b$, is it possible to transform $a$ into $b$?

That is: are $a$ and $b$ equivalent under the given set of identities?

# Properties of Relations

A reduction is called **terminating** if there is no infinite chain of reductions $a_0 \to a_1 \to ...$

A reduction is called **normalizing** if every element has a normal form.

A reduction is called **convergent** if it is both confluent and terminating.

## The Church-Rosser Property

A relation is called **Church-Rosser** iff $x \stackrel{*}{\leftrightarrow} y \implies x \join y$. More formally:

$$
\text{ChurchRosser}(R) \iff \left( x \stackrel{*}{\leftrightarrow} y \implies x \join y \right)
$$

(The name is because Alonzo Church and J. Barkley Rosser proved the $\lambda$ calculus has this property.)

Note that the dual of the right hand side is always true. That is: it is a theorem that  $x \downarrow y \implies x \stackrel{*}{\leftrightarrow} y$. The intuitive proof is that if you can get from $x$ or $y$ to a common normal form $z$, then, once you erase the direction of the arrows, there is a path connecting $x$ and $y$.

The formal proof is:

- Have: $x \join y$
- By def joinability: $\exists z : x \starpath z \land y \starpath z$.
- Therefore $x \stackrel{*}{\leftrightarrow} y$ by joining the chains at the $z$.

Because of the above, the Church-Rosser property can be restated as:

$$
\text{ChurchRosser}(R) \iff \left( x \stackrel{*}{\leftrightarrow} y \iff x \join y \right)
$$

In other words, in a CR relation, connectedness and joinability are the same property.

## Confluence

A reduction is called **confluent** when $x \pluspath y_1$ and $x \pluspath y_2$ implies $y_1 \join y_2$. In English: if two terms have a common ancestor, then they are joinable. More formally:

$$
\text{Confluent}(R) \iff \left(
	\CA{y_1}{x}{y_2} \implies y_1 \join y_2
\right)
$$

Confluence feels like a weaker property than CR: "two terms have a common ancestor" is clearly a strict subset of "two terms are connected in some way".

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

Clearly, "having a common ancestor" implies convertibility. Because two terms are convertible if they are connected in any way, and common ancestry is a way of being connected.

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

From this we can derive $y_1 \conv y_2$. Why? Because both terms have a common ancestor at $x$, so we can "join the chains" at $x$.

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

The proof that confluence implies the CR property is harder. To make is simpler, we introduce an intermediate definition.

A relation is **semi-confluent** iff $x \to y$ and $x \pluspath y_2$ implies $y_1 \join y_2$. More formally:

$$
\text{Semiconfluent}(R) \iff \left(
y_1 \leftarrow x \starpath y_2 \implies y_1 \join y_2
\right)
$$

The intuitive meaning is: if $y_1$ has $x$ as a direct ancestor, and $y_2$ has $x$ as an ancestor (direct or otherwise), then $y_1$ and $y_2$ are joinable.

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

Less formally: if $R$ is semiconfluent and we have $x$ and $y$ which are convertible and joinable, extending the chain by one element means the new chain is still joinable.

**Intuitive Proof:**

In the forward case we have $x \conv y \to y'$. The definition of semiconfluence means $x \join y'$.

In the backward case we have $x \conv y \leftarrow y'$. By transitivity: $y' \to y \starpath z$. And since $x \starpath z$ we know $x \join y'$.

**Formal Proof:**

TODO

## Theorem: SC implies CR

Semiconfluence implies the CR property. Formally:

$$
\text{Semiconfluent}(R) \implies \text{ChurchRosser}(R)
$$

**Intuitive Proof:**

Let $R$ be a semiconfluent relation. Assume a chain $x \conv y$ exists. Then, using the fact of semiconfluence, prove that the chain is joinable.

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

Let $P_n$ stand for the statement: $x \conv y$ has length $n$ and $x \join y$. We need to prove $P$ for all $n$.

The base case, $P_0$, is trivial: in a chain of length zero, $x = y$ and $x = x \join x$.

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

That is: we have a chain $x \conv y$ of length $k$ and we have proven there exists a $z = x \join y$.

We now extend the chain to length $k+1$ by adding a term $y'$ on the right. There are two ways of doing this: $x \conv y \to y'$ or $x \conv y \leftarrow y'$.

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

By the se2miconfluent chain extension lemma, we can derive $x \join y'$.

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

## Theorem 2.1.6

If $\text{Confluent}(R)$ and $x \conv y$ then:

1. $y$ is a normal form implies $x \starpath y$
2. $x$ and $y$ being normal forms implies $x = y$.

**Intuition:**

What this means intuitively is that for a confluent relation, terms being in normal form put constraints on how they can be connected. If we know $x \conv y$ and $y$ is a normal form, then $x$ can only be an ancestor of $y$  (or $y$ itself). If both are normal forms, then neither can be converted to the other, since no rewrite rules apply to either. Therefore, they must be the same term.

**Proof:**

Self-evidently true.

**Weakening:**

Why is confluence a requirement for this theorem?

Consider the relation defined by:

$$
b' \leftarrow b \leftarrow a \to c
$$

Or, in set notation:

$$
R = \set{(a, b), (a, c), (b, b')}
$$

Here, $c$ is in normal form, and $b \conv c$, but it is not true that $b \starpath c$. Also, both $b'$ and $c$ are normal forms, and $b' \conv c$, but it is not true that $b=c$.

## Theorem

If a relation $R$ is confluent, then every element has _at most_ one normal form, which we denote $x \join$.

**Proof:**

Let $\text{Confluent}(R)$ and pick a term $x$. There are two possibilities:

1. $x$ has zero normal forms
2. $x$ has one normal form
3. $x$ has more than one normal form

The first two cases satisfy the theorem. For the third case: let $x$ have two normal forms $y$ and $z$. Therefore: $x \starpath y$ and $x \starpath z$.

However, by the definition of confluence, $y \join z$, which means they can't be in normal form. A contradiction.

So $x$ can have either zero or one normal forms.

## Theorem

If a relation $R$ is normalizing and confluent, every element has a unique normal form.

**Proof:**

Normalizing means every $x$ must have a normal form $x \join$. So this is the previous theorem, excluding the zero case.

## Theorem

If $R$ is confluent and normalizing then:

$$
x \conv y \iff \left( x \downarrow = y \downarrow \right)
$$

**Forward Proof:**

If $x \conv y$ then $x \join \conv y \join$ by reachability. Then, by [[#Theorem 2.1.6]], $x \join = y \join$.

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

That is: if you can prove $P(x)$ under the assumption that $P$ holds for all successors of $x$, then $P(x)$ holds universally.

Where is the base case? For elements without a successor, there is no $y$ such that $x \pluspath y$, so the assumption is vacuously true.

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

If WFI does not hold, that means there exists a predicate $P$ for which the premise holds but the conclusion does not. That is, this holds.

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

Now repeat the above logic with $a_1$ and we can derive the existence of $a_2$, $a_3$, and so on infinitely. Therefore, $R$ does not terminate.

## Theorem

If WFI holds, then $R$ terminates.

**Proof:**

Let $P(x)$ stand for "there is no infinite chain starting at $x$".

Proving the induction step is easy: if there is no infinite chain starting at any successor of $x$, then there is no infinite chain starting from $x$. Since WFI holds by hypothesis, we conclude $P(x)$ for all $x$. And $P(x)$ holds for all $x$, then $R$ terminates.

## Naming

Terminating relations are also called **well-founded** or **Noetherian**. WFI is also called **Noetherian induction**.

# Branching and Cycles

We can use WFI to study some further properties of relations that relate to termination.

A relation is called **finitely branching** if each element has only finitely many direct successors.

A relation is called **globally finite** of each element has only finitely many successors. This _does not_ mean that the chains from each element are finite (since that would be the same as termination) but rather that the number of _distinct_ successors is finite. The cyclic relation:

$$
\begin{align*}
	a &\to b \\
	b &\to a
\end{align*}
$$

is globally finite (since the set of successors for each element is $\set{a, b}$) but not terminating because it has a cycle.

A relation is called **acyclic** if there is no element $a$ such that $a \pluspath a$.

## Counterexample

Are all terminating relations finitely-branching? No.

Consider the set of terms $A = \set{a, b} \cup \N$ and the relation:

$$
R = \set{ (a, n) \mid n \in \N } \cup \set{ (n, b) \mid n \in \N }
$$

That is: $a$ rewrites to any natural number (infinitely-branching), and every natural number rewrites to $b$.

## Theorem

A finitely branching and terminating relation is globally finite. Or,

$$
\text{FB}(R) \land \text{Terminating}(R) \implies \text{GF}(R)
$$

**Informal Proof:**

An element can have infinitely many successors in two ways:

- Depth: a chain that never terminates.
- Width: infinitely many direct successors (e.g., a relation that rewrites a natural number $n$ into every number $m$ with $m > n$)

If a relation is terminating, every chain is finite. And if it's finitely-branching, every element has finitely many direct successors.

Therefore, the number of successors of an element must be finite.

**Formal Proof:**

We use WFI.

Let $R$ be a finitely-branching, terminating relation. Assume the induction hypothesis:

$$
\forall x . \forall y . x \pluspath y \implies P(y)
$$

Where $P(x)$ means "$x$ has finitely many successors". What the above says is: for all terms $x$, the successors of $x$ all have finitely many successors.

Let $x$ be an arbitrary term with direct successors $\{y_0, y_1, \ldots, y_n\}$. We know this set is finite because $R$ is finitely-branching. From the induction hypothesis, we know that $y_0, \ldots, y_n$ each have finitely-many successors. Therefore, $x$ itself has finitely many successors, since the sum of finitely-many natural numbers is finite.

We have proven:

$$
\forall x . \left( \forall y . x \pluspath y \implies P(y) \right) \implies P(x)
$$

Since $R$ is terminating, we can invoke WFI. By WFI: $P(x)$ holds for all $x$, i.e., $R$ is globally finite.

## Theorem

An acyclic, globally finite relation is terminating. Or:

$$
\text{Acyclic}(R) \land \text{GF}(R) \implies \text{Terminating}(R)
$$

**Informal Proof:**

If $R$ is acyclic, the chains starting from an element $a$ cannot reach back to $a$. And if $R$ is globally finite, $a$ has finitely-many successors. So eventually each of those chains must terminate.

**Formal Proof:**

Let $R$ be an acyclic GF relation. Assume $R$ does not terminate, that is: there exists an infinite chain:

$$
x_0 \to x_1 \to x_2 \to \ldots
$$

Since $R$ is acyclic, $x_i \neq x_j$ for all $i \neq j$. Therefore $x_0$ has infinitely-many distinct successors. Which contradicts the GF assumption. So we have a contradiction. Therefore $R$ terminates.

# Proving Termination

is undecidable in general (Turing, 1936), but not impossible in every single case. For example it is trivial to prove that $\set{a, b}$ with the relation $\set{(a,b)}$ terminates, however uninteresting this may be. Analogously there are proof techniques to prove termination in larger systems. This is a problem in the design of languages for high-integrity systems: how to have the most expressivity while retaining the ability to prove termination.

## Preliminary: Preimages

Let $f : A \to B$. The **preimage** of $S \subseteq B$ under $f$ is the set:

$$
f^{-1}[S] = \set{ x \in A \mid f(x) \in S }
$$

That is: the preimage of $S$ under $f$ is the set of $x \in A$ that $f$ maps to $S$.

We can extend this to relations.

Let $f: A \to B$ and $R \subseteq B \times B$. The preimage of $R$ under $f$ is the set:

$$
f^{-1}[R] = \set{ (x, x') \in A \times A \mid (f(x), f(x')) \in R }
$$

That is: the preimage of a relation $R \subseteq B \times B$ under a function $f: A \to B$ is the set of pairs in $A \times A$ that are mapped by $f$ to pairs which are in $R$.

## Inverse Image Construction

The most basic method for proving a rewrite system $(A, R)$ terminates is to embed it in another rewrite system $(B, S)$ that is known to terminate, using a monotone mapping function $\varphi : A \to B$, where **monotone** means that $R(x, x') \implies S(\varphi(x), \varphi(x'))$.

If $R$ was non-terminating, an infinite chain $x_0 \stackrel{R}{\to}x_1 \stackrel{R}{\to} \ldots$ would map into an infinite chain $\varphi(x_0) \stackrel{S}{\to} \varphi(x_1) \stackrel{S}{\to} \ldots$ which we know can't be true since $S$ terminates.

The mapping $\varphi$ is called a **measure function** and the whole process is called the **inverse image construction** because $R \subseteq \varphi^{-1}[S]$ where:

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

The system $(\N, \gt)$ is known to terminate. Let $A$ bet the set of strings for some alphabet $\Sigma$.

One choice of mapping is length. Let $\varphi(w)$ be the length of the string $w$. This proves that all length-decreasing reductions terminate.

Another choice is the number of letters. For each $a \in \Sigma$ define $\varphi_a(w)$ as the number of occurrences of $a$ in $w$.

## Lemma

A finitely-branching rewrite system $(A,B)$ terminates iff there is a monotone embedding into $(\N, \gt)$. More formally:

$$
\text{FB}(R) \implies \left(
\text{Terminating}(R) \iff \text{Monotone}(A, R, \N, \gt)
\right)
$$

**Informal Proof:**
