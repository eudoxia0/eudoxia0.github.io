---
title: Effective Spaced Repetition
math: yes
---

You won't get smarter by drilling IQ tests or playing the violin. Dual n-back
won't improve your memory. But you can remember anything you choose to with
[spaced repetition][sr].

[sr]: https://gwern.net/spaced-repetition

Spaced repetition is, by far, the most effective cognitive hack I've used. It
used to be that I'd read a book and afterwards remember almost
nothing. Sometimes I'd take Kindle highlights, or notes, but never review
them. And I hoped that though I could not necessarily _name_ what I recalled
(because whose memory has an index?) that somehow the important information was
woven into my knowledge, tacitly, somehow. This is mostly a cope.

I love learning, and spaced repetition has helped me become extremely good at
it. But it took a long time to become effective at it. There's a lot of advice
on the Internet on how to do it effectively, most of which is phrased in terms
of very general principles, but very few concrete examples. But what people
struggle with is: how do I turn this _specific, concrete_ piece of information
into a set of flashcards?

This post describes the rules I use to write effective flashcards, with as many
examples as I could reasonably find.

# Contents

1. [Overview of Spaced Repetition](#overview)
1. [Limiting Factors](#limit)
   1. [Habit Formation](#habit)
   1. [Card-Writing Skills](#skill)
1. [Words of Encouragement](#encouragement)
1. [Rules](#rules)
   1. [Rule: Understand First](#understand)
   1. [Rule: Repeat Yourself](#repeat)
   1. [Rule: Organize by Source](#source)
   1. [Rule: Write Atomic Flashcards](#atomicity)
   1. [Rule: Write Two-Way Questions](#bidirectionality)
   1. [Rule: Ask Questions in Multiple Ways](#poly)
   1. [Rule: Concept Graphs](#graph)
   1. [Rule: Learning Hierarchies](#hier)
   1. [Rule: Learning Sequences](#seq)
1. [Examples](#ex)
   1. [Example: Vector Spaces](#vect)
   1. [Example: Parity Group](#logical-consequence)
   1. [Example: Logical Consequence](#parity)
   1. [Example: Periodization](#perodization)
   1. [Example: Pharmacology](#pharma)
   1. [Example: Magma Formation](#magma)
   1. [Example: Rational Numbers](#ratnums)
   1. [Example: Regular Expressions](#regex)
   1. [Example: Voltage](#voltage)
   1. [Example: Informal and Formal Statements](#informal-theorem)
   1. [Example: Proof Sketches](#proof-sketch)
   1. [Example: Isomers](#isomers)
   1. [Example: Neural Cells](#neural)
   1. [Example: Neuron Types](#neurons)
   1. [Example: Plate Tectonics](#tectonics)
   1. [Example: Random Variables](#random)
   1. [Example: Months of the Year](#months)
   1. [Example: Poetry](#poetry)
   1. [Example: Powers of Two](#powers)
1. [Scripts](#scripts)
   1. [Sequence Script](#seq-script)
   1. [Poetry Script](#poetry-script)
1. [Software](#software)
1. [Prior Art](#prior)
1. [Conclusion](#conclusion)

# Overview of Spaced Repetition {#overview}

The idea is mechanically very simple: as you learn things, you write
_flashcards_, which are question/answer pairs. Then you review them: you look at
the question, recall the answer, and turn the card over to see the answer. Then
you grade yourself: did you recall correctly or incorrectly?

If you keep recalling correctly, the review interval grows longer; if you get it
wrong, the interval gets shorter.

Some people use paper cards, most people use software such as [Anki][anki],
because the algorithm schedules reviews efficiently, so you don't over-review
material. Unless you have a paper fetish, just use software.

# Limiting Factors {#limit}

If you're so smart, why aren't you rich? Or: if spaced repetition is so
effective, why doesn't everyone do it? Why isn't it as common as drinking
coffee?

There are two main limiting factors to effective spaced repetition.

## Habit Formation {#habit}

For spaced repetition to be useful, it has to be a habit. I drill flashcards
every day as part of my morning routine. But habit formation is difficult,
doubly so for people who have ADHD or are low in conscientiousness.

The reason you have to do it every day is that the spaced repetition algorithm
schedules the reviews for you, freeing you from having to do that manually. But
you don't know what cards are due in a given day until you open the app. And if
you skip a day, those cards pile up and are due the next day.

A common failure mode (and I did this more than once, before I got the hang of
it) is to use Anki for two weeks, then drop it, and pick it back up six months
later only to find you have 600 cards due for review. This is not encouraging,
and it defeats the point of spaced repetition, which is to review the cards on
the intervals the algorithm chooses.

I don't have much advice in this area, except that if you have persistent
problems with conscientiousness, untreated ADHD etc. you should address that
first.

## Card-Writing Skills {#skill}

Writing effective flashcards is a skill that took me a while to acquire. Many of
the cards I wrote in the first four or six months of using spaced repetition
consistently turned out pretty much useless, and this can be frustrating. The
main reason to write this post was to communicate the lessons I learnt so you
can jump in to using spaced repetition effectively from the start.

One reason this can be frustrating is you'll often remember a flashcard for the
first few weeks of it (when you're seeing it with high frequency), but after a
couple of months, you start failing it. It didn't take root in your long term
memory, because it was poorly written in some way. And this long feedback cycle
means it takes time to acquire these skills through trial and error.

# Words of Encouragement {#encouragement}

Learning is an automatic, instinctual process. It's a fundamental feature of
intelligence. It's a testament to how bad schooling is that people think they
have to have a special kind of brain to learn effectively, and that the idea of
learning triggers aversion in people. Remember the words of Feynman: "what one
fool can do, another can".

# Rules {#rules}

Here are my rules for effective spaced repetition.

The rules are sorted by applicability, with more general ones first, and most
specific ones last.

## Rule: Understand First {#understand}

Don't try to memorize what you don't understand. The concepts should be clear in
your head before you try to commit them to memory. "Clear" can be a fuzzy
thing. What I tend to do is: dig, expand, and clarify the text until I'm
comfortable I have a good grasp of this region of the concept graph, and then
write the flashcards.

Often, when reading a book, you can't write the flashcards exactly as you read
the text, because further information can clarify or tie together important
concepts. It can be useful to keep a scratchpad where you write tentative
flashcard text as you read a chapter, and at the end you organize and
re-organize your scratchpad until you can commit it to flashcards.

## Rule: Repeat Yourself {#repeat}

Memory is frequency times volume. Individual cards should be extremely brief,
but your deck as a whole can be as repetitive as you want.

Ask questions in multiple ways. Ask for formal and informal definitions of
terms. Ask for the formal and informal statements of a theorem. Ask questions
forwards and backwards. Add contextual questions: "what is the intutition for
[concept]?". Add questions that link different concepts across your knowledge
graph.

## Rule:  Organize by Source {#source}

Organize content by source, not topic.

The reason is you'll often bring in information from multiple sources: multiple
textbooks, plus Wikipedia, plus lecture notes, etc. Each one of these sources
likely has a different way of organizing knowledge.

Don't waste time trying to find the perfect ontology.

Make a deck for each source. In the case of textbooks, make a sub-deck for each
chapter. In the case of math textbooks, possibly make a sub-sub-deck in each
chapter to put theorem cards.

This also makes it easier to keep track of how far along you got into a text.

## Rule: Write Atomic Flashcards {#atomicity}

Cards should be short. They should refer to as little information as
possible. They should be like chemical bonds, linking individual _atoms_ of
knowledge.

_This is the most important thing._ By far the most common failure mode is to
put too much in a flashcard.

There's two reasons for this rule:

1. Larger cards are harder to remember.
2. It's harder to objectively grade yourself: when you reveal the answer, you
   might have got some things right and some things wrong. If you click forget,
   you will be over-reviewing the parts you already know. If you click
   remembered, you will under-review the parts you forgot.

There is one exception to this: you can have big cards if you also have smaller
cards that add up to the same information. You can think of the larger card as
testing that you can collate the information from the smaller cards.

## Rule: Write Two-Way Questions {#bidirectionality}

When possible, ask questions in two directions.

Whenever you have a term with a definition, the obvious thing to do is to ask
for the definition from the term, e.g.:

>Q: What is the order of a group?
>
>A: The cardinality of its underlying set.

But you can also ask for the term from the definition, e.g.:

>Q: What is the term for the cardinality of a group?
>
>A: The group's order.

When you have some notation, like $\mathbb{R}$ for the real numbers, or $\dim V$
for the dimension of a vector space, the natural thing to ask is what the
notation means.

>Q: What does $\mathbb{R}$ stand for?
>
>A: The set of real numbers.

You can also ask the question backwards:

>Q: What is the notation for the set of real numbers?
>
>A: $\mathbb{R}$

## Rule: Ask Questions in Multiple Ways {#poly}

TODO

## Rule: Concept Graphs {#graph}

It can help to visualize the concepts you're acquiring as being like a graph,
where each node represents a discrete concept having certain properties, and the
edges in the graphs are links between the knowledge.

## Rule: Learning Hierarchies {#hier}

A lot of knowledge is hierarchical, of the form "Foo can be either A, B, or C",
or, dually, "A is a kind of Foo". By analogy to OOP: these concepts are joined
by superclass and subclass relations.

The idea is to ask questons in the top down direction ("What are the subclasses of
Foo?") and the bottom-up direction ("What is Bar a subclass of?").

This ties into keeping flashcards atomic. Even when some information is not
hierarchical, intrinsically, breaking down large flashcards into smaller
flashcards is fundamentally building a hierarchy of flashcards.

## Rule: Learning Sequences {#seq}

In general, to learn a sequence $(A_1, \dots, A_n)$, you want to generate the
following flashcards for each $i \in [1,n]$:

| Question                         | Answer      |
|----------------------------------|-------------|
| What is the $i$-th element?      | $A_i$       |
| What is the position of $A_i$?   | $i$         |
| What element comes after $A_i$?  | $A_\{i+1\}$ |
| What element comes before $A_i$? | $A_\{i-1\}$ |

You might also want:

1. A **test card:** a flashcard asking you to recite the sequence from beginning
   to end.
2. A **cloze sequence:** flashcard with a cloze deletion for each element in the
   sequence, to fill in the blank given the context.

How thorough you want to be depends on the nature of the information. Most of
the time I use a cloze card and a test card.

# Examples {#ex}

Because many of the examples involve multiple rules at the same time, I decided
to group them separately from the rules.

## Example: Vector Spaces {#vect}

Here's what we want to learn:

<div class="border-box">

A vector space, informally, is a set whose elements---called vectors---can be
added or scaled.

More formally: a vector space over a field $\mathbb{F}$ is a set $V$ plus two
operations:

1. Vector addition: $V \times V \to V$
1. Scalar multiplication: $V \times \mathbb{F} \to V$

Satisfying the following axioms:

Commutativity of Addition
:  $u + v = v + u$

Associativity of Addition
: $u + (v + w) = (u + v) + w$

Identity of Addition
: $\exists 0 \in V : v + 0 = v$

Inverse of Addition
: $\forall v \in V, \exists -v \in V : v + (-v) = 0$

Identity of Scaling
: $1v = v$

Distributivity
: $\forall v \in V, a,b \in \mathbb{F} : (a+b)v = av + bv$

</div>

We have to break this down. Severely. We will do this step by step.

First, we have to separate the informal (intuitive) and formal definitions:

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Informally: what is a vector space?</td>
      <td>A set whose elements can be added or scaled.</td>
    </tr>
    <tr>
      <td>Formally: what is a vector space?</td>
      <td>A vector space over a field $\mathbb{F}$ is a set $V$ plus two operations: vector addition and scalar multiplication.</td>
    </tr>
  </tbody>
</table>

We add one brief question about notation (you may choose to skip this one, it's
an example):

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>What are the elements of a vector space called?</td>
      <td>Vectors.</td>
    </tr>
  </tbody>
</table>

Now we ask about the signatures of the operations:

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>What is the signature of vector addition?</td>
      <td>$V \times V \to V$</td>
    </tr>
    <tr>
      <td>What is the signature of scalar multiplication?</td>
      <td>$V \times \mathbb{F} \to V$</td>
    </tr>
  </tbody>
</table>

Next, we ask for the axioms:

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>What are the axioms that define a vector space?</td>
      <td>
        <ol>
          <li>Commutativity of Addition</li>
          <li>Associativity of Addition</li>
          <li>Identity of Addition</li>
          <li>Inverse of Addition</li>
          <li>Identity of Scaling</li>
          <li>Distributivity</li>
        </ol>
     </td>
    </tr>
  </tbody>
</table>

Finally, we ask what each axiom means:

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Vector spaces: state: commutativity of addition</td>
      <td>$u + v = v + u$</td>
    </tr>
    <tr>
      <td>Vector spaces: state: associativity of addition</td>
      <td>$u + (v + w) = (u + v) + w$</td>
    </tr>
    <tr>
      <td>Vector spaces: state: identity of addition</td>
      <td>$\exists 0 \in V:  v + 0 = v$</td>
    </tr>
    <tr>
      <td>Vector spaces: state: inverse of addition</td>
      <td>$\forall v \in V, \exists -v \in V:  v + (-v) = 0$</td>
    </tr>
    <tr>
      <td>Vector spaces: state: identity of scaling</td>
      <td>$1v = v$</td>
    </tr>
    <tr>
      <td>Vector spaces: state: distributivity</td>
      <td>$\forall v \in V, a,b \in \mathbb{F}:  (a+b)v = av + bv$</td>
    </tr>
  </tbody>
</table>

Graphically, you can try visualizing the flashcards and their relationships like this:

<a href="/assets/content/effective-spaced-repetition/vect.svg"><img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/vect.svg"/></a>

TODO: REST

## Example: Parity Group {#parity}

TODO

## Example: Logical Consequence {#logical-consequence}

TODO

## Example: Periodization {#perodization}

TODO

## Example: Pharmacology {#pharma}

TODO

## Example: Magma Formation {#magma}

TODO

## Example: Rational Numbers {#ratnums}

Let's commit this to spaced repetition:

<div class="border-box">

The set of rational numbers, denoted $\mathbb{Q}$, is the set of fractions with
integer numerator and denominator, where the denominator is non-zero.

Formally:

$$
\mathbb{Q} = \left\{\, \frac{p}{q} \,\, \middle| \,\, p, q \in \Z \land q \neq 0 \,\right\}
$$

The $\mathbb{Q}$ stands for _quotient_.

</div>

Let's visualize the concept graph as we build up the flashcards. We start with
the central node, the concept of the rational numbers:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/rats1.svg"/>

Then we add a notation node, linked by two forward and backwards questions:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/rats2.svg"/>

| Question                                              | Answer                       |
|-------------------------------------------------------|------------------------------|
| What is the notation for the set of rational numbers? | $\mathbb{Q}$.                |
| What does $\mathbb{Q}$ stand for?                     | The set of rational numbers. |

Formal as well as informal definitions:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/rats3.svg"/>

<table>
  <thead>
    <tr>
      <th>Question</th>
      <th>Answer</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Informally, what is the set of rational numbers?</td>
      <td>The set of fractions with integer numerator and denominator, where the denominator is non-zero.</td>
    </tr>
    <tr>
      <td>Formally, what is the set of rational numbers?</td>
      <td>$\mathbb{Q} = \left\{\, \frac{p}{q} \,\, \middle| \,\, p, q \in \Z \land q \neq 0 \,\right\}$</td>
    </tr>
    <tr>
      <td>What's the term for the set of integer fractions?</td>
      <td>The rational numbers.</td>
    </tr>
    <tr>
      <td>What is the name of this set? $\left\{\, \frac{p}{q} \,\, \middle| \,\, p, q \in \Z \land q \neq 0 \,\right\}$</td>
      <td>The rational numbers.</td>
    </tr>
  </tbody>
</table>

And a final note on notation: what the $\mathbb{Q}$ stands for:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/rats4.svg"/>

| Question                                               | Answer          |
|--------------------------------------------------------|-----------------|
| What are the rational numbers denoted by $\mathbb{Q}$? | Q for quotient. |

## Example: Regular Expressions {#regex}

This is an example about asking questions in two ways.

These cards go from a concept to a regex:

| Question                                | Answer |
|-----------------------------------------|--------|
| What regex matches the start of a line? | `^`    |
| What regex matches the end of a line?   | `$`    |
| What regex matches a digit?             | `\d`   |

In addition to the above, add cards that go from the regex to the concept:

| Question              | Answer               |
|-----------------------|----------------------|
| What does `^` match?  | The start of a line. |
| What does `$` match?  | The end of a line.   |
| What does `\d` match? | A digit 0-9.         |

## Example: Voltage {#voltage}

TODO

## Example: Informal and Formal Statements {#informal-theorem}

TODO

## Example: Proof Sketches {#proof-sketch}

TODO

## Example: Isomers {#isomer}

<div class="border-box">

Isomers can be structural or stereoisomers. Stereoisomers can be diastereomers
or enantiomers.

</div>

TODO

## Example: Neural Cells {#neural}

Context:

>Cells in the nervous system are divided into neurons and glia. Glial cells are
>divided into macroglia and microglia. Macroglia are divided into astrocytes,
>oligodendrocytes, and Schwann cells.

Visually:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/neuro.svg"/>

We first write the top-down questions:

| Question                                       | Answer                                           |
|------------------------------------------------|--------------------------------------------------|
| What kinds of cell make up the nervous system? | Neurons and glia.                                |
| What are the kinds of glial cell?              | Microglia and macroglia.                         |
| What are the kinds of macroglia?               | Astrocytes, oligodendrocytes, and Schwann cells. |

And the bottom-up questions. We don't ask these when the answers are obvious:
"what are microglia a kind of" has an obvious answer.

| Question                          | Answer     |
|-----------------------------------|------------|
| Astrocytes are a kind of...       | Macroglia. |
| Oligodendrocytes are a kind of... | Macroglia. |
| Schwann cells are a kind of...    | Macroglia. |

## Example: Neuron Types {#neurons}

TODO

## Example: Plate Tectonics {#tectonics}

Here's the information:

<div class="border-box">

The zone where two or more tectonic plates meet is called a _plate
boundary_. There are three kinds:

1. Convergent boundaries: plates come together.
2. Divergent boundaries: plates move apart.
3. Transform boundaries: plates slide past each other.

</div>

Applying the rule that cards should be two-way, we want two flashcards for the
term 'plate boundary'.

| Question                                                   | Answer                                |
|------------------------------------------------------------|---------------------------------------|
| What is a plate boundary?                                  | The place where tectonic plates meet. |
| What is the term for the place where tectonic plates meet? | Plate boundary.                       |

For the different types of plate boundary, we only ask the question in the
top-down direction (we don't need to ask "what kind of thing is a transform
boundary?", since the name kind of gives it away):

| Question                              | Answer                            |
|---------------------------------------|-----------------------------------|
| What are the types of plate boundary? | Convergent, divergent, transform. |

For each kind of plate boundary, we also ask the question in two ways:

| Question                                                 | Answer                                       |
|----------------------------------------------------------|----------------------------------------------|
| Definition: convergent boundary.                         | Where tectonic plates come together.         |
| Definition: divergent boundary.                          | Where tectonic plates move apart.            |
| Definition: transform boundary.                          | Where tectonic plates slide past each other. |
| Term: place where tectonic plates come together.         | Convergent boundary.                         |
| Term: place where tectonic plates move apart.            | Divergent boundary.                          |
| Term: place where tectonic plates slide past each other. | Transform boundary.                          |

Graphically, here's how the questions link the concepts in the knowledge graph:

<img style="margin-left: auto; margin-right: auto;" src="/assets/content/effective-spaced-repetition/plates.svg"/>

## Example: Random Variables {#random}

TODO

## Example: Months of the Year {#months}

Suppose you want to memorize:

<div class="border-box">

1. January
2. February
3. March
4. April
5. May
6. June
7. July
8. August
9. September
10. October
11. November
12. December

</div>

The index-to-element flashcards:

| Question                            | Answer    |
|-------------------------------------|-----------|
| What is the 1st month of the year?  | January   |
| What is the 2nd month of the year?  | February  |
| What is the 3rd month of the year?  | March     |
| What is the 4th month of the year?  | April     |
| What is the 5th month of the year?  | May       |
| What is the 6th month of the year?  | June      |
| What is the 7th month of the year?  | July      |
| What is the 8th month of the year?  | August    |
| What is the 9th month of the year?  | September |
| What is the 10th month of the year? | October   |
| What is the 11th month of the year? | November  |
| What is the 12th month of the year? | December  |

The element-to-index flashcards:

| Question                                | Answer |
|-----------------------------------------|--------|
| January is the ... month of the year.  | 1      |
| February is the ... month of the year. | 2      |
| March is the ... month of the year.    | 3      |
| April is the ... month of the year.    | 4      |
| May is the ... month of the year.      | 5      |
| June is the ... month of the year.     | 6      |
| July is the ... month of the year.     | 7      |
| August is the ... month of the year.   | 8      |
| September is the ... month of the year.| 9      |
| October is the ... month of the year.  | 10     |
| November is the ... month of the year. | 11     |
| December is the ... month of the year. | 12     |

The successor flashcards:

| Question                    | Answer    |
|-----------------------------|-----------|
| What comes after January?   | February  |
| What comes after February?  | March     |
| What comes after March?     | April     |
| What comes after April?     | May       |
| What comes after May?       | June      |
| What comes after June?      | July      |
| What comes after July?      | August    |
| What comes after August?    | September |
| What comes after September? | October   |
| What comes after October?   | November  |
| What comes after November?  | December  |

And the predecessor flashcards:

| Question                     | Answer    |
|------------------------------|-----------|
| What comes before February?  | January   |
| What comes before March?     | February  |
| What comes before April?     | March     |
| What comes before May?       | April     |
| What comes before June?      | May       |
| What comes before July?      | June      |
| What comes before August?    | July      |
| What comes before September? | August    |
| What comes before October?   | September |
| What comes before November?  | October   |
| What comes before December?  | November  |

## Example: Poetry {#poetry}

TODO

## Example: Powers of Two {#powers}

Let's memorize the first sixteen powers of two:

<div class="border-box">

$$
\begin{align*}
2^{2} &= 4\\
2^{3} &= 8\\
2^{4} &= 16\\
2^{5} &= 32\\
2^{6} &= 64\\
2^{7} &= 128\\
2^{8} &= 256\\
2^{9} &= 512\\
2^{10} &= 1024\\
2^{11} &= 2048\\
2^{12} &= 4096\\
2^{13} &= 8192\\
2^{14} &= 16384\\
2^{15} &= 32768\\
2^{16} &= 65536
\end{align*}
$$

</div>

The forward cards ask for the power:

| Question | Answer  |
|----------|---------|
| $2^2$    | $4$     |
| $2^3$    | $8$     |
| $2^4$    | $16$    |
| $2^5$    | $32$    |
| $2^6$    | $64$    |
| $2^7$    | $128$   |
| $2^8$    | $256$   |
| $2^9$    | $512$   |
| $2^{10}$ | $1024$  |
| $2^{11}$ | $2048$  |
| $2^{12}$ | $4096$  |
| $2^{13}$ | $8192$  |
| $2^{14}$ | $16384$ |
| $2^{15}$ | $32768$ |
| $2^{16}$ | $65536$ |

While the backwards cards ask for the exponent from the power:

| Question       | Answer |
|----------------|--------|
| $\log_2 4$     | $2$    |
| $\log_2 8$     | $3$    |
| $\log_2 16$    | $4$    |
| $\log_2 32$    | $5$    |
| $\log_2 64$    | $6$    |
| $\log_2 128$   | $7$    |
| $\log_2 256$   | $8$    |
| $\log_2 512$   | $9$    |
| $\log_2 1024$  | $10$   |
| $\log_2 2048$  | $11$   |
| $\log_2 4096$  | $12$   |
| $\log_2 8192$  | $13$   |
| $\log_2 16384$ | $14$   |
| $\log_2 32768$ | $15$   |
| $\log_2 65536$ | $16$   |

Finally, I have a test card that asks me to recall the entire sequence in order.

# Scripts {#scripts}

TODO

## Sequence Script {#seq-script}

TODO

## Poetry Script {#poetry}

TODO

# Software {#software}

Most people use [Anki][anki]. It's open source, has lots of plugins, and has a
free card sync service. This is the default choice.

[anki]: https://apps.ankiweb.net/

I use [Mochi][mochi]. It has a much nicer UI than Anki, which helps in
maintaining the habit because it's more pleasant to use. It also supports [cloze
deletion for images][occlusion] out of the box, as opposed to relying on a
plugin.

[mochi]: https://mochi.cards/
[occlusion]: https://ankiweb.net/shared/info/1374772155

The only Anki feature I care about that Mochi lacks is note types. Anki's note
types let you generate multiple cards from the same structured information. For
example, you could have a "Chemical Element" note type with fields like "Name",
"Symbol", "Atomic Number", and card templates like:

| Question                              | Answer   |
|---------------------------------------|----------|
| What's the symbol of {name}?          | {symbol} |
| What's the atomic number of {name}?   | {z}      |
| What element has the symbol {symbol}? | {name}   |
| What element has atomic number {z}?   | {name}   |

So from one note ("Hafnium", "Hf", "72") you automatically derive four
flashcards. And if you edit the note data, all the cards get updated. Mochi
lacks this feature, in practice it's not terrible, and you can get most of the
way there with cloze deletions.

# Prior Art {#prior}

The most cited resource on writing flashcards is a classic 1999 article by
[Piotr Woźniak][woz], creator of [SuperMemo][sm]. The article is [_Effective
learning: twenty rules of formulating knowledge_][twenty] and I endorse most of
it. Let me reiterate the important parts:

[woz]: https://en.wikipedia.org/wiki/Piotr_Wo%C5%BAniak_(researcher)
[sm]: https://en.wikipedia.org/wiki/SuperMemo
[twenty]: http://super-memory.com/articles/20rules.htm

1. _Understand_ before _memorizing_: the concepts have to be clear in your head
   before you try to commit them to memory. "Clarity" is a fuzzy thing, but what
   I do is dig, expand, clarify until I feel comfortable formulating knowledge.
1. Cards should be _atomic_, and should refer to the smallest possible
   conceptual units.
1. Avoid trying to memorize long sequences.
1. Avoid trying to memorize big unordered sets of things.
1. Keep the wording simple.
1. Redundancy is good. Do repeat yourself!

# Conclusion {#conclusion}

I hope that you will find these rules and examples useful. Now go pick up a
textbook and learn something useful.
