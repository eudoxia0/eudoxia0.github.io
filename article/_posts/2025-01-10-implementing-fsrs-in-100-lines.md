---
title: Implementing FSRS in 100 Lines
summary: .
math: yes
---

A while back I wrote an implementation of [SM-2][sm2], the algorithm used by [Anki] to schedule flashcard reviews. While reading up on it I heard about [FSRS], a new algorithm that is slated to replace SM-2 as Anki's default scheduler. The pitch for FSRS is efficiency: 30% less review time for isoretention compared to SM-2. So I got curious.

Initially I had difficulty understanding FSRS, because the information about it is scattered all over the place: GitHub wikis, blogs, Reddit. On top of that there are different versions of the algorithm, and sources don't always specify which one they talk about.

So I put everything I could find together, and cleaned it up. Pleasantly, the implementation turned out to be just 100 lines, though much of it remains cryptic: while SM-2 was intelligently designed, FSRS was evolved by training a model on a dataset of Anki reviews.

The rest of this post explains the theory of FSRS, with equations interleaved with code. If you just want the code, scroll to the end.

# The DSR Model

FSRS is based on the [3-component model of memory][dsr], also called the DSR model. Under DSR, the state of a memory in the brain is modeled by three variables:

**Retrievability** ($R$) is the probability of recalling the memory. This is a real number in the range $[0, 1]$.

```rust
type R = f64;
```

**Stability** ($S$) is the time in days for $R$ to go from $1$ to $0.9$ (i.e. 90% probability). This is a real number in the range $[0, +\infty]$.

```rust
type S = f64;
```

**Difficulty** ($D$) models how hard it is to recall the memory. This is a real number in $[1, 10]$. Note that we start at $1$, not $0$.

```rust
type D = f64;
```

# The Main Loop

- For each card schedule today:
	- Show the user the question (the front of the card).
	- User mentally recalls the answer
	- User flips the card.
	- User rates their answer by giving it a grade.
	- The system updates the card's stability and difficulty. It calculates the next review interval, and schedules the card for that day.

When a user reviews a flashcard, they are shown a question, mentally recall the answer, flip the card, and rate their recall performance. This rating is called the **grade**, and it's one of:

- 1: forgot the answer ("forgot").
- 2: recalled the answer, but it was hard ("hard")
- 3: recalled the answer ("good")
- 4: recalled the answer, and it was easy ("easy")

```rust
#[derive(Clone, Copy, PartialEq, Debug)]
enum Grade {
    Forgot,
    Hard,
    Good,
    Easy,
}

impl From<Grade> for f64 {
    fn from(g: Grade) -> f64 {
        match g {
            Grade::Forgot => 1.0,
            Grade::Hard => 2.0,
            Grade::Good => 3.0,
            Grade::Easy => 4.0,
        }
    }
}
```

# Retrievability

TODO define item

The retrievability of an item is approximated by:

$$
R(t) = \left( 1 + F\frac{t}{S} \right)^C
$$

Where $t$ is time in days since the last review, $S$ is the stability of the item, and $F$ and $C$ are constants to control the shape of the curve:

$$
\begin{align*}
F &= \frac{19}{81} \\
C &= -0.5
\end{align*}
$$

This allows us to predict how retrievability decays over time as a function of time and stability. In code:

```rust
type T = f64;

const F: f64 = 19.0 / 81.0;
const C: f64 = -0.5;

fn retrievability(t: T, s: S) -> R {
    (1.0 + F * (t / s)).powf(C)
}
```

Note: at $t=0$, the equation simplifies to $R(0) = 1$, that is, when we have just seen an item, we have a 100% chance of recalling it.

TODO charts
