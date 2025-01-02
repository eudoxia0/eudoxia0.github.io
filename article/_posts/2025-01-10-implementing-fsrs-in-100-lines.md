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
