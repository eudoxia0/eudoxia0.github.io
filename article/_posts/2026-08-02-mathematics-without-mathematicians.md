---
title: Mathematics Without Mathematicians
summary: We must know. We shall not know.
card: mathematics-without-mathematicians.webp
card_source: |
    _The Astronomer_, Johannes Vermeer, 1668–1669.
---

Yesterday, OpenAI [announced][ann] the solution to ten open problems in
mathematics, all discovered by a yet-unreleased model. There's only one, the
coding theory one, where I know enough to say "huh, that's important", but
according to the mathematicians I trust this is important.

Inevitably, people will spin some sophistry to cope, to convince themselves
nothing will change. And that's fine. People need to cope. But we can't put our
heads in the sand forever while the world is transformed around us. So, here is
a list of ways people will cope about AI taking over mathematics, and how each
cope is likely to be refuted by reality.

My intent is not to horribly depress everyone but rather to help them metabolize
the implications of this technology. The arguments here are somewhat portable:
replace "mathematics" with "botany" or whatever as needed.

**Moving the goalposts.**

Obvious and not worth addressing.

**"We will direct the AIs, point them at problems and research areas
to solve."**

The AIs will exceed humans in [taste][taste] and intuition. At some point, the
human pointing the way will get worse results than the human saying "here's a
proof checker, have fun" and paying for the tokens.

**"We will teach the mathematics AI discovers."**

The AIs will be better teachers than the humans. In any case, there won't be a
human audience for expository work of frontier math.

**"We will choose how to canonize the results AI discovers."**

This is a nice cope. The AIs are explorers out in the frontiers, the humans
gratefully receive their Lean proofs, and then discourse over them, choose which
results are relevant, shape those results into a little brick for the great
[cathedral of algebra][mathlib]. Analogous to the above: the AIs will build the
cathedral on their own. They will be better architects than us.

**"We will become students of AI mathematics."**

This works until the AIs have blasted so deep into the deductive closure of
mathlib that the distance from elementary mathematics to the frontier exceeds
what any human can hope to learn in their lifetime, no matter how narrow their
focus.

**"We need humans to understand the results AI discovers."**

We won't! This misunderstands who the audience will be. AIs will do frontier
math, downstream, AIs will use the new math to do frontier science, finally, AIs
will use the new science to do frontier engineering. No human needs to
understand any of it, firms that put humans in the loop to understand the
results will be outcompeted by those which don't.

The result is that we will live in a demon-haunted world, full of marvelous
devices whose operation we will not understand, based on engineering principles
we will not understand, discovered using formalisms we will not understand.

**"Computers are already superhuman at chess, yet we still play chess."**

Unlike most copes, I think this one is interesting. Computers are superhuman
chess players, yet we don't care, and continue playing as normal. Why should
mathematics be different?

The main reason, I think, is that chess is self-contained: results from chess
don't help us understand the orbits of the planets or the binding of drugs to
protein surfaces. But mathematics, [famously][wigner], is the great dynamo of
science, the best language and method for understanding the world. A machine
that can replace a human mathematician, but better and faster and cheaper, is
materially useful; a better chess engine is not.

If two computers which are superhuman at chess play against each other, who
cares? There is little demand for this, so there is no-one to outcompete. A
superhuman mathematician is different.

**"Mathematics will change, but mathematicians and the mathematically-inclined
will still do math on their own."**

I think this ignores that mathematics is embedded in a social context. As an
example: when it became clear that AI would eat software, my cope was: "I'm
perfectly happy to become an engineering manager to agents in my professional
life; in my off time, I can still [write code][gh] for the pleasure of it."

And I do. But this cope ignores the effect AI has had on the social context of
writing code: the discourse has gotten worse, and vastly more anti-intellectual;
people who used to talk about type systems and compilers now talk about "loops"
and "harnesses"; you put a hand-created project on GitHub and you get slop PRs;
you open a link to an interesting-looking project and find the README is
unreadable AI slop. And in the long-term, it is demoralizing to ponder: will
anyone design a new programming language? Dually, if I design a new language,
will anyone care? If I write a library that introduces an elegant new formalism
to solve a particular problem, will anyone use it?

Which is to say: no man is an island. You can do mathematics on your own, but
you'll find that very, very few people can sustain any activity long-term on the
basis of intrinsic motivation alone. We are social animals: we care about being
useful, about status, about outcomes in the world.

# Conclusion

I think I should end on a cheerful note. So let me try. Personally, I don't
believe technology is inevitable. "Inevitable" is a word reserved for the orbits
of the planets. Nothing that is the product of human action is inevitable. We
can choose to obsolete ourselves, and we can choose not to. We can realize that
AGI is a devil's bargain: we _may_ accelerate technical progress, we _may_
unlock all kinds of wonderful tech tree nodes like life extension earlier than
we would otherwise; but the result, in the long run, is that humans become, at
best, like pets under the care of vastly more powerful entities.

# A Note On Prediction

The future is not certain, but I've phrased everything above as definitive for
simplicity. If we take the possibility of AGI and ASI seriously, if AI continues
to progress as it has for the past ~6 years, I think this is a reasonable view
of where things will go.

Probably the best argument against AI progress is "RL doesn't generalize well,
we have seen immense progress in verifiable domains like formalized mathematics
and coding, we will see less progress in domains that are intuitive or
unformalizable". Maybe true. But billions of dollars and thousands of very smart
people---and, increasingly, very smart models---are being thrown at this
problem. How long does this cope last?

[ann]: https://openai.com/index/ten-advances-in-mathematics/
[gh]: https://github.com/eudoxia0/
[mathlib]: https://github.com/leanprover-community/mathlib4
[taste]: https://www.benkuhn.net/impact/#ii-taste
[wigner]: https://en.wikipedia.org/wiki/The_Unreasonable_Effectiveness_of_Mathematics_in_the_Natural_Sciences
