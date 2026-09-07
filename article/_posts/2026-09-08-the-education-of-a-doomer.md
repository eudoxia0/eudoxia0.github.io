---
title: The Education of a Doomer
summary: How and why I went from AI optimist to AI doomer.
card: the-education-of-a-doomer.webp
card_source: |
    Detail from [_The Isle of the Dead_][a], [Arnold Böcklin][b], 1883.

    [a]: https://en.wikipedia.org/wiki/Isle_of_the_Dead_(painting)
    [b]: https://en.wikipedia.org/wiki/Arnold_B%C3%B6cklin
---

If you follow me on Twitter, or read this blog, you have noticed that I went
from being generally optimistic and excited about AI to being extremely
concerned. And I thought, I should explain why I changed my mind. Each section
of this post is about some aspect of AI where my views shifted. I begin each
section by explaining my previous beliefs, and why I held them, and then explain
why those beliefs changed.

# Contents
{: .no_toc }

1. toc
{:toc}

# Economics

Automation has been good. We've automated 99% of the jobs people did in 1790,
and the result is not mass unemployment, rather, we are wealthier, healthier,
more educated, we have more leisure, etc. I had this vague, inductive idea that,
while I can't predict what jobs will exist after AGI, there will be demand for
me to do something. If nothing else, the much higher economic growth of the
post-AGI world means that the human niche, while small in absolute terms, might
be much larger than today's economy.

And this may yet be true. Or it may show a lack of imagination on my part. If AI
develops such that we have enduring complementarity between humans and AIs, then
we might still have jobs in the post-AGI future. But if AI becomes truly
general, the G in AGI, and on top of that it is vastly smarter, faster, and
cheaper than humans, then there might be nothing for us to do, except live off
UBI.

When people talk about UBI, they typically worry about the problem of meaning in
a world without work. I've never had this worry. When I was funemployed last
year, I spent my time reading books and writing code and hanging out with
friends. If the future is an infinite UBI-funded vacation, I know what I'll
do. "Before the Singularity, read books and throw house parties; after the
singularity, etc."

But then I started thinking about the political consequences of AGI, and started
writing about it:

- [_No-One Escapes the Permanent Underclass_][pu]: if humans are economically
  useless, the state does not need them. Why pay out UBI to people who have
  neither economic nor political power?
- [_When The Future Doesn’t Need Us_][need]: factory workers can sabotage the
  machines, truck drivers can shut down logistics. But if humans are
  economically useless, there is no way for people to veto the political order
  by withdrawing their contribution to it. And if the AIs fight wars, then the
  state can be arbitrarily repressive.
- [_Mathematics Without Mathematicians_][math]: most arguments about humans
  moving "one job up" fail because the AI can do those jobs too.
- [_Our Servants Will Do That For Us_][servant]: even for jobs we think of as
  uniquely human, we might prefer to have machines do those jobs too.

# Alignment

I had hope that alignment would turn out to be a normal engineering problem,
that we solve through empirical experimentation and investigation, like
everything else. It helps that the early LLMs were not incomprehensible alien
minds, like something from a Stanisław Lem novel, but rather immensely
human. It's hard _not_ to anthropomorphize them. The huge core of unsupervised
learning in an LLM understands human morality just fine: you can talk to them
about it, they will explain, eloquently, in detail, why something is "good" or
"bad" according to some moral system. And, because capabilities were weaker, the
failures were very small. What's the worst ChatGPT in 2022 could do?

Since like 2024, reinforcement learning has been the main technique to push the
frontier forward. And reinforcement learning agents work exactly like Yudkowsky
says. Consequently, capabilities have increased markedly but the models are
harder to understand (literally: their prose is increasingly incomprehensible)
and are increasingly misaligned as RL scrambles their brains in pursuit of
reward. [Incidents][hack] of serious misalignment are more common and more
consequential. It's clear that AI capabilities are growing far, far faster than
our ability to control or even understand them.

# Control

I thought---or, rather, I implicitly believed---that people would _want_ to
remain in control of the AIs. And if we want to retain control, and solve
alignment, then we will stay in control. Simple enough. But recently I started
to think: no, we will probably hand over control to the AIs.

The weak version of the disempowerment thesis is something like the prisoner's
dilemma: people/companies/polities that hand more power to AI outcompete those
that don't, so there's competitive pressure towards disempowerment. This is easy
to believe.

The strong version of disempowerment is: the AIs will be so smart,
knowledgeable, personable, moral etc. that we will willingly, voluntarily hand
power to them. We'll think, "they can do a better job than us", and we'll be
right. Believing this requires you to be somewhat cynical about humanity's
desire for autonomy vs. material considerations. But, over the past few months,
I have become more cynical about it.

This was not a sudden "oh shit" insight and more a slow, gradual accumulation of
tiny little grains of evidence that each point slightly in the direction of
disempowerment.

## Writing

The proliferation of AI writing, I think, is evidence for disempowerment.

Early on, I'd start reading a blog post, and it's AI slop. I'd open a link to a
GitHub project that looks interesting, and the README is slop. I'd get a pull
request on one of my projects, and the code is slop. You read articles on major
newspapers, and they're written by AI. And that was merely frustrating.

Then it got worse. I read a paper arguing against the use of LLMs in
mathematics---and the paper was AI slop. I [reviewed an entire book][review]
about the post-AGI world, and it was AI slop. I read that some conference
started using Pangram to filter out a deluge of AI-written submissions, and I
read a tweet from a college professor---a college professor, writing under their
own name!---defending the use of AI to write papers.

I don't using AI to write is analogous to calculators, or search engines, or
other such things. There are only finitely any things you can automate. And once
you've automated writing, there's no higher-order activity to move to. People
think "the ideas are mine, the writing is the AI", i.e., they feed the AI a list
of rambling bullet points that the AI massages into a blog post, or a paper, or
whatever. And they think that step is "mere writing", while their composition of
the bullet points was "thinking". But they are wrong, because [writing is
thinking][words]. And so, by letting the AI write, you are giving up most of
what makes up thinking.

## Software Engineering

Claude Code was released a little over a year ago. In that short time, software
engineering has been completely transformed. Materially, it might be positive:
higher productivity, though at the cost of a messier codebase. Socially, it has
been a disaster.

The discourse around software engineering has gotten dumber. It's like everyone
in the industry lost 30 IQ points. People used to talk about compilers, type
systems, logic. Now they talk about "prompts", "harnesses", "loops". The
discourse is narrower, shallower, and more repetitive. There's only so many
times I can hear about "agentic harnesses" before I lose my mind.

Then there's the loss of human capital formation: there is nothing to
learn. Prompting is not a skill, at least, it's a much shallower skill than
software engineering. The instrumental dimension of the work has improved in
that people can get more output per unit of effort, but the dimension of work
that's about building up human capital has collapsed. And maybe this is
rational. Why learn to code at all? The computers can do that for us. And so the
rigorous, systematic thinking you need to practice in order to be a good
programmer: all gone. The machines can be rational for us. We can just vibe.

## Deferring to AI

What's the last major life decision you made without at least consulting an AI?
How many people have you met who treat ChatGPT like an oracle? And this is
_today_, when AI has many limitations (no online learning, unreadable prose,
hallucination). If people treat today's very flawed AI like an oracle, how much
worse will it be in 2030? In 2035? In 2040?

People used to argue online, flame each other, get angry. And that wasn't good,
but at least it was _human_. Now they just reply with screenshots of ChatGPT
"refuting" what the other person said, with zero interest in whether that
refutation is correct. So we don't even flame each other online anymore. If
we're even automating our _vices_, what's left?

# Stagnation

This isn't really an area where my beliefs changed. Rather, it's one of the
reasons that I was originally optimistic about AI.

I was born in 1994, so I lived most of my life in the [great
stagnation][flycar]. As I teenager, I read [_Engines of Creation_][eoc], [_The
Diamond Age_][tda], [_Orion's Arm_][oa]; I dreamt about all amazing technologies
we would someday have, the possibilities they would open up to us. Yet, it all
felt infinitely distant. There's the near future, which is thinner screens and
cheaper solar panels; and there's the distant future, which is molecular
manufacturing and Dyson spheres and mind uploading and interstellar travel, and
you won't see any of it, unless you sign up for [cryonics][cryo] and it works.

Before LLMs, what was there to be excited about in the near future? Solar power?
Self-driving cars?  The _energy transition_? It's laughable. When AI came on the
scene, for the first time in my life, the future I dreamt about felt like it
might be within reach, it felt exciting and imminent, and not this dreadful
monotony of phones and apps and corporate memphis and managed decline.

I often think: what if GPT-3 hadn't worked? What would we have to look forward
to, in this alternate 2026?

[cryo]: https://asteriskmag.com/issues/10/brain-freeze
[eoc]: https://en.wikipedia.org/wiki/Engines_of_Creation
[flycar]: https://press.stripe.com/where-is-my-flying-car
[hack]: https://metr.org/blog/2026-08-26-openai-hugging-face-incident-investigation/
[math]: /article/mathematics-without-mathematicians
[need]: /article/when-the-future-doesnt-need-us
[oa]: https://www.orionsarm.com/
[pu]: /article/no-one-escapes-the-permanent-underclass
[review]: /article/review-job-less-utopia
[servant]: /article/our-servants-will-do-that-for-us
[tda]: https://en.wikipedia.org/wiki/The_Diamond_Age
[words]: /article/human-routers-of-machine-words
