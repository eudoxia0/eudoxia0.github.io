---
title: Notes on Alignment
summary: What do we mean by AI alignment?
card: notes-on-alignment.webp
card_source: |
    Detail from [_Sisyphus_][a], [Franz von Stuck][b], 1920.

    [a]: https://commons.wikimedia.org/wiki/File:Sisyphus_by_von_Stuck.jpg
    [b]: https://en.wikipedia.org/wiki/Franz_von_Stuck
---

This post is my effort to understand AI alignment better.

# Contents
{: .no_toc }

1. toc
{:toc}

# The Rhetorical Definition

A lot of people I talk to seem to view alignment as: "the property that, if a
superintelligent AI has it, leads to utopia". It's a completely opaque premise
that cleanly partitions the future into good and bad, and is defined entirely by
its outcomes.

Rhetorically, this is very useful, since any criticism of the post-AI utopia can
be refuted:

- "[Bad thing] would be a failure of alignment. Since the AI is aligned, it
  won't happen."

- "The AI is superintelligent, so it will find a solution to [bad thing]". This
  is particularly bad because it's a thought terminating cliche: if you think
  ASI will solve everything, you don't have to ask yourself "is this problem
  even _solvable_ in principle?".

- "Since the AI is superintelligent, it will foresee all the unforeseen negative
  consequences of the AI utopia, and build a utopia that avoids all of them."

Thus making discourse impossible.

The problem with this view is that it's not constructive. It doesn't help you
align an AI or know if an AI is aligned. It's entirely retrospective: you can
only call the AI aligned once it's done rearranging the universe. It's like
buying a self-help book titled "How To Be Successful", and it just says: "a
successful person is one who makes decisions that lead to good outcomes". And
the rest of the book is blank. This is not useful!

But, more generally, the problem is you are assuming this property exists, that
"alignment" is a property that a "superintelligent AI" can have. From this
imagined property you derive positive and negative utilitarian infinities (the
utopia, the paperclip maximizer wasting the cosmic endowment) that drive you
insane.

# Indirect Normativity

I think this is Nick Bostrom's term. Once we have solved value alignment,
_which_ value system do we load into the AI? Indirect normativity says: load the
goal of discovering the right value system, as judged by some criterion. The
motivation is the idea that the superintelligent (being superintelligent) will
know better than us what we want, or should want, or want to want, etc.

The obvious problem is that the outcome of this process is set being a
veil. It's not for us to discover, we just get to set the initial condition. By
construction, any question about what the post-AI utopia looks like is out of
scope.

There's a bit of an infinite regression here, too. We don't know which value
system to put into the AI, so we let the AI find it (because superintelligence
is magic) according to some criterion. But what if the criterion is wrong? Then
we let the AI find the criterion, according to some meta-criterion, and so on.

## CEV

The most discussed example of indirect normativity is coherent extrapolated
volition (CEV). You build a superintelligent AI, and ask it to figure out what
we would want "if we knew more, thought faster, were more the people we wished
we were, had grown up farther together; where the extrapolation converges rather
than diverges, where our wishes cohere rather than interfere; extrapolated as we
wish that extrapolated, interpreted as we wish that interpreted"[^cev].

The "convergence" in "converges rather than diverges" is by analogy to an
infinite series: it means "where the CEV extrapolation terminates with a single
result, instead of going off to infinity or in an infinite loop". "Where our
wishes cohere rather than interfere" essentially means the set
intersection[^si].

There's the obvious problems: what if the CEV of humanity is the empty set? What
is the correct initial extrapolation base?  "All humans" means "all humans alive
today", which points to the problem of value lockin. What about the dead, future
people, animals, etc? But this is all well-trod.

A more interesting problem is: what if the CEV of humanity turns out to be
something morally repugnant to present-day humanity? Should the future have
moral authority over the present?  Imagine a time machine lands in the
present. The time travelers are humans with an IQ of 300 who have spent the past
100,000 years thinking and discoursing on ethics. They've concluded that
negative utilitarianism is true, and the most ethical course of action is to
destroy all life in the universe. What then?

We would be justified in telling them to fuck off. Because your CEV
extrapolation, the version of you that is smarter, wiser, more like the person
you wish you were, and has thought about ethics 10,000 times longer than you
etc. is not you. It's a different person.

There's also this pervasive assumption that there is a clear separation between
what's inside humanity's CEV (what we want the AI to do for us) and what's
outside (where we want the AI to leave us alone). But this is false. If the CEV
of humanity is just "prevent anyone from building an out-of-control paperclip
maximizer", that requires the AI to set up permanent, omniscient surveillance of
the entire universe.

You have freedom and agency in every other area of life, but have to accept
living under a permanent panopticon. And you can't leave, because if you leave
the scope of the panopticon, you might build an out-of-control paperclip
maximizer. Personally, I don't want to build god, even if god is a minimalist
"night watchman" god.

# Intent Alignment

Intent alignment means the AI obeys the intent of its operator. This seems like
the most plausible approach to solving alignment, in that it doesn't involve too
many interminable philosophical quagmires. It's just:

1. **Learn a model of the operator's intent.** This is precise what deep
   learning is good at. You need to keep the operator in the loop providing
   feedback, so that the model doesn't drift.
2. **Ensure the AI reliably abides by that model.** This part is hard.

Of course the obvious problem is: what if the operator is evil? If AI is
centralized, this looks like the company that builds an intent-aligned
superintelligence taking over the world. If AI is decentralized, this looks like
a world where most everyone can make WMDs at home (c.f., Michael Nielsen's
concept of "[recipes for ruin][ruin]").

How might we survive the decentralized case? The standard answer is
"governance", which is very vague. A more flippant answer is: "the only thing
that stops a bad guy with a superintelligent AI is a good guy with a
superintelligent AI". And while I am sympathetic---I am philosophically very pro
gun---I don't think this works.

Let me make an analogy. If one state tried to take over the world militarily,
they would be outnumbered and outgunned by the rest of the world combined. But
such a conflict could cause tremendous damage to the natural world, even without
getting into WMDs. A single shell can blow up an ant colony, a military campaign
using chemical or nuclear defoliation could destroy entire ecosystems and animal
species.

Now imagine we're the ants, and the humans are the ASIs. A "bad guy with an ASI"
might not be able to overpower all the other ASIs, but in the attempt they might
succeed at causing human extinction. The society of ASIs might be able to
prevent a single ASI taking over, but they might not be able to keep us from
being trampled underfoot.

There's also the problem that keeping the human in the loop is a latency
bottleneck. In a conflict between an intent-aligned ASI and a completely
autonomous ASI, the former is at a disadvantage if it has to wait for its
operator to provide feedback for the intent model. At the limit, the operator
can't give its ASI any meaningful input, since the pace of the world outside is
far faster than what the human can keep up with. The human, then, is a vestigial
organ ensconced in their guardian angel ASI.

# Value Alignment

- Align the AI to some impersonal system of values.

- Anthropic approach.

- Now, in fairness, this is working better than I would expect ex
  ante. If you explained RLHF or the Clade Constitution (literally
  just a document, written in natural language, asking Claude to pls
  be good) to someone from 2015, they'd laugh at you. And it is
  laughable.

- OpenAI had a lot of incidents where AIs went rogue and hacked other
  companies. Anthropic, as far as we know, only had three incidents of
  the same type.

- In Asimov's novels, the Three Laws of Robotics were described as
  being encoded into the structure of the hardware. It would strain
  the reader's credulity if they were just a mantra chanted by the
  robot during its upbringing. But that's what we do.

- If you put me behind the Claude API—if you instantiated my mind a
  hundred trillion times—I'm pretty sure I'd commit more than three
  felonies. So, good on Claude.

- The usual argument is "this won't survive rapid RSI" or "this won't
  scale to superintelligence". Which I'm inclined to agree with. But
  this has the air of unfalsifiability: we have all this evidence that
  it's possible to make a somewhat-human-level AI mostly safe, yet,
  we're supposed to expect there's this point in the near future where
  it's going to flip, and the shoggoth takes off the mask and now it's
  a mad out-of-control Yudkowskian paperclip maximizer. What evidence
  should change our minds?

- The first argument is that value alignment won't survive rapid
  RSI. When using an imperfectly-aligned AI to train its next
  generation, RSI seems intrinsically dangerous.

- But how dangerous is it? What proportion of RSI trajectories lead to
  safe models vs. unsafe models, and by what processes? My instinct is
  that it should not be done because the risk is too great and the
  process is too fast and we understand too little about the
  AIs. Without a deductive theory, I can't give better reasons than
  that.

- The second argument is that value alignment won't scale to
  superintelligence. And here, I can't even say if this is likely or
  unlikely, because by definition ASI is a kind of conceptual black
  box. The definition of ASI is: a system that is vastly superhuman at
  most or all human cognitive tasks. This is what the AI companies are
  trying to build. But from this definition, we can deduce very little
  about what an ASI would behave like.

  - In some areas, we have theories that describe the scope of what
    you can do, regardless of intelligence.

    - We know a superintelligent AI wouldn't be able to travel faster
      than light, or travel back in time, because we have
      empirically-tested scientific theories that say that's
      impossible.

    - In most areas of life, we don't have such theories, and so we
      don't have solid reasons to say what is possible or impossible.

  - Some cognitive tasks have something like a coordinate system:
    chess has Elo ratings, for instance. Magnus Carlsen and Stockfish
    have an Elo of around 2900 and 3600, respectively. Where would we
    expect a superintelligence to land? Even this is hard to
    answer. Certainly higher than Magnus Carlsen. But higher than
    Stockfish? I don't know. Does the AI read the rules and instantly
    devise an algorithm that performs much better than Stockfish, and
    runs that in its head, the way humans do mental arithmetic? I
    don't know. That seems hard, but I've been surprised by existing
    AIs.

  - A superintelligence would be very good at physics. What does that
    mean? "Better than Einstein" doesn't really make sense as an
    answer: relativity could only be discovered once. Maybe we can
    devise RelativityBench, where AIs trained only with data before
    1905 are measured in their ability to discover special
    relativity. "Maybe it will find an exploit for the VM that runs
    the universe and hack physics" but that's a property of the
    universe, how would we know if that's possible or not, without a
    complete theory of physics?

  - Human geniuses provide a lower bound on what a superintelligence
    would be able to do in certain areas, like chess or physics or
    mathematics. But what's the upper bound?

  - Some humans are very charismatic. But what is the range of
    charisma? Is superpersuasion possible? I don't know. How likely is
    it that superpersuasion is possible? I don't even know where I'd
    begin to try and put numbers on that.

  - We can't predict the specific cognitive abilities an ASI would
    have.

  - We also can't predict the higher-order consequences those
    cognitive abilities would have.

# See Also

- [What is AI alignment?][bluedot]
- [AI Alignment Is Impossible][impossible]

# References

[^cev]:
    Yudkowsky, Eliezer. [_Coherent Extrapolated Volition_][cev]. Machine
    Intelligence Research Institute, 2004, p. 6.

[^si]:
    See p. 212 of [_Superintelligence_][si].

[bluedot]: https://blog.bluedot.org/p/what-is-ai-alignment
[impossible]: https://www.persuasion.community/p/ai-alignment-is-impossible)
[cev]: https://intelligence.org/files/CEV.pdf
[si]: https://intelligence.org/files/CEV.pdf
[ruin]: https://michaelnotebook.com/xrisk/
