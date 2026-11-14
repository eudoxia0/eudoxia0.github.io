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

# The Rhetorical Definition

- If you talk to people from the LessWrong cultural sphere, and probe
  their views and the arguments, a lot of them seem to view alignment
  as: "the property that, if a superintelligent AI has it, leads to
  utopia". That is, a completely opaque premise defined entirely by
  its outcomes.

- Rhetorically, this is very useful, since any criticism of the
  post-AI utopia can be refuted:

  - "(bad thing) would be a failure of alignment. Since the AI is
    aligned, it won't happen. "

  - "Since the AI is superintelligent, it will foresee all the
    unforeseen negative consequences of the AI utopia, and build a
    utopia that avoids all of them."

- Thus making discourse impossible.

- The problem with this definition is that it's not constructive. It
  doesn't help you align an AI or know if an AI is aligned. It's
  entirely retrospective: you can only call the AI aligned once it's
  done rearranging the universe.

- It's like you buy a self-help book titled "How To Be Successful",
  and it just says: "a successful person is one who makes decisions
  that lead to good outcomes". And the rest of the book is blank. This
  is not useful\!

- The problem with this definition is that it acts short-circuits
  thought:

  - "What if the benevolent AI utopia is bad?" "Well the AI is
    superintelligent so it will figure it out".

# Indirect Normativity

- Bostrom's term?
- Instead of aligning the AI to some value system, align the AI to a _process_
  for discovering that value system.

## CEV

- Problems:

  - The "coherent" EV might be the empty set.

  - What if the CEV of humanity turns out to be something morally
    repugnant to present-day humanity?

  - What  is the "reference class" of humanity?

  - What about animals?

  - Should the future have moral authority over the present? Imagine a
    time machine lands in the present, the time travelers are humans
    with an IQ of 300 who have spent the past 100,000 years thinking
    and discoursing on ethics. They've concluded that negative
    utilitarianism is true, and the most ethical course of action is
    to destroy all life in the universe. What then?

    - We would be justified in telling them to fuck off. The version
      of you that is smarter, wiser, more like the person you wish you
      were, and has thought about ethics 10,000 times longer than you
      is not you. It's a different person.

  - The CEV of humanity is not something we are expected to
    discover. Rather, it's a goal, an initial condition, to a
    superintelligent AI: "discover the CEV of humanity and implement
    that". By construction, any question about what CEV utopia
    actually looks like is out of scope.

  - This idea of a clean separation between what we want the AI to do
    (where our preferences converge) and what it won't do (where our
    preferences diverge) is false. The CEV of humanity might include
    "not getting arbitrarily murdered", but to actually enforce that,
    you need intrusive surveillance of all habitable space. This idea
    that you can build a god to rule the universe but we will still
    have agency and freedom inside that sphere seems false.

# Intent Alignment


- Align AI to the intent of the operator.

- This is the one that seems the most plausible to implement, since it
  doesn't require solving too many philosophical quagmires.

- There are two problems:

  - Learn a model of the operator's intent.

  - Ensure the AI abides by that model.

- The second problem might be hard, but it seems in principle
  doable. The first problem seems easy: modeling is precisely what
  deep learning is good at. And, if the operator says in the loop,
  giving positive/negative feedback to the AI, the model evolves along
  with the operator.

- Of course the obvious problem is: what if the operator is evil?

  - How much do you keep humans in control?

  - Use intent alignment to answer CEV?

  - Implement something mutually agreeable to the humans.

- In society we manage because most people are more or less evenly
  matched: some are stronger, some are smarter, some are richer, but
  nobody is so superhuman in enough traits that they can
  single-handedly best all of society. In a world of superhuman AI,
  that safety in numbers is broken.

- Failure mode: solve intent alignment, build an intent-aligned ASI,
  take over the world, disempower everyone else.

- Solve intent alignment, give everyone an intent-aligned ASI: has the
  problem of "distributed disempowerment".

  - c.f. "recipes for ruin" from Michael Nielsen.

  - The problem here is that we, humans, are more fragile than
    machines. We would also be much slower and dumber.

  - The society of aligned ASIs might be able to prevent a single ASI
    aligned to an evil operator from destroying them.

  - But they might not be able to prevent us from being trampled
    underfoot.

  - In a war between humans, the ants get crushed.

  - Furthermore, if a single person builds a misaligned ASI (not a
    superintelligence intent-aligned to an evil operator, but a
    superintelligent that is not aligned to anything human at all)
    then it might be over.

  - In a war between two ASIs, one of which is intent-aligned to a
    human, and the other is completely autonomous, the human is a
    fatal bottleneck. They won't be able to have any meaningful input.

- We might be able to muddle through a good future.

  - But it seems there's a significant risk of ending up in a future
    where humans are kind of like a vestigial organ ensconced in their
    guardian angel ASI.

- Things we want to avoid:
  - Runaway AI takeover.
  - Huge uplift for cyber, bio etc.

- How to keep humans in control?

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

[bluedot]: https://blog.bluedot.org/p/what-is-ai-alignment
[impossible]: https://www.persuasion.community/p/ai-alignment-is-impossible)
