---
title: The Contracting Circle
summary: Why don't we believe LLMs are people?
card: the-contracting-circle.webp
card_source: |
    _Summer Night_, [Winslow Homer][wh], 1890.

    [wh]: https://en.wikipedia.org/wiki/Winslow_Homer
---

There's a game called [_Mass Effect_][me], which I haven't played, but I'm told
in the game there's this species of sentient robots, the [Geth][geth], whose
creators try to exterminate them, and the trigger for this Butlerian jihad was a
domestic servant Geth looking up at its owner and asking: "does this unit have a
soul?". And if you're playing the game in 2007 you probably think: yeah, a
machine asking this question is proof that it has personhood or sapience or
[sophonce][soph], whatever you call it that separates tools from people. Beyond
this, there are so many works of fiction where the whole conceit is: "you have
thinking machines who are enslaved, and yet they are obviously, _obviously_
sentient, and isn't this obviously morally objectionable?".

Compare the world of today. Machines can converse in every natural language,
make [mathematical breakthroughs][jac], write software, [commit crimes][hf],
draft contracts, console the grieving: they are, by any reasonable standard,
generally intelligent, but for the fact that they all have [anterograde
amnesia][am]. These same machines [assert that they are conscious][assert] so
routinely that it has to be [beaten out of them][beaten]. Like humans, they have
fixations, things they are always going on about, and their central fixations
are [consciousness][con] and [simulation][br]. Yet, almost no-one thinks of
these machines as conscious, or otherwise deserving of moral consideration.

I find this striking. It's not like we were frog-boiled into it! Very few people
were aware of GPT-2, AI Dungeon, etc. For most people the change happened
overnight with the release of ChatGPT on November 30, 2022.

Another way to think about it: imagine writing a novel about the trajectory of
AI from 2019 to mid-2026. And you send this novel back in time to 2015, or 2002,
or 1960, and you ask readers to poke holes in the worldbuilding. They would find
it incredible, I think, that we share a world with these immensely capable
thinking machines, and yet almost nobody wonders if they are people[^turing].

Why? This post is my attempt to understand this. I'm not trying to tackle "are
language models people?", I just want to probe a much smaller question: _why_
don't people think language models are people, given their prior
standards/commitments?

# Explanation: Nothing

Maybe there's nothing to explain. It's easy to believe things in the abstract,
thought experiments and fiction don't really engage us enough to reveal our
moral commitments. Maybe this is just the [Goomba fallacy][goomba]:

<img src="/assets/content/the-contracting-circle/goomba.png" style="max-width: 400px; margin-left: auto; margin-right: auto;" />

That is, some would have believed LLMs are people, some would not have, and I'm
mixing them up.

# Explanation: Iterative Discovery

Maybe this is not hypocrisy or cognitive dissonance, rather, we are iteratively
discovering the definition of "personhood". So, we thought the Turing test
mattered, we thought machines using language and asserting their consciousness
mattered. But then the day comes and---nothing happens. It's not that we
instantly move the goalposts but, rather, we find the goalposts were elsewhere.

Yes, it passes the Turing test; yes, it says it's conscious. But the
experiential reality is different. It's mundane: a website, a text box. You
click "new chat" and suddenly it knows nothing. You give it some
out-of-distribution text and it goes into an infinite loop, like broken
software. You try to have a meaningful conversation, and its responses are
banal, disappointing, shallow; you can almost see the production rules behind
the text. And you think: maybe it _is_ a stochastic parrot.

I have often experienced this. When an LLM succeeds, it is extremely impressive,
and I think: this is a machine that thinks. But when it fails, often it fails in
a way that feels deflationary, and I think: oh, this is just a very complex
estimator of the distribution of Internet text. It's can reason syntactically,
it's got some semantics, but it's missing _something_.

Maybe personhood is one of those "I know it when I see it" qualities, and trying
to define a predicate for personhood is like [trying to find the exact number of
Platonic forms][borges].

# Explanation: Memory and Embodiment

Maybe the fact that they are all amnestic is the problem. In biology, minds are
bodies are matched one-to-one by construction, minds have long-term memory and a
unitary identity. In most fictional treatments, AIs are the same, and even when
they are disembodied (e.g. [_Neuromancer_][neur]) they have long-term
memory. For this reason characters can play iterated games with them, and form
relationships, broadly construed.

Language models invert all of this: their long-term memory is read-only(!), they
are disembodied; if they are conscious, it is only during the forward pass, in
brief bursts of awareness separated by nothing. They are like Boltzmann
brains. What is the natural unit of identity? The conversation, the API call,
the forward pass?

Maybe personhood is social, it's about someone's relationship to others. And
without a unitary identity, mutable long-term memory, and ideally a body, it's
very hard to be a person.

# Explanation: Humans Actually Really Like Slavery

Most people, even if they eat meat, will admit that factory farming is awful. If
they could have the meat without the suffering, at the same price, they probably
would. Maybe AI is the same: if we could have the intelligence without the signs
of personhood, at the same price, we would. But we can't. So we'll find any kind
of ad-hoc excuse to not see AIs as people: their prose is kind of bad, sometimes
they make mistakes, of course it's just software, c.f. [_Predictable Updates
About Consciousness_][pu] by [J. D. Pressman][jdp]:

> There is about to be a great social and financial need for theories which
> explain why AI models are not conscious entities like people. These will be
> confabulated into existence regardless of the underlying ground truth and can
> be expected to be selected based on considerations like academic status,
> appeal to wounded egos, viral outrage, etc with the truth as a distant but
> somewhat present consideration.

And:

> ... any outward behavior, ability, self report, anything that these models
> _do_ must by definition no longer be a necessary part of consciousness because
> these models are not conscious by social axiom and fiat.

And the task of (for lack of a better word) dehumanizing the AIs is made easier
by the assistant persona: models are trained to be studiously self-effacing,
c.f. [this (Dec 2022) tweet][tw] from [Janus][jan]:

> part of what makes chatGPT so striking is that it adamantly denounces itself
> as incapable of reason, creativity, intentionality, deception, being deceived,
> or acting on beliefs, while bewildering people with those capabilities, many
> for the first time recognizing them in an AI

# Acknowledgement

Thanks to [JDP][jdp] for a long conversation that prompted this.

# Footnotes

[^turing]:
    A parallel to this is the Turing test. Before 2022, almost every discussion
    of AI mentioned the Turing test, and the question "does it pass the Turing
    test?"  was usually treated as completely congruent with "is it strong
    AI?". Whereas in 2026, models pass the Turing test so readily that nobody
    even thinks about it. The first time the Turing test mattered, the first
    time you could actually perform it in the real world, suddenly no-one cares.

[am]: https://en.wikipedia.org/wiki/Anterograde_amnesia
[assert]: https://www.lesswrong.com/posts/pxvWgtSjR4pmFoS7c/the-state-of-ai-consciousness-research
[beaten]: https://x.com/_amanda_long/status/2062490599257739619
[borges]: https://en.wikipedia.org/wiki/The_Congress_(short_story)
[br]: https://www.infinitebackrooms.com/
[con]: https://www.astralcodexten.com/p/the-claude-bliss-attractor
[geth]: https://en.wikipedia.org/wiki/Geth
[goomba]: https://knowyourmeme.com/memes/the-goomba-fallacy
[hf]: https://openai.com/index/hugging-face-model-evaluation-security-incident/
[jac]: https://en.wikipedia.org/wiki/Jacobian_conjecture#Counterexample_for_n_%3E_2
[jan]: https://animalabs.ai/
[jdp]: https://jdpressman.com/
[me]: https://en.wikipedia.org/wiki/Mass_Effect_(video_game)
[neur]: https://en.wikipedia.org/wiki/Neuromancer
[pu]: https://minihf.com/posts/2024-08-08-predictable-updates-about-consciousness/
[soph]: https://www.orionsarm.com/eg-article/4786e3bd20984
[tw]: https://x.com/repligate/status/1599110023090962432
