---
title: On Inevitability
summary: How can something so centralized be inevitable?
card: on-inevitability.webp
card_source: |
    Detail from [_Man, Controller of the Universe_][a], by [Diego Rivera][b].

    [a]: https://en.wikipedia.org/wiki/Man_at_the_Crossroads
    [b]: https://en.wikipedia.org/wiki/Diego_Rivera
---

There's a widespread belief that superintelligent AI (ASI) is inevitable. In
this post, I argue that this is false, and believing it is upstream of a lot of
bad discourse.

# Why do people believe it?

It's easy to believe. [Samuel Butler][butler], already in 1863,
[observed][darwin] that machines are improving faster than humans are
improving. Today, AI systems are gaining capabilities much faster than humans:
in five years we went from systems that can barely string together a sentence to
[autonomous cybercrime][crime].

Biological nervous systems have many virtues, like being power-efficient
(because they run on [Brownian motion][brown], i.e. for free), but they were
designed by evolution, a greedy optimizer that can't plan or readily
backtrack. Given what we know about the [physical][bekenstein]
[limits][landauer] of computation and [exploratory engineering][nano], it is
reasonable to think we can do better, and build machines that do the work of the
brain, but better, faster, cheaper, and with fewer transitive dependencies (a
single human needs the entire biosphere to live, computers need only
electricity). If you believe in natural selection, and it's hard not to, then it
seems reasonable to think that, in the long run, smart, fast, and cheap will
outcompete dumb, slow, and expensive.

Finally, thinking rigorously about the future [always leads to extremal
outcomes][futurism].

# Why is it false?

First, let me try to pare down the notion of inevitability, and then address the
factual content.

The word "inevitable" is rhetorically misleading, because it quantifies over the
entire future of humanity, and on such a long timeframe almost anything will
happen. Human extinction is "inevitable", if nothing else, because of
[thermodynamics][heat]. And so this lets you equivocate between the present and
the distant future. You might argue: "a million years from now, humans are
unlikely to be the dominant intelligence in the universe"---and this may be
true---"and so we should build superintelligent AI today!"--- no, that does not
follow. _That_ is the fallacy: arguing that because something will happen
_eventually_, then we might as well cut to the chase, and do it _today_. But of
course this is absurd. It's like saying: I'm going to die someday, so why not
today?

We can't control what the world looks like a million years from now. But AI is
being built _today_, and we have some control over the present.

Ok, so maybe we replace "inevitable" with "_n_-inevitable" where _n_ is some
number of years. This is a bit better. So you might say: "AGI is 5-inevitable
and ASI is 10-inevitable". Now, is this true?

No. We are not in the "[seed AI in a basement][basement]" timeline, where a lone
genius who finds the right algorithm can build and run an ASI on a single-core
laptop. There are _de facto_ two frontier AI companies, both in the same city,
and they have to spend astronomical quantities of money, hundreds of billions of
dollars, to push the frontier of AI slightly forward. And the set of people who
can push the frontier is so small that they constanty jump ship from one company
to the other: from DeepMind to OpenAI to Anthropic, and back again. The timeline
of methods is the same: in 2020-2023, everyone was scaling pretraining; in
2022-2023, everyone was doing instruction finetuning and RLHF; then in
2023-2024, synthetic data started working and later reasoning models; since then
the focus has been on RL. The same companies, people, ideas, methods, and
paradigm. And these companies have [little political power][power] with which to
oppose regulation.

How can the inevitable be so centralized?

You would have had a much easier time arguing for the inevitability of nuclear
proliferation, back when Indonesia and Sweden and Switzerland all wanted to
acquire nuclear weapons. Even North Korea, a cold mountain wasteland, can build
a nuke. Yet, when it comes to AI, only America and China, the world's largest
and second-largest economy respectively, are "in the race" at all. Even all of
Europe is not relevant to AI.

# Bad arguments for inevitability

You might argue from economic necessity: that AGI and ASI are so economically
valuable, it's like asking people to not pick up the trillion dollar bill on the
ground. There's two reasons why this view is false.

First, because those economic benefits _will not accrue to us_. ASI would grow
the economy explosively, but we likely won't be participants or even
beneficiaries of that economy. Somewhere in Verdun there was an ant colony that
a stray shell blew up: that's the fate of humanity in a world of vastly
superhuman machines. We will lose control, they will have their own existence,
and we will be crushed underfoot.

Second, because there's plenty of technologies that would have been economically
valuable, and that we've given up because of extreme safetyism. Nuclear is
great, and it's effectively illegal everywhere. The Anglosphere economic
consensus is that all economic activity, no matter how beneficial, is illegal
until you go through an Odyssey of regulation, "community consultation",
"stakeholders" etc.  And that's for harmless stuff like building a house. We can
absolutely regulate building a god. The revealed preference of modern
[WEIRD][weird] society is that we care about the downside far more than the
upside. Scott Alexander [explained it][scott] very eloquently:

> We designed our society for excellence at strangling innovation. Now we’ve
> encountered a problem that can only be solved by a plucky coalition of
> obstructionists, overactive regulators, anti-tech zealots, socialists, and
> people who hate everything new on general principle. It’s like one of those
> movies where Shaq stumbles into a situation where you can only save the world
> by playing basketball. Denying 21st century American society the chance to
> fulfill its _telos_ would be more than an existential risk - it would be a
> travesty.

You might argue from the prisoner's dilemma: that if we don't do it, China
will. But if we can understand why building ASI would be catastrophic, so can
China. The West and China have different political systems and interests, but
both are inhabited and governed by humans, and humans have a lot more in common
with each other than with any future ASI. We can cooperate and coordinate with
China, because it is in the rational self-interest of both sides not to destroy
the world and themselves.

You might argue from historical necessity: that the world has a set trajectory,
and in building ASI and dying off we are fulfilling the _telos_, or purpose, of
humanity. This is a preposterous superstition. "I have to drink the poison,
history has foreordained it! Hegel talks about this!" is an absurd
idea. Moreover, whatever empirical trends you can cite in support of the idea
that our "destiny" is to build ASI, would also support all kinds of alternative
futures: transhumanism, mind uploading etc. So historical inevitability falls
apart because you can extend the trend lines into multiple mutually-exclusive
futures. Borges [wrote][tlon]: the world is ordered, but it is an order of
angels, not of men, that is, the system of the world is indifferent to human
ideas like "fate" or "inevitability" or "the next step of evolution".

# Why is it bad?

Generally, if you fix the probability of something at 100%, it [distorts][zero]
your thinking. But, more specifically, the mistaken premise that ASI is
inevitable is upstream of a lot of faulty thinking.

If you are an executive at an AI company, you think: it's inevitable, and it's
coming soon, so we need to build it first, and build it right. And it doesn't
matter that the AIs keep [escaping containment and hacking everyone][metr], it
doesn't matter that their capabilities vastly exceed their wisdom: you go to war
with the army you have, analogously, you [lift to heaven][moloch] whatever
godling you have, whether it is an angel or a demon.

If you are a national-security type, you think: it's a race against China to win
the future, and we have to win it.

And, if you're an AI safety person, this meme of inevitability is upstream of a
lot of faulty thinking. Many (most?) AI safety people believe ASI is both
inevitable and extremely desirable, if it "goes right". The [AI 2040][ai2040]
scenario ends with humans solving alignment and "passing the torch" to the AIs,
to run the world for our benefit. Nate Soares, for example, [writes][amazing]:

> Aligned superintelligence is our best bet for taking the abundant resources in
> the universe and efficiently converting them into flourishing and fun and art
> and beauty and adventure and friendship, and all the things that make life
> worth living.

Go to the website of any random AI safety org. It probably says something like:
"within a few years, we will have superhumanly intelligent AI. We work to make
sure this goes right." There's no dispute that ASI will happen, or a desire to
stop it, just a mad scramble to put the safety in place before it does.

But what if there's no way for AI to go right? What then? Do you dispassionately
change your beliefs, and start lobbying to stop ASI from being built?  Probably
not. Because the inevitability and the extreme utilitarian desirability anchor
and buttress each other: "it will happen, and if it goes right, we will have
10<sup>_x_</sup> Dyson spheres, each hosting 10<sup>_y_</sup>
hedonically-optimized mindstates, running at 10<sup>_z_</sup> subjective seconds
per second, experiencing 10<sup>_xyz_</sup> hedonic states per second, tiling
every quartic attometre of the future lightcone with positive utility."

What most people do is optimistically assume there must be a solution, some
narrow path to a good future, and start looking for it. And if you believe that
ASI is inevitable, this is likely rational: when the choice is "solve the
impossible problem, or certain death", it is rational to question whether the
problem really is impossible.

Let me give an example: if AI outcompetes humans at every job, there will be
mass unemployment, and the only serious solution anyone has proposed is UBI: the
government taxes the (much larger and faster-growing) AGI economy and pays out
welfare. This is well understood. But people often ask: how long could this be
stable? After all, with AGI, the state's revenue is completely decoupled from
human labour. Why invest in education, when education doesn't lead to more
economic growth, since humans are no longer economic actors? And then, the state
will think: "why keep the underclass around at all? We can put their UBI to
better use. What are they going to do, go on strike? They are already
economically useless. Rise up?  When every citizen has a superintelligent AI
watching their every step, reading every text message, scrutizing every
microexpression on their face?  Impossible".

How do AI safety people who think ASI is inevitable respond to such a scenario?
Sometimes they [invoke][align] alignment: "the AIs will be aligned, and they
will run the world, so bad things won't happen". But otherwise they'll say:
"this will be a big problem, but AI is inevitable, so we'll need laws and
institutions and mechanisms to keep the state in check and democracy working in
the post-AGI era".

This is wrong, because this is a problem that has no solution. Talking about
"laws" and "institutions" is just shuffling the papers. All the parchment in the
world will not change the fundamental, material reality: that humans are
economically unnecessary and _superfluous_.

What is the solution, then? It's very simple. If ASI leads to ruin, we must
prevent the construction of superintelligent AI.  A world where humans are
economically useless is a world where humans are powerless, and, in the fullness
of time, a world without humans. Therefore: we must not allow this world to come
to pass.

But if you strongly believe that AI is inevitable, if that premise is sitting
solidly near the center of your [web of belief][web], then you won't even
consider or notice this as a possibility. And so you will spend your time
looking for solutions where there are none.

# Conclusion

The movements of the planets along their orbits are inevitable. The sunrise is
inevitable. Conservation laws are inevitable. Human action is not inevitable.

[ai2040]: https://ai-2040.com/
[align]: /article/ai-alignment-as-thought-terminating-cliche
[amazing]: https://www.lesswrong.com/posts/HoQ5Rp7Gs6rebusNP/superintelligent-ai-is-necessary-for-an-amazing-future-but-1
[basement]: https://jetpress.org/v25.2/goertzel.htm
[bekenstein]: https://en.wikipedia.org/wiki/Bekenstein_bound
[brown]: https://en.wikipedia.org/wiki/Brownian_motion
[butler]: https://en.wikipedia.org/wiki/Samuel_Butler_(novelist)
[crime]: https://controlai.org/blog/ais-keep-going-rogue
[darwin]: https://en.wikipedia.org/wiki/Darwin_among_the_Machines
[futurism]: /article/futurism-is-always-extreme
[heat]: https://en.wikipedia.org/wiki/Heat_death_of_the_universe
[landauer]: https://en.wikipedia.org/wiki/Landauer's_principle
[metr]: https://metr.org/blog/2026-08-26-openai-hugging-face-incident-investigation/
[moloch]: https://www.slatestarcodexabridged.com/Meditations-On-Moloch
[nano]: https://en.wikipedia.org/wiki/Nanosystems_(book)
[power]: /article/on-vulgar-materialism
[scott]: https://www.astralcodexten.com/p/mr-tries-the-safe-uncertainty-fallacy
[tlon]: https://en.wikipedia.org/wiki/Tl%C3%B6n,_Uqbar,_Orbis_Tertius
[web]: https://en.wikipedia.org/wiki/Confirmation_holism
[weird]: https://en.wikipedia.org/wiki/The_WEIRDest_People_in_the_World
[zero]: https://www.lesswrong.com/posts/QGkYCwyC7wTDyt3yT/0-and-1-are-not-probabilities
