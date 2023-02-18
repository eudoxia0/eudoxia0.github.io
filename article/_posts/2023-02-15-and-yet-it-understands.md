---
title: And Yet It Understands
summary: The more powerful the AI, the greater the dismissal.
---

 >To think is to forget differences, to generalize, to abstract.
>
>— Jorge Luis Borges, [_Funes el memorioso_][borges]

[borges]: https://en.wikipedia.org/wiki/Funes_the_Memorious

Someone, I think Bertrand Russell[^brooks], said we compare the mind
to whatever is the most complex machine we know. Clocks, steam
engines, telephone relays, digital computers. For AI, it's the
opposite: as capabilities increase, and our understanding of AI
systems decreases, the analogies become more and more dismissive.

In 1958, when Herbert Simon introduced his [General Problem
Solver][gps], he famously said: "there are now in the world machines
that think", though in reality GPS is to GPT what a centrifugal
governor is to a tokamak reactor. Machines you can use for free today
pass the Turing test[^lamda], these are called "[stochastic
parrots][parrot]", which is dismissive, but at least parrots are
living things that engage in goal-directed activity. Every day
machines do something that if you were told of it not one year ago you
would have called it witchcraft, but LLMs are "[blurry
JPEGs][jpeg]". Yesterday I heard "Markov chain on steroids". When the
first computer wakes up we'll call it "a pile of sed scripts", and
there are people so deep in denial they could be killed by a T-800,
all the while insisting that some German philosopher has proven AI is
impossible.

[gps]: https://en.wikipedia.org/wiki/General_Problem_Solver [parrot]:
https://dl.acm.org/doi/10.1145/3442188.3445922 [jpeg]:
https://www.newyorker.com/tech/annals-of-technology/chatgpt-is-a-blurry-jpeg-of-the-web

The other day I saw [this Twitter thread][thread]. Briefly: GPT knows
many human languages, [InstructGPT][instruct] is GPT plus some
finetuning in English. Then they fed InstructGPT requests in some
other human language, and it carries them out, following the
English-language finetuning.

[thread]: https://twitter.com/janleike/status/1625207251630960640

[instruct]: https://openai.com/blog/instruction-following/

And I thought: so what? Isn't this expected behaviour? Then a friend
pointed out that this is only confusing if you think InstructGPT
doesn't understand concepts.

Because if GPT is just a [Chinese room][room] it shouldn't be able to
do this. A Chinese room might be capable of machine translation, or
following instructions within one human language, but the task here is
so self-evidently outside the training set, and so convoluted, that is
requires genuine understanding. The task here involves:

[room]: https://en.wikipedia.org/wiki/Chinese_room

1. Abstracting the English finetuning into concepts.
2. Abstracting the foreign-language requests into concepts.
3. Doing the "algebra" of the task at the conceptual level.
4. Mapping the results back down to the foreign language.

The mainstream, respectable view is that this is still not real
understanding, which requires frames or [symbols][pssh] or logic or
some other sad abstraction completely absent from real brains. But
what physically realizable Chinese room can do this?

[pssh]: https://en.wikipedia.org/wiki/Physical_symbol_system

Every pair of token sequences can, in principle, be stored in a lookup
table. You could, in principle, have a lookup table so vast any finite
conversation with it would be indistinguishable from talking to a
human, the [Eliza of Babel][babel]. Just crank the _n_ higher for a
conversation lasting a time _t_. But it wouldn't fit in the entire
universe. And there is no compression scheme---other than general
intelligence---that would make it fit. But GPT-3 is [a nimble
800GiB][gpt3].

[babel]: https://en.wikipedia.org/wiki/The_Library_of_Babel
[gpt3]: https://en.wikipedia.org/wiki/GPT-3

How is it so small, and yet capable of so much? Because it is
_forgetting irrelevant details_. There is another term for this:
abstraction. It is forming concepts. There comes a point in the
performance to model size curve where the simpler hypothesis has to be
that the model _really does understand_, and we have clearly passed
it.

These dismissive analogies only serve to create a false sense of
security---that if I can name something I understand it and know how
it works and it is no longer a threat---and to signal to the listeners
that the speaker has some revealed knowledge that they lack. But
nobody knows how GPT works, internally, because [minds are irreducibly
complex][roon], which is why the [GOFAI][gofai] program failed.

[roon]: https://twitter.com/tszzl/status/1620230069682839552
[gofai]: https://en.wikipedia.org/wiki/GOFAI

There is a species of denialist for whom no evidence whatever will
convince them that a computer is doing anything other than shuffling
symbols without understanding them. "Concepts" and "Ideas" are
exclusive to humans (they live in the [Leibniz organ][leibniz],
presumably, where they pupate from the black bile). Debate is
pointless because it's a threat to the business model: there is
infinite demand for deeply credentialed morons who will tell you that
everything is fine, that machines can't think, that humans are and
always will be at the apex, people so commited to human chauvinism
they will soon start denying their own sentience because their brains
are connectionist systems. Things they confidently predict machines
will never do happen with regularity, and they just move the
goalposts, or they retreat into unfalsifiable metaphysics.

[leibniz]: https://en.wikipedia.org/wiki/Calculus_ratiocinator

All that's left of the denialist view is pride and vanity. And vanity
will bury us. Because Herbert Simon was right, though sixty years
early:

>There are now in the world machines that think, that learn, and that
>create. Moreover, their ability to do these things is going to
>increase rapidly until in a visible future---the range of problems
>they can handle will be coextensive with the range to which the human
>mind has been applied.[^simon]

# Footnotes

[^brooks]:
    Or [Rodney Brooks][rodney] or maybe John Searle.

[^simon]:
    Source is [here][simon].

[^lamda]:
    Worded more strongly: "[passing the Turing test so hard that the
    interrogator sacrifices their career at Google to advocate for its
    personhood][sims]".

[sims]: https://archive.is/tQ2zc#selection-2301.0-2303.2

[rodney]: https://www.edge.org/response-detail/25336

[simon]: https://conversableeconomist.blogspot.com/2020/04/1957-when-machines-that-think-learn-and.html
