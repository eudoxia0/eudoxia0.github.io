---
title: And Yet It Understands
summary: The more powerful the AI, the greater the dismissal.
---

 >To think is to forget differences, to generalize, to abstract.
>
>— Jorge Luis Borges, [_Funes el memorioso_][borges]

[borges]: https://en.wikipedia.org/wiki/Funes_the_Memorious

Someone, I think Bertrand Russell[^brooks], said we compare the mind to whatever is the most complex machine we know. Clocks, steam engines, telephone relays, digital computers. For AI, it's the opposite: as capabilities increase, and our understanding of AI systems decreases, their descriptions become more and more dismissive.

In 1958, when Herbert Simon introduced his [General Problem Solver][gps], he famously said: "there are now in the world machines that think", though in reality GPS is to GPT what a centrifugal governor is to a turbofan. Machines you can use for free today pass the Turing test, these are called "[stochastic parrots][parrot]", which is dismissive, but at least parrots are living things that engage in goal-directed activity. Every day machines do something that if you were told of it not one year ago you would have called it witchcraft, but LLMs are "[blurry JPEGs][jpeg]". Yesterday I heard "Markov chain on steroids". When the first computer wakes up we'll call it "a pile of sed scripts", and there are people so deep in denial they could be killed by a T-800, all the while insisting that some German philosopher has proven AI is impossible.

[gps]: https://en.wikipedia.org/wiki/General_Problem_Solver
[parrot]: https://dl.acm.org/doi/10.1145/3442188.3445922
[jpeg]: https://www.newyorker.com/tech/annals-of-technology/chatgpt-is-a-blurry-jpeg-of-the-web

Yesterday I saw [this Twitter thread][thread]. Briefly: GPT was trained on many natural languages, the authors finetuned an instance of it with instructions in English. Then they fed it requests in some other language, and it carries them out, following the English-language instructions.

[thread]: https://twitter.com/janleike/status/1625207251630960640

And I thought: so what? This is expected behaviour, no? Then a friend pointed out that this is only confusing if you think InstructGPT doesn't understand concepts.

Because if GPT is just a [Chinese room][room] it shouldn't be able to do this.

[room]: https://en.wikipedia.org/wiki/Chinese_room

Every sequence input-output pair of token sequences can, in principle, be produced by a function. You could, in principle, have a lookup table so vast any finite conversation with it would be indistinguishable from talking to a human, the [Eliza of Babel][babel]. But it wouldn't fit in the entire universe. And there is no rigorously imaginable compression scheme that would make it fit. But [GPT-3 is 800GiB][gpt3].

[babel]: https://en.wikipedia.org/wiki/The_Library_of_Babel
[gpt3]: https://en.wikipedia.org/wiki/GPT-3

How is it so small, and yet capable of so much? Because it is _forgetting details_, which is another term for abstraction. For concept formation. There comes a point in the performance to model size curve where the simpler hypothesis has to be that the model _really does understand_, and we have clearly passed it.

There is a speciest of denialist for whom no evidence whatever will convince them that a computer is doing anything other than shuffling symbols without understanding them. "Concepts" and "Ideas" are exclusive to humans (they live in the [Leibniz organ][leibniz], where they're synthesized from the black bile). Debate is pointless because it's a threat to the business model: there is infinite demand for deeply credentialed morons who will tell you that everything is fine, that machines can't think, that humans are and always will be at the apex. Things they confidently predict machines will never do happen with regularity, and they just move the goalposts, or they retreat into unfalsifiable metaphysics.

It's time to wake up. Because Herbert Simon was right, though sixty years early:

>there are now in the world machines that think, that learn, and that create. Moreover, their ability to do these things is going to increase rapidly until in a visible future--the range of problems they can handle will be coextensive with the range to which the human mind has been applied.[^simon]

[leibniz]: https://en.wikipedia.org/wiki/Calculus_ratiocinator

[^brooks]:
    Or [Rodney Brooks][rodney] or maybe John Searle.

[^simon]:
    Source is [here][simon].

[rodney]: https://www.edge.org/response-detail/25336

[simon]: https://conversableeconomist.blogspot.com/2020/04/1957-when-machines-that-think-learn-and.html
