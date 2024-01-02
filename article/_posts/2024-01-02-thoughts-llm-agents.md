---
title: Thoughts on LLM Agents
summary: Entropy, criticality, and complexity classes of cellular automata.
---

[LLMs][llm] are the closest we've come to AGI and the most important invention
since the Internet. In terms of economic productivity, the most useful LLM
products are [ChatGPT][cgpt] and [GitHub Copilot][copilot], respectively a
freeform AI chatbot and code autocomplete. But they can't carry out complex
tasks: very soon you run up against the limits of the context window. LLMs are
like a tireless 120 IQ polymath with [anterograde amnesia][hm] who forgets
everything after ~10m of activity.

[llm]: https://writings.stephenwolfram.com/2023/02/what-is-chatgpt-doing-and-why-does-it-work/
[cgpt]: https://openai.com/blog/chatgpt
[copilot]: https://github.com/features/copilot
[hm]: https://en.wikipedia.org/wiki/Henry_Molaison

[Agents][agent] are an attempt to get around this: they are software systems
that string together LLM calls to accomplish complex tasks. In the same way a
patient with anterograde amnesia might use software tools to compensate for
their deficit in working memory, agents use "classical" software to orchestrate
LLMs to accomplish complex tasks across multiple completions.

[agent]: https://lilianweng.github.io/posts/2023-06-23-agent/

A lot of agent architecture resemble [cognitive architures][cogarch] in the
style of [Soar][soar] and [ACT-R][actr]: they have an agenda of tasks, a
long-term memory (often using a [vector database][vdb]) with [RAG][rag], and
other components. It's actually very interesting how programmers have basically
reinvented cognitive architectures from scratch, often knowing nothing of the
field's existence.

[cogarch]: https://en.wikipedia.org/wiki/Cognitive_architecture
[soar]: https://en.wikipedia.org/wiki/Soar_(cognitive_architecture)
[actr]: http://act-r.psy.cmu.edu/
[vdb]: https://en.wikipedia.org/wiki/Vector_database
[rag]: https://www.pinecone.io/learn/retrieval-augmented-generation/

And yet they don't really work. There isn't a software development agent that
you can download, point it at a repo and give it a high level description of a
change, and come back and hour later to a ready pull request. There's
[AutoGPT][autogpt], [babyagi][babyagi], [gpt-engineer][gpt-engineer] and I've
yet to hear of any of these doing anything impressive.

[autogpt]: https://github.com/Significant-Gravitas/AutoGPT
[babyagi]: https://github.com/yoheinakajima/babyagi
[gpt-engineer]: https://github.com/gpt-engineer-org/gpt-engineer

And I wonder why. If AI is going to increase human productivity, it has to be
able to solve complex tasks. Copilot is great, but it's just a faster horse: I
can write the same code, but faster. An agent that can write code _on my behalf_
lets me move up the value chain and do more worthwhile things. If LLM agents can
work in principle, then understanding how to make them work is crucial.

# Necessity

One factor is that LLM agents are born of necessity. If you have the skills and
resources ($$$) to pretrain an LLM, you're not building agents, you're building
something like Yann LeCun's [differentiable neural architecture][lecun], where
the model architecture _is_ the agent, or, at the very least, you're pretraining
an LLM where the shape of the training data tells it is's going to be a
submodule of a larger mind.

[lecun]: https://openreview.net/pdf?id=BZ5a1r-kVsf

If you're just a simple country programmer with an OpenAI API key, you can't
innovate at the model layer, you have to innovate at the API layer. So you build
a cognitive architecture with the LLM as the [central executive][exec]. The
innovation is the architecture: the flow of information and the processes that
build up the prompt, while the LLM itself remains a COTS black box.

[exec]: https://en.wikipedia.org/wiki/Baddeley%27s_model_of_working_memory

# Generality

This was prompted by [this tweet][jdptw] from [J. D. Pressman][jdp] (whom you should
follow).

[jdptw]: https://twitter.com/jd_pressman/status/1732959020800221691
[jdp]: https://twitter.com/jd_pressman

People use ChatGPT and see that a single LLM completion can be incredibly
general: in a single message, ChatGPT can write poetry, it can write code, it
can explain a complex topic, extract LaTeX from a photograph, etc.

And so people think: I want to build an agent that is equally general,
therefore, I should build a general architecture. And they go on to build an
architecture that looks like every diagram from a cognitive science paper: there
is a small number of very large, very general components. There is a long term
memory, a short term memory, a central executive, sensors, effectors (function
calls). It's very general, very abstract, and very underconstrained.

These architectures never work. Compare Copilot: there's a huge software layer
that converts file context and repository context into a prompt, to generate a
completion. It's not general, it doesn't have a general-purpose vector database
for RAG, or a tree of recursively broken-down, priority-weighted subtasks. It's
_specific_ and _overconstrained_. It can't write free verse, but it improves my
programming productivity by like 40%. Because it does _the one thing_.

Maybe LLMs work best as "magic functions calls": performing some narrow,
concrete, specific, overconstrained, supervisable task. Not writing a program
but writing a single function.

# Entropy

A [compelling analogy][analogy] for why LLMs seem to be unable to accomplish
complex tasks: if you have a lamp running at 90°C, then you can have a thousand
such lamps pointing at an object, and it will never reach 100°C.

[analogy]: https://twitter.com/raffareis/status/1731172265222443333

It is well known that LLM-written text has lower entropy than human-written
text. So maybe there's something analogous to a thermodynamic limit, where the
complexity of the LLM completion and the complexity provided by the architecture
taken together are insufficient to reach criticality and get self-sustaining
output.

# Complexity Classes

When you watch the transcripts of LLM agents failing to do a task, it's often
one of three things:

1. The agent starts going in circles, visiting the same goals.
2. The agent gets "lazy", takes a high-level goal and splits it into a subgoal
   that's just "actually do the work", and when it gets to that subgoal it does
   it again, in an infinite recursion of procrastination.
3. The goals or actions are vague and meandering, and rather than looping, the
   agent that wanders around talking to itself, accomplishing nothing.

And these failures remind me of Stephen Wolfram's complexity classes of cellular
automata.



- failure modes
  - looping
  - infinite recursion
  - lights analogy
  - cellular automata
  - humans are class iv intelligences
  - we can go on creating forever
  - llms, within a single completion, seem to be weakly class iv
  - llms, within a cognitive architecture, occupy classes stage iii and stage ii
  - why does it happen?
    - not enough entropy for "criticality"

# Conclusion

I don't have one. There's a number of possibilities that fit the evidence:

1. LLM agents are like perpetual motion machines: forbidden by some
   yet-undiscovered fundamental theorem in comparative psychology.
1. LLMs themselves aren't good enough yet, and the next generation of models
   will work.
1. General architectures are intrinsically doomed because they're too general,
   and developers should focus on narrow areas of applications, with more
   complex software architectures using LLMs in narrow places.
1. Simple architectures (with a single-digit number of discrete modules) are too
   simple, too abstract, too vague and underdetermined. Architectures have to be
   a lot more involved and a lot more complex to show complex behaviours. Less
   elegant diagrams, more like biology.
1. The LLM-classical software boundary is where things break down. Agents need
   to be pretrained in an agent context, with the agent as a whole being a
   single end-to-end differentiable system.

# Bibliography

1. [RepoCoder: Repository-Level Code Completion Through Iterative Retrieval and
   Generation](https://arxiv.org/pdf/2303.12570.pdf)
