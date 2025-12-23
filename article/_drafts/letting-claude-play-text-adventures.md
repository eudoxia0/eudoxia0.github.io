---
title: Letting Claude Play Text Adventures
summary: Experiments in cognitive architecture.
---

Last week I went to an [AI hackathon][hack] organized by my friends Lucia and
Malin. The theme was mech interp, but I hardly know PyTorch so I planned to do
something at the API layer rather than the model layer.

Something I think about a lot is [cognitive architectures][cogarch] (like
[Soar][soar] and [ACT-R][actr]). This is like a continuation of GOFAI research,
inspired by cognitive science. And like GOFAI it's never yielded anything
useful. But I often think: can we scaffold LLMs with cog arch-inspired harnesses
to overcome their limitations?

LLM agents like [Claude Code][cc] are basically "accidental" cognitive
architectures: they are designed and built my practitioners rather than
theorists, but they have commonalities, they all need a way to manage memory,
tool use, a task agenda etc. Maybe building an agent on a more "principled"
foundation, one informed by cognitive science, yields a higher-performing
architecture.

So I sat around a while thinking how to adapt Soar's architecture to an LLM
agent. And I sketched something out, but then I thought: how can I prove this
performs better than baseline? I need an eval, a task.

Math problems? Too one-shottable. A chatbot? Too interactive, I want something
hands-off and long-horizon. A coding agent? That's too freeform and requires too
much tool use. And then I thought: [text adventures][adv]! You have a stylized,
hierarchically-structured world accessible entirely tthrough text, long-term
goals, puzzles, physical exploration and discovery of the environment. Even the
data model of text adventures resembles [frame-based][frame] knowledge
representation systems.

- text adventures
  - found dfrotz interpreter
  - i built a really simple harness
  - haiku wanders around
  - sonnet finishes the first task, then wanders around
  - anchorhead is not the ideal choice for a test enviroment
    - the game is too long
    - the game is very open-world, and from the very start you can wander around
      most of the game map before you are forced to engage with the first puzzle
      of the game
  - needed something smaller
  - claude can make its own games
    - there's no inform7 package for nixos
    - so i had to use inform6
    - asked claude to write small "Portal test chamber" type games
      - a small, single room, escape the room type game, which Claude beat in less than 10 turns
      - then larger, multi-room heist game
        - this one was more fun
        - claude beats it in the normal harness but it takes a while
        - i thought it would be large enough to try a different harness
        - new harness: show only the last five turns, give claude a working memory
        - claude gets caught up in all this bullshit
          - trapped by a red herring: a garden with a well, kept trying to go down the well

[hack]: https://luma.com/ycc02hpc
