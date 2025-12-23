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

So I started hacking. The [frotz] interpreter runs on the command line and has a
"dumb" interface called `dfrotz`, which takes the ncurses fluff out, and gives
you a very stripped command-line experience. It looks like this:

```
$ dfrotz games/anchor.z8
...
 Outside the Real Estate Office                      day one

ANCHORHEAD
An interactive gothic by Michael S. Gentry

(Type HELP or ABOUT for some useful information.)

Release 5 / Serial number 990206 / Inform v6.15 Library 6/7

Outside the Real Estate Office
A grim little cul-de-sac, tucked away in a corner of the claustrophobic tangle
of narrow, twisting avenues that largely constitute the older portion of
Anchorhead. Like most of the streets in this city, it is ancient, shadowy, and
leads essentially nowhere. The lane ends here at the real estate agent's office,
which lies to the east, and winds its way back toward the center of town to the
west. A narrow, garbage-choked alley opens to the southeast.

>go southeast
 Alley                                               day one

Alley
This narrow aperture between two buildings is nearly blocked with piles of
rotting cardboard boxes and overstuffed garbage cans. Ugly, half-crumbling brick
walls to either side totter oppressively over you. The alley ends here at a
tall, wooden fence.

High up on the wall of the northern building there is a narrow, transom-style
window.
```

It is easy to write a little Python wrapper to drive the interpreter through
`stdin` and `stdout`:

```python
class Interpreter:
    """Manages the dfrotz Z-machine interpreter process."""

    p: Popen

    def __init__(self):
        log("Starting dfrotz.")
        p: Popen = Popen(
            ["dfrotz", "-m", GAME],
            stdin=PIPE,
            stdout=PIPE,
            stderr=PIPE,
        )
        log(f"Started dfrotz with PID={p.pid}.")
        # Set stdout/stderr to non-blocking mode.
        for stream in [p.stdout, p.stderr]:
            assert stream is not None
            fd = stream.fileno()
            flags = fcntl.fcntl(fd, fcntl.F_GETFL)
            fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)
        self.p = p

    def read(self) -> str:
        assert self.p.stdout is not None
        b: bytes | None = self.p.stdout.read()
        if b is not None:
            t: str = b.decode("utf-8")
            return t
        else:
            return ""

    def write(self, t: str) -> None:
        assert self.p.stdin is not None
        self.p.stdin.write(t.encode("utf-8"))
        self.p.stdin.flush()
        # Give the interpreter time to respond. Not ideal!
        time.sleep(0.1)
```

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


# Small Worlds

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
- tangent: transmission?

[hack]: https://luma.com/ycc02hpc
