---
title: The Trajectory of Automation
summary: Why the post-scarcity utopia is unlikely.
math: yes
card: the-trajectory-of-automation.webp
card_source: |
    Fragment C of the [Antikythera mechanism][an], showing two circular scales.

    [an]: https://en.wikipedia.org/wiki/Antikythera_mechanism
---

> Live? Our servants can do that for us.
>
> — [_Axël_][axel], [Auguste Villiers de l'Isle-Adam][adam]

The implicit promise of modernity is: we have to work really hard for a few
centuries, but eventually we'll get to the _Star Trek_ future where we've
automated all the drudgery, and all work is meaningful, heroic, and dignifies
the spirit. We will be artists, scholars, captains of the starship _Enterprise_,
etc. There's a few problems with this idea:

1. "Drudgery" and "meaningful work" are in the same complexity class, so the
   technology to automates the former also automates the latter.
2. Our revealed preference seems to be that we prefer solipsistic convenience
   over human relationships.

The rest of this post elaborates the argument.

# Task Complexity and AGI

We've spent the last 250 years building machines to automate one task at a time,
and the result has been overwhelmingly positive, for the simple reason that if a
human can design a machine or an effective procedure to perform some task, then
by definition the task is drudgery. And so the economic incentive to automate
work has led to more human flourishing: over time work becomes safer,
higher-paying, less monotonous, healthier, more intellectually and socially
stimulating.

You can think of it as a graph where the $x$ axis is time and the $y$ axis is
task complexity. Somewhere along the $y$ axis there's a limit: tasks below the
limit are those which we can design machines or programs to perform, tasks above
the limit are too complex, we have tried and [failed][gofai] (and [failed][cyc]
and [failed][soar] and [failed][actr]) to automate them with software. The
industrial revolution, and later the computer revolution, automated most tasks
under this limit:

![A line plot. The X axis is time, ranging from the year 1800 to the year 2100. A vertical dotted line marks the year 2026. The Y axis is qualitative, showing a list of tasks, sorted from top to bottom by increasing complexity. A horizontal dotted line marks the 'Limit of Formalizability'. The region above this line is labeled 'Human-Complete Region'. The region below this line is labeled 'Formalizable Region'. A sigmoid curve, representing technological progress, is drawn starting from just above the origin in the year 1800, to just below the limit of formalizability in 2026.](/assets/content/the-trajectory-of-automation/graph1.png)

The human-complete tasks are those you need a human to perform. Some are
meaningful, like writing essays, some are drudgery, but human-level drudgery,
like life admin.

But note that nowhere in the $y$ axis is there a "Limit of Drudgery": the line
we draw between "good" and "bad" work is not the line nature draws between easy
and hard to automate. The invention of AGI---"highly autonomous systems that
outperform humans at most economically valuable work", as per the [OpenAI
charter][chart]---creates an obvious discontinuity: suddenly we shoot up to 100%
automation, both of the meaningful work and the drudgery:

![A line plot identical to the above, with one addition: the sigmoid curve representing technical progress shoots up almost vertically a few years after 2026, reaching the top of the Y axis.](/assets/content/the-trajectory-of-automation/graph2.png)

And at this point AGI the economic incentives [work against
humanity][uc]. The only niche to escape to is the relational economy, where
having a human face is an intrinsic part of the job description, like being a
podcaster or a human liability crumple zone.

# Solipsism

The second problem has to do with people's preferences. We think of our own work
as meaningful, but we think of most everyone else's work in instrumental terms:
we care about the ends rather than the human means. Thus when selling we
emphasize human qualities like experience, reputation, trust, relationships,
etc., and when buying we want cheap, fast, and convenient. And, all else being
equal, we prefer the transactional and faceless over the human and
relational. Waymo is a thousand times better than Uber, even though it's slower
and more expensive, because there's no human.

The flip side of this is: whatever work you do, most people would rather an
alternative that gave them the same outcome or better[^fn1], without having to
deal with a human person. That is, if you ask everyone on Earth what jobs they'd
like to see automated, and take the union of that, it's everything. There's
nothing left.

We see this with ordinary consumer products and services, where often the
advertising seems premised on you being some kind of stylite who lives in a
little human terrarium, probably in a building called something like "The Soho",
food arrives through a hole in the wall, and you have this sovereign,
solipsistic existence that leaves no physical trace in the world.

We see this, increasingly, with knowledge work. Personally, I think software
engineering is a very meaningful and intellectually-rewarding activity. But
users just want working software. They're not paying me to sit around like a
Greek philosopher pondering the next rewrite of the authentication module. They
don't care about my opinions on the Standard ML module system. They just want
the software. Why deal with some opinionated maniac when, for $20 a month, you
get access to this little machine ghost that writes reams and reams of mostly
working code, tirelessly, instantly?

The revealed preference of many software firms is: let AI write the code, and
software engineers move one level up, to wrangle the AIs and complement the
things they can't do. But this change in job description is not because firms
love employing humans, it's because today's AI models have serious limitations:
no online learning, no mutable long-term memory, and while they can write code
that works, they're not good at organizing and structuring that code. If, in the
future, these limitations are overcome (and trillions of dollars are being spent
on overcoming them), then software engineering as a profession will
disappear.

Mathematicians [are going through this process][dark] now, too, and it's an
[exact parallel][tweet] of what happened to software engineering, only twelve
months later. And if mathematics, the crown jewel, goes, then everything else
goes.

What about art? Art is the _ne plus ultra_ of meaning, so artists should be
safe. But every work of art is two things: there's a concrete thing, a text or a
painting or a song; and there's a constellation of human aspects: who made it,
and why, what madness they were trying to exorcise in making it, what minor
deity breathed the work into them, the discourse around the work, later art
inspired by it, and so on. And these things are separable: you can enjoy a work
of art for its aesthetic value, as a sequence of sensory impressions, and ignore
every other dimension.

Now say that a machine could generate texts or images of equivalent aesthetic
value[^fn2] to your own, instantly and at ~zero cost. How much of your audience
would desert you? Relational jobs have an analogous problem: you might like
being a podcaster, but how much of your audience would prefer an infinite stream
of AI-generated speech on whatever topic they want to hear about?

This is an empirical question, and we can't answer it, because AI is not there,
yet. But we might discover that our preferences are a lot less humanist and a
lot more solipsistic that we want to believe.

# Acknowledgements

Emmy Noether once said that "everything is already in Dedekind". As for me,
everything was already said by [J. D. Pressman][jdp].

# Footnotes

[^fn1]:
    This is a horrendous oversimplification, because I've stuffed all the
    complexity into the notion of "same outcome or better". So let's try it
    again, [_more geometrico_][spinoza] and with unnecessary LaTeX. You want to
    buy a good. You look around, assemble a set of options, sort them by utility
    and pick the winner. And you can think of the utility as like a polynomial:

    $$
    \mathfrak{u}(x) = \sum\limits_{a \in A} \alpha_a x_a
    $$

    Where the a's are the aspects you are weighing (price, shipping speed, trust
    in the brand, how much social status I get from this thing, etc.), the
    variables quantify these aspects, and the alphas are the coefficients. So if
    you're a hypebeast your $\alpha_\text{clout}$ is much higher than your
    $\alpha_\text{price}$, and if you're a neckbeard the $x_\text{clout}$ of a
    busted ThinkPad is higher than that of a MacBook.

    Now, humans are slow, because biology; and because of [Baumol][baumol] we
    get more expensive over time. Computers are fast and cheap. On top of that,
    the pace of improvement in AI is vastly greater than the pace of improvement
    in humans. So the utility of automated production gets pulled up, and the
    utility of human production gets pulled down, and eventually they are so far
    apart that you have to be an extremist with a very high
    $\alpha_\text{human}$ coefficient to choose the human alternative.

[^fn2]:
    I'm being careful to say "texts or images" rather than "art", and "aesthetic
    value" rather than "quality", because you might object that machine-made art
    is not art. And what I'm trying to say is that is doesn't have to be art. It
    can just be a pleasing image, a pleasing text, and so on.

[actr]: https://en.wikipedia.org/wiki/ACT-R
[adam]: https://en.wikipedia.org/wiki/Auguste_Villiers_de_l'Isle-Adam
[axel]: https://en.wikipedia.org/wiki/Ax%C3%ABl
[baumol]: https://en.wikipedia.org/wiki/Baumol_effect
[chart]: https://openai.com/charter/
[cyc]: https://yuxi.ml/cyc
[dark]: https://kirwinhampshire.substack.com/p/the-dark-night-of-mathematics
[gofai]: https://en.wikipedia.org/wiki/GOFAI
[jdp]: https://jdpressman.com/
[soar]: https://en.wikipedia.org/wiki/Soar_(cognitive_architecture)
[spinoza]: https://en.wikipedia.org/wiki/Spinoza%27s_Ethics
[tweet]: https://x.com/qualiascript/status/2090168914445508873
[uc]: /article/no-one-escapes-the-permanent-underclass
