---
title: "Relativistic Propulsion"
---

# Project Valkyrie

The Project Valkyrie spacecraft was designed by Charles Pellegrino and James
Powell, and first presented in the book Flying to Valhalla. A more technicaly
description, similar to that in the book's afterword, was presented by
Pellegrino in the essay *"Relativistic robots and the feasibility of
interstellar flight"*[^relrob].

In it, Pellegrino makes this claim:

>The Valkyrie, as our rocket is called, will have a maximum cruising speed of 92
>percent lightspeed

I have yet to see a detailed explanation of how this number was reached. The
design of the Valkyrie, at least in the abstract, is certainly very promising,
but I wouldn't make any specific numerical claims.

I've bought *Flying to Valhalla*. It's one of the few books I've actually
bought, and while I wouldn't say it's very good in its specifics, the general
ideas make it a worthwhile read.

## Design Problems

Possibly the most obvious problem is the violation of the Golden Rule: The ship
has two engines, one at the front and one at the back, because turning broadside
while coasting at relativistic speeds opens the ship up to a disastrous impact.

I doubt the risk would be significant, but even if it were -- Even if
interstellar space does turn out to be a constant shower of ice crystals --
there's simply no way to justify doubling the mass of an engine that is already
bound to be massive.

The solution is fairly obvious: Engine, tether, crew and cargo, in that order
from zenith to nadir. The process of turning the engine safely would be:

1. While coasting, and when the engine is off, roll up the tether until the
   cargo is next to the engine.
2. Eject the shielding and its power source, and vernier it a few hundreds of
   meters forward.
3. Rotate the engine/cargo. Most of it would remain within the shadow of the
   shield, but some areas would probably be exposed to impacts (Although this
   depends on the specifics of the shielding).
4. Attach the shielding to the cargo section.
5. Extend the tether.
6. Relight the engine at will.

Note that while the ship was accelerating, the forces were those of the engine
and those of drag from the shielding. As the ship decelerates, drag from the
shielding will push the cargo section closer to the engine. The engine, then,
has to run perpetually and counteract this drag so the tether remains taut.

# Humans are Fragile

## Acceleration

## Radiation

## Life Support

## Are humans worth it?

Seen from a rather extraterrestrial perspective, the idea of sending humans --
canned primates -- across interstellar distances is a completely hubristic
fantasy. At the very least there's a very sharp contrast, between animals that
evolved to live a few decades within a couple kilometers of the tribe, and
incredible machines that can cross the distance between stars.

Solid state is easier. Hook it up to the tether, and it's up, up and
away[^eleitl].

# Propulsion

## Antimatter

## Monopole-Catalyzed Total Conversion

[Magnetic monopoles][mag] are particles that act, as their name implies, as
magnets with a single pole. What makes them interesting, from a practical
perspective, is that under some GUTs, monopoles trigger proton decay[^pdecay] on
contact. The process is essentially the same as pair annihilation, only **the
monopole remains**. Which means it can be reused: Throw a stream of plasma at a
group of magnetically-suspended monopoles, and you have a guaranteed fusion
reactor.

Magnetic monopoles are guaranteed to exist. Every school of modern physics
agrees that they exist. They explain the quantization of electric charge and
certain subtleties about the post-inflation early Universe I'm too simple to
understand.

I would like to provide here an estimation of the luminosity and diameter of an
accelerator capable of producing stable magnetic monopoles, but that's too far
from my grasp. "Circumstellar" seems like a good estimate of size.  As for the
power requirements, I don't think a [civilization][kardashev] that can produce
monopoles has any need for them in energy production. However, they remain
useful for two obvious purposes: Power generation in deep-space operations, and
space propulsion.

Fission and fusion both require particular ingredients, but protons are all the
same, and monopoles don't discriminate.

As an aside: We don't know the exact mass of magnetic monopoles. Estimates vary
from $4 \times 10^4~\mathrm{GeV}$ to $10^{17}~\mathrm{GeV}$[^monmass], the
latter being *"comparable to the mass of a bacterium, or the kinetic energy of a
charging rhinoceros"*[^preskill].

### Difficulties

<div class="sidenote">
  (Aside from, you know, *building a circumstellar supercollider*)
</div>

#### Producing Magnetic Monopoles

"Build a very big accelerator" is incredibly vague. What exactly would it have
to *do* to produce magnetic monopoles? Lindell references a paper[^production]
that proposes using high energy $pp$ (That's proton-proton) collissions to
produce a monopole-antimonopole pair through photon fusion, but the paper is
behind a paywall so I can't comment on that.

Instead I tried to get my hands on two papers that cite it, *Monopolium
production from photon fusion at the Large Hadron Collider*[^monopolium] and
*Looking for magnetic monopoles at LHC with diphoton events*[^lhc].
  
#### Stability

Monopoles, because of their nature, don't have the safety guarantees of nuclear
fusion: A monopole-catalyzed fusion reactor may go supercritical, much like a
fission reactor. Antimatter-catalyzed fusion has this same problem but to a much
smaller degree: Antimatter is annihilated, monopoles remain.

#### Separating Monopoles from Fusion Products

### Monopolar Ramjets

This section is somewhat of an afterthought. In 1985, Robert Zubrin and Dana
Andrews analyzed the Bussard fusion ramjet, and concluded that the drag created
by protons brushing against the magnetic field is greater than the thrust of the
engine -- by nine orders of magnitude. So much drag, in fact, that it inspired
the idea of the [magnetic sail][magsail].

It is unlikely that a fusion ramjet upgraded to use antimatter or
monopole-catalyzed fusion will be able to overcome this gap, but the thought is
left here.

# Design

## Shielding

## Dust

## Radiation

## Tether

# References

[^relrob]:
    *[Relativistic robots and the feasibility of interstellar flight][relrob]*. Charles
    R. Pellegrino.

    The essay has since been taken down and redirects to the related essay
    *These New Oceans*, so a link to the Internet archive is provided.

[^pdecay]:
    *[Searches for Proton Decay and Superheavy Magnetic Monopoles][pdecay]*. B. V. Sreekantan.

    >Rubakov (1981) and independently Callan (1982a, b) have shown that the
    grand unification monopole, when it passes through matter, can induce proton
    decay through reactions of the type $M + p \rightarrow Me^+\pi^0, M\mu^+K^0,
    Me^+\mu^+\mu^-$ etc. The monopole would come out unscathed in the reaction,
    but would cause the break up of the proton in a manner identical to what
    happens in proton decay.

[^eleitl]:
    [Link](http://gnusha.org/logs/2013-01-22.log).

    ```
    07:28 < eleitl> Solid state travels very lightly, especially if you leave
    the drive at home.
    07:28 < eleitl> Up to 0.9 c in mere weeks.
    07:28 < eleitl> Up, up and away.
    ```

[^preskill]:
    *[Magnetic Monopoles][monmass]*. John Preskill.

[^monmass]:
    *[Magnetic Monopole Searches with AMANDA and other detectors][monmass]*.
    Matilda Åberg Lindell.

    The author remarks that "[Monopoles] could never be produced in any man-made
    accelerator, existing or conceivable".

    Here, "conceivable" could mean "reasonable for a sane, non-transhumanist"
    (ie a particle accelerator smaller than a continent) or "physically
    possible" (A particle accelerator the size of the galaxy doesn't count).

[^production]:
    *[On Production of Magnetic Monopoles Via $\gamma\gamma$ Fusion at High Energy $pp$ Collisions][production]*.
    Yu. Kurochkin.

[mag]: http://en.wikipedia.org/wiki/Magnetic_monopole
[kardashev]: http://en.wikipedia.org/wiki/Kardashev_scale
[relrob]: https://web.archive.org/web/20111116172452/http://www.charlespellegrino.com/propulsion.htm
[pdecay]: http://www.ias.ac.in/jarch/jaa/5/251-271.pdf
[preskill]: http://www.theory.caltech.edu/~preskill/pubs/preskill-1984-monopoles.pdf
[monmass]: http://www.diva-portal.org/smash/get/diva2:320548/FULLTEXT02
[production]: http://www.worldscientific.com/doi/abs/10.1142/S0217732306022237
