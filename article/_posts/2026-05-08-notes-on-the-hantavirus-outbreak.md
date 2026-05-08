---
title: Notes on the Hantavirus Outbreak
summary: Cruise ships are bad for the world.
math: yes
card: notes-on-the-hantavirus-outbreak.webp
card_source: |
    TEM micrograph of Sin Nombre virus. Colours altered by me.
---

Right now there's a [cruise ship][ship] parked outside [Cabo Verde][cb] because
of an outbreak of [Andes virus][av]. Yep, [another][dia] cruise ship. I don't
get the appeal. It's like a big open-air [serial passage][sp] experiment: you
get a bunch of old people with failing immune systems in close contact and race
a pathogen through them.

How much should I worry about this? Is this early January of 2020? I tried
asking [Claude][cl] but the [biosecurity filter][bio] kept blocking my
queries. The WHO [says][who]:

> Although uncommon, limited human‑to‑human transmission of HPS due to _Andes_
> virus has been reported in community settings involving close and prolonged
> contact. Secondary infections among healthcare workers have been previously
> documented in healthcare facilities, though remain rare.
>
> WHO currently assesses the risk to the global population from this event as
> low [...] WHO advises against the application of any travel or trade
> restrictions based on the current information available on this event.

So, [hantavirus][hanta] is the family. They are carried by rodents and spread by
aerosols. In humans they can cause [hantavirus pulmonary syndrome][hps] (HPS),
which has a case fatality rate (CFR) of between 30 and 60%. Not great! Used to
be these infections were mouse-to-human dead-ends. But [Andes virus][av] (ANDV),
first identified in 1995, is known to spread from human to human.

The last time there was an outbreak was 2018--2019 in [Epuyén][ep],
[Chubut][chu], a town of 1,500 on the lee side of the Andes ([quite
beautiful][streetview]). Described in [this paper][martinez2020]. 34 known
infections and 11 deaths for a CFR of 32%. The $R_0$ was 2.12, reduced to 0.96
after control measures were implemented. Given the small number of cases, there
should be some uncertainty about the $R_0$. But $R_0 > 1$ is the threshold for
sustainable transmission. In this outbreak, the [index case][ic], while
symptomatic, attended a birthday party with 100 other people, and infected five
guests in 90 minutes, who went on to infect more people. The authors write:

> The super-spreading capability of the ANDV Epuyén/18−19 strain shows a
> facility ($R>2$) for sustaining continuous chains of transmission if no control
> measures are enforced.

The [appendix][app] has some interesting stuff on how patients were infected at
the birthday party. A further concern here is the incubation period: Wikipedia
says the incubation period is between one and eight _weeks_. In the Chubut
outbreak, the distribution was:

| Onset      | Number of Cases | % of total |
|------------|-----------------|------------|
| ≤15 days   | 5               | 15%        |
| 16-21 days | 10              | 29%        |
| 22-28 days | 13              | 38%        |
| ≥29 days   | 6               | 18%        |

Which is not good. I don't have more data to draw a nice-looking [CDF][cdf].

Now this all sounds quite bad. Are there reasons to be optimistic?

First, Argentina has had 710 cases of HPS in the period 1995--2008 ([Martínez
2010][martinez2010]) and a further 533 cases in the period 2009--2017 ([Alonso
2019][alonso2019]), and we are all still alive. In the latter period, most of
these cases are from occupational/recreational exposure to rodent feces and only
1.8% of cases are from suspected human-to-human transmission. So, over 1,200
cases and every one of them fizzled out, but for one outbreak which was limited
after successful contact tracing and quarantine.

Second, the virus has left Argentina before: once to [Switzerland][ch] in 2016,
and once to the [United States][us] in 2018. In the second case the patient
"while ill, [traveled] on two commercial domestic flights". And neither export
led to a general outbreak.

What does this add up to? I don't know. On the balance of evidence, I think this
outbreak is more likely than not to fizzle out. In the interest of
accountability, and putting my beliefs on record (which is the only objective
way to judge the accuracy of your mental model) I'm gonna say:

- 70% probability the outbreak ends with fewer than 300 deaths.
- 90% probability the outbreak ends with fewer than 1,000 deaths.

And yet. And yet it feels so much like early COVID, particularly with public
health authorities making very complacent remarks that "it's not that
transmissible, contact tracing will work, quarantine will work". Complacency at
the start, and severity at the end, is exactly why COVID was such a fuckup.

[alonso2019]: https://onlinelibrary.wiley.com/doi/10.1002/jmv.25446
[app]: https://www.nejm.org/doi/suppl/10.1056/NEJMoa2009040/suppl_file/nejmoa2009040_appendix_1.pdf
[av]: https://en.wikipedia.org/wiki/Andes_virus
[bio]: https://support.claude.com/en/articles/12436559-understanding-sonnet-4-5-s-safety-filters
[cb]: https://en.wikipedia.org/wiki/Cape_Verde
[cdf]: https://en.wikipedia.org/wiki/Cumulative_distribution_function
[ch]: https://pmc.ncbi.nlm.nih.gov/articles/PMC6233683/
[chu]: https://en.wikipedia.org/wiki/Chubut_Province
[cl]: https://claude.com/product/overview
[dia]: https://en.wikipedia.org/wiki/COVID-19_pandemic_on_Diamond_Princess
[ep]: https://en.wikipedia.org/wiki/Epuy%C3%A9n
[hanta]: https://en.wikipedia.org/wiki/Hantaviridae
[hps]: https://en.wikipedia.org/wiki/Hantavirus_pulmonary_syndrome
[ic]: https://en.wikipedia.org/wiki/Index_case
[martinez2010]: https://pmc.ncbi.nlm.nih.gov/articles/PMC3294556/
[martinez2020]: https://www.nejm.org/doi/full/10.1056/NEJMoa2009040
[ship]: https://en.wikipedia.org/wiki/MV_Hondius_hantavirus_outbreak
[sp]: https://en.wikipedia.org/wiki/Serial_passage
[streetview]: https://maps.app.goo.gl/pFcZf1F14gqQNwpt8
[us]: https://pmc.ncbi.nlm.nih.gov/articles/PMC6193684/
[who]: https://www.who.int/emergencies/disease-outbreak-news/item/2026-DON599
