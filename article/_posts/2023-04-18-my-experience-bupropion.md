---
title: My Experience With Bupropion
summary: Autism, antipsychotics, and the striatum.
card: my-experience-bupropion.jpg
card_source: |
    “Diagram of a fission reactor, Paul Klee, 1927, watercolor, auctioned by Christie's”, DALL-E, June 2022.
math: yes
---

This post is about how I realized I have ADHD, what my symptoms were, and how I
managed it with Bupropion.

Let me prefix this by saying that none of this is medical advice: I'm just an
autistic software engineer with a lot of free time. If you have mental health
problems, for the love of God just talk to a psychiatrist. I promise it's not
bad. Psychiatry is good, antidepressants aren't happy pills that zombify you,
and the pharmaceutical industry has saved countless lives. That aside, let's
begin.

# An Epiphany

I have a funny story of how I realized I have ADHD.

In late 2017 I started taking [fluvoxamine][flv] for anxiety. It helped, but
partially, so the psychiatrist added [aripiprazole][ari], an atypical
antipsychotic sometimes used as an [adjuvant][adj] to SSRIs.

[flv]: https://en.wikipedia.org/wiki/Fluvoxamine
[ari]: https://en.wikipedia.org/wiki/Aripiprazole
[adj]: https://en.wikipedia.org/wiki/Adjuvant

I had to quit after a week because of [Parkinsonian tremors][eps] (it turns
out that fluvoxamine inhibits the [cytochrome][cyp3a4] that metabolizes
aripiprazole, so I was effectively getting a higher dose). But that was the most
productive week I'd had in ages. After _years_ of burnout, I had the
mental energy and motivation to _do things_. I resumed work on my [personal
projects][gh]. I stopped procrastinating. I was studying things that interest
me. I felt like the person I am.

[eps]: https://en.wikipedia.org/wiki/Extrapyramidal_symptoms
[cyp3a4]: https://en.wikipedia.org/wiki/CYP3A4
[gh]: https://github.com/eudoxia0

I told this to three psychiatrists but none of them seemed to think it was
meaningful. One of them offered to restart me on aripiprazole, but I didn't want
to risk [tardive dyskinesia][td]. So I went back to being a lazy bum.

[td]: https://en.wikipedia.org/wiki/Tardive_dyskinesia

Years later, while going down an autistic special interest rabbithole, I got
interested in pharmacology. I bought a copy of [_Molecular
Neuropharmacology_][molneuro]. And I didn't read it (because I didn't have the
executive function) but I did leaf through it. And in the first chapter I read
about how [partial agonists][pagonist] work.

[molneuro]: https://neurology.mhmedical.com/book.aspx?bookID=2963
[pagonist]: https://en.wikipedia.org/wiki/Partial_agonist

Briefly: low dopamine activity makes you ADHD, high dopamine activity makes you
schizophrenic. That's why stimulants (which increase dopamine) can cause
psychosis. A partial agonist of dopamine acts as an [agonist][agon] (dopamine ↑)
when dopamine is low, but acts as an [antagonist][antagon] (dopamine ↓) when
dopamine is high. In short: partial agonists are
[stabilizers][stab]. Aripiprazole is a partial agonist of dopamine.

[agon]: https://en.wikipedia.org/wiki/Agonist
[antagon]: https://en.wikipedia.org/wiki/Receptor_antagonist
[stab]: https://pubmed.ncbi.nlm.nih.gov/15982997/

And I had an epiphany. And I looked it up: can Aripiprazole be used to treat
ADHD? I found a [case report][case] report of a woman with ADHD who was treated
with aripiprazole, she reported exactly what I experienced, down to the daytime
sleepiness. I found a [clinical trial][trial] where aripiprazole was
successfully used for ADHD.

[case]: https://academic.oup.com/ijnp/article/11/3/439/761618
[trial]: https://pubmed.ncbi.nlm.nih.gov/18759644/

This was 2022. It took me four years to figure it out (it turns out it's hard to
solve problems when you keep getting distracted from them), but who can say
they've diagnosed themselves from first principles after an unusual reaction to
a drug?

And I talked about it with friends who have ADHD, and things began falling into
place.

# Symptoms

For someone who has 500 tabs of [PubMed][pubmed] open at all times it's
embarrassing how long it took me to put things together. I think, partly, it's
because life-long pathologies are hard to notice: we know mental states by
mutual contrast, if you've never been very focused or effective you might not
think of yourself as a particularly unfocused or unproductive person.

[pubmed]: https://pubmed.ncbi.nlm.nih.gov/

Partly, also, because everything in the DSM overlaps: someone who is anhedonic
and has low executive function could be ADHD, or they could also be chronically
depressed, or even schizophrenic[^schizo]. Chronic decision paralysis is an
aspect of ADHD, but also of anxiety. And so on.

Partly because whenever I'd try to tell a psychiatrist about my horrible
problems with finishing projects they'd redirect to some other topic they felt
was more important.

And partly it's because we make physical and mental deficiencies into moral
ones. I've spent vastly more time castigating myself for being undisciplined,
lazy, etc. than I've spent rationally and systematically trying to solve my
problems.

But everyone who has undergone successful psychiatric treatment understands: we
are automata, like the [gears of Babbage][babbage]; we travel in fixed orbits,
as predictable as the orbits of the planets, and the movements of objects in space.

[babbage]: https://en.wikipedia.org/wiki/Analytical_engine

So a lot of these things only became visible as symptoms of ADHD after the fact,
after discussing it with friends who have ADHD and with my psychiatrist:

1. **Autism:** obviously I have ASD (I told the psychiatrist who diagnosed me it
   would be more surprising if I didn't) and some 40% of autistic people have
   ADHD symptoms.

1. **Focus:** I never felt like I was particularly distractible or unfocused,
   mostly because I've unwittingly structured me life around not having to
   focus, using various coping skills, mostly centered around text.

   I've always struggled with paying attention during meetings, but I just
   thought that's because the meetings were pointless. I was very surprised
   when, after initiating Bupropion, I suddenly realized during a work meeting
   that I'd been paying attention for _multiple minutes_ on end without having
   to remember to take notes.

   I've tried meditation time and time again but I felt unable to make progress
   with it, because I'd lose focus within seconds. When I looked for advice on
   how to overcome this, people online would say: "just observe your
   thoughts". But the whole problem is that the thoughts surface on their own,
   and carry me away before I can notice them, and it's frustrating to resume
   meditating when you realize you've spent the past five minutes writing code
   in your head.

1. **Physical Restlessness:** my default mode of thinking is to pace the room
   endlessly, which I'd chalked up to [autistic stimming][pacing][^pacing]. When
   seated I'll drum my feet, if my legs are crossed I'll often shake the foot
   that's off the ground.

1. **Low Self-Control:** for years on end, every day I'd tell myself, "today
   we're going to sleep at a reasonable hour". The alarm would go off, and I'd
   find some excuse---I have to finish this Wikipedia rabbithole, or finish this
   video, or read one more paper---to stay up, and go to sleep somewhere between
   1 and 5 am. This was a big problem. Unfortunately the Bupropion doesn't
   exactly help here because the twice-daily dosing schedule gives me a bit of
   insomnia.

   Wikipedia in particular is like a time machine. I go to the front page,
   intending to look something up, click on the featured article---and come to
   forty five minutes later with twenty open tabs, somehow having gone from
   "[_Enceladus_][wiki1]" to "[_List of Allied bombings raids on Wilhelmshaven
   in World War II_][wiki1]". My life is like Memento: I wake up with no
   recollection of who I am but instead of tattoos I have all these PDFs I'll
   never read.

1. **Productivity:** [Austral][austral] took years to finish. Years. So much so
   that at a certain point I became embarrassed to bring it up, because I knew
   that when I did, people thought, "oh, yeah, that's Fernando's idée fixe that
   he keeps saying he'll finish". It took so long because I'd start working on
   it, and run out of steam, and leave it untouched for months and months on
   end. The [commit log][log] reflects this: tall, narrow peaks of
   [hyperfocus][hf] separated by long stretches of inactivity.

   [_The Epiphany of Gliese 581_][eog581] is barely a novella at 26k words. It
   took me a year to finish. I worked on it by accumulating [fragments][frag]
   that came to me spontaneously, then growing text around them like geological
   strata, until the fragments began to touch in time. And it shows, in the
   structure: the frequent `<hr>` breaks are simply where I joined too fragments
   without connecting tissue. The idea of sitting down and writing it down,
   linearly, chapter by chapter and paragraph by paragraph was beyond
   ridiculous. I could hardly sit down to get three paragraphs out of my head at
   a time.

   Sometimes I'd get a burst of executive function, and I'd be able to work on a
   project consistently for a weekend or even a week or two. And I'd rack my
   brain to figure out: what did I do differently, and how do I extend this? Is
   it my diet? Have I accidentally fixed some persistent nutritional deficiency?
   Is it my caffeine dosing schedule? Should I do 150mg of caffeine in the
   morning and a cup of tea in the afternoon? What's the clearance rate of
   sugar-free Monster? Maybe I can supplement [L-tyrosine][tyr] since it's a
   dopamine precursor. I'd try to twist every knob: caffeine intake, sun
   exposure, cardio, lifting, micronutrients, etc. And nothing worked
   consistently.

   It made it extremely difficult to plan for the future, because I had no idea
   when or how long I could work on something.

1. **Chronic Underachievement:** I start an ambitious project, and it withers
   away. I dropped out of college. I start reading a book, put it away and
   forget it. The years pass, the projects remain unfinished. This is a great
   source of pain, because I'm a creative person, and if I can't create it's
   painful.

1. **Engineering Methodology:** I suppose this is somewhat self-serving, but the
   north star of all of my engineering work is complexity. I always insist on
   simplifying, keeping moving parts to a minimum, ensuring the whole system is
   small enough that it fits in my head so I can reason about it.

   I designed [Austral][austral] to be a programming language that I would enjoy
   using. Now I realize that the _entire_ language is designed as an assistive
   technology for a programmer with ADHD.

   The core language principle is simplicity. Everything is as explicit as
   possible, and implicit behaviour (like type conversions and exception
   propagation) are thrown out: this helps because everything you need to reason
   about the code is _right there on the page_. It places minimal demands on
   working memory. Writing code is, effectively, about serializing your working
   memory, and reading code is about restoring that serialization. If I get
   distracted, it doesn't matter, because I can just read the code again to
   restore context.

   There is a whole list of [_anti_-features][antifeatures], things I threw away
   explicitly to reduce cognitive overhead. The linear type system is designed
   to be based on a small core set of simple rules that [fit on a
   page][linearrules]. The module system, with the separation of module
   interfaces and bodies, is designed to minimize how much I have to read and
   hold in working memory in order to work. The language's features are designed
   to be overwhelmingly orthogonal. I threw out async because async + linear
   types is hard to reason about. I threw out exceptions because exceptions are
   hard to reason about.

[austral]: https://github.com/austral/austral
[antifeatures]: https://austral-lang.org/features
[linearrules]: https://austral-lang.org/spec/spec.html#linearity
[pacing]: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6061115/
[wiki1]: https://en.wikipedia.org/wiki/Enceladus
[wiki2]: https://en.wikipedia.org/wiki/Bombing_of_Wilhelmshaven_in_World_War_II
[tyr]: https://en.wikipedia.org/wiki/Tyrosine
[frag]: https://twitter.com/search?q=from%3Azetalyrae%20fiction%20fragment&src=typed_query&f=live
[log]: https://github.com/austral/austral/graphs/contributors
[hf]: https://en.wikipedia.org/wiki/Hyperfocus

# What Didn't Work

I resent when people say to stop taking pills because the solution to [insert
mental health] problem is exercise. First, because I can outlift everyone who
tells me that, and secondly because I feel that it confuses cause and effect. If
someone has crippling depression, and they start a rigorous exercise routine,
that might just mean they've spontaneously entered remission. So if your anxiety
or whatever can be handled by exercise and drinking a mango smoothie: good for
you. Please don't tell me about it.

Here's the things that didn't help with my ADHD: todo lists, the [X
effect][xeffect], Vipassana meditation, lifting, cardio, methylated vitamin B12,
vitamin D, magnesium, zinc, keto, carnivore, vegetarian, generic talk
therapy. More generally: pharmacology is the only thing that's even changed
anything for me, and the "classical" or non-pharmacological approaches only
become useful _with_ pharmacological assistance.

[xeffect]: https://www.reddit.com/r/theXeffect/

It was only after I started taking Bupropion that I was able to use a todo list
consistently, that I was able to make [spaced repetition][sr] into a daily
habit, that I was able to build up productive habits and practices.

[sr]: /article/effective-spaced-repetition

My mental model of this is: when your reward function is just $f(x) = 0$,
putting effort on a productive task gives you as much reward as putting no
effort at all, or procrastinating on some instant-gratification Skinner
box. Habit formation becomes impossible. You might manage to use a todo list
consistently for a week, or two weeks, but eventually something happens that
breaks that streak, and you find that despite repeated effort, you have not
formed a habit. The associations in your brain simply _are not there_.

# Bupropion

[Bupropion][bup], also known as Wellbutrin, is an antidepressant. And an aid in
quitting smoking. And a treatment for ADHD.

[bup]: https://en.wikipedia.org/wiki/Bupropion

Chemically, it's a substituted amphetamine; in terms of its activity on
receptors it's an [NDRI][ndri], a norepinephrine-dopamine reuptake inhibitor. It
binds to [NET][net] and [DAT][dat], effectively increasing the activity of
norepinephine and dopamine. And for this reason, it has a stimulant effect.

[ndri]: https://en.wikipedia.org/wiki/Norepinephrine%E2%80%93dopamine_reuptake_inhibitor
[net]: https://en.wikipedia.org/wiki/Norepinephrine_transporter
[dat]: https://en.wikipedia.org/wiki/Dopamine_transporter#cite_ref-3

The reason it's classed as an antidepressant rather than a stimulant is the
stimulant effect is very mild (which is why actual stimulants are the first
choice in ADHD treatment) and it's not a euphoriant. People rarely [try to abuse
it][abuse], with horrible results, but it's more likely to kill you than get you
high.

[abuse]: https://www.cambridge.org/core/services/aop-cambridge-core/content/view/DA1F5F2146346669684891102EA5141F/S1481803500012203a.pdf/seizures-induced-by-recreational-abuse-of-bupropion-tablets-via-nasal-insufflation.pdf

There are three formulations:

1. Immediate release (IR)
2. Sustained release (SR)
3. Extended release (XR or XL)

SR is typically two 150mg pills taken eight hours apart, XL is usually one 300mg
pill taken once daily. SR is safer than IR and XL is safer than SR, because the
more extended the release, the lower the peak concentration, and the effects of
Bupropion are proportional to its plasma concentration.

I suppose most doctors would prescribe the XL by default for this reason. In
Australia, however, SR is the only version available[^aus], so I had to take
300mg twice daily. This isn't great more sleep (more on this later) but 300mg is
more effective than 150mg.

# Effects

Since starting Bupropion I've observed the following positive changes:

1. **Anxiety:** it didn't eliminate anxiety, but it did reduce it by about
   20%. This is surprising, since for some people Bupropion has the opposite
   effect and makes them more anxious.

1. **Intrusive Thoughts:** these were _vastly_ reduced, along with negative
   self-talk.

1. **Low Mood:** before starting Bupropion, I occasionally had days where I was
   in a very low mood. Since starting Bupropion, I've not had what I would call
   a bad day, where I feel terrible and just want to stay in bed. This is
   distinct from anxiety, which I still struggle with despite the Bupropion: I
   still have moments of blindingly acute anxiety, but not low mood or
   depression. Overall most of the time I feel optimistic.

1. **Productivity:** Bupropion increased my productivity by about 10%-30%. The
   effect is fairly muted: I don't _feel_ more energetic, but empirically I can
   look at my output and see that it has increased. Occasionally, I am able to
   dedicate an entire day to hyperfocusing on a problem, most of the time, the
   benefit is I can simply _start_ something and do a few hours of work.

   Bupropion gave me enough executive function to overcome the worst of ADHD and
   to allow me to "bootstrap" productivity. Since starting Bupropion, I:

   1. Started using a [todo list][todoist] daily and effectively.
   1. Became able to consciously form good habits, including doing [spaced
      repetition][sr] daily.
   1. Became much better at keeping on top of chores, including keeping the
      apartment clean.

   So it's not superhuman productivity, but I can make progress on projects and
   stay on top of chores. Doing creative work no longer feels like I'm in a
   hostage negotiation with my own brain.

   There are still things I procrastinate on. Correspondence is something I
   struggle a lot with, I'm ashamed to think of all the emails in my archive
   where people have reached out to me and I procrastinated on replying to them
   until it became too embarrassing to even reply apologetically. But I now have
   the tools to at least improve things.

   Interestingly the effects were not immediate, but gradual, since doubling to
   300mg daily I felt like productivity increasing like a gentle ramp-up across
   weeks and even months. It was like unlocking skills in a video game: I first
   unlocked the todo list, then the calendar, then the [Pomodoro timer][pomo],
   etc.

1. **Anhedonia:** before Bupropion, I could easily experience negative emotion
   (mostly anxiety) but positive emotion was very muted. My emotional range
   topped out at "content". Formerly pleasurable activities such as reading,
   playing videogames, watching movies brought me little to now joy. With
   Bupropion I can enjoy things a bit more (even though all movies are bad).

1. **Accomplishments:** after years of on-and-off work, I managed to finish
   [Austral][introaustral][^finish]. After a year of struggling to put a single
   paragraph down on the page, I finished [_The Epiphany of Gliese
   581_][eog581]. It was still difficult, still time consuming, but I had enough
   help to manage. The work paid off. I _felt_ that I succeeded: my reward
   function was non-zero.

   There's other empirical evidence that my productivity increased. For
   example: this blog. I asked GPT-4 to write a script to make a histogram of
   posts per year for this blog:

   ![Histogram of posts per year for this blog, showing a marked increase from mid 2022 to Q1 2023](/assets/content/my-experience-bupropion/posts-per-year.png)

   2020 was a bad year. Note (if you're from the future) that 2023 covers only
   Q1.

[todoist]: https://todoist.com/
[introaustral]: /article/introducing-austral
[eog581]: /fiction/eog581/
[pomo]: https://pomofocus.io/

# Side Effects

Some side effects I experienced:

- **Rash:** this appears to be a common problem, judging from r/Bupropion search
  results. Within two weeks of taking Bupropion, I felt itchy and saw the skin
  around my neck was erythematous. It didn't feel life threatening but it did
  look consistent with an allergic reaction. So, acting on medical advice, I
  discontinued Bupropion, took [desloratadine][des] for a month, and then tried
  starting the Bupropion again. The rash didn't come back, so it couldn't have
  been an allergic reaction.
- **Appetite:** the first day I took it I forgot to eat until I almost went to
  bed. This went away within a few days. Bupropion is a known [anorectic][ano].
- **Insomnia:** this was strongest the first few days I took it (150mg once
  daily) and quickly subsided. But when I moved to taking it twice daily the
  insomnia became more of a problem. Because the spacing between the doses is so
  long (8h), I'd often forget to take the second dose on time, so I'd take it
  later, which means I'd go to sleep later, wake up later, take the next first
  dose later, and so it's a vicious cycle of sleep schedule drift.

[des]: https://medlineplus.gov/druginfo/meds/a602002.html
[ano]: https://en.wikipedia.org/wiki/Anorectic

There are known side effects which I did not experience but merit a mention:

- **Seizure:** the headlining risk of Bupropion is seizures. Bupropion lowers
  the seizure threshold, if you have a history of seizures, this is worth
  mentioning to the doctor. My read on this is that, early on, Bupropion was
  prescribed in much higher doses (500 or 600mg or higher) and in immediate
  release formulations, which led to many instances of seizures. Nowadays dosage
  stops at 400mg/day, and is typically just 300mg/day, and overwhelmingly in the
  form of sustained release or extended release formulations, which have lower
  peak concentrations. So modern formulations and doses are much safer. More on
  this below.
- **Worsening Depression:** some people report that Bupropion worsens their
  mood. I know at least two people who reported it made them feel more
  depressed.
- **Psychosis:** I don't know the incidence but, as with stimulants, sometimes
  this happens.
- **Alcohol:** this is a known bad interaction with Bupropion since it further
  lowers the seizure threshold. I just avoided drinking.

# See Also

- [Bupropion on MedlinePlus][medline]. Searching anything medical on Google
  turns up nothing but spam, I like MedlinePlus because it's accurate and
  there's no SEO bullshit.
- [Robert Wiblin][wiblin]'s account about [his experience with
  Bupropion][wiblinbup].
- Scott Alexander's practice website has [a page][lorien] on Bupropion.
- Zimmerman et. al. 2005, [_Why isn't bupropion the most frequently prescribed antidepressant?_][zimmerman2005].
- Stahl et. al. 2004, [_A Review of the Neuropharmacology of Bupropion, a Dual Norepinephrine and Dopamine Reuptake Inhibitor_][stahl2004].

[medline]: https://medlineplus.gov/druginfo/meds/a695033.html
[wiblin]: https://www.robwiblin.com/
[wiblinbup]: https://docs.google.com/document/d/1niiV8I4cgk_xZ1Blou15ImPmqXU4eb_li9eRVp5NgYo/edit
[lorien]: https://lorienpsych.com/2020/10/25/wellbutrin/
[zimmerman2005]: https://pubmed.ncbi.nlm.nih.gov/15889947/

# Appendix: Data on Seizure Risk

From [_15 Years of Clinical Experience With Bupropion HCl: From Bupropion to Bupropion SR to Bupropion XL_][fava2005] by Fava et. al., 2005:

>An important adverse event associated with bupropion use is seizure. With the
>IR formulation, the rate is 0.4% (4/1000) at doses of 300 to 450 mg/day;
>however, the rate increases substantially at doses above this level. With the
>SR formulation, the rate is 0.1% (1/1000) at the target dose of 300 mg/day.
>The incidence of seizure with bupropion XL has not been evaluated. Bupropion is
>not the only antidepressant associated with seizures. SSRI antidepressants are
>associated with seizure at a similar rate of approximately 0.1%. Certain
>factors may increase the risk of seizure; therefore, prior to the prescription
>of bupropion, patients should be screened for the presence of medical
>comorbidities, clinical situations, or concomitant medications that may lower
>seizure threshold.

[fava2005]: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1163271/

From [_A Review of the Neuropharmacology of Bupropion, a Dual Norepinephrine and Dopamine Reuptake Inhibitor_][stahl2004] by Stahl et. al., 2004:

>An often-debated issue is the incidence of seizure associated with
>antidepressant therapy. Most antidepressant clinical trials report that the
>seizure incidence ranges from 0.1% to 0.3% for the newer-generation
>antidepressants and up to 1.1% for the TCAs. The spontaneous seizure rate
>reported in the general population is approximately 0.1%. For bupropion, the
>incidence of seizure reported in the product information for the older,
>immediate-release formulation (Wellbutrin) is 0.4% at doses up to 450 mg/day,
>and for Wellbutrin SR and Zyban (also a sustained-release formulation), 0.1%
>for doses up to 300 mg/day. In addition, a recently conducted review by the
>manufacturer of its clinical trials database for the sustained-release
>formulation of bupropion (N = 15,213) showed an overall seizure incidence of
>0.07% at doses up to 400 mg/day. The mechanisms by which antidepressants may
>lower the seizure threshold are largely unknown.

[stahl2004]: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC514842/

From the [FDA][fda]:

[fda]: https://www.accessdata.fda.gov/drugsatfda_docs/label/2009/020358s046s047lbl.pdf

>Dose: At doses of WELLBUTRIN SR up to a dose of 300 mg/day, the incidence of
>seizure is approximately 0.1% (1/1,000) and increases to approximately 0.4%
>(4/1,000) at the maximum recommended dose of 400 mg/day.
>
>Data for the immediate-release formulation of bupropion revealed a seizure
>incidence of approximately 0.4% (i.e., 13 of 3,200 patients followed
>prospectively) in patients treated at doses in a range of 300 to 450
>mg/day. The 450-mg/day upper limit of this dose range is close to the currently
>recommended maximum dose of 400 mg/day for WELLBUTRIN SR Tablets. This seizure
>incidence (0.4%) may exceed that of other marketed antidepressants and
>WELLBUTRIN SR Tablets up to 300 mg/day by as much as 4-fold. This relative risk
>is only an approximate estimate because no direct comparative studies have been
>conducted.
>
>Additional data accumulated for the immediate-release formulation of bupropion
>suggested that the estimated seizure incidence increases almost tenfold between
>450 and 600 mg/day, which is twice the usual adult dose and one and one-half
>the maximum recommended daily dose (400 mg) of WELLBUTRIN SR Tablets. This
>disproportionate increase in seizure incidence with dose incrementation calls
>for caution in dosing.
>
>Data for WELLBUTRIN SR Tablets revealed a seizure incidence of approximately
>0.1% (i.e., 3 of 3,100 patients followed prospectively) in patients treated at
>doses in a range of 100 to 300 mg/day. It is not possible to know if the lower
>seizure incidence observed in this study involving the sustained-release
>formulation of bupropion resulted from the different formulation or the lower
>dose used. However, as noted above, the immediate-release and sustained-release
>formulations are bioequivalent with regard to both rate and extent of
>absorption during steady state (the most pertinent condition to estimating
>seizure incidence), since most observed seizures occur under steady-state
>conditions.
>
>Patient factors: Predisposing factors that may increase the risk of seizure
>with bupropion use include history of head trauma or prior seizure, central
>nervous system (CNS) tumor, the presence of severe hepatic cirrhosis, and
>concomitant medications that lower seizure threshold.
>
>Clinical situations: Circumstances associated with an increased seizure risk
>include, among others, excessive use of alcohol or sedatives (including
>benzodiazepines); addiction to opiates, cocaine, or stimulants; use of
>over-the-counter stimulants and anorectics; and diabetes treated with oral
>hypoglycemics or insulin.
>
>Concomitant medications: Many medications (e.g., antipsychotics,
>antidepressants, theophylline, systemic steroids) are known to lower seizure
>threshold.
>
>Recommendations for Reducing the Risk of Seizure: Retrospective analysis of
>clinical experience gained during the development of bupropion suggests that
>the risk of seizure may be minimized if
>
>- the total daily dose of WELLBUTRIN SR Tablets does not exceed 400 mg,
>
>- the daily dose is administered twice daily, and
>
>- the rate of incrementation of dose is gradual.
>
>- No single dose should exceed 200 mg to avoid high peak concentrations of
>  bupropion and/or its metabolites.

# Footnotes

[^finish]:
    Software projects are only ever asymptotically finished, but I consider that
    by the time I wrote that blogpost it was at least in a state where I could
    show it off to others.

[^pacing]:
    From the linked paper:

    >When overwhelmed by strong emotions, including anxiety and boredom, some
    >people with ASD may exhibit repetitive “stimming” behavior such pacing,
    >flapping or verbally repeating a certain word or phrase.

[^schizo]:
    Amotivation and anhedonia and among the negative symptoms of schizophrenia

[^aus]:
    And strictly speaking any psychiatric use of it is off-label: the only
    licensed used for Bupropion in Australia is smoking cessation.
