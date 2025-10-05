---
title: "Hashcards: A Plain-Text Spaced Repetition System"
summary: Announcing my latest open-source project.
---

[hashcards] is a [spaced repetition][sr] app, along the lines of [Anki] or [Mochi]. Like Anki, it uses [FSRS], the most advanced scheduling algorithm yet, to schedule review.

The thing that makes hashcards unique: it doesn't use a database. Rather, your flashcard collection is just a directory of Markdown files, like so:

```
Cards/
  Math.md
  Chemistry.md
  Astronomy.md
  ...
```

And each file, or "deck", looks like this:

```md
Q: What is the role of synaptic vesicles?
A: They store neurotransmitters for release at the synaptic terminal.

Q: What is a neurite?
A: A projection from a neuron: either an axon or a dendrite.

C: Speech is [produced] in [Broca's] area.

C: Speech is [understood] in [Wernicke's] area.
```

You write flashcards more or less like you'd write ordinary notes, with lightweight markup to denote basic (question/aswer) flashcards, and [cloze deletion][cl] flashcards. Then, to study, you run:

```
$ hashcards drill <path to the cards directory>
```

This opens a web interface on `localhost:8000`, where you can review the flashcards. Your performance and review history is stored in an SQLite database in the same directory as the cards, cards are content-addressed, that is, identified by the hash of their text.

This central design decision yields many benefits: you can edit your flashcards with your editor of choice, store your flashcard collection in a Git repo, track its changes, share it on GitHub with others. You can use scripts to generate flashcards from some source of structured data (e.g. a CSV of English/French vocabulary pairs). You can query and manipulate your collection using standard Unix tools, or programmatically, without having to dig into the internals of some app's database.

Why build a new spaced repetition app? Mostly because I was dissatisfied with both Anki and Mochi. But also, additionally, because my flashcards collection is very important to me, and having it exist either in some remote database, or as an opaque unusable data blob on my computer, doesn't feel good. "Markdown files in a Git repo" gives me a level of ownership that other approaches lack.

The rest of this post explains my frustrations with Anki and Mochi, and how I landed on the design decisions for hashcards.

# Anki

[Anki] was the first SR system I used. It's open source, so it will be around forever; it has a million plugins; it was the first SR system to use [FSRS] for scheduling. It has really rich stats, which I think are mostly useless but are fun to look at. And the [note types][nt] feature is really good: it lets you generate a large number of flashcards automatically from structured data.

The central problem with Anki is that the interface is really bad. This manifests in various ways.

First, it is ugly to look at, particularly the review screen. And this diminishes your enjoyment of what is already an often boring and frustrating process.

Second, doing simple things is hard. A nice feature of Mochi is that when you start the app you go right into review mode. You're drilling flashcards before you even realize it. Anki doesn't have a "study all cards due today", rather, you have to manually go into a deck and click the "Study Now" button. So what I would do is put all my decks under a "Root" deck, and study that. But this is a hack.

And, third: card input uses WYSIWYG editing. So, you're either jumping from the keyboard to the mouse (which increases latency, and makes flashcard creation more frustrating) or you have to remember all these keybindings to do basic things like "make this text a cloze deletion" or "make this TeX math".

Finally, plugins are a double-edged sword. Because having the _option_ to use them is nice, but the experience of _actually_ using most plugins is bad. The whole setup feels rickety, like a house of cards, like at any point you're going to click the wrong thing and it's going to break.

# Mochi

- the good
  - nice interface
  - this is the main selling point
  - markdown input is much better than anki's
- the bad
  - until very recently, when they added beta support for fsrs, the algo was terrible
    - it wasn't even sm2
    - it was just based on multipliers
    - if you click remember, it multiplies the interval by a number >1
    - if you forget, it multiplies the interval by a number between 0 and 1
    - this is terrible
      - my experience of it is that i often felt mochi works well enough for the short term but not well for the long-term
    - especially if you forget a card with a long interval (multiple months)
      - because the interval doesn't reset
      - you're gonna see the card in weeks
      - not tomorrow, which would help reconsolidate that memory
    - the rationale for this was simplicity
      - make it easier for the user to reason about intervals
      - but the whole point of SRS is you don't calculate the intervals yourself
      - the algorithm is entirely transparent to the user
      - the optimality is that the app implements the most advanced (most learning for fewest reviews) algorithm possible
      - and the user just reaps the benefits
    - in general, i think spaced repetition algoriths in general are a bit too optimistic about recall
      - they could be improved by seeing things just a bit more often
  - in addition, there's only two buttons, forgot and remembered, which is not precise enough
  - cloze deletions are too verbose. they take infinity time to type
  - also, mochi has no equivalent of anki's note types, just templates

# etc

- desiderata
  - plain-text
    - enter prompts by writing into texts
      - i came up with this idea because, well, I was doing it myself
      - i would take notes before transferring them into flashcards
      - sometimes i would use shorthands, like square brackets for cloze deletions, and then write a quick python script to translate them into the mochi format
        - so that:
        - ```
          [foo] bar [baz]
          ```
        - becomes:
        - ```
          {% raw %}{{1::foo}} bar {{2::baz}}{% endraw %}
          ```
    - content addressing is like a clever hack: if you had to assign an ID manually to each flashcard it would be a huge pain in the ass
    - the plain-text thing has many "unexpected good side effects", e.g.:
      - scriptability: you can write a script to generate flashcards from some data source (e.g. a CSV), and use a Makefile to tie everything together
        - i do this in my personal deck
  - web interface
    - cli is out of the question because many of my flashcards contain tex
    - i wanted a web interface so could use katex to render the latex blocks
    - and also: i wanted the UI to look beautiful, or at least elegant
  - frictionless

[hashcards]: https://github.com/eudoxia0/hashcards
[sr]: /article/effective-spaced-repetition
[Anki]: https://apps.ankiweb.net/
[Mochi]: https://mochi.cards/
[FSRS]: /article/implementing-fsrs-in-100-lines
[cl]: https://docs.ankiweb.net/editing.html#cloze-deletion
[nt]: https://docs.ankiweb.net/getting-started.html#note-types
