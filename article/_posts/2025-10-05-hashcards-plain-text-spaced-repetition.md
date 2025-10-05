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

This opens a web interface on `localhost:8000`, where you can review the flashcards. Your performance and review history is stored in an [SQLite] database in the same directory as the cards, cards are content-addressed, that is, identified by the hash of their text.

This central design decision yields many benefits: you can edit your flashcards with your editor of choice, store your flashcard collection in a [Git] repo, track its changes, share it on [GitHub] with others. You can use scripts to generate flashcards from some source of structured data (e.g. a CSV of English/French vocabulary pairs). You can query and manipulate your collection using standard Unix tools, or programmatically, without having to dig into the internals of some app's database.

Why build a new spaced repetition app? Mostly because I was dissatisfied with both Anki and Mochi. But also, additionally, because my flashcards collection is very important to me, and having it exist either in some remote database, or as an opaque unusable data blob on my computer, doesn't feel good. "Markdown files in a Git repo" gives me a level of ownership that other approaches lack.

The rest of this post explains my frustrations with Anki and Mochi, and how I landed on the design decisions for hashcards.

# Anki

[Anki] was the first SR system I used. It's open source, so it will be around forever; it has a million plugins; it was the first SR system to use [FSRS] for scheduling. It has really rich stats, which I think are mostly useless but are fun to look at. And the [note types][nt] feature is really good: it lets you generate a large number of flashcards automatically from structured data.

The central problem with Anki is that the interface is really bad. This manifests in various ways.

First, it is ugly to look at, particularly the review screen. And this diminishes your enjoyment of what is already an often boring and frustrating process.

Second, doing simple things is hard. A nice feature of Mochi is that when you start the app you go right into review mode. You're drilling flashcards before you even realize it. Anki doesn't have a "study all cards due today", rather, you have to manually go into a deck and click the "Study Now" button. So what I would do is put all my decks under a "Root" deck, and study that. But this is a hack.

And, third: card input uses WYSIWYG editing. So, you're either jumping from the keyboard to the mouse (which increases latency, and makes flashcard creation more frustrating) or you have to remember all these keybindings to do basic things like "make this text a cloze deletion" or "make this [TeX math][tex]".

Finally, plugins are a double-edged sword. Because having the _option_ to use them is nice, but the experience of _actually_ using most plugins is bad. The whole setup feels janky, like a house of cards. Most of the time, if a feature is not built into the app itself, I would rather live without it than use a plugin.

# Mochi

[Mochi] feels like it was built to address the main complaint about Anki: the interface. It is intuitive, good looking, shortcut-rich. No jank. Instead of WYSIWYG, card text is Markdown: this is delightful.

There's a few problems. While Markdown is a very low-friction way to write flashcards, cloze deletions in Mochi are very verbose. In hashcards, you can write this:

```md
Speech is [produced] in [Broca's] area.
```

The equivalent in Mochi is this:

```md
{% raw %}Speech is {{1::produced}} in {{2::Broca's}} area.{% endraw %}
```

This is a lot of typing. And you might object that it's only a few characters longer. But when you're studying from a textbook, or when you're copying words from a vocabulary table, these small frictions add up. If writing flashcards is frustrating, you'll write fewer of them: and that means less knowledge gained. Dually, a system that makes flashcard creation as frictionless as possible means more flashcards, and more knowledge.

Another problem is that Mochi doesn't have an equivalent of Anki's [note types][nt]. For example: you can make a note type for chemical elements, with fields like atomic number, symbol, name, etc., and write templates to generate flashcards asking questions like:

- What is the atomic number of [name]?
- What element has atomic number [number]?
- What is the symbol for [name]?
- What element has symbol [symbol]?

And so on for other properties. This is good. Automation is good. Less work, more flashcards. Mochi doesn't have this feature. It has [templates][mt], but these are not as powerful.

But the biggest problem with Mochi, I think, is the algorithm. Until [very recently][mf], when they added beta support for FSRS, the algorithm used by Mochi was even simpler than [SM-2]. It was based on multiplers: remembering a card multiplies its interval by a number >1, forgetting a card multiplies its interval by a number between 0 and 1.

The supposed rationale for this is simplicity: the user can reason about the algorithm more easily. But I think this is pointless. The whole point of an SR app is the software manages the schedule for you, and the user is completely unaware of how the scheduler works. The optimality is to have the most advanced possible scheduling algorithm (meaning the one that yields the most recall for the least review time) under the most intuitive interface possible, and the user just reaps the benefits.

Obviously without an RCT we can't compare Mochi/[SM-2]/FSRS, but my subjective experience of it is that the algorithm works well for the short-term, and falters on the long-term. It's very bad when you forget a mature card: if a card has an interval of sixty days, and you click forget, you don't reset the interval to one day (which is good, because it helps you reconsolidate the lost knowledge). Rather, the interval is multiplied by the forget multiplier (by default: 0.5) down to _thirty days_. What's the use? If I forgot something after sixty days, I surely won't have better recall in thirty.

You can fix this by setting the forget multiplier to zero. But you have to know this is how it works, and, crucially: I don't want to configure things! I don't want "scheduler parameter finetuning" to be yet another skill I have to acquire: I want the scheduler to _just work_.

(In general, I think spaced repetition algoriths are a too optimistic. I'd rather see cards slightly more often, and spend more time reviewing things, than get stuck in "forgetting hell").

In Anki, it's the interface that's frustrating, but the algorithm works marvelously. In Mochi, the interface is delightful, but it's the algorithm that's frustrating. Because you can spend months and months drilling flashcards, building up your collection, but when the cards cross some invisible age threshold, you start to forget them, and the algorithm does not help you to relearn things you have forgotten. Eventually I burned out on it and stopped doing my reviews, because I expected to forget everything eventually anyhow. And now they added support for FSRS, but by now I have 1700 cards overdue.

Additionally: Mochi has only two buttons, "Forgot" and "Remembered". This is simpler for the user, yes, but most SR scheduling algorithms have more options for a reason: different degrees of recall adjust the card parameters by different magnitudes.

# Hashcards

What do I want from a spaced repetition system?

The first thing is frictionless. I have learned that the biggest bottleneck in spaced repetition, for me, is not doing the reviews (I am very disciplined about this and have done SR reviews daily for months on end), it's not even converting conceptual knowledge into flashcards, the biggest bottleneck is just entering cards into the system.

The surest way to shore up your knowledge of some concept or topic is to write more flashcards about it: asking the same question in different ways, in different directions, from different angles. More volume means you see the same information more often, asking in different ways prevents "memorizing the shape of the card", and it acts as a kind of redundancy: there are multiple edges connecting that bit of knowledge to the rest of your mind.

And there have been many times where I have thought: I would make this more solid by writing another flashcard. But I opted not to because it was too effortful.

If getting cards into the system involves a lot of friction, you write fewer cards. And there's an opportunity cost: the card you don't write is a concept you don't learn. Integrated across time, it's entire oceans of knowledge which are lost.

So: the system should make card entry effortless. This was the guiding principle behind the design of hashcards' text format. For example, cloze deletions use square brackets because in a US keyboard, square brackets can be typed without pressing shift (compare Mochi's curly brace). And it's one bracket, not two. Originally, the format was one line per card, with blank lines separating flashcards, and question-answer cards used slashes to separate the sides, like so:

```markdown
What is the atomic number of carbon? / 6

The atomic number of [carbon] is [6].
```

And this is strictly less friction. But it creates a problem for multi-line flashcards, which are common enough that they should not be a second-class citizen. Eventually, I settled on the current format:

```
Q: What is the atomic number of carbon?
A: 6

C: The atomic number of [carbon] is [6].
```

Which is only slightly more typing, and has the benefit that you can easily visually identify where a card begins and ends, and what kind of card it is. I spent a lot of time arguing back and forth with [Claude] about what the optimal format should be.

Another source of friction is not creating the cards but _editing_ them. The central problem is that your knowledge changes and improves over time. Often textbooks take this approach where Chapter 1 introduces one kind of ontology, and by Chapter 3 they tell you, "actually that was a lie, here's the real ontology of this subject", and then you have to go back and edit the old flashcards to match. Because otherwise you have one card asking, e.g., for the undergraduate definition of some concept, while another asks you for the graduate-level definition, creating ambiguity.

For this reason, when studying from a textbook, I create a deck for the textbook, with sub-decks for each chapter. That makes it easy to match the flashcards to their source material (to ensure they are aligned) and each chapter deck only has a few tens of cards usually, keeping them navigable.

Sometimes you wrote multiple cards for the same concept, so you have to update them all at once. Finding the related ones can be hard if the deck is large. In hashcards, a deck is just a Markdown file. The cards immediately above and below a card are usually semantically related. You just scroll up and down and make the edits in place.

But why plain-text files in a Git repo? Why not use the above format, but in a "normal" app with a database?

The vague idea of a spaced repetition system where flashcards are stored as plain-text files in a Git repo had been kicking around my cranium for a long time. I remember asking an Ankihead on IRC circa 2011 if such a thing existed. At some point I read [Andy Matuschak's note][andy] on his implementation of an SR system. In his system, the flashcards are colocated with prose notes. The notation is similar to mine: `Q` and `A` tags for question-answer cards, and `{curly braces}` for cloze deletions. And the cards are content-addressed: identified by their hash. Which is an obviously good idea. But his code is private and, besides, I feel that prose notes and flashcards are very different beasts, and I don't need or want them to mix.

But I think the idea of plain-text spaced repetition got bumped up the priority queue because I spontaneously started using a workflow that was similar to my current hashcards workflow.

When studying from a textbook or a website, I'd write flashcards in a Markdown file. Usually, I used a shorthad like `[foo]` for cloze deletions. Then I'd use a Python script to transform the shorthand into the `{% raw %}{{1::foo}}{% endraw %}` notation used by Mochi. And I'd edit the flashcards in the file, as my knowledge built up and my sense of what was relevant and important to remember improved. And then, when I was done with the chapter or document or whatever, only then, I would manually import the flashcards into Mochi.

And it struck me that the last step was kind of unnecessary. I was already writing my flashcards as lightly-annotated Markdown in plain-text files. I had [already implemented FSRS][fsrsblog] out of curiosity. I was looking for a personal project to build during funemployment. So hashcards was by then a very neatly-shaped hole that I just needed to paint inside.

It turns out that using plain-text storage has a lot of synergies:

- You can edit the cards using whatever editor you use, build up a library of card-creating macros, and navigate the collection using the editor's file browser.
- You can query and update the collection using standard Unix tools, or a programming language, e.g. using `wc` to get the total number of words in the collection, or using `awk` to make a bulk-update to a set of cards.
- You can use Git for version control. Git is infinitely more featureful than the change-tracking of any SR app: you can edit multiple cards in one commit, branch, merge, use pull requests, etc.
- You can make your flashcards public on GitHub.
- You can generate flashcards using scripts (e.g., turn a CSV of foreign language vocabulary into a deck of flashcards), and write a Makefile to tie the script, data source, and target together. I [do this][makefile] in my personal deck. Anki's [note types][nt] don't have to be built into hashcards, rather, you can DIY it using some Python and make.

The result is a system where creating and editing flashcards is nearly frictionless, that uses an advanced spaced repetition scheduler, and, when drilling cards, provides an elegant (if not beautiful) web interface.

[Anki]: https://apps.ankiweb.net/
[Claude]: https://claude.ai/
[FSRS]: /article/implementing-fsrs-in-100-lines
[GitHub]: https://github.com/
[Git]: https://git-scm.com/
[Mochi]: https://mochi.cards/
[SM-2]: /article/implementing-sm2-in-rust
[SQLite]: https://sqlite.org/
[andy]: https://notes.andymatuschak.org/My_implementation_of_a_personal_mnemonic_medium
[cl]: https://docs.ankiweb.net/editing.html#cloze-deletion
[fsrsblog]: /article/implementing-fsrs-in-100-lines
[hashcards]: https://github.com/eudoxia0/hashcards
[makefile]: https://github.com/eudoxia0/flashcards/blob/87b082e4723e5b1b286e3bb5378316f464cfc28f/Makefile
[mf]: https://x.com/MochiCardsApp/status/1924692507570667630
[mt]: https://mochi.cards/docs/#templates
[nt]: https://docs.ankiweb.net/getting-started.html#note-types
[sr]: /article/effective-spaced-repetition
[tex]: https://docs.ankiweb.net/math.html
