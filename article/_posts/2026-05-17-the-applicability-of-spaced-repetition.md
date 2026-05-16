---
title: The Applicability of Spaced Repetition
summary: On factual vs. conceptual knowledge.
math: yes
card: the-applicability-of-spaced-repetition.webp
card_source: |
    _La Rivolta_, 1911, by Luigi Russolo.
---

Spaced repetition has a natural domain of applicability: information that is systematically organized as an unambiguous key-value mapping with short keys and values. The "Hello, world!" of flashcards is the [NATO phonetic alphabet][nato]: A → alpha, B → bravo, etc.  Similarly, the periodic table can be thought of as defining a collection of mappings: element name ↔ symbol, element name ↔ atomic number, etc. You can just drill these cards and memorize the facts without a prior step of understanding, or building a conceptual model.

Applying spaced repetition is trivial for this kind of information. That's why most people who use spaced repetition are either language learners or medical students. In biology the main intuition you need is for "3D shapes bumping around in Brownian motion", which comes free with your human brain, and afterwards it's mostly just a lot of facts you have to memorize. Analogously with language: you already have a [language center][lc], you just need to drill vocabulary and grammar.

And the further you go from this domain, the harder it is to apply spaced repetition.

Highly conceptual knowledge, like math, is hard to encode. You have to spend a lot of time just understanding the information, and building a conceptual model in your head, and then you start writing flashcards to solidify that model, like taking tomographic cuts of some complex object. And coming up with questions that make good flashcards (short, unambiguous, etc.) out of this highly abstract knowledge is very hard. Often you have some deceptively simple fact, a simple assertion, but there's no good way to encode it as a flashcard, so you have to encode "around it" by asking questions that assume or require that knowledge (e.g. asking why X is true), and hoping that in drilling those, your brain will remember the actual target.

In general, _relational_ facts are easier to encode, since a binary predicate like $\text{Property}(\text{Object}, \text{Value})$ readily becomes a question. "Caffeine is metabolized by [cytochrome 1A2][1a2]", in [Prolog][prolog], is $\text{Metabolism}(\text{Caffeine}, \text{CYP1A2})$, and becomes "Q: What is the cytochrome that metabolizes caffeine? A: 1A2". But how do you encode stand-alone assertions like "all [unitary matrices][uni] are [invertible][inv]"? You could encode that as a yes-or-no question, but that's useless, because rationally you can expect such questions to be biased towards yes. Both "what is a property of unitary matrices?" and "what kinds of matrices are invertible?" are useless because they have hundreds of possible equally-valid answers, so they're ambiguous. You have to be creative and find all kinds of tricks and stratagems to encode around the knowledge.

Tangentially: this, I think, is why using AI to write flashcards is often misguided. In highly systematized domains, you don't need AI in the first place, because there's nothing for the AI to do except import a CSV into Anki. In domains that are highly conceptual and abstract, you're not memorizing a set of objectively-knowable facts, you're trying to solidify a private, internal mental model that you build by reading and thinking and solving problems. You can give the AI all kinds of _general_ rules on how to write good flashcards, but the AI can't look into your mind and know which facts are salient _for you_, what you already know, which micro-volumes of knowledge can be encoded lightly with just a few flashcards, and which things need more shoring up and consequently more coverage.

Can this situation be improved, or is this just an intrinsic limitation of spaced repetition? I don't know. But it seems reasonable to think some limited gains are possible. I think not a lot of people are using spaced repetition on these more "conceptual" domains, and (by the rule that [most people in a community are lurkers][lurk]) even fewer of those people are writing, in detail, to share their knowledge. [Plenty][a] [of][b] [people][c] have written about how to write good flashcards in general, what I want to read is closer to case studies where someone sits down with a text (or, even better, a textbook) and describes the process by which they turned that text into flashcards, like [this][mike] from Michael Nielsen. From a corpus of similar case studies we might derive general rules for, not how to write effective flashcards, but how to encode complex, conceptual knowledge into question-answer form.

[1a2]: https://en.wikipedia.org/wiki/CYP1A2
[a]: https://www.supermemo.com/en/blog/twenty-rules-of-formulating-knowledge
[b]: https://andymatuschak.org/prompts/
[c]: /article/effective-spaced-repetition
[inv]: https://en.wikipedia.org/wiki/Invertible_matrix
[lc]: https://en.wikipedia.org/wiki/Language_center
[lurk]: https://en.wikipedia.org/wiki/1%25_rule
[mike]: https://cognitivemedium.com/srs-mathematics
[nato]: https://en.wikipedia.org/wiki/NATO_phonetic_alphabet
[prolog]: https://en.wikipedia.org/wiki/Prolog
[uni]: https://en.wikipedia.org/wiki/Unitary_matrix
