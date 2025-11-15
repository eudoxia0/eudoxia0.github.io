---
title: Building a DIY Clozemaster
summary: Replacing Duo the owl with a simple Python script.
card: building-diy-clozemaster.jpg
card_source: |
    [_Catalogo delle Opere Date Finora Alla Lvce Da Gio Battista Piranesi_][link],
    Giovanni Battista Piranesi and Francesco Piranesi, c. 1780.

    [link]: https://commons.wikimedia.org/wiki/File:Giovanni_Battista_Piranesi_and_Francesco_Piranesi_%28authors%29,_Catalogo_delle_Opere_Date_Finora_Alla_Lvce_Da_Gio_Battista_Piranesi,_NGA_62417.jpg
---

I want to learn French. I was doing the Duolingo course, until they
[redesigned][redesign] the whole UI, and now there's five times more
gamification than there is actual learning. It's just become incredibly tedious.

[redesign]: https://blog.duolingo.com/new-duolingo-home-screen-design/

A friend introduced me to [Clozemaster][cm], which is a very simple concept: you
get two sentences, one in English, another in French, and one of the words is a
[Cloze deletion][cloze]. You have to figure out what goes in the blank. You're
testing vocabulary, grammar, and, since the blank can appear in either sentence,
you're testing in both directions. But Clozemaster has an absolutely demonic UX:
the font is this ridiculous, unserious, 8 bit font literally from Super Mario
Bros.; pasted over some tired Bootstrap theme. And the free version is limited
to 30 sentences a day.

[cm]: https://www.clozemaster.com
[cloze]: https://en.wikipedia.org/wiki/Cloze_test

So I looked for ways to build my own language learning flashcards for use with
[Mochi][mochi]. I found some [French frequency lists][frlist], and thought to
use that to learn vocabulary, but vocabulary alone is not useful. Then I found a
[list of English-French sentence pairs][frsen], ranked by frequency; but the
corpus is [OpenSubtitles][os], so the vocabulary is very skewed to movie
dialogue. And then I found [Tatoeba][tatoeba]: an open-source database of
sentences and their translations.

[frlist]: https://en.wiktionary.org/wiki/Wiktionary:Frequency_lists/French
[frsen]: https://frequencylists.blogspot.com/2016/08/5000-french-sentences-sorted-from.htm
[os]: https://www.opensubtitles.org/en/en%20
[mochi]: https://mochi.cards/
[tatoeba]: https://tatoeba.org/en/

The rest of this post is a walkthrough of the Python code I wrote to generate
Cloze flashcards from Tatoeba sentence pairs.

# The Code

So I started by [downloading][download] all the English-French sentence
pairs. The result is a 30 MiB TSV with 344,000 sentence pairs.

[download]: https://tatoeba.org/en/downloads

First things first: a `Pair` class to represent sentence pairs. We'll store both
the original text of each sentence, and a list of the words in each:

```python
@dataclass(frozen=True)
class Pair:
    eng: str
    eng_words: list[str]
    fra: str
    fra_words: list[str]

    def dump(self):
        print(f"\teng={self.eng}")
        print(f"\teng_words={self.eng_words}")
        print(f"\tfra={self.fra}")
        print(f"\tfra_words={self.fra_words}")
```

The `words` function splits sentences along the usual word boundaries. It also
throws out words if they're just numbers. Regexes are arguably overkill here:

```python
WORD_BOUNDARY: re.Pattern[str] = re.compile(r"""[\s,\.!?"]""")


def words(line: str) -> list[str]:
    l = [w.strip() for w in re.split(WORD_BOUNDARY, line) if w.strip()]
    # Skip numbers.
    l = [w for w in l if not w.isdigit()]
    return l
```

Then we parse sentences from the TSV:

```python
FILE: str = "Sentence pairs in English-French - 2023-02-06.tsv"

WORD_LIMIT: int = 10

# List of French sentences to skip.
SKIP_LIST: list[str] = ["Eu cheguei ontem."]

def parse_sentences():
    """
    Parse sentence pairs.
    """
    pairs: list[Pair] = []
    with open(FILE, "r") as stream:
        reader = csv.reader(stream, delimiter="\t")
        for row in reader:
            eng: str = row[1].strip().lower()
            fra: str = row[3].strip().lower()
            if fra in SKIP_LIST:
                continue
            eng_words: list[str] = words(eng)
            fra_words: list[str] = words(fra)
            # Skip long sentences.
            if len(fra_words) > WORD_LIMIT:
                continue
            # Skip if there are no proper words.
            if (not eng_words) or (not fra_words):
                continue
            pair: Pair = Pair(
                eng=eng,
                eng_words=eng_words,
                fra=fra,
                fra_words=fra_words,
            )
            pairs.append(pair)
    print(f"Found {len(pairs):,} sentence pairs.")
    return pairs
```

We lowercase the entire sentence, this ensures that we don't make duplicate
clozes for the same word just because the case is different. We also reject
sentence pairs that are too long. Also, there is at least one sentence in the
TSV that's in Portuguese rather than French, which for some reason hasn't been
removed, so we special-case rejecting it.

To generate Cloze deletions for sentence pairs, the simplest, obvious thing is
to generate a distinct Cloze for every word. But this creates an unmanageable
combinatorial explosion, as well as a lot of repetition.

The Clozemaster [approach][cm-faq], according to their FAQ, is to make only one
Cloze for each sentence: they Cloze out the rarest word in the sentence. We
could separately download a frequency table for English and French, but a
simpler approach (and one that guarantees every word has a frequency) is to
build the frequency map from the collection itself.

[cm-faq]: https://www.clozemaster.com/faq#how-are-the-blanks-in-the-sentences-selected

The `language_frequency_table` function takes a list of sentences (lists of
words) and returns a `Counter` object associating words with the number of times
they appear in the corpus:

```python
def language_frequency_table(sentences: list[list[str]]) -> Counter[str]:
    """
    Given a list of sentences (lists of words), build up a frequency table.
    """
    table: Counter[str] = Counter()
    for sentence in sentences:
        table.update(sentence)
    print(f"\tFound {len(table)} words.")
    first = most_common(table)
    last = least_common(table)
    print(f"\tMost common: '{first}' ({table[first]}).")
    print(f"\tLeast common: '{last}' ({table[last]}).")
    print(f"\tAverage English frequency: {counter_avg(table)}")
    return table


def most_common(c: Counter[str]) -> str:
    return c.most_common(1)[0][0]


def least_common(c: Counter[str]) -> str:
    min_frequency = min(c.values())
    least_common_items = [
        item for item, count in c.items() if count == min_frequency
    ]
    return least_common_items[0]


def counter_avg(c: Counter) -> float:
    total = sum(c.values())
    n = len(c)
    average_frequency = total / n
    return average_frequency
```

We also implement a frequency cutoff: we don't need to learn the very
obscure words, only the top 5000 words from the corpus.

And, in order to avoid juggling frequency values and making numeric comparisons,
we just build a set of the most common words, and test words for membership:

```python
MOST_COMMON_WORDS_CUTOFF: float = 5000


def most_common_words(c: Counter) -> set[str]:
    return set([p[0] for p in c.most_common(MOST_COMMON_WORDS_CUTOFF)])
```

We want the cards to be organized from the simple to more complex. So we sort
them by the average frequency of the words in the French sentence, divided by
sentence length. Shorter and more common flashcards appear first, longer
sentences and rarer words appear later.


```python
def sort_pairs(pairs: list[Pair], fra_freq: Counter[str]) -> list[Pair]:
    """
    Sort pairs from shortest and most common French words. Specifically, we
    sort by the average frequency of the words in the French sentence, divided
    by the length of the sentence, in reverse order.
    """
    return sorted(
        pairs,
        key=lambda p: avg_freq(p.fra_words, fra_freq) / len(p.fra_words),
        reverse=True,
    )


def avg_freq(words: list[str], tbl: Counter[str]) -> float:
    """
    Return the average frequency for the words.
    """
    return sum(tbl[w] for w in words) / len(words)
```

We also remove sentence pairs that have the same text in either French or
English. Otherwise, the cards become non-deterministic: if one sentence maps to
multiple sentences in the other language, the Cloze deletion could have multiple
valid answers. Which is bad for recall.

```python
def remove_duplicates(pairs: list[Pair]) -> list[Pair]:
    result: list[Pair] = []
    seen_eng: set[str] = set()
    seen_fra: set[str] = set()
    skipped: int = 0
    for pair in pairs:
        stripped_eng: str = (
            pair.eng.replace("!", "").replace(".", "").replace(",", "").strip()
        )
        stripped_fra: str = (
            pair.fra.replace("!", "").replace(".", "").replace(",", "").strip()
        )
        if stripped_eng in seen_eng:
            skipped += 1
        elif stripped_fra in seen_fra:
            skipped += 1
        else:
            result.append(pair)
            seen_eng.add(stripped_eng)
            seen_fra.add(stripped_fra)
    print(f"Skipped {skipped} sentence pairs that had the same text.")
    return pairs
```

Finally, we can create the flashcards. The basic loop is:

1. Iterate over every pair.
2. Find the rarest word in the English sentence.
3. If it's not been clozed too many times, and if it's in the frequency cutoff
   (within the set of common words), make a flashcard for it.
4. Same for the French sentence.

```python
CLOZE_LIMIT: int = 3


@dataclass(frozen=True)
class Cloze:
    eng: str
    fra: str


def minimize(lst, fn):
    """
    Return the value that gives the smallest value of f.
    """
    assert len(lst) > 0
    smallest_index: int = 0
    smallest_value: float = float("inf")
    for (idx, elem) in enumerate(lst):
        val: float = fn(elem)
        if val < smallest_value:
            smallest_index = idx
            smallest_value = val
    return lst[smallest_index]


def build_clozes(
    pairs: list[Pair],
    eng_freq: Counter[str],
    fra_freq: Counter[str],
    eng_common: set[str],
    fra_common: set[str],
) -> list[Cloze]:
    clozes: list[Cloze] = []
    # Track how many times we've made a cloze for each word. We don't need too
    # many clozes per word.
    cloze_count_fra: Counter[str] = Counter()
    cloze_count_eng: Counter[str] = Counter()
    skipped_limit: int = 0
    skipped_freq: int = 0
    for pair in pairs:
        # Find the rarest words in English and French.
        rarest_eng: str = minimize(pair.eng_words, lambda w: eng_freq[w])
        rarest_fra: str = minimize(pair.fra_words, lambda w: fra_freq[w])
        # Cloze the English word.
        if cloze_count_eng[rarest_eng] == CLOZE_LIMIT:
            skipped_limit += 1
        elif rarest_eng not in eng_common:
            skipped_freq += 1
        else:
            cloze_eng: Cloze = Cloze(
                eng=pair.eng.replace(rarest_eng, "{{ "{{" }}c::" + rarest_eng + "}}"),
                fra=pair.fra,
            )
            clozes.append(cloze_eng)
            cloze_count_eng.update({rarest_eng: 1})
        # Cloze the French word.
        if cloze_count_fra[rarest_fra] == CLOZE_LIMIT:
            skipped_limit += 1
        elif rarest_fra not in fra_common:
            skipped_freq += 1
        else:
            cloze_fra: Cloze = Cloze(
                eng=pair.eng,
                fra=pair.fra.replace(rarest_fra, "{{ "{{" }}c::" + rarest_fra + "}}"),
            )
            clozes.append(cloze_fra)
            cloze_count_fra.update({rarest_fra: 1})
    print(
        f"Skipped {skipped_limit} clozes because the word appeared too many "
        "times."
    )
    print(
        f"Skipped {skipped_freq} clozes because the word was under the "
        "frequency cutoff."
    )
    return clozes
```

When dumping the clozes, we write them into multiple CSV files of at most 100
flashcards each. These are analogous to units in a language learning
course. This makes it easier to import them into separate Mochi decks. To make
the text more natural, and since we lowercased it during the parsing step, we
call `.capitalize()` to uppercase the first character of each sentence. Nouns
like "Canada" or "Christmas" stay lowercased but that's a small inconvenience.

```python
def dump_clozes(clozes: list[Cloze]):
    print(f"Compiled {len(clozes)} clozes.")
    # Group sentences into units of 100 each.
    units: list[list[Cloze]] = group(clozes, 100)
    print(f"Dumping {len(units)} units.")
    for (unit_id, unit) in enumerate(units):
        with open(f"output/unit_{unit_id}.csv", "w") as stream:
            writer = csv.writer(
                stream,
                delimiter=",",
                quotechar='"',
                quoting=csv.QUOTE_ALL,
                lineterminator="\n",
            )
            writer.writerow(["English", "French"])
            for cloze in unit:
                writer.writerow(
                    [cloze.eng.capitalize(), cloze.fra.capitalize()]
                )


def group(lst, n):
    result = []
    for i in range(0, len(lst), n):
        result.append(lst[i : i + n])
    return result
```

Tying it together:

```python
def main():
    # Parse sentence pairs.
    pairs: list[Pair] = parse_sentences()
    # Building frequency table.
    print("English frequency table:")
    eng_freq: Counter[str] = language_frequency_table(
        [pair.eng_words for pair in pairs]
    )
    print("French frequency table:")
    fra_freq: Counter[str] = language_frequency_table(
        [pair.fra_words for pair in pairs]
    )
    # Find the frequency cutoff.
    eng_common = most_common_words(eng_freq)
    fra_common = most_common_words(fra_freq)
    print("Sorting...")
    pairs = sort_pairs(pairs, fra_freq)
    pairs = remove_duplicates(pairs)
    # Print first and last sentences.
    print("First sentence:")
    pairs[0].dump()
    print("Last sentence:")
    pairs[-1].dump()
    # Build clozes.
    clozes: list[Cloze] = build_clozes(
        pairs, eng_freq, fra_freq, eng_common, fra_common
    )
    dump_clozes(clozes)


if __name__ == "__main__":
    main()
```

Running the script takes six seconds on my laptop, and prints these diagnostics:

```
Found 304,304 sentence pairs.
English frequency table:
	Found 25685 words.
	Most common: 'i' (67167).
	Least common: 'disappoints' (1).
	Average English frequency: 67.66474596067744
French frequency table:
	Found 43845 words.
	Most common: 'je' (57774).
	Least common: 'astrophysicien' (1).
	Average English frequency: 40.90099213137188
Sorting...
Skipped 96278 sentence pairs that had the same text.
First sentence:
	eng=i am!
	eng_words=['i', 'am']
	fra=je suis !
	fra_words=['je', 'suis']
Last sentence:
	eng=impudent strumpet!
	eng_words=['impudent', 'strumpet']
	fra=impudente courtisane !
	fra_words=['impudente', 'courtisane']
Skipped 412548 clozes because the word appeared too many times.
Skipped 166299 clozes because the word was under the frequency cutoff.
Compiled 29761 clozes.
Dumping 298 units.
```

Here's sample text from Unit 0:

```csv
"I'm {{ "{{" }}c::staying}}.","Je reste."
"I'm staying.","Je {{ "{{" }}c::reste}}."
"I {{ "{{" }}c::pray}}.","Je prie."
"I pray.","Je {{ "{{" }}c::prie}}."
"I am {{ "{{" }}c::working}}.","Je travaille."
"I am working.","Je {{ "{{" }}c::travaille}}."
"I {{ "{{" }}c::work}}.","Je travaille."
"I work.","Je {{ "{{" }}c::travaille}}."
"I'm {{ "{{" }}c::working}}.","Je travaille."
"I'm working.","Je {{ "{{" }}c::travaille}}."
```

And from Unit 132:

```csv
"The sun is {{ "{{" }}c::shining}}.","Le soleil brille."
"The sun is shining.","Le soleil {{ "{{" }}c::brille}}."
"They don't have a car.","Elles {{ "{{" }}c::n'ont}} pas de voiture."
"Let me {{ "{{" }}c::repair}} it.","Laissez-moi le réparer."
"{{ "{{" }}c::hats}} off to him!","Chapeau à lui !"
"I don't live in {{ "{{" }}c::finland}}.","Je ne vis pas en finlande."
"I didn't feel like waiting.","Je n'avais pas envie {{ "{{" }}c::d'attendre}}."
"Fire {{ "{{" }}c::burns}}.","Le feu brûle."
```

The code is in [this repository][repo]. Overall this took about as much time as
it takes to go through three units of the new Duolingo, but it was a lot less
tiresome.

[repo]: https://github.com/eudoxia0/diy-clozemaster

# Prior Art

[Kevin Sookocheff][kev] did the same thing years ago, but for Anki: [_Bulk
Generating Cloze Deletions for Learning a Language with Anki_][post]. His
approach is arguably more rigorous, and uses [Amazon Polly][polly] to generate
sound samples. His code has a feature to specifically not make Cloze cards for
proper nouns, reading this I realized I was doubly-counting differently-cased
words and went back and added those `.lower()` calls in `parse_sentences`.

[kev]: https://sookocheff.com/
[post]: https://sookocheff.com/post/language/bulk-generating-cloze-deletions-for-learning-a-language-with-anki
[polly]: https://aws.amazon.com/polly/
