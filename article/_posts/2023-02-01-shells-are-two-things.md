---
title: Shells are Two Things
summary: Command languages and scripting languages are and should be different things.
card: shells-are-two-things.jpg
---

The fundamental problem of shells is they are required to be two things.

1. **A high-frequency REPL**, which requires terseness, short command names,
   little to no syntax, implicit rather than explicit, so as to minimize the duration of REPL cycles.
2. **A programming language**, which requires readable and maintainable syntax,
   static types, modules, visibility, declarations, explicit configuration rather
   than implicit conventions.

And you can't do both. You can't be explicit and implicit, you can't be terse
and readable, you can't be flexible and robust.

Shells optimize the former case, so that you can write `cat beef.txt | grep
"lasagna" | sort -n | uniq` instead of:

```python
with open(Path("beef.txt")) as stream:
    lines = filter(
        stream.readlines(),
        lambda line: re.match(line, "lasagna") is not None
    )
    print(set(reverse(sorted(lines))))
```

Which does not spark joy.

So the programming language aspect suffers: shell scripts are an unreadable
nightmare of stringly-typed code resembling cyphertext.

Of course No True Scotsman would write a large and complex program as a shell
script, but according to this lovely seven-line one-liner:

```bash
find / -type f -exec awk '/^#!.*sh/{print FILENAME}' {} + \
  | xargs file \
  | awk '!/ASCII text/{next} {print}' \
  | cut -d: -f1 \
  | xargs -I {} wc -l {} \
  | sort -n \
  | uniq
```

There are 5,635 shell scripts on my humble Ubuntu box. Of these, 79 are over one
thousand lines of text, the largest being `/usr/share/libtool/configure`[^config],
a 16,394-line shell script the devil wrote[^lines]. In total, there are 726,938
lines of stringly-typed shell script on my machine. This is more than I am
comfortable with.

And the solution is obvious, but hard to implement because preserving backwards
compatibility would require a great deal of elbow grease.

The solution is that we have one tool, but there are two things, and so there
should be two tools. Shells should be terse, fast, interactive, and not too scriptable. Programs should export the terse command-line interface for use in the shell:

```bash
pandoc -t latex \
       -f markdown \
       --pdf-engine=xelatex \
       --table-of-contents \
       --toc-depth=2 \
       --resource-path=. \
       --standalone \
       input.md \
       -o output.pdf
```

And the typed interface, for use in a more scalable programming language:

```
Pandoc(
    input = (Path("input.md"), MD),
    output = (Path("output.pdf"), PDF),
    pdfEngine = XELATEX,
    resourcePath=Path("."),
    completenessMode=STANDALONE,
)
```

And the former can be derived from the latter, because it is a strict weakening
of the typed interface.

The challenge is how to fit this into the POSIX universe where the sole
entrypoint to a program is an array of strings. Historically, operating systems
that have fanciful structured interfaces between programs have been left in the
dust by Unix, because Unix maximizes flexibility by favoring the
lowest-common-denominator interface, which is the string.

# Footnotes

[^config]:
    Here are some of my favorite regions from this file. First there's this
    brainfuck-like thing which I am convinced is a Turing machine implemented in
    `sed`:

    ```
    as_me=`$as_basename -- "$0" ||
    $as_expr X/"$0" : '.*/\([^/][^/]*\)/*$' \| \
        X"$0" : 'X\(//\)$' \| \
        X"$0" : 'X\(/\)' \| . 2>/dev/null ||
    printf "%s\n" X/"$0" |
        sed '/^.*\/\([^/][^/]*\)\/*$/{
            s//\1/
            q
        }
        /^X\/\(\/\/\)$/{
            s//\1/
            q
        }
        /^X\/\(\/\).*/{
            s//\1/
            q
        }
        s/.*/./; q'`
    ```

    Further down there's some 300 lines of C code embedded in a multi-line
    string.

    Lines 11,266 to 11,417 are simply empty. Just a whole page of white. Lines 15,569 to 16,361 are another shell script, embedded a string literal.

[^lines]:
    The fact that the line count is just a hair off 2<sup>14</sup> surely has
    some sinister, esoteric significance.
