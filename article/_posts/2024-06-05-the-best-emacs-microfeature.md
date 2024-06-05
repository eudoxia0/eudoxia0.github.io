---
title: The Best Emacs Microfeature
summary: Why I keep using Emacs in 2024.
card: the-best-emacs-microfeature.jpg
card_source: |
    “Electron beam lithography machine, Paul Klee, 1929, watercolor, auctioned by Christie's”, DALL-E, June 2022.
---

When [Emacs] users explain why they use it, it's usually big picture features:
[elisp], [org-mode], [dired]. The fact that it's been around forever and will
continue to be around for decades to come. For me it's the humble `M-q`, or, in
the vernacular, `Alt+q` or `Option+q`. This is the key combination for
[`fill-paragraph`][fill]. It reshapes a paragraph of text so that it fits under
80 columns.

[fill]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Fill-Commands.html

This is a marvelously useful feature that is sadly absent from many other
editors. I would probably have switched to [Zed] already if it had this, or the
ability to extend the editor with custom buffer-manipulation commands.

If you're writing prose your lines will frequently exceed the width of the
editor. Then you have two choices. You can use word-wrap, which doesn't play
well with indentation for nested content:

![A screenshot of Emacs, showing word wrapping around the 80 column mark. The word wrap does not respect indentation, since lines continue from the leftmost side of the screen.](/assets/content/the-best-emacs-microfeature/wrap.webp)

Or you can `toggle-truncate-lines`, which puts much of the text out of reach:

![A screenshot of Emacs, showing lines truncated at the 80 column mark, which the remainder of the text hidden.](/assets/content/the-best-emacs-microfeature/truncate.webp)

Or you can insert newlines so that the text fits within the column limit, but
doing this by hand is incredibly tedious. Enter `M-q`:

![A screenshot of Emacs, showing paragraphs filled at the 80 column mark, and properly indented.](/assets/content/the-best-emacs-microfeature/fill.webp)

Gorgeous. My only complaint is the ragged edges. Maybe when LLMs are good enough
we can have `semantic-fill-paragraph`, where the text content is rewritten so
that on each line, a word happens to end at the 80th column.

Emacs is full of hundreds of microscopic quality of life features like this,
accreted over decades: [`sort-lines`][sort] does what it says, `M-=` counts the
number of words in the selection, and [`markdown-mode`][markdown-mode] has a
really useful feature for automatically aligning Markdown tables, so you can
very quickly write this:

<img width="60%" style="margin: 0 auto;" src="/assets/content/the-best-emacs-microfeature/before.webp" alt="A screenshot of Emacs showing a Markdown table without alignment." />

And `C-c C-d` turns it into this:

<img width="60%" style="margin: 0 auto;" src="/assets/content/the-best-emacs-microfeature/after.webp" alt="A screenshot of Emacs showing a Markdown table where all columns are aligned with whitespace to the same width." />

And so despite the jank I persist in using Emacs.

[Emacs]: https://www.gnu.org/software/emacs/
[elisp]: https://www.gnu.org/software/emacs/manual/html_node/eintr/
[org-mode]: https://orgmode.org/
[dired]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
[sort]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Sorting.html
[markdown-mode]: https://jblevins.org/projects/markdown-mode/
