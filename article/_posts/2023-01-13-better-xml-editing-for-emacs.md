---
title: Better XML Editing for Emacs
summary: Improving completion in nXML mode.
card: better-xml-editing-for-emacs.jpg
---

[nXML mode][nxml] is the standard XML editing mode for [Emacs][emacs]. It's
alright, but the ergonomics could be improved: authoring XML is a pain, and you
need powerful completion to make it tolerable.

[nxml]: https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html
[emacs]: https://www.gnu.org/software/emacs/

nXML's main quality of life feature is automatically completing end tags. That
is, if you type `</`, it automatically completes this to `</foo>`, by searching
backwards to find which tag you're in.

Many XML editors have a feature where typing the start tag causes them to
complete the end tag, and leave the cursor between the two. So if you have
`<foo|` (where the pipe character is the [point][pt], or cursor), typing the
right angle bracket completes this to `<foo>|</foo>`, leaving the point between
the two elements.

[pt]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html

nXML can do this, but you have to input an awkward key combination. We can
instead do this automatically:

```elisp
(require 'nxml-mode)

(defun my-in-start-tag-p ()
  ;; Check that we're at the end of a start tag. From the source code of
  ;; `nxml-balanced-close-start-tag`.
  (let ((token-end (nxml-token-before))
	    (pos (1+ (point)))
	    (token-start xmltok-start))
    (or (eq xmltok-type 'partial-start-tag)
		(and (memq xmltok-type '(start-tag
					             empty-element
					             partial-empty-element))
		     (>= token-end pos)))))

(defun my-finish-element ()
  (interactive)
  (if (my-in-start-tag-p)
      ;; If we're at the end of a start tag like `<foo`, complete this to
      ;; `<foo></foo>`, then move the point between the start and end tags.
      (nxml-balanced-close-start-tag-inline)
      ;; Otherwise insert an angle bracket.
      (insert ">")))

(define-key nxml-mode-map (kbd ">") 'my-finish-element)
```

Another complaint I have is indentation. By default, if you have:

```xml
<foo>
    <bar>|</bar>
</foo>
```

And you press enter, you get:

```xml
<foo>
    <bar>
    |</bar>
</foo>
```

But this isn't what I want. I want to leave the point in an indented blank line between the two elements:

```xml
<foo>
    <bar>
        |
    </bar>
</foo>
```

We can do this very straightforwardly:

```elisp
(defun my-nxml-newline ()
  "Insert a newline, indenting the current line and the newline appropriately in nxml-mode."
  (interactive)
  ;; Are we between an open and closing tag?
  (if (and (char-before) (char-after)
           (char-equal (char-before) ?>)
           (char-equal (char-after) ?<))
      ;; If so, indent it properly.
      (let ((indentation (current-indentation)))
        (newline)
        (indent-line-to (+ indentation 4))
        (newline)
        (indent-line-to indentation)
        (previous-line)
        (end-of-line))
    ;; Otherwise just insert a regular newline.
    (newline)))

(define-key nxml-mode-map (kbd "RET") 'my-nxml-newline)
```

I don't know the first thing about Emacs Lisp, but I could specify,
algorithmically, the kind of thing I wanted to see happen, so I asked
[ChatGPT][gpt] how to do it:

[gpt]: https://openai.com/blog/chatgpt/

![Screenshot of the ChatGPT interface. I ask it how to run some code when a key is pressed in Emacs, and it responds with example Emacs Lisp code and explanatory comments.](/assets/content/better-xml-editing-for-emacs/a.png)

![ChatGPT screenshot. I tell it how I want to indent text on newlines, and it shows me correct code.](/assets/content/better-xml-editing-for-emacs/b.png)

![ChatGPT screenshot. I tell it a step I forgot to list, and it corrects the code accordingly.](/assets/content/better-xml-editing-for-emacs/c.png)

![ChatGPT screenshot. I ask it to add an extra condition before running the code, and it correctly modifies the code.](/assets/content/better-xml-editing-for-emacs/d.png)

I made two of mistakes (forgetting one step of the algorithm, and forgetting to
specify a necessary precondition). ChatGPT made one: it suggested
`beginning-of-line` rather than `end-of-line`, where the latter is required to
put the cursor after the whitespace for indentation.
