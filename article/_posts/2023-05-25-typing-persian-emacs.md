---
title: Typing Persian in Emacs
summary: A tutorial introduction to farsi-transliterate-banan.
keyboard: [
  [
    {
      "key": "1",
      "fa_lower": "۱",
      "fa_upper": ""
    },
    {
      "key": "2",
      "fa_lower": "۲",
      "fa_upper": ""
    },
    {
      "key": "3",
      "fa_lower": "۳",
      "fa_upper": ""
    },
    {
      "key": "4",
      "fa_lower": "۴",
      "fa_upper": ""
    },
    {
      "key": "5",
      "fa_lower": "۵",
      "fa_upper": ""
    },
    {
      "key": "6",
      "fa_lower": "۶",
      "fa_upper": ""
    },
    {
      "key": "7",
      "fa_lower": "۷",
      "fa_upper": ""
    },
    {
      "key": "8",
      "fa_lower": "۸",
      "fa_upper": ""
    },
    {
      "key": "9",
      "fa_lower": "۹",
      "fa_upper": ""
    },
    {
      "key": "0",
      "fa_lower": "۰",
      "fa_upper": ""
    }
  ],
  [
    {
      "key": "q",
      "fa_lower": "غ",
      "fa_upper": "ق"
    },
    {
      "key": "w",
      "fa_lower": "ع",
      "fa_upper": "ء"
    },
    {
      "key": "e",
      "fa_lower": "ِ",
      "fa_upper": "ٍ"
    },
    {
      "key": "r",
      "fa_lower": "ر",
      "fa_upper": "R"
    },
    {
      "key": "t",
      "fa_lower": "ت",
      "fa_upper": "ط"
    },
    {
      "key": "y",
      "fa_lower": "ی",
      "fa_upper": "ي"
    },
    {
      "key": "u",
      "fa_lower": "و",
      "fa_upper": "ٓ"
    },
    {
      "key": "i",
      "fa_lower": "ی",
      "fa_upper": "ئ"
    },
    {
      "key": "o",
      "fa_lower": "ُ",
      "fa_upper": "ٌ"
    },
    {
      "key": "p",
      "fa_lower": "پ",
      "fa_upper": "P"
    }
  ],
  [
    {
      "key": "a",
      "fa_lower": "ا",
      "fa_upper": "آ"
    },
    {
      "key": "s",
      "fa_lower": "س",
      "fa_upper": "ص"
    },
    {
      "key": "d",
      "fa_lower": "د",
      "fa_upper": "ٱ"
    },
    {
      "key": "f",
      "fa_lower": "ف",
      "fa_upper": "إ"
    },
    {
      "key": "g",
      "fa_lower": "گ",
      "fa_upper": "غ"
    },
    {
      "key": "h",
      "fa_lower": "ه",
      "fa_upper": "ح"
    },
    {
      "key": "j",
      "fa_lower": "ج",
      "fa_upper": "‍"
    },
    {
      "key": "k",
      "fa_lower": "ک",
      "fa_upper": "ك"
    },
    {
      "key": "l",
      "fa_lower": "ل",
      "fa_upper": "L"
    }
  ],
  [
    {
      "key": "z",
      "fa_lower": "ز",
      "fa_upper": "ذ"
    },
    {
      "key": "x",
      "fa_lower": "ض",
      "fa_upper": "ظ"
    },
    {
      "key": "c",
      "fa_lower": "ث",
      "fa_upper": "ٕ"
    },
    {
      "key": "v",
      "fa_lower": "و",
      "fa_upper": "ؤ"
    },
    {
      "key": "b",
      "fa_lower": "ب",
      "fa_upper": "B"
    },
    {
      "key": "n",
      "fa_lower": "ن",
      "fa_upper": "«"
    },
    {
      "key": "m",
      "fa_lower": "م",
      "fa_upper": "»"
    }
  ]
]
---

A gentleman need not know Persian, but he should at least have forgotten it. And
to learn it you have to type it. You could change your computer's input method,
or buy a Persian keyboad, but it's inconvenient.

[Emacs][emacs] has an [input method layer][im] separate from the operating
system's. For typing Persian there's [two choices][per.el]:

[emacs]: https://www.gnu.org/software/emacs/
[im]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Input-Methods.html
[per.el]: https://github.com/emacs-mirror/emacs/blob/88d1e9b436c8778c90b7a57a152aee8071ac77b9/lisp/leim/quail/persian.el

1. `farsi-isiri-9147` implements [ISIRI 9147][isiri], the keyboard layout used
   in Iran.
2. `farsi-transliterate-banan`, by [Mohsen Banan][banan], gives you a natural
   mapping of a QWERTY keyboard to Persian.

[isiri]: https://en.wikipedia.org/wiki/ISIRI_9147
[banan]: http://mohsen.1.banan.byname.net/

You enable the latter with:

```
M-x set-input-method RET farsi-transliterate-banan RET
```

And disable it with:

```
M-x toggle-input-method
```

The mapping is very natural: `b` gives `ب`, `p` gives `پ`, `s` gives `س`. There
are also digraphs: `ch` gives `چ` and `sh` gives `ش`.

My only objection is that `q` gives `غ`, while `gh` gives `ق`, which is contrary
to what I'd expect, but acceptable.

You can figure it out by trial and error, but I wanted a convenient cheatsheet,
so here it is:

<div class="keyboard">
{% for row in page.keyboard %}
<div class="row">
{% for key in row %}
<div class="letter">
<div class="key">
{{ key.key }}
</div>
<div class="fa-lower">{{ key.fa_lower }}
</div>
<div class="fa-upper">{{ key.fa_upper }}
</div>
</div>
{% endfor %}
</div>
{% endfor %}
</div>

The bottom left is lowercase, the bottom right is uppercase.

And the digraphs:

| Input | Output |
|-------|--------|
| `ch`  | `چ`    |
| `kh`  | `خ`    |
| `zh`  | `ژ`    |
| `sh`  | `ش`    |
| `gh`  | `ق`    |

<style>
.keyboard {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 5px;
}

.row {
  display: flex;
  gap: 5px;
}

.letter {
  display: flex;
  flex-direction: row;
  align-items: stretch;
  flex-wrap: wrap;
  border-radius: 2px;
  padding: 3px;
  border: 1px solid #aaa;
  width: 50px;
  height: 70px;
}

.letter p {
  margin: 0;
}

.key, .fa-lower, .fa-upper {
  padding: 3px;
}

.key {
  flex-basis: 100%;
  font-weight: bold;
}

.fa-lower, .fa-upper {
  flex: 50%;
  color: #B1002E;
}

.fa-upper p {
  font-weight: bold;
}
</style>
