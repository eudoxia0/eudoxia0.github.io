---
title: The Design Space of Wikis
summary: etc.
---

- object structure
  - plain text
    - an object's contents are plain, unformatted text
    - plain text conventions are used for "formatting"
    - links are just text
  - plain text with links
    - plain text
    - but with specific markup for links
  - mixin: fixed-length content
    - to simulate the limitations of paper
    - content may be limited to some fixed length
    - examples: none that i know of
- identifiers

- storage
  - files
    - plain-text files in a folder
    - examples:
      - obsidian
      - jekyll and any ssg
      - ikiwiki
      - gitit
    - pros
      - version control for free
        - can track changes in git
      - change review
        - for collaborative wikis, changes can be proposed in PRs
      - multi-file changes
        - with git, changes can apply to multiple files
          - this is unique to git
    - cons
- database

- links
  - none
  - one way
  - two-way
  - typed links
  - mixin: dead links
  - mixin: link integrity
