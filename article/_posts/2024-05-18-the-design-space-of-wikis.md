---
title: The Design Space of Wikis
summary: etc.
---

- intro
  - this post describes the design space of wikis
  - sections are axes in design space: they correspond to design questions, "how should we do X?"
  - sub-sections are points or intervals along that axis: they are design choices, "we do X this way"
  - sections titled "mixin" are design choices that can be applied to multiple other choices
  - axes are not entirely orthogonal: there are incompatible regions of space
  - "wiki" here is meant as a shorthand for the broad space of things called: note-taking apps, document management systems, wikis, tools for thought, zettelkasten.

# Contents

1. [Objects](#objects)
   1. [Plain Text](#plain-text-content)
   1. [Plain Text + Links](#plain-text-links)
   1. [Rich Text](#rich-text)
   1. [Rich Text + Metadata](#rich-text-meta)
   1. [Typed Properties](#typed-properties)
   1. [Mixin: Fixed-Length Content](#mixin-fixed-length-content)
1. [Identifiers](#ident)
   1. [Unreadable Identifiers](#uuid)
   1. [Unique Title](#title)
   1. [File Path](#path)
1. [Links](#links)
   1. [No Links](#no-links)
   1. [One-Way](#one-way)
   1. [Two-Way](#two-way)
   1. [Typed Links](#typed-links)
   1. [Mixin: Red Links](#mixin-red-links)
   1. [Mixin: Link Integrity](#mixin-link-integrity)
1. [Organization](#organization)
   1. [Singleton Folder](#singleton-folder)
   1. [Boxes](#boxes)
   1. [Hierarchical Folders](#folders)
   1. [Unified Folders and Pages](#)
   1. [Tags](#tags)
   1. [Pure Hypertext](#hypertext)
   1. [Spatial Organization](#spatial)
   1. [Organization by Type](#organize-by-type)
   1. [Mixin: Constrained Folders](#mixin-constrained-folders)
1. [Markup](#markup)
   1. [WYSIWYG](#wysiwyg)
   1. [Markdown](#md)
   1. [XML](#xml)
   1. [MDX](#mdx)
   1. [Other Markup](#other-markup)
1. [Storage](#storage)
   1. [Plain Text Files](#storage-plain)
   1. [Database](#storage-db)
1. [Client](#client)
   1. [Wiki Compiler](#wiki-compiler)
   1. [Wiki Server](#wiki-server)

# Objects {#objects}

What kinds of data can objects hold?

## Plain Text {#plain-text-content}

Objects contain plain, unformatted text.
Plain-text conventions are used for formatting.
Links don't exist as first-class objects.

**Examples:** mostly older ones.

## Plain Text + Links {#plain-text-links}

Plain text, but the only formatting construct is the link.

**Examples:**

- [denote](https://protesilaos.com/emacs/denote)
- [howm](https://kaorahi.github.io/howm/)

## Rich Text {#rich-text}

Bold text, bulleted list, tables, code blocks.
Essentially everything you can do with Markdown.

**Examples:** essentially all.

## Rich Text + Metadata {#rich-text-meta}

An object has body text, but also a mapping of properties to values.

**Examples:**

- [Notion databases](https://www.notion.so/help/intro-to-databases) are probably the most prominent example.
- Tools like Obsidian or org-mode let you add properties to pages.

## Typed Properties {#typed-properties}

An object is just a mapping of properties to values, and some of those values may be rich text.
Body text is no longer a privileged, separate thing.

The main advantage of this is:
you can have multiple different blocks of body text.

**Examples:** relational databases.

## Mixin: Fixed-Length Content {#mixin-fixed-length-content}

To simulate the limitations of physical paper (e.g. [index cards][idx] or A6 paper),
content may be limited to some fixed length.

[idx]: https://en.wikipedia.org/wiki/Index_card

**Examples:** none that I know of.

# Identifiers {#ident}

How are objects identified?

## Unreadable Identifiers {#uuid}

Like serial IDs or UUIDs.
These make is easy to rename objects without breaking links, but generally have to be hidden from the user
(e.g.: requires a [WYSIWYG](#wysiwyg) editor and a [database](#storage-db)).

**Examples:** Notion

## Unique Title {#title}

The object title is globally unique.
This makes it easy to reference objects when using plain-text markup:
you just write the title in `[[wikilinks]]`.

**Examples:** MediaWiki

## File Path {#path}

With [plain-text wikis](#storage-plain),
the path to a file is a globally unique identifier by definition.

Pros:

- Object titles need not be unique.
- Can rename objects without breaking anything.

Cons:

- Linking is more verbose (have to include the filename rather the more human-readable title)
- Reorganizing the folder structure will break the link structure, benefits from [link integrity](#mixin-link-integrity).

# Links {#links}

How are objects connected?

## No Links {#no-links}

No links. The wiki is just a collection of objects. Objects can only be referred to by an unlinked name.

**Examples:** reality, [Cardfile][cardfile].

[cardfile]: https://en.wikipedia.org/wiki/Cardfile

## One-Way {#one-way}

Links are one-way. Objects don't know which other objects have linked to them.

**Examples:** HTML, since one-way links are pretty much the only way to do it in a decentralized setup.

## Two-Way {#two-way}

The original grand vision of hypertext: objects know which other objects have linked to them. There's usually a tab or pane to view the "backlinks" in a given page.

**Examples:**

- Surprisingly, [MediaWiki](/article/roam-twenty-years-before-roam).
- Anything post-[Roam][roam].

[roam]: https://en.wikipedia.org/wiki/Roam_(software)

## Typed Links {#typed-links}

Links have metadata associated with them, e.g. you can write something like:

```markdown
_Pale File_ was written by [[Vladimir Nabokov]]{type=author}.
```

**Examples:**

- [Obsidian Dataview](https://blacksmithgu.github.io/obsidian-dataview/)
- Notion at the level of [database properties](https://www.notion.so/help/database-properties).

## Mixin: Red Links {#mixin-red-links}

Some wikis let you create links to pages that don't yet exist. Clicking the link takes you to the interface to create a page with that title. Ideally you also have a way to find all red links in the database.

**Examples:**

- Obsidian
- MediaWiki

## Mixin: Link Integrity {#mixin-link-integrity}

Deleting a page that is linked-to by another triggers an error. This ensures all internal links are unbroken. Especially useful if you have e.g. links to a particular section of a page, and so renaming/removing a heading will also trigger an error.

**Examples:** none that I know of.

# Organization {#organization}

How are objects organized?

## Singleton Folder {#singleton-folder}

All objects in the wiki exist in a single set or ordered list.

**Examples:**

- [The Archive](https://zettelkasten.de/the-archive/)
- [Cardfile](https://en.wikipedia.org/wiki/Cardfile)

## Boxes {#boxes}

The wiki has a two level hierarchy:
there's a list of boxes,
each of which contains a list of objects.

**Examples:**

- [Xerox NoteCards](https://en.wikipedia.org/wiki/NoteCards)

## Hierarchical Folders {#folders}

Like a hierarchical filesystem.
Folders contain objects and other folders.

**Pros:**

- Well-known.
- Appeals to spatial intuition: everything is in exactly one place, which makes it easier to find things.
- Easily maps to [plain-text storage](#storage-plain).

**Cons:**

- The problem with every hierarchical taxonomy is the edge cases: what do I do about things that are, conceptually, in two places in the taxonomy?
- Folders are just containers and don't have data.
  You can't add a description to a folder.
  You can't associate a folder with an "index page" as an atlas of its contents.

**Examples:**

- [Obsidian](https://obsidian.md/)

## Unified Folders and Pages {#unified-folders}

Pages and folders are unified: pages can have contain subpages.
Or, from a more SQL perspective: pages can have parent pointers.

**Examples:**

- [Notion](https://www.notion.so)
- [MediaWiki](https://www.mediawiki.org/wiki/Help:Subpages)
- For some reason this incredibly useful feature is not more widely implemented.

## Tags {#tags}

Give up on hierarchy:
objects can be given a list of tags,
clicking on a tag shows all objects with the tag,
boolean operation on tags (`a and (b or c)`) can be used to search.

**Pros:**

- Handles the fact that objects can live in multiple places.

**Cons:**

- Tags are themselves flat.

## Pure Hypertext {#hypertext}

Give up on hierarchy. Just links.

**Pros:**

- Strictly more general than a hierarchy because it's a graph rather than a tree.

**Cons:**

- Does not appeal to spatial intuition: objects are not in "one place", they are floating in the aether.
- The graph can become a tangled mess.
- Folders are inevitably reinvented "one level up": you have pages that act as atlases for some subgraph, and those link to other atlas pages.

**Examples:**

- [Roam](https://roamresearch.com/)

## Spatial Organization {#spatial}

Objects exist on a canvas that you can pan or scroll.

**Pros:**

- Leverages human spatial intuition: you can remember where things are.

**Cons:**

- Infinite zoom/scroll is non-physical.

**Examples:**

- [Obsidian Canvas](https://obsidian.md/canvas)
- [Napkin](https://napkin.one/)

## Organization by Type {#organize-by-type}

Hierarchies collapse on contact with the first counter-example.
Tags are too flat.
Hypertext leads to a tangled mess.

Another way to organize information is by type:
all objects which have the same properties are grouped together.
All journal entries in one folder, all rolodex entries in another, all book reviews in another, etc.

**Examples:**

- [Notion databases](https://www.notion.so/help/intro-to-databases)
- Relational databases work like this: a database is a list of tables, and tables have rows.
- Real life also works like this, somewhat: your bookshelves have books, your CD shelf has CDs, etc.

## Mixin: Constrained Folders {#mixin-constrained-folders}

One important feature of reality:

1. All containers are finite.
2. All containers of the same kind have the same capacity.

Looking at a shelf, you can get an immediate overview of how much stuff there is: only so many books fit in the shelf, only so many envelopes fit in a shoebox.

Computers are not like this! You can have two folders on your desktop, one is 300KiB and another 300GiB, and there is no indication that they are unbalanced. The "weight" of folders is not easily visible. And folders can nest infinitely. And folders at the same level need not have the same number of children.

A constrained system can be more tractable to deal with.
You may have an upper bound on nesting, where folders can only be two or three levels deep.
You may have a _fixed_ level of nesting, where every object _must be_ inside a second or third-level folder.
Analogously, you may have limits on arity, where folders have an upper bound on how many folders they have.

**Pros:**

- A constrained system can be more tractable to deal with.

**Cons:**

- The more strict the ontology, the harder it is to adhere to it.

**Examples:**

- [Johnny.Decimal](https://johnnydecimal.com/)

# Markup {#markup}

How is text represented and interacted with?

## WYSIWYG {#wysiwyg}

The user edits text using a WYSIWYG editor.

**Pros:**

- Minimizes friction for editing text.
- Drag-and-drop image upload lowers the friction of adding files to the wiki.
- Complex markup (e.g. tables) can be implemented without breaking out an XML parser.

**Cons:**

- Vastly harder to implement than plain-text markup.
- Every single WYSIWYG editor is janky in some sui generis, hard to describe way,
  e.g.: Markdown shortcuts don't work,
  backspacing into formatting applies the formatting to new text you write,
  indenting/dedenting lists can be a pain,
  simple text editing operations can have unpredictable results.
- Complex markup can be exponentially harder to implement:
  e.g. the full power of HTML tables (with `colspan` and `rowspan`) requires essentially a full-blown spreadsheet engine to implement,
  whereas in XML the same thing only requires parsing.
- Change preview is harder. Diffing Markdown or XML is easy, and it's very clear from looking at a diff what the output is going to be. Diffing the JSON blob of a ProseMirror AST is not meaningful, and showing deltas on the rendered HTML is very hard. It's easy to mess something up and not see it in the diff.

**Examples:**

- [Notion](https://www.notion.so)
- [Obsidian](https://obsidian.md/)

## Markdown {#md}

- examples: obsidian
- pros
  - constrained
    - constraints liberate and liberties constrain
    - a lot of the things you can't do in markup you probably shouldn't do
    - forces you to keep it simple
  - well-known
  - widely implemented
    - markdown parsing isn't as easy as throwing a grammar at a parser generator
    - still, markdown parsers are widely available
  - covers most of what you need
  - change preview is easy
    - viewing a markdown diff, it's easy to mentally render what the changes would be
- cons
  - not extensible
    - wikilinks require hacking the parser
    - or abusing link syntax
    - you can try embedding HTML, but you can't have markdown inside the HTML
  - the UX for plain-text editing of markdown varies widely
    - some editors have a Markdown mode that understands the syntax so it knows e.g. how to indent lists
    - Emacs has `fill-paragraph` that makes the resulting documents far more readable.

## XML {#xml}

- pros:
  - extensible
    - wikilinks, shortcodes, etc, are just a new element type
    - graphviz? new node type
    - embedded plantuml? new node type
    - embedded gnuplot? new node type
  - widely implemented
  - complex markup is trivial
    - things that are impossible with markdown are trivial in XML
    - for example, tables as complex as what you can do in HTML are trivial to do in XML
    - can have deep structure e.g. TEI or standard ebooks markup
- cons:
  - verbose
    - this is the thing that killed xml in addition to people trying to use it for data
    - something as simple as a bulleted list in Markdown requires endless typing in XML
    - paragraphs have to be explicitly demarcated, which really, really fucks with the flow of writing
    - links are tedious to write: instead of `[[Foo]]` you have to write `<link to="Foo" />`
    - instead of `[[Foo|link text]]` you have to write `<link to="Foo">link text</link>`
    - no good!
  - bad editing experience
    - most text editors have an XML mode
    - but it is very much neglected
    - something as simple as "complete the closing node when I type `</`" is usually not implemented
    - simple stuff like indenting the nodes so that the text is on a different line than the markup, like so (example), is very hard

## MDX {#mdx}

- what if we could have the simplicity of markdown for common use cases, and the generality of XML for complex use cases?
- it exists: it's called MDX
- pros
  - does exactly what I want
  - common things are quick
  - complex things are possible
- cons:
  - not widely implemented
  - javascript

## Other Markup {#other-markup}

- asciidoc
- textile
- creole
- Mediawiki markup

# Storage {#storage}

How is data stored?

## Plain Text Files {#storage-plain}

- plain-text files in a folder
- edited with your choice of editor
- the UI can be read-write (obsidian, gitit)
- or read-only, e.g. serving HTML or even compiling to static HTML, like any static site generator
- examples:
- obsidian
- jekyll and any ssg
- ikiwiki
- gitit
- there's two ways to do this:
- the client "owns" the git repo, and makes commits
  - e.g. gitit, ikiwiki
- the client just reads from, and writes to, the repo, but does not make commits
  - e.g. obsidian
- pros
  - version control for free
    - can track changes in git
    - vcs are a lot more sophisticated than built-in vc
      - for example: most version control in wikis that have it is per-object
      - that means reverting a version can introduce links to objects that don't exist, breaking link integrity
  - change review
    - for collaborative wikis, changes can be proposed in PRs
  - multi-file changes
    - with git, changes can apply to multiple files
      - this is unique to git
  - file generation
    - can do cool things with makefiles
    - e.g. generate pages from some source of truth data
    - transclusion
    - compilation into different formats
    - e.g. say you have some corporate policies
      - you can have the markdown files in the wiki be the source of truth
      - and a makefile to transclude them and compile them to a PDF
      - to upload e.g. to a compliance management platform
    - dually, you can turn files from the filesystem into database pages
      - you can have a CSV file with your reading list
      - and a makefile to compile that into like one page per entry
  - bring your own tools
    - can edit in emacs or vim or zed or vscode or whatever editor you want
  - durable
    - plain text files are gonna last longer than some proprietary format
    - can be read from and written to with standard tools
- cons
  - forces lightweight markup
    - if the idea is that you can bring your own editor
    - you pretty much need something like markdown
    - this may be a pro
  - visibility
    - auth for git repos is repo-wide
    - you can either view the entire repo, or none of it
  - two-app failure modes:
    - if one app is used to write, and another to view, the latter tends to be ignored
    - the reason is if you are editing files through e.g. a text editor, you have to be able to find that file by browsing
    - therefore the editor can browse, read, edit, and search the files
    - what does the viewer do?
    - it provides a cute interface, maybe higher-level search, it renders tex math, and maybe it can do things like queries over the database (liek notion views)
    - in my experience of using jekyll as a person wiki, i only actually launched the wiki when I wanted to view rendered tex math
    - otherwise I'd just use Zed or Emacs to read and write
    - it helps that markdown is very readable like that
    - obsidian doesn't have this because the viewer is also a very convenient editor
      - it never occurs to me to edit an obsidian vault with Emacs
      - this might interact poorly with e.g. version control
  - sync is harder
    - using the thing from e.g. your phone is harder, you need git and the filesystem
  - visibility of changes
    - changes are stuck in the git history
    - may not be easy to make them visible to the application
  - file upload is harder

## Database {#storage-db}

- objects are stored in a database
- manipulation is through a client application
- object histories are kept in the database, if any
- pros:
  - hostable:
    - if there's a database and a server, the app can be hosted somewhere and accessed over the internet
    - can use from multiple devices, from your phone etc.
  - security:
    - for teams, custom permissions and visibility rules can be implemented by the server
    - examples: notion
  - renaming:
    - objects can be given database IDs and referred to by those IDs
    - renaming can be done transparently, without breaking anything
  - free structure:
    - files in a git repo, the folder structure has to make it easy to browse, so that files can be found for editing
    - it's natural that the folder structure should be visible in the client application (how obsidian works)
    - in a database, the storage system does not have to dictate how pages are organized
    - can have any kind of organization at the application layer
  - VC is built in
    - version control does not have to involve a separate app
  - single client
    - there is one way to interact with the wiki's contents
    - don't need a text editor and a viewer as separate apps
- cons:
  - portability:
    - plain-text markdown files are more readable than a database schema
    - sqlite is portable enough but there's extra layers of friction
      - install the sqlite client
      - figure out the database schema
      - figure out queries to get/insert the information you want
  - custom client:
    - unlike plain text files, you can't edit this with e.g Emacs
    - need a custom client software
  - cant interact with files
    - data is stuck in the database
    - extracting data into files is hard
    - importing data from files is hard
  - version control
    - needs to be reimplemented

# Client {#client}

How does the user interact with the wiki?

## Wiki Compiler {#wiki-compiler}

- the software reads the database, and either compiles to static HTML or serves a static, read only view of the app. most ssgs have a `serve` command that listens on the filesystem and updates the live view.
- examples: most ssgs
- pros:
  - fast:
    - reading compiled HTML files from disk is very fast
  - portable:
    - one time i built a wiki compiler and then i lost the source code in my giant folder of semi-finished projects
    - but i was able to read the pages i wrote because i saved the compiled output from the last time I ran the compile step
  - publishing: publishing to a website is trivial
- cons:
  - two app problem
  - compiled content can drift
  - search is hard for SSGs
    - you can compile a search index which gets loaded (like a JSON) file and do search the frontend
    - alternatively, you can just do search in the live view and not in the compiled view
  - forces plain-text files
    - if the app is read-only, there must be a way to modify the wiki
    - typically that means bring your own editor
    - and BYOE often means you need a lightweight markup language like markdown

## Wiki Server {#wiki-server}

- examples: most software
- pros:
  - you only need one app
  - can do validation at interaction time
  - compatible with any kind of storage
    - can be plain text
    - can also be a database
      - most commonly a database
  - compatible with any kind of markup
    - can have a complex WYSIWYG editor
    - can also just do lightweight markup or XML
  - file upload is easier
- cons
  - publishing: harder
