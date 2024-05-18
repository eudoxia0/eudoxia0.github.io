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

# Objects

What kinds of data can objects hold?

## Plain Text

- an object's contents are plain, unformatted text
- plain text conventions are used for "formatting"
- links are just text

## Plain Text + Links

- plain text
- but with specific markup for links

## Rich Text

- simple formatting: bold, lists, etc.

## Rich Text + Metadata

- example: notion
- typed properties aside from the text
- "main text" remains a privileged own thing

## Typed Properties

- "main text" no longer a privileged thing
- an object is a map of property names to values
- some values may be rich text

## Mixin: Fixed-Length Content

- to simulate the limitations of paper
- content may be limited to some fixed length
- examples: none that i know of

# Identifiers

## Unreadable Identifiers

- uuids
- make it possible to rename things in a consequence free fashion
- usually have to be hidden, requires WYSIWYG editing and a database
- examples: notion

## Unique Title

- the object title is globally unique
- this makes it easy to reference objects in a plain-text wiki: you just write the name
- examples: wikipedia

## File Path

- can rename the title, without affecting anything
- linking has to include the filename rather than the more human readable page name
- reorganizing the folder structure breaks the link structure

# Links

How are objects connected?

## No Links

No links. The wiki is just a collection of objects. Objects can only be referred to by an unlinked name.

**Examples:** reality, [Cardfile][cardfile].

[cardfile]: https://en.wikipedia.org/wiki/Cardfile

## One-Way

Links are one-way. Objects don't know which other objects have linked to them.

**Examples:** HTML, since one-way links are pretty much the only way to do it in a decentralized setup.

## Two-Way

The original grand vision of hypertext: objects know which other objects have linked to them. There's usually a tab or pane to view the "backlinks" in a given page.

**Examples:**

- Surprisingly, [MediaWiki](/article/roam-twenty-years-before-roam).
- Anything post-[Roam][roam].

[roam]: https://en.wikipedia.org/wiki/Roam_(software)

## Typed Links

Links have metadata associated with them, e.g. you can write something like:

```markdown
_Pale File_ was written by [[Vladimir Nabokov]]{type=author}.
```

**Examples:**

- [Obsidian Dataview](https://blacksmithgu.github.io/obsidian-dataview/)
- Notion at the level of [database properties](https://www.notion.so/help/database-properties).

## Mixin: Red Links

Some wikis let you create links to pages that don't yet exist. Clicking the link takes you to the interface to create a page with that title. Ideally you also have a way to find all red links in the database.

**Examples:**

- Obsidian
- MediaWiki

## Mixin: Link Integrity

Deleting a page that is linked-to by another triggers an error. This ensures all internal links are unbroken. Especially useful if you have e.g. links to a particular section of a page, and so renaming/removing a heading will also trigger an error.

**Examples:** none that I know of.

# Organization

- how are objects organized?

## Search

- objects exist in the aether
- can be found by searching, or linking to other objects

## Boxes

- examples:
  - xerox notecards
  - reality
- there's a list of fileboxes, and objects exist in the filebox

## Singleton Folder

- objects exist in an ordered list
- example: card files

## Folders

- very common
- examples: obsidian
- objects exist in hierarchically-structured folders
- folders and objects are distinct
- pros:
  - well known
  - appeals to spatial intuition: everything is in one place
- cons:
  - hierarchy has limitations
    - things can be in multiple places

## Unified Folders and Pages

- examples: notion
- this is a really useful feature that for some reason is rarely implemented
- a page can have sub-pages
- there is no special category of "folder"
- in obsidian i'm often bothered by the fact that i'd like to have something like an "index" page for a folder
- that can act as the atlas, or description, of the folder's contents
- notion lets you have that

## Tags

- give up the hierarchy
- information can be tagged and found through boolean operations on tags

## Pure Hypertext

- examples: roam
- give up the hierarchy
- only way to find things is clicking a link, or search
- pros:
  - strictly more general than hierarchy
- cons:
  - does not appeal to spatial intuition
    - this are not in any one place
    - they are floating in the aether
  - can become tangled mess
  - folders are invented "one level up"
    - you have pages that act as folders
    - and which list their contents
    - which is like a folder but it has to be maintained manually

## Spatial Organization

- objects exist on a canvas you can pan or scroll
- pros:
  - appeals to the human spatial intuition
- cons:
  - infinite zoom in/out is non-physical

## Organization by Type

- example:
  - relational databases
  - notion (kind of)
  - real life
- instead of organizing by topic, or in folders in a hierarchy, you can organize information by type
- for example in notion you can have a database, which contains records that all have the same properties
- so you can have a database of books, a database of journal entries, etc, a database of index cards for ideas
- and this may be all the organization you need
- relational databases work this way: a database has a list of tables, and tables have rows.
  - all other structure is "one level up" through foreign keys
  - and must be revealed by querying
- this is also how real life works:

  - your bookshelf has books
  - your CD shelf has CDs
  - etc

## Mixin: Constrained Folders

- examples: johnny decimal
- one feature of reality is:
  - containers are finite
  - all containers of the same size have the same capacity
- computers are not like this
- you can have two folders on your desktop
  - one has a few kilobytes
  - one has tens of gigabytes
  - there is no indication that they are unbalanced
  - the "weight" of folders is not visible
- a constrained system can appeal more to human intutition
- there can be limits to nesting:
  - you can only have objects _up to_ two or three levels deep
  - or a fixed level: objects _must_ be three levels deep, not above, not below (this is how johnny decimal works)
- there may be limits to arity:
  - folders may have an upper bound on how many subfolders they can have
  - folders have have a _fixed_ number of folders they _must_ have
- pros:
  - constrained
  - balanced
- cons:
  - a strict ontology may be too limited

# Storage

How is data stored?

## Plain Text Files

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

## Database

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

# Markup

How is text represented and interacted with?

## WYSIWYG

- examples: notion
- pros
  - low friction
  - drag and drop image upload is easy
- cons
  - a lot more code to implement
  - every WYSIWYG editor is janky in some sui generis, hard to describe way
    - Markdown shortcuts that don't work
    - backspacing into formatting applies the formatting to new text you write
    - indenting/dedenting lists can be a pain
  - more complex stuff is often catastrophically hard to implement
    - e.g. the full power of HTML tables (with colspan, rowspan) requires essentially a full-blown spreadsheet engine to implement
    - in XML it just requires... parsing
  - change preview is harder
    - diffing markdown is 1) easy and 2) meaningful
    - diffing a JSON blob of a ProseMirror AST is not
    - showing deltas on rendered HTML is very hard
    - as a consequence, easy to mess something up and not see it

## Markdown

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

## XML

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

## MDX

- what if we could have the simplicity of markdown for common use cases, and the generality of XML for complex use cases?
- it exists: it's called MDX
- pros
  - does exactly what I want
  - common things are quick
  - complex things are possible
- cons:
  - not widely implemented
  - javascript

## Other Markup

- asciidoc
- textile
- creole
- Mediawiki markup

# Client

How does the user interact with the wiki?

## Wiki Compiler

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

## Wiki Server

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
- cons
  - publishing: harder
