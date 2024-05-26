---
title: The Design Space of Wikis
summary: etc.
---

This post describes the design space of wikis. Sections are axes in the design
space: they correspond to design questions. Subsections are intervals along that
axis: they correspond to answers to those questions. Sections titled "mixin" are
design choices that can be applied to multiple volumes in design space.

The axes are not entirely orthogonal. A completely orthogonal reframing is a
challenge for the reader.

"Wiki" here is used as a shorthard for the broad category of application with
names like wiki, note-taking app, tool for thought, zettelkasten etc.

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
   1. [Djot](#djot)
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

- [Notion databases](https://www.notion.so/help/intro-to-databases) are probably
  the most prominent example.
- Tools like [Obsidian][obsidian] or [org-mode][org] let you add properties to
  pages.

[org]: https://orgmode.org/

## Typed Properties {#typed-properties}

An object is just a mapping of properties to values, and some of those values
may be rich text.  Body text is no longer a privileged, separate thing.

The main advantage of this is: you can have multiple different blocks of body
text.

**Examples:** relational databases.

## Mixin: Fixed-Length Content {#mixin-fixed-length-content}

To simulate the limitations of physical paper (e.g. [index cards][idx] or A6 paper),
content may be limited to some fixed length.

[idx]: https://en.wikipedia.org/wiki/Index_card

**Examples:** none that I know of.

# Identifiers {#ident}

How are objects identified?

## Unreadable Identifiers {#uuid}

Like serial IDs or UUIDs.  These make is easy to rename objects without breaking
links, but generally have to be hidden from the user (e.g.: requires a
[WYSIWYG](#wysiwyg) editor and a [database](#storage-db)).

**Examples:** [Notion][notion]

## Unique Title {#title}

The object title is globally unique.
This makes it easy to reference objects when using plain-text markup:
you just write the title in `[[wikilinks]]`.

**Examples:** [MediaWiki][mediawiki]

## File Path {#path}

With [plain-text wikis](#storage-plain),
the path to a file is a globally unique identifier by definition.

Pros:

- Object titles need not be unique.
- Can rename objects without breaking anything.

Cons:

- Linking is more verbose (have to include the filename rather the more
  human-readable title)
- Reorganizing the folder structure will break the link structure, benefits from
  [link integrity](#mixin-link-integrity).

# Links {#links}

How are objects connected?

## No Links {#no-links}

No links. The wiki is just a collection of objects. Objects can only be referred
to by an unlinked name.

**Examples:** reality, [Cardfile][cardfile].

[cardfile]: https://en.wikipedia.org/wiki/Cardfile

## One-Way {#one-way}

Links are one-way. Objects don't know which other objects have linked to them.

**Examples:** HTML, since one-way links are pretty much the only way to do it in
a decentralized setup.

## Two-Way {#two-way}

The original grand vision of hypertext: objects know which other objects have
linked to them. There's usually a tab or pane to view the "backlinks" in a given
page.

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
- [Notion][notion] at the level of [database
  properties](https://www.notion.so/help/database-properties).

## Mixin: Red Links {#mixin-red-links}

Some wikis let you create links to pages that don't yet exist. Clicking the link
takes you to the interface to create a page with that title. Ideally you also
have a way to find all red links in the database.

**Examples:**

- [Obsidian][obsidian]
- [MediaWiki][mediawiki]

## Mixin: Link Integrity {#mixin-link-integrity}

Deleting a page that is linked-to by another triggers an error. This ensures all
internal links are unbroken. Especially useful if you have e.g. links to a
particular section of a page, and so renaming/removing a heading will also
trigger an error.

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
- Appeals to spatial intuition: everything is in exactly one place, which makes
  it easier to find things.
- Easily maps to [plain-text storage](#storage-plain).

**Cons:**

- The problem with every hierarchical taxonomy is the edge cases: what do I do
  about things that are, conceptually, in two places in the taxonomy?
- Folders are just containers and don't have data.  You can't add a description
  to a folder.  You can't associate a folder with an "index page" as an atlas of
  its contents.

**Examples:**

- [Obsidian][obsidian]

## Unified Folders and Pages {#unified-folders}

Pages and folders are unified: pages can have contain subpages.
Or, from a more SQL perspective: pages can have parent pointers.

**Examples:**

- [Notion][notion]
- [MediaWiki](https://www.mediawiki.org/wiki/Help:Subpages)
- For some reason this incredibly useful feature is not more widely implemented.

## Tags {#tags}

Give up on hierarchy: objects can be given a list of tags, clicking on a tag
shows all objects with the tag, boolean operation on tags (`a and (b or c)`) can
be used to search.

**Pros:**

- Handles the fact that objects can live in multiple places.

**Cons:**

- Tags are themselves flat.

## Pure Hypertext {#hypertext}

Give up on hierarchy. Just links.

**Pros:**

- Strictly more general than a hierarchy because it's a graph rather than a
  tree.

**Cons:**

- Does not appeal to spatial intuition: objects are not in "one place", they are
  floating in the aether.
- The graph can become a tangled mess.
- Folders are inevitably reinvented "one level up": you have pages that act as
  atlases for some subgraph, and those link to other atlas pages.

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
all objects which have the same properties are grouped together.  All journal
entries in one folder, all rolodex entries in another, all book reviews in
another, etc.

**Examples:**

- [Notion databases](https://www.notion.so/help/intro-to-databases)
- Relational databases work like this: a database is a list of tables, and
  tables have rows.
- Real life also works like this, somewhat: your bookshelves have books, your CD
  shelf has CDs, etc.

## Mixin: Constrained Folders {#mixin-constrained-folders}

One important feature of reality:

1. All containers are finite.
2. All containers of the same kind have the same capacity.

Looking at a shelf, you can get an immediate overview of how much stuff there
is: only so many books fit in the shelf, only so many envelopes fit in a
shoebox.

Computers are not like this! You can have two folders on your desktop, one is
300KiB and another 300GiB, and there is no indication that they are
unbalanced. The "weight" of folders is not easily visible. And folders can nest
infinitely. And folders at the same level need not have the same number of
children.

A constrained system can be more tractable to deal with.  You may have an upper
bound on nesting, where folders can only be two or three levels deep.  You may
have a _fixed_ level of nesting, where every object _must be_ inside a second or
third-level folder.  Analogously, you may have limits on arity, where folders
have an upper bound on how many folders they have.

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
- Complex markup (e.g. tables) can be implemented without breaking out an XML
  parser.

**Cons:**

- Vastly harder to implement than plain-text markup.
- Every single WYSIWYG editor is janky in some sui generis, hard to describe
  way, e.g.: Markdown shortcuts don't work, backspacing into formatting applies
  the formatting to new text you write, indenting/dedenting lists can be a pain,
  simple text editing operations can have unpredictable results.
- Complex markup can be exponentially harder to implement: e.g. the full power
  of HTML tables (with `colspan` and `rowspan`) requires essentially a
  full-blown spreadsheet engine to implement, whereas in XML the same thing only
  requires parsing.
- Change preview is harder. Diffing Markdown or XML is easy, and it's very clear
  from looking at a diff what the output is going to be. Diffing the JSON blob
  of a ProseMirror AST is not meaningful, and showing deltas on the rendered
  HTML is very hard. It's easy to mess something up and not see it in the diff.

**Examples:**

- [Notion][notion]
- [Obsidian][obsidian]

## Markdown {#md}

The user writes plain-text in
[Markdown](https://en.wikipedia.org/wiki/Markdown).

**Pros:**

- Constrained. There's a lot you can't represent in Markdown, but that may be a
  blessing, because it forces you to keep texts simple.
- Well-known.
- Widely available: Markdown parsing isn't as easy as throwing a grammar at
  ANTLR but Markdown parsers are implemented for most widely-used languages/
- Covers most of the markup and formatting needs you might have.
- Change preview is easy: a Markdown diff is easily interpreted.

**Cons:**

- Not extensible: you can't add new formatting elements easily. You can try to
  get around this by embedding HTML into Markdown, but the HTML is not parsed
  into a DOM tree, but left as an inline string. Additionally, you can't have
  Markdown _inside_ embedded HTML.
- No Wiki Link Syntax: adding `[[wikilinks]]` requires either hacking the
  parser, adding a second layer of parsing on text contents, or abusing standard
  link syntax.
- The UX for Markdown editing varies widely. Some editors have a Markdown mode
  that knows how to do simple things like indent lists. Emacs has the
  `fill-paragraph` command and `markdown-mode` has `C-d` for indenting tables,
  both of which are really useful quality of life features, but only exist
  within Emacs.

**Examples:**

- [Obsidian][obsidian]
- Most other apps.

## XML {#xml}

The extensible markup language is exactly what it says on the tin.  Before you
close this tab in disgust, please read this [brief
apologia](/article/brief-defense-of-xml).

**Pros:**

- Extensible. It's in the name. Wikilinks, shortcodes, macros, are just a new
  element type. Want to embed graphviz, plantuml, gnuplot? Just add a new
  element.
- Widely implemented: there are XML parsers in most widely-used languages.
- Complex markup is trivial: if you want to have e.g. tables as powerful as HTML
  tables, you essentially just copy the HTML table model into your schema.

**Cons:**

- Verbose: this, and people trying to use it everywhere, is what killed XML.
  Something as simple as a bulleted list requires endless typing. While in
  Markdown you can write:

  ```markdown
  - Foo
  - Bar
  - Baz
  ```

  In XML the best-case scenario is:

  ```xml
  <ul>
    <li>Foo</li>
    <li>Bar</li>
    <li>Baz</li>
  </ul>
  ```

  Links, too, are tedious: instead of `[[Foo]]` you have to write `<link
  to="Foo" />`, instead of `[[Foo|link text]]` you have to write `<link
  to="Foo">link text</link>`.

  Finally, each paragraph has to be individually demarcated with a `<p>`
  element.

  It's death by a thousand cuts.

  For complex documents, there is no alterative, but wikis have to span a very,
  very broad range of texts: from very quick, low-friction notes to
  deeply-structured documents. XML is very good at the latter, but imposes too
  much friction for the former.

- Editing: most editors have an XML mode, but it is often very much neglected.
  Something as simple as "complete the closing tag when I type `</` is rarely
  implemented.  Indenting the nodes automatically, so that block nodes have text
  on a separate line from the tags, like so:

  ```xml
  <p>
    Foo.
  </p>
  ```

  Is also usually absent. So all the indentation has to be done by hand, which
  is very painful.

## Djot {#djot}

What if we could have the simplicity of Markdown for common use cases, and the
generality of XML for complex use cases?

[Djot](https://djot.net/) is a new markup language from the creator of
[Pandoc](https://pandoc.org/).  It is designed to be easier to parse than
Markdown, and to have a broader feature set than CommonMark. But the key feature
is that it has:

> generic containers for text, inline content, and block-level content, to which
> arbitrary attributes can be applied. This allows for extensibility using AST
> transformations.

**Pros:**

- Simple documents are easy to write.
- Complex documents are possible to write.

**Cons:**

- Not widely implemented, yet.
- Very new, not battle tested.

## MDX {#mdx}

[MDX][mdx] is Markdown that you can embed JavaScript and XML---pardon me,
JSX---into.

[mdx]: https://mdxjs.com/

**Pros:**

- Satisfies both ends of the spectrum: simple documents are easy, complex
  documents are possible.

**Cons:**

- Not widely implemented.
- Embedding JavaScript is unwelcome.

## Other Markup {#other-markup}

[AsciiDoc](https://en.wikipedia.org/wiki/AsciiDoc) is like Markdown for
[DocBook](https://en.wikipedia.org/wiki/DocBook). It is not widely implemented.

[Wikitext](https://www.mediawiki.org/wiki/Wikitext) is the markup language used
by [MediaWiki][mediawiki]. It can be extended through template syntax. There are
a number of parsers outside of core MediaWiki.

# Storage {#storage}

How is data stored?

## Plain Text Files {#storage-plain}

Data is stored as plain-text files in a directory structure. Editing is BYOE:
bring your own editor.

**Pros:**

- **Version Control:** you get version control for free, because the files can
  be comitted to a Git repo.

  What's more, VCS software is always more sophisticated than in-app version
  control: changesets can apply to multiple files, for example, and you can time
  travel to view the state of the repo at a given point in time.

  There's typically two ways to do this. The [Obsidian][obsidian] way is the app
  just reads from, and writes to, the files, and it's up to the user to manage
  the Git side of things. The other approach, implemneted by Ikiwiki and Gitit,
  is that the app "owns" the repository and can make commits on behalf of the
  user by providing a web interface.

- **Change Review:** for collaborative wikis, changes can be proposed in PRs,
  discussed, edited, and finally merged.

- **Export:** having the wiki's contents be files in the filesystem makes it
  easier to export the data and do other things with it.

  For example, you can have a script that scrapes your journal entries for
  metadata (e.g. `gym=yes, bed_on_time=no`), and compiles a habit-tracking
  spreadsheet.

  Consider a corporate wiki, where your corporate policies are
  described in separate wiki pages. Then at some point you need to make a big
  PDF of all your corporate policies, e.g. to give to investors or auditors. If
  the wiki is stored in the filesystem, it is easy to write a script that takes
  the text from the wiki pages, and compiles it into a single document (with
  nothing more complex than `awk` and `cat`) and then uses `pandoc` to compile
  it to a PDF. A CI script can even ensure this happens automatically whenever
  policy pages are updated.

- **Import:** analogously, storing the wiki's contents in the filesystem makes
  it easier to bring external data into the wiki.

  For example: you can use CLI-based tools (like [gnuplot][gp], [graphviz][gv],
  or [PlantUML][puml]) to compile diagrams-as-code into images to embed in the
  wiki. You can compile some source of truth data into multiple distinct wiki
  pages, e.g. you can turn a CSV with your reading list into a set of wiki pages
  with one page per entry.

  [gp]: http://www.gnuplot.info/
  [gv]: https://graphviz.org/
  [puml]: https://plantuml.com/

- **Bring Your Own Tools:** you can edit in Emacs or Vim or Zed or VSCode or
  whatever it is you want. So if e.g. you've configured Emacs to have the best
  Markdown editing experience in the world, you don't have to give that up for a
  web editor.

- **Durable:** plain text files will last longer than any proprietary
  database. They can be read from, written to, and searched with standard tools.

**Cons:**

- **Permissions:** auth for Git repos is generally repo-wide, so the
  finer-grained visibility policies of apps like [Notion][notion] are harder to
  implement.

- **Change Visibility:** if changes are stored in Git, rather than in a
  database, it is harder to surface them to the app level.

- **Hosting:** hosting the wiki on the Internet, where it can be read and edited
  collaboratively, is harder. Compiling a static wiki and serving that is easy,
  but for editing, you either need a web frontend that makes Git commits (like
  Gitit) or a Git client (which is harder on mobile).

**Compatibility:**

- If the goal is BYOE, you pretty much need [lightweight markup](#markup) like
  Markdown for the text representation.

**Examples:**

- [Obsidian][obsidian]: while Obsidian's client provides browsing, reading, and
  editing, the contents of a vault are stored in Markdown files in a directory,
  so they can be edited with anything.
- Most static site generators work like this.
- [Ikiwiki](https://ikiwiki.info/)
- [Gitit](https://github.com/jgm/gitit)

## Database {#storage-db}

Objects are stored in a database. Viewing and editing is done through a client
application, and object histories are stored in the database.

**Pros:**

- **Permissions:** for collaboratibe wikis, custom permissions and visibility
  rules can be implemented on top of the database, unlike in Git.
- **Free Structure:** with plain text storage, a directory structure has to be
  used to make it easy to browse large wikis. It's natural to make the folder
  structure correspond to the hierarchy by which pages are organized (e.g. how
  [Obsidian][obsidian] works). With a database there is a lot for freedom in how
  to organize pages, and the limitations of the filesystem do not apply.
- **Built-in VC:** version control does not require an external app (e.g. Git).

**Cons:**

- **Portability:** plain-text Markdown files are more easily read than a
  database, especially if the database schema is such that complex queries have
  to be made to reconstruct an object.
- **Custom Client:** plain-text wikis can save a lot of code because off the
  shelf editing software can do much of the work. With a database, custom client
  software has to be written to query and mutate the database.
- **Silo:** data in a database is more siloed than files in a filesystem. If you
  want e.g. diagrams-as-code you have to either compile the diagram externally
  and manually import the resulting image into the database, or implement a
  plugin that lets you write the diagram code as markup, and compiles it
  transparently.
- **Version Control:** VC needs to be reimplemented from scratch.

# Client {#client}

How does the user interact with the wiki?

## Wiki Compiler {#wiki-compiler}

A wiki compiler reads the wiki contents (usually, plain text files in the
filesystem) and compiles them to static HTML. Most static site generators work
like this. Usually there is a `serve` command that listens for changes in the
filesystem and minimally updates the compiled output.

**Pros:**

- **Fast:** reading compiled HTML files from disk is very fast.

- **Portable:** the compiled HTML can be read without the wiki software used to
  build it.

  One time I built a wiki compiler and then lost the source code in my giant
  folder of semi-finished projects, but I was still able to read the pages I
  wrote because I kept the `build` directory with the compiled output from the
  last time I ran the compile step.

- **Publishable:** publishing the compiled HTML to the web is trivial.

**Cons:**

- **Two-App Failure Mode:** if you have one app to write, and another to view,
  the latter tends to be ignored for the former. For example, if you edit the
  wiki using Emacs and compile to HTML using a static site generator, you will
  tend to mostly use Emacs for everything and only view the compiled output
  occasionally.

  The reason is that the editor has to be able to browse and read files in order
  to edit them. So already most of the functions of the wiki (browsing, reading,
  writing) can be done in the editor itself. What does the rendered output
  provide? Search, following links, a cute interface, maybe it renders TeX math
  which is useful.

  In my experience of using Jekyll as a personal wiki, I found that I really
  only looked at the rendered output when writing math notes, to ensure the TeX
  was correct. Otherwise I'd just use Zed or Emacs for everything.

  [Obsidian][obsidian] doesn't have this failure mode because the same app
  provides viewing and editing, it _just happens_ to be backed by plain text
  rather than a database. But if it was backed by a database, the UI would be
  basically indistinguishable.

- **Drift:** compiled content can drift if it's not automatically updated.

- **Search:** search is harder for static site generators. One way to implement
  it is to compile a search index at build time into a JSON file, and implement
  search in the frontend using JavaScript.

**Compatibility:**

- Requires [plain-text file storage](#storage-plain), since the build output is
  ready-only.

**Examples:**

- Most static site generators
- Ikiwiki

## Wiki Server {#wiki-server}

An application provides features to browse, read, and edit the wiki.

**Pros:**

- **Single Client:** a single app provides for editing and reading, there is no
  need for a separate text editor.

- **Hosting:** if there's a database and a server, the app can be hosted and
  accessed over the Internet, and from multiple devices.

- **Validation:** validation (e.g.: link integrity) can be done at interaction
  time, rather than at build time).

- **Renaming:** objects can be renamed without breaking links, because the
  server can transparently update all backlinks when an object is edited.

**Cons:**

- **Publishing:** publishing the wiki as static HTML is harder, if that is
  desirable.

**Compatibility:**

- Storage: compatible with either databases or plain-text storage.
- Markup: compatible with any kind of [markup](#markup) or text representation.

**Examples:**

- [Obsidian][obsidian]
- [MediaWiki][mediawiki]
- [Notion][notion]

[obsidian]: https://obsidian.md/
[notion]: https://www.notion.so
[mediawiki]: https://en.wikipedia.org/wiki/MediaWiki
