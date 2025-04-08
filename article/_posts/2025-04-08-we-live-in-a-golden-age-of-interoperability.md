---
title: We Live In a Golden Age of Interoperability
summary: On the growth of open standards.
card: we-live-in-a-golden-age-of-interoperability.webp
---

Yesterday I was reading [_Exploring the Internet_][eti], an oral history of the
early Internet. The first part of the book describes the author's efforts to
publish the [ITU]'s Blue Book: 19 kilopages of standards documents for telephony
and networks. What struck me was the description of the ITU's documentation
stack:

[eti]: https://public.resource.org/eti/
[ITU]: https://en.wikipedia.org/wiki/International_Telecommunication_Union

> A week spent trolling the halls of the ITU had produced documentation on about
> half of the proprietary, in-house text formatting system they had developed
> many years ago on a Siemens mainframe. The computer division had given me nine
> magnetic tapes, containing the Blue Book in all three languages. [...] We had
> two types of files, one of which was known to be totally useless.
>
> The useless batch was several hundred megabytes of AUTOCAD drawings, furnished
> by the draftsmen who did the CCITT illustrations. Diagrams for the Blue Book
> were done in AUTOCAD, then manually assembled into the output from the
> proprietary text formatting system. [...]
>
> Turned out that AUTOCAD was indeed used for the diagrams, with the exception
> of any text in the illustrations. The textless diagrams were sent over to the
> typing pool, where people typed on little pieces of paper ribbon and pasted
> the itsy-bitsy fragments onto the illustrations. Come publication time, the
> whole process would be repeated, substituting typeset ribbons for typed
> ribbons. A nice production technique, but the AUTOCAD files were useless.
>
> The rationale for this bizarre document production technique was that each
> diagram needed text in each of the three official languages that the ITU
> published. While AUTOCAD (and typing) was still being used, the ITU was slowly
> moving over to another tool, MicroGrafix Designer. There, using the magical
> concept of layers, they were proudly doing “integrated text and graphics.”
>
> The second batch of DOS files looked more promising. Modern documents, such as
> the new X.800 recommendations, were being produced in Microsoft Word for
> Windows. My second batch of tapes had all the files that were available in the
> Word for Windows format, the new ITU publishing standard.

Proprietary tape drives with proprietary file systems. AutoCAD for vector
graphics. Text documents in the proprietary, binary Word format.

Note that the diagrams were being assembled _physically_, by pasting pieces of
paper together. And then they were photographed. That's why it's called a
"camera ready" copy. And this is 1991, so it's not a digital camera: it's film,
silver-halogen crystals in collagen. It's astouding to think that this medieval
process was happening as recently as the 90s.

Compare this to today: you drag some images into Adobe FrameMaker and press
print.

> The ITU had documented the format we could expect the tapes to be in. Each
> file had a header written in the [EBCDIC] character set. The file itself used
> a character set seemingly invented by the ITU, known by the bizarre name of
> Zentec. The only problem was that the header format wasn’t EBCDIC and the
> structure the ITU had told us would be on the tape wasn’t present.

[EBCDIC]: https://en.wikipedia.org/wiki/EBCDIC

Proprietary character sets!

> Next, we had to tackle TPS. This text formatting language was as complicated
> as any one could imagine. Developed without the desire for clarity and
> simplicity I had come to expect from the UNIX operating system and its tools,
> I was lost with the Byzantine, undocumented TPS.
>
> The solution was to take several physical volumes of the Blue Book and compare
> the text to hexadecimal dumps of the files. I then went to the Trident Cafe
> and spent a week drinking coffee trying to make sense of the data I had,
> flipping between the four files that might be used on any given page of text
> trying to map events in the one-dimensional HexWorld to two-dimensional events
> in the paper output.
>
> [...]
>
> Finally, after pages and pages of PERL code, we had the beginnings of a
> conversion program. We had tried to use the software developed at the ITU to
> convert from TPS into [RTF], but the code had been worse than useless.

[RTF]: https://en.wikipedia.org/wiki/Rich_Text_Format

A proprietary, in-house, (ironically) undocumented document-preparation system!
Today this would be a Git repo with Markdown files and [TikZ]/[Asymptote][as]
source files for the diagrams, and a Makefile to tie it all together with
Pandoc. Maybe a few custom scripts for the things Markdown can't represent, like
complex tables or asides. Maybe [DITA] if you really like XML.

[TikZ]: https://en.wikipedia.org/wiki/PGF/TikZ
[as]: https://en.wikipedia.org/wiki/Asymptote_(vector_graphics_language)
[Pandoc]: https://pandoc.org/
[DITA]: https://en.wikipedia.org/wiki/Darwin_Information_Typing_Architecture

This reminded me of a similar side quest I attempted many years ago: I tried to
build a modern version of the Common Lisp HyperSpec from the source text of the
ANSI Common Lisp draft (the draft being in the public domain, unlike the
officially blessed version). The sources are in TeX, not "modern" LaTeX but 80's
TeX. Parsing TeX is hard enough, the language is almost-but-not-quite context
free, it really is meant to be executed as it is parsed; rather than parsed,
represented, and transformed. But even if you managed to parse the TeX sources
using a very flexible and permissive TeX parser, you have to apply a huge long
tail of corrections just to fix bad parses and obscure TeX constructs. In the
end I gave up.

---

We live in much better times.

For every medium, we have widely-used and widely-implemented open formats:
Unicode and Markdown for text, JSON and XML for data exchange, JPEG/PNG/SVG for
images, Opus for audio, WebM for videos.

Unicode is so ubiquitous it's easy to forget what an achievement it
is. Essentially all text today is UTF-8 except the Windows APIs that were
designed in the 90s for "wide characters" i.e. UTF-16. I remember when people
used to link to the UTF-8 Everywhere manifesto. There was a time, not long ago,
when "use UTF-8" was something that had to be said.

Rich text is often just Markdown. Some applications have more complex constructs
that can't be represented in Markdown, in those cases you can usually get the
document AST as JSON. The "worst" format most people ever have to deal with is
XML.

Data exchange happens through JSON, CSV, or Parquet. Every web API uses JSON as
the transport layer, so instead of a thousand ad-hoc binary formats, we have one
plain-text, human-readable format that can be readily mapped into domain
objects. Nobody would think to share vector graphics in DWG format because we
have SVG, an open standard.

TeX is probably the most antideluvian text "format" in widespread use, and maybe
Typst will replace it. Math is one area where we're stuck with embedding TeX
(through KaTeX or equivalent) since MathML hasn't taken off (understandably,
since nobody wants to write XML by hand).

Filesystems are usually proprietary, but every operating system can read/write a
FAT32/NTFS flash drive. In any case networking has made filesystems less
important: if you have network access you have Google Drive or S3. And
filesystems are a lot less diverse nowadays: except for extended attributes, any
file tree can be mapped losslessly across ext4, NTFS, and APFS. This was not
true in the past!  It took decades to converge on the definition of a filesystem
as "a tree of directories with byte arrays at the leaf nodes", e.g. HFS had
resource forks, the VMS file system had versioning built in. File paths were
wildly different.

Open standards are now the default. If someone proposes a new data exchange
format, a new programming language, or things of that nature, the expectation is
that the spec will be readable online, at the click of a button, either as HTML
or a PDF document. If implementing JSON required paying 300 CHF for a 900 page
standards document, JSON would not have taken off.

Our data is more portable than ever, not just across space (e.g. if you use a
Mac and a Linux machine) but across time.

In the mid-80s the BBC wanted to make a latter-day Domesday Book. It was like a
time capsule: statistical surveys, photographs, newsreels, people's accounts of
their daily life. The data was stored on Laserdisc, but the formats were
entirely _sui generis_, and could only be read by the client software, which was
deeply integrated with a specific hardware configuration. And within a few years
the data was essentially inaccessible, needing a team of
programmer-archeologists to reverse engineer the software and data formats.

If the BBC Domesday Book was made nowadays it would last forever: the text would
be UTF-8, the images JPEGs, the videos WebM, the database records would be CSVs
or JSON files, all packaged in one big ZIP container. All widely-implemented
open standards. A century from now we will still have UTF-8 decoders and JSON
parsers and JPEG viewers, if only to preserve the vast trove of the present; or
we will have ported all the archives forward to newer formats.

All this is to say: we live in a golden age of interoperability.
