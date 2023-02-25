---
title: A Brief Defense of XML
summary: On the difference between markup and data exchange languages.
card: brief-defense-xml.jpg
card_source: |
  [_Dimostrazione del Prospetto del Tempio di Vesta in Tivoli_][link], Francesco Piranesi, 1780.

  [link]: https://commons.wikimedia.org/wiki/File:Piranesi-6004.jpg
---

In computing, the word "document" is unfortunately overloaded. It used to mean
text document: a unit of structured human-readable text. But now we talk about
"JSON documents" and "YAML documents". Document databases are not text
repositories but databases where the records are JSON blobs.

"Markup" is similarly confused. [YAML][yaml] used to mean _Yet Another Markup
Language_. But it's not a markup language, it's a data exchange language.[^ym]

[yaml]: https://yaml.org/spec/history/2001-08-01.html

XML is widely derided by programmers because it's verbose---when used a data
exchange language, where the tag-content ratios are very poor[^elem], e.g.:

```xml
<star>
  <bayerName>Beta Pictoris</bayerName>
  <spectralType>A6V</spectralType>
  <mass>1.75</mass>
  <luminosity>8.7</luminosity>
  <position>
    <rightAscension>05h 47m 17.1s</rightAscension>
    <declination>−51°03′59″</declination>
    <distance>63.4ly</distance>
  </position>
  <age>
    <min>20My</min>
    <max>24My</max>
  </age>
</star>
```

But as a _markup language_ XML is much more tolerable:

```xml
<document>
  <title>A UNIVERSAL DECLARATION OF HUMAN RIGHTS</title>
  <h1>Preamble</h1>
  <p>
    Whereas recognition of the inherent dignity and of the equal and inalienable
    rights of all members of the human family is the foundation of freedom,
    justice and peace in the world,
  </p>
  <p>
    Whereas disregard and contempt for human rights have resulted in barbarous
    acts which have outraged the conscience of mankind, and the advent of a
    world in which human beings shall enjoy freedom of speech and belief and
    freedom from fear and want has been proclaimed as the highest aspiration of
    the common people,
  </p>
</document>
```

XML is precisely what it says on the tin: an extensible markup language. It's a
markup language with a completely uniform syntax so that the alphabet of markup
elements is customizable. And for what it is, there is _truly_ no
replacement. Every other markup language supports only a limited set of markup
directives defined from the factory. The tradeoff is generality for ease of
authoring: limited markup languages can have terser syntax for specific
elements.

So why did XML come to be used as a data exchange language? Partly because,
despite its roots in SGML (the Common Lisp of markup languages), the creators
advertised it as a general format to exchange any digital information.

But also because it's a fairly natural evolution. You start out writing your XML
documents using a generic alphabet with things like `header`, `p`, `table`,
etc. Then after writing enough documents of the same type, you start to notice
common patterns, and decide to factor out those patterns by making a new schema
with domain-specific elements, and an XSLT stylesheet to compile that down to
the generic alphabet. And now you've turned your XML documents into data blobs,
to be compiled into documents.

# Footnotes

[^ym]:
    You _can_ use it to mark up text, but you'd go insane.

[^elem]:
    Another reason XML is a poor data exchange language is that the
    element-attribute distiction, which makes perfect sense for marking up text,
    makes little sense for data serialization. It creates endless opportunities
    for bikeshedding: should this field be an attribute, or an element?
