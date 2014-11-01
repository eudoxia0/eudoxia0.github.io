---
title: On Markup
layout: essay
---

The essence and purpose of markup is to bring annotated text into an abstract
representation, where it can be manipulated through code (Usually simple
pattern-template macros).

Lightweight markup languages like Markdown work for simple documents, because
they have syntactic sugar for 80% of what one requires for documentation. But
fall apart when you want to add new features: What can I do if I want tables?
Or references? Or just a simple macro to reduce repetition?  Pandoc provides all
of those, through non-standard extensions. But what if you want to combine
those?

Consider the following hypothetical example in Pandoc Markdown:

~~~markdown
# Monopole Detection

... Most of the information is presented in *Magnetic Monopoles*[preskill]
and *Magnetic Monopole Searches with AMANDA and other detectors*[lindell]...

...

[^preskill]:
    John Preskill. *[Magnetic Monopoles][http://...]*.

[^monmass]:
    Matilda Åberg Lindell.
    *[Magnetic Monopole Searches with AMANDA and other detectors][http://...]*.
~~~

The pattern is pretty clear: Italicized text followed by a reference. I wish I
could do something along these lines:

~~~markdown
@defmacro cite(id, text): *$text*[$id]

... Most of the information is presented in @cite(preskill, Magnetic
Monopoles) and @cite(lindell, Magnetic Monopole Searches with AMANDA and
other detectors)...
~~~

Of course, Markdown is not extensible. Although Pandoc can be extended to
transform one group of elements into another, *adding* a new element is not
exactly easy.

On the other hand, consider the following XML:

~~~markdown
... Most of the information is presented in <cite id="preskill">Magnetic
Monopoles</cite> and <cite id="lindell">Magnetic Monopole Searches with
AMANDA and other detectors</cite>...
~~~

And this XSLT:

~~~xml
<xsl:template match="cite">
  <!-- Citations -->
  <xsl:variable name="id" select="@id"/>
  *<xsl:apply-templates/>*[<xsl:value-of select="$id"/>]
</xsl:template>
~~~

A macro. This translates the XML to Markdown, so what you'd do is preprocess the
`.md` files into pure Markdown, so you can enjoy the simplicity of Markdown with
the benefits of macros.

But consider another side to this same example: The text of the references. They
both follow the same pattern: The author, followed by the italicized name of the
reference, and a link. Repetition violates DRY and introduces room for
mistakes. And what about exceptional situations, such as no link being provided?
What we need is a macro.

~~~xml
<xsl:template match="references">
  <!-- Text of the references -->
  <xsl:variable name="link" select="link//@uri"/>
  <h1>References</h1>
  <section id="references">
    <xsl:for-each select="./ref">
      <div class="text">
        <xsl:if test="author">
          <span class="author"><xsl:apply-templates select="author"/></span>.
        </xsl:if>
        <xsl:choose>
          <xsl:when test="author">
            <a href="{$link}"><em><xsl:apply-templates select="title"/></em></a>.
          </xsl:when>
          <xsl:otherwise>
            <a href="{$link}">Link</a>.
          </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="desc">
          <p class="desc">
            <xsl:apply-templates select="desc"/>
          </p>
        </xsl:if>
      </div>
    </xsl:for-each>
  </section>
</xsl:template>
~~~

Well, that's big. But now we can write something like this:

~~~xml
<references>
  <ref id="preskill">
    <link>http://www.theory.caltech.edu/~preskill/pubs/preskill-1984-monopoles.pdf</link>
    <title>Magnetic Monopoles</title>
    <author>John Preskill</author>
  </ref>
  <ref id="lindell">
    <title>Magnetic Monopole Searches with AMANDA and other detectors</title>
    <author>Matilda Åberg Lindell</author>
    <link>http://www.diva-portal.org/smash/get/diva2:320548/FULLTEXT02</link>
    <desc>
      The author remarks that "[Monopoles] could never be produced in any
      man-made accelerator, existing or conceivable".
    </desc>
  </ref>
</references>
~~~

What if you want to include files? Pandoc has a simple extension that lets you
do this, but what if you want to add a start-end range? For example, let's say
you're writing a blog post describing some code, and you keep the code in a file
(Where it can be run and tested) and include ranged snippets in your post.
You'd need to modify the extension, which requires some knowledge of Haskell, or
filtering the raw JSON.

Or, you can do this (Which only works with XSLT 2.0):

~~~xml
<xsl:template match="include">
  <xsl:variable name="uri" select="@uri"/>
  <xsl:variable name="start" select="number(@start)"/>
  <xsl:variable name="end" select="number(@end)-1"/>
  <xsl:variable name="text" select="tokenize(unparsed-text($uri, 'utf-8'), '\n')"/>
  <xsl:variable name="subseq" select="subsequence($text, $start, $end)"/>
  <pre class="included">
    <xsl:value-of select="string-join($subseq, '&#10;')"/>
  </pre>
</xsl:template>
~~~

Let's go further. You're writing a personal website -- such as this one -- using
Markdown and Pandoc. You have a simple Makefile that runs the Markdown files
through Pandoc, but you don't want all files to be treated equally: Files in the
`posts/` folder are converted to HTML with the template for posts, files in
`slideshows/` are converted using a template for, say, [reveal.js][reveal]
slideshows. You could do this by separating by files into folders, and telling
Make to search for different files in different places. Or, you could have a
root XML element that determines what template will be used, and write the
templates along with the XSLT. Like this:

~~~xml
<post title="Why XML is horrible and we should all be using Lisp">
  First, S-expressions are more compact than...
</post>
~~~

After doing all this, what *exactly* is left for Markdown? Replacing
hashes/octothorpes/pounds/number signs with header tags isn't really
problematic. I've come up with just two items:

* Smart typography: Replacing double dashes with en dashes, smart quotes, et
  cetera.
* I don't want to put `p` tags around every paragraph.

The former is actually two things: Replacing dashes and ellipses with their
proper characters is easy. Smart quotes, less so (But still doable in XSLT
2.0). The latter is even more problematic.

Let's start with the simple stuff:

~~~xml
<xsl:template match="text">
  <!-- Replace '---' with the em dash, '--' with the en dash, and '...' with the
  ellipsis character for typographer cred -->
  <xsl:variable name="dashed" select="replace(.//text(), '---', '—')"/>
  <xsl:variable name="mdashed" select="replace($dashed, '--', '—')"/>
  <xsl:variable name="ellipsed" select="replace($mdashed, '\.\.\.', '…')"/>
  <!-- Double quote regular expressions -->
  <xsl:analyze-string select="$ellipsed" regex="&quot;([^&quot;]*)&quot;">
    <xsl:matching-substring>
      <xsl:value-of select="concat('&#8220;', regex-group(1), '&#8221;')"/>
    </xsl:matching-substring>
    <xsl:non-matching-substring>
      <xsl:value-of select="."/>
    </xsl:non-matching-substring>
  </xsl:analyze-string>
</xsl:template>
~~~

Paragraphs are harder. I never figured it out, but thanks to the Internet, I
[didn't have to][stackoverflow-p]. However, it requires two transforms, piped
one to the other. The first transform prevents a bug:

~~~xml
<xsl:template match="p">
  <!-- There's a bug in the paragraph splitting code. If the last paragraph
       contains no tags it will be ignored. This template introduces a tag
       that will then be removed. -->
  <xsl:copy>
    <xsl:apply-templates/>
    <p-pad-tag></p-pad-tag>
  </xsl:copy>
</xsl:template>
~~~

Now, the actual paragraph splitting code. Are you ready?

~~~xml
<!-- Double newlines are our delimiter -->
<xsl:variable name="delim" select="'&#10;&#10;'"/>

<xsl:template match="//p">
  <div class="p-block">
    <xsl:apply-templates select="node()[1]"/>
  </div>
</xsl:template>

<xsl:template match="node()" mode="open" name="open">
  <xsl:copy-of select="."/>
  <xsl:apply-templates select="following-sibling::node()[1]"
                       mode="open"/>
</xsl:template>

<xsl:template match="//p/node()">
  <xsl:param name="pTail" select="''"/>
  <p>
    <xsl:value-of select="$pTail"/>
    <xsl:call-template name="open"/>
  </p>
  <xsl:variable name="vNext"
                select="following-sibling::text()[contains(., $delim)][1]"/>
  <xsl:apply-templates select="$vNext">
    <xsl:with-param name="pString"
                    select="substring-after($vNext, $delim)"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="text()[contains(., $delim)]"
              mode="open" priority="1">
  <xsl:value-of select="substring-before(., $delim)"/>
</xsl:template>

<xsl:template match="//p/text()[contains(., $delim)]"
              priority="1" name="text">
  <xsl:param name="pString" select="."/>
  <xsl:choose>
    <xsl:when test="contains($pString, $delim)">
      <xsl:variable name="vOutput"
                    select="normalize-space(substring-before($pString, $delim))"/>
      <xsl:if test="$vOutput">
        <p>
          <xsl:value-of select="$vOutput"/>
        </p>
      </xsl:if>
      <xsl:call-template name="text">
        <xsl:with-param name="pString"
                        select="substring-after($pString, $delim)"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="following-sibling::node()[1]">
        <xsl:with-param name="pTail" select="$pString" />
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- Remove the 'p-pad-tag' thing -->
<xsl:template match="p-pad-tag"></xsl:template>

<xsl:template match="@* | node()">
  <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
  </xsl:copy>
</xsl:template>
~~~

Is that clear?

Anyways, back to the point: Markdown is simple but not extensible, XML is very
dense on the eyes but can be extended, with some difficulty. What's missing is a
reasonable midpoint: a markup language that has a higher signal-to-noise ratio
than XML. One that keeps markup elements to a minimum, giving the most room to actual
content. One that has extensibility built in, and doesn't require that you run
the source through some external program to expand macros.

And so I rolled my own: [Wax][wax] is a markup language with TeX syntax that lets you
define macros. A hypothetical example:

~~~tex
\tag[key='val']{
    content
}
~~~

[reveal]: http://lab.hakim.se/reveal-js/
[stackoverflow-p]: http://stackoverflow.com/questions/3733411/xslt-convert-carriage-returns-to-paragraphs-in-mixed-node-while-preserving-html
[wax]: https://github.com/eudoxia0/wax
