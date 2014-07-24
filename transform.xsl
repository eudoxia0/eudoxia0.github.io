<?xml version="1.0"?>
<xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>

  <!-- Page templates -->
  <xsl:template match="page">
    <!-- Generic page -->
    <xsl:variable name="title" select="@title"/>
    <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE html&gt;</xsl:text>
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title><xsl:value-of select="$title"/></title>
        <link rel="stylesheet" href="/static/style.css"/>
      </head>
      <body>
        <div id="main">
          <xsl:apply-templates/>
        </div>
      </body>
    </html>
  </xsl:template>

  <!-- Content tags -->
  <xsl:template match="sidenote">
    <div class="sidenote">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="quote">
    <blockquote><xsl:apply-templates/></blockquote>
  </xsl:template>

  <xsl:template match="b">
    <strong><xsl:apply-templates/></strong>
  </xsl:template>

  <xsl:template match="i">
    <em><xsl:apply-templates/></em>
  </xsl:template>

  <xsl:template match="fig">
    <!-- Links -->
    <xsl:variable name="uri" select="@uri"/>
    <xsl:variable name="text" select="translate(normalize-space(.//text()), ' ', '')"/>
    <figure>
      <img src="{$uri}" alt="{$text}" title="{$text}"/>
      <figcaption>
        <xsl:apply-templates/>
      </figcaption>
    </figure>
  </xsl:template>

  <xsl:template match="l">
    <!-- Links -->
    <xsl:variable name="id" select="@id"/>
    <xsl:variable name="uri" select="@uri"/>
    <xsl:choose>
      <xsl:when test="$uri">
        <a href="{$uri}"><xsl:apply-templates/></a>
      </xsl:when>
      <xsl:otherwise>
        <a href="{//links/link[@id=$id]/@ref}">
          <xsl:apply-templates/>
      </a>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="links"><!-- Delete the contents --></xsl:template>

  <xsl:template match="r">
    <!-- References -->
    <xsl:variable name="id" select="@id"/>
    <a href="#{$id}" class="ref">[<xsl:value-of select="$id"/>]</a>
  </xsl:template>

  <xsl:template match="references">
    <!-- Text of the references -->
    <section id="references">
      <xsl:for-each select="./ref">
        <div class="text">
          <span class="author"><xsl:value-of select="author"/></span>.
          <em><xsl:value-of select="title"/></em>
          <xsl:if test="desc">
            <p class="desc">
              <xsl:value-of select="desc"/>
            </p>
          </xsl:if>
          <xsl:value-of select="link"/>
        </div>
      </xsl:for-each>
    </section>
  </xsl:template>

  <xsl:template match="cite">
    <!-- Citations -->
    <xsl:variable name="id" select="@id"/>
    <em><xsl:apply-templates/></em>
    <a href="#{$id}" class="ref">[<xsl:value-of select="$id"/>]</a>
  </xsl:template>

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

  <!-- Basic HTML tags I don't want to see disappear -->
  <xsl:template match="strike|nav|header|div|ul|ol|li|h1|h2|h3|h4|p">
    <xsl:copy>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
