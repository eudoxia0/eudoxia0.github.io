<?xml version="1.0"?>
<xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="no"/>
  <xsl:preserve-space elements="code"/>
  <xsl:strip-space elements="*"/>

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
        <a href="{//links/l[@id=$id]/@uri}">
          <xsl:apply-templates/>
        </a>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="r">
    <!-- References -->
    <xsl:variable name="id" select="@id"/>
    <a href="#{$id}" class="ref">[<xsl:value-of select="$id"/>]</a>
  </xsl:template>

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

  <xsl:template match="code">
    <pre>
      <code>
        <xsl:value-of select="string(.)"/>
      </code>
    </pre>
  </xsl:template>

  <xsl:template match="ic">
    <code><xsl:value-of select="string(.)"/></code>
  </xsl:template>

  <!-- Smart typography -->
  <xsl:template match="text">
    <xsl:variable name="dashed" select="replace(.//text(), '---', '—')"/>
    <xsl:variable name="mdashed" select="replace($dashed, '--', '—')"/>
    <xsl:variable name="ellipsed" select="replace($mdashed, '\.\.\.', '…')"/>
    <xsl:analyze-string select="$ellipsed" regex="&quot;([^&quot;]*)&quot;">
      <xsl:matching-substring>
        <xsl:value-of select="concat('&#8220;', regex-group(1), '&#8221;')"/>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
        <xsl:value-of select="."/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>

  <!-- Basic HTML tags I don't want to see disappear -->
  <xsl:template match="strike|nav|header|div|ul|ol|li|h1|h2|h3|h4|p|post|page">
    <xsl:copy>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
