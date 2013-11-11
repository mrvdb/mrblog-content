<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:atom="http://www.w3.org/2005/Atom"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:output method="html"/>

  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>Atom feed for: <xsl:value-of select="atom:feed/atom:title"/></title>
      </head>
      <body>
	<h1>Atom feed for site of: <xsl:value-of select="atom:feed/atom:title"/></h1>
        <ul>
          <xsl:apply-templates select="//atom:entry"/>
        </ul>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="atom:entry">
    <li>
      <div><a href="{atom:link}"><xsl:value-of select="atom:title" disable-output-escaping="yes"/></a></div>
    </li>
  </xsl:template>
</xsl:stylesheet>
