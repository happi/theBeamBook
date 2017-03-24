<!DOCTYPE xsl:stylesheet>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:output method="xml" indent="yes" omit-xml-declaration="yes"/>
  
  <xsl:param name="include.paths" select="1"/>
  <xsl:param name="include.copies" select="1"/>
  
  <xsl:template match="logs">
    <revhistory>
      <xsl:apply-templates/>
    </revhistory>
  </xsl:template>
  
  <xsl:template match="logentry">
    <revision>
      <xsl:apply-templates select="@revision|*"/>
    </revision>
  </xsl:template>
  
  <xsl:template match="logentry/@revision">
    <revnumber>
      1.<xsl:apply-templates/>
    </revnumber>
  </xsl:template>
  
  <xsl:template match="tag">
    <revremark>
      <xsl:apply-templates/>
    </revremark>
  </xsl:template>
  
  <xsl:template match="date">
    <date>
      <xsl:apply-templates/>
    </date>
  </xsl:template>
  
  <xsl:template match="author">
      <authorinitials>
        <xsl:apply-templates/>
    </authorinitials>
  </xsl:template>
  
  <xsl:template match="msg">
    <revremark>
        <xsl:apply-templates/>
    </revremark>
  </xsl:template>
  
  <xsl:template match="paths">
    <xsl:if test="$include.paths != 0">
      <itemizedlist>
        <title>Paths</title>
        <xsl:apply-templates/>
      </itemizedlist>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="path">
    <listitem>
      <para>
        <xsl:apply-templates select="@action|text()"/>
      </para>
    </listitem>    
  </xsl:template>
  
  <xsl:template match="path/@action">
    <xsl:text>[</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>] </xsl:text>
  </xsl:template>
  <xsl:template match="path/text()">
    <filename>
      <xsl:value-of select="."/>
    </filename>
  </xsl:template>

  <xsl:template match="copyies">
    <xsl:if test="$include.paths != 0">
      <listitem>
        <title>Copies</title>
        <para>
          <xsl:apply-templates select="@*|text()"/>
        </para>
      </listitem>
    </xsl:if>
  </xsl:template>
  <xsl:template match="copy">
    <listitem>
      <para>
        <xsl:apply-templates select="@*|text()"/>
      </para>
    </listitem>    
  </xsl:template>
  
  <xsl:template match="copy/@source">
    <filename>
      <xsl:value-of select="."/>
    </filename>
  </xsl:template>
  <xsl:template match="copy/text()">
    <xsl:text> -> </xsl:text>
    <filename>
      <xsl:value-of select="."/>
    </filename>
  </xsl:template>
  
</xsl:stylesheet>
