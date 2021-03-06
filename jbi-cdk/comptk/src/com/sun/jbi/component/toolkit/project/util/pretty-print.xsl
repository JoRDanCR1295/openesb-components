<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <!-- Mark A. Ziesemer, www.ziesemer.com, 9-11-2005 -->
  
  <xsl:param name="indent" select="'&amp;#x9;'"/>
  <xsl:param name="newline" select="'&amp;#xA;'"/>
  <xsl:preserve-space elements="*" />
  <!--
  <xsl:template match="/">
    <xsl:message><xsl:value-of select="concat(name(.), ' - ', count(./comment()))"/></xsl:message>
    <xsl:apply-templates />
  </xsl:template>
    -->
  <xsl:template match="node() | comment()">
    <xsl:param name="indent-sum"/>
    <!-- Indent begin tag. -->
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$indent-sum"/>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <!-- This if allows for self-closing tags. -->
      <xsl:if test="count(node())">
        <xsl:apply-templates>
          <xsl:with-param name="indent-sum" 
            select="concat($indent, $indent-sum)"/>
        </xsl:apply-templates>
        <xsl:if test="count(node()) > count(text())">
          <!-- Indent end tag. -->
          <xsl:value-of select="$newline"/>
          <xsl:value-of select="$indent-sum"/>
        </xsl:if>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="text()[normalize-space(.)=''] | comment()[normalize-space(.)='']"/>
  
  <xsl:template match="processing-instruction()"/>
  <xsl:template match="text()">
    <xsl:value-of select="normalize-space(.)"/>
  </xsl:template>
</xsl:stylesheet>
