<?xml version="1.0" encoding="UTF-8"?>

<!--
    Document   : SimpleTransformation.xsl
    Created on : July 15, 2008, 9:21 AM
    Author     : rlieou
    Description:
        Purpose of transformation follows.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml"/>

    <!-- TODO customize transformation rules 
         syntax recommendation http://www.w3.org/TR/xslt 
    -->
    <xsl:template match="/">
        <xsl:element name="SimpleHelloElement">
            <xsl:element name="SimpleHello">
            <xsl:text>Hello </xsl:text><xsl:value-of select="//*[local-name() = 'firstName']"/>
            <xsl:text> </xsl:text><xsl:value-of select="//*[local-name() = 'lastName']"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
