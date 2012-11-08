<?xml version="1.0" encoding="UTF-8"?>

<!--
    Document   : SportXSLT.xsl
    Created on : July 15, 2008, 12:21 PM
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
        <xsl:element name="sports">
            <xsl:apply-templates select="//*[local-name() = 'game']"/>
        </xsl:element>
    </xsl:template>
    <xsl:template match="//*[local-name() = 'game']">
        <xsl:element name="{@title}">
            <xsl:attribute name="id">
                <xsl:value-of select="//*[local-name() = 'id']"/>
            </xsl:attribute>
            <comment>
                <xsl:value-of select="//*[local-name() = 'para']"/>
            </comment>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
