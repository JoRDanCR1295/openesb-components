<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" 
    xmlns:ns1="http://xml.netbeans.org/schema/cdcatalog" 
    xmlns:ns="http://xml.netbeans.org/schema/artistCollection">
    <xsl:template match="/">
        <xsl:element name="ns:artistinfo">
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="ns1:cdcatalog">
        <xsl:apply-templates select="ns1:filterartist"/>
        <xsl:apply-templates select="ns1:cd[( ns1:artist = ../ns1:filterartist )]"/>
        <xsl:apply-templates select="ns1:dvd[( ns1:artist = ../ns1:filterartist )]"/>
    </xsl:template>
    
    <xsl:template match="ns1:filterartist">
        <xsl:element name="ns:name">
            <xsl:value-of select="."/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="ns1:cd">
        <xsl:element name="ns:album">
            <xsl:element name="ns:title">
                <xsl:value-of select="ns1:title"/>
            </xsl:element>
            <xsl:element name="ns:price">
                <xsl:value-of select="ns1:price"/>
            </xsl:element>
            <xsl:element name="ns:year">
                <xsl:value-of select="ns1:year"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template match="ns1:dvd">
        <xsl:element name="ns:video">
            <xsl:element name="ns:title">
                <xsl:value-of select="ns1:title"/>
            </xsl:element>
            <xsl:element name="ns:price">
                <xsl:value-of select="ns1:price"/>
            </xsl:element>
            <xsl:element name="ns:year">
                <xsl:value-of select="ns1:year"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
