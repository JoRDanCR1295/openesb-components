<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
            xmlns:ns1="http://xml.netbeans.org/schema/cdcatalog" 
            xmlns:ns="http://xml.netbeans.org/schema/artistCollection">
    <xsl:template match="/">
        <xsl:element name="ns:artistinfo">
            <xsl:element name="ns:name">
                <xsl:value-of select="/ns1:cdcatalog/ns1:filterartist"/>
            </xsl:element>
            <xsl:for-each select="/ns1:cdcatalog/*[ ( ns1:artist = /ns1:cdcatalog/ns1:filterartist ) ]">
                <xsl:variable name="actualType" select="ns1:trackType"/>
                <xsl:choose>
                    <xsl:when test="$actualType = 'audio'">
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
                    </xsl:when>
                    <xsl:otherwise>
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
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
