<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:syn="http://xml.netbeans.org/schema/Synchronous" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://xml.netbeans.org/schema/Synchronous Synchronous.xsd">
    <xsl:output method="xml" indent="yes"/>
    <xsl:template match="/syn:order">
        <syn:items>
            <xsl:for-each select="syn:item">
                <xsl:if test="(syn:qty &gt; 5) and (syn:price &gt; 99)">
                    <syn:item>
                        <xsl:copy-of select="syn:name"/>
                        <xsl:copy-of select="syn:qty"/>
                        <xsl:copy-of select="syn:price"/>
                    </syn:item>
                </xsl:if>
            </xsl:for-each>
        </syn:items>
    </xsl:template>
</xsl:stylesheet>
