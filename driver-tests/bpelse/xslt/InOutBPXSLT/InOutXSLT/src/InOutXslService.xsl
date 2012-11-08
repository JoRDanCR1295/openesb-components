<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:ns1="http://xml.netbeans.org/schema/XSLTBPSchema">
    <xsl:template match="/">
        <xsl:element name="ns1:MsgElem">
            <xsl:element name="intElem" namespace="">
                <xsl:value-of select=" ( /ns1:MsgElem/intElem + 1 ) "/>
            </xsl:element>
            <xsl:element name="strElem" namespace="">
                <xsl:value-of select="concat(/ns1:MsgElem/strElem, &apos; Added in the XSLT:&apos;)"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
