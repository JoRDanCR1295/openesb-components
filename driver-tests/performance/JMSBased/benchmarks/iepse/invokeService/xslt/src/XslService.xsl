<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:ns1="test_iep">
    <xsl:template match="/">
        <xsl:element name="ns1:InvokeService0_ResponseObj">
            <xsl:element name="InvokeService0_ResponseItem" namespace="">
                <xsl:element name="msgNid" namespace="">
                    <xsl:value-of select="concat(/ns1:InvokeService0_RequestObj/message, /ns1:InvokeService0_RequestObj/id)"/>
                </xsl:element>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
