<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:ns1="http://service.hello/">
    <xsl:template match="/">
        <xsl:element name="ns1:sayHelloResponse">
            <xsl:element name="return" namespace="">
                <xsl:value-of select="concat(&apos;Hello &apos;, /ns1:sayHelloRequest/name, &apos;!&apos;)"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
