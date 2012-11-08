<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:ns1="http://j2ee.netbeans.org/wsdl/MessageTypes">
    <xsl:template match="/">
        <xsl:element name="ns1:MessageElement1">
            <xsl:element name="Counter" namespace="">
                <xsl:value-of select="/ns1:MessageElement1/Counter"/>
            </xsl:element>
            <xsl:element name="Text" namespace="">
                <xsl:value-of select="/ns1:MessageElement1/Text"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
