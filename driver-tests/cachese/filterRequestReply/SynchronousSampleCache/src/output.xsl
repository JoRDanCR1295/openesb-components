<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tns="http://localhost/SynchronousSample/SynchronousSample"
                xmlns:ns0="http://j2ee.netbeans.org/wsdl/CacheAspect">
    
    <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
    <xsl:strip-space elements="*"/>
    
    <xsl:template match="node()[not(self::*)] | @* ">
        <xsl:copy>
            <xsl:apply-templates select="@* | node()"/>
        </xsl:copy>
    </xsl:template>
    
    <xsl:template match="*">
        <xsl:element name="{name()}">
            <xsl:copy-of select="namespace::*[name() != 'tns']"/>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template> 
    
    <xsl:template match="tns:*">
        <xsl:element name="ns0:{local-name()}">
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template> 
    
</xsl:stylesheet>

