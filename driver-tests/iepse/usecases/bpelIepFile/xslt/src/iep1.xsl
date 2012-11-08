<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : iep1.xsl
    Created on : July 4, 2006, 6:07 PM
    Author     : rdwivedi
    Description:
        Purpose of transformation follows.
-->

                    
                    
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="iep1_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="iep1_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
            <xsl:apply-templates/>
        </jbi:message>
    </xsl:template>

    <xsl:template match="jbi:message">
        <jbi:part>
            <xsl:apply-templates/>
        </jbi:part>
    </xsl:template>
    
    <xsl:template match="jbi:part">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="msgns:StreamOutput0_MsgObj">
        <msgns:FileOut_MsgObj>
            <ProdID><xsl:value-of select="ProdID"/></ProdID>
            <ProdName><xsl:value-of select="ProdName"/></ProdName>
            <ProdQuantity><xsl:value-of select="ProdQuantity"/></ProdQuantity>
            
        </msgns:FileOut_MsgObj>    
    </xsl:template>
</xsl:stylesheet>