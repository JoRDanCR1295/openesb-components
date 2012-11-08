<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : zmart_ExitAlarm.xsl
    Created on : May 6, 2005, 1:31 PM
    Author     : blu
    Description:
        Purpose of transformation follows.
-->

<!--
Translate from 
<jbi:message 
    xmlns:msgns="xsltnewscounter" 
    type="msgns:Request_Msg" 
    version="1.0" 
    xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:Request_MsgObj>
      <msgns:p1>...</msgns:p1>
      <msgns:p2>...</msgns:p2>
    </msgns:Request_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message 
    xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper"
    xmlns:msgns="yahoonews" 
    type="msgns:m1" version="1.0">
  <jbi:part>a</jbi:part>
  <jbi:part>b</jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns0="xsltnewscounter" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="yahoonews" type="msgns:m1" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
             <xsl:apply-templates/>
        </jbi:message>
    </xsl:template>

    <xsl:template match="jbi:message">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="jbi:part">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="msgns0:Request_MsgObj">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="p1">
        <jbi:part><xsl:value-of select="."/></jbi:part>
    </xsl:template>

    <xsl:template match="p2">
        <jbi:part><xsl:value-of select="."/></jbi:part>
    </xsl:template>
</xsl:stylesheet>