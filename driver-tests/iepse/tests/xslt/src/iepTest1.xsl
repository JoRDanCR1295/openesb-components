<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : iepTest.xsl.xsl
    Created on : May 4, 2005, 6:07 PM
    Author     : blu
    Description:
        Purpose of transformation follows.
-->
<!--
Translate from 
<jbi:message xmlns:msgns="iepTest1_iep" type="msgns:StreamOutput0_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:StreamOutput0_MsgObj>
      <msgns:name>...</msgns:name>
      <msgns:value>...</msgns:value>
      <msgns:Timestamp>...</msgns:Timestamp>
    </msgns:StreamOutput0_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message xmlns:msgns="iepTest1_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:FileOut_MsgObj>
      <msgns:name>...</msgns:name>
      <msgns:value>...</msgns:value>
    </msgns:FileOut_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="iepTest1_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="iepTest1_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
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
            <msgns:name><xsl:value-of select="msgns:name"/></msgns:name>
            <msgns:value><xsl:value-of select="msgns:value"/></msgns:value>
        </msgns:FileOut_MsgObj>
    </xsl:template>
</xsl:stylesheet>