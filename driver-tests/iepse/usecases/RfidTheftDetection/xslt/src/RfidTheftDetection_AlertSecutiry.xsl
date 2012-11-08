<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : RfidTheftDetection_AlertSecutiry.xsl
    Created on : May 6, 2005, 1:31 PM
    Author     : blu
    Description:
        Purpose of transformation follows.
-->

<!--
Translate from 
<jbi:message xmlns:msgns="RfidTheftDetection_iep" type="msgns:AlertSecutiry_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:AlertSecutiry_MsgObj>
      <msgns:productId>...</msgns:productId>
      <msgns:location>...</msgns:location>
      <msgns:Timestamp>...</msgns:Timestamp>
    </msgns:AlertSecutiry_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message xmlns:msgns="RfidTheftDetection_iep" type="msgns:AlertSecutiryFile_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:AlertSecutiryFile_MsgObj>
      <msgns:productId>...</msgns:productId>
      <msgns:location>...</msgns:location>
    </msgns:AlertSecutiryFile_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="RfidTheftDetection_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="RfidTheftDetection_iep" type="msgns:AlertSecutiryFile_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
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

    <xsl:template match="msgns:AlertSecutiry_MsgObj">
        <msgns:AlertSecutiryFile_MsgObj>
            <msgns:productId><xsl:value-of select="msgns:productId"/></msgns:productId>
            <msgns:location><xsl:value-of select="msgns:location"/></msgns:location>
        </msgns:AlertSecutiryFile_MsgObj>
    </xsl:template>
</xsl:stylesheet>