<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : iepTupleSerialCorrelation.xsl
    Created on : May 4, 2005, 6:07 PM
    Author     : blu
    Description:
        Purpose of transformation follows.
-->
<!--
Translate from 
<jbi:message xmlns:msgns="iepTupleSerialCorrelation_iep" type="msgns:StreamOutput0_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:StreamOutput0_MsgObj>
      <msgns:value_0>...</msgns:value_0>
      <msgns:value_1>...</msgns:value_1>
      <msgns:value_2>...</msgns:value_2>
      <msgns:Timestamp>...</msgns:Timestamp>
    </msgns:StreamOutput0_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message xmlns:msgns="iepTupleSerialCorrelation_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:FileOut_MsgObj>
      <msgns:value_0>...</msgns:value_0>
      <msgns:value_1>...</msgns:value_1>
      <msgns:value_2>...</msgns:value_2>
    </msgns:FileOut_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="iepTupleSerialCorrelation_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="iepTupleSerialCorrelation_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
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
            <msgns:value_0><xsl:value-of select="msgns:value_0"/></msgns:value_0>
            <msgns:value_1><xsl:value-of select="msgns:value_1"/></msgns:value_1>
            <msgns:value_2><xsl:value-of select="msgns:value_2"/></msgns:value_2>
        </msgns:FileOut_MsgObj>
    </xsl:template>
</xsl:stylesheet>