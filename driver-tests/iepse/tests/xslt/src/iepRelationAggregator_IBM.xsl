<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : iepRelationAggregator_IBM.xsl
    Created on : May 6, 2005, 1:31 PM
    Author     : blu
    Description:
        Purpose of transformation follows.
-->

<!--
Translate from 
<jbi:message xmlns:msgns="iepRelationAggregator_iep" type="msgns:IBM_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:IBM_MsgObj>
      <msgns:min0>...</msgns:min0>
      <msgns:avg0>...</msgns:avg0>
      <msgns:max0>...</msgns:max0>
      <msgns:Timestamp>...</msgns:Timestamp>
    </msgns:IBM_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message xmlns:msgns="iepRelationAggregator_iep" type="msgns:IBMFile_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:IBMFile_MsgObj>
      <msgns:min>...</msgns:min>
      <msgns:avg>...</msgns:avg>
      <msgns:max>...</msgns:max>
    </msgns:IBMFile_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="iepRelationAggregator_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="iepRelationAggregator_iep" type="msgns:IBMFile_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
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

    <xsl:template match="msgns:IBM_MsgObj">
        <msgns:IBMFile_MsgObj>
            <msgns:min><xsl:value-of select="msgns:min0"/></msgns:min>
            <msgns:avg><xsl:value-of select="msgns:avg0"/></msgns:avg>
            <msgns:max><xsl:value-of select="msgns:max0"/></msgns:max>
        </msgns:IBMFile_MsgObj>
    </xsl:template>
</xsl:stylesheet>