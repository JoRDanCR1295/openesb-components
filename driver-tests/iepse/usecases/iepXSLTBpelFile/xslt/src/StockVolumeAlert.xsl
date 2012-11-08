<?xml version="1.0" encoding="UTF-8" ?>

<!--
    Document   : iepStockVolumeAlert.xsl
    Created on : July 4, 2006, 6:07 PM
    Author     : rdwivedi
    Description:
        Purpose of transformation follows.
-->
<!--
Translate from 
<jbi:message xmlns:msgns="iepStockVolumeAlert_iep" type="msgns:StreamOutput0_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:StreamOutput0_MsgObj>
   
      <SYMB>...</SYMB>
      <AVGVolumeA>...</AVGVolumeA>
      <AVGVolumeB>...</AVGVolumeB>
      
      <Timestamp>...</Timestamp>
    </msgns:StreamOutput0_MsgObj>
  </jbi:part>
</jbi:message>
to
<jbi:message xmlns:msgns="iepStockVolumeAlert_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <msgns:FileOut_MsgObj>
      <SYMB>...</SYMB>
      <AVGVolumeA>...</AVGVolumeA>
      <AVGVolumeB>...</AVGVolumeB>
    </msgns:FileOut_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns="iepStockVolumeAlert_iep" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="iepStockVolumeAlert_iep" type="msgns:FileOut_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
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
            <SYMB><xsl:value-of select="SYMB"/></SYMB>
            <AVGVolumeA><xsl:value-of select="AVGVolumeA"/></AVGVolumeA>
            <AVGVolumeB><xsl:value-of select="AVGVolumeB"/></AVGVolumeB>
            
        </msgns:FileOut_MsgObj>    
    </xsl:template>
</xsl:stylesheet>