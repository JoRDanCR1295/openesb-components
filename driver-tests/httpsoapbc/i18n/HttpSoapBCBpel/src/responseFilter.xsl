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
    xmlns:msgns="yahoonews" 
    type="msgns:Request_Msg" 
    version="1.0" 
    xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
  <jbi:part>
    <rss version="2.0" xmlns:mdeia="http://search.yahoo.com/mrss">
        <channel>
            <title>..</title>	
            <copyright>..</copyright>	
            <link>..</link>	
            <description>..</description>	
            <language>..</language>	
            <lastBuildDate>..</lastBuildDate>	
            <ttl>..</ttl>	
            <image>..</image>
            <item>..</item>*	
        </channel>
    </rss>
  </jbi:part>
</jbi:message>
to
<jbi:message 
    xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper"
    xmlns:msgns="xsltnewscounter" 
    type="msgns:Response_Msg" version="1.0">
  <jbi:part>
    <msgns:Response_MsgObj>
      <items>...</items>
    </msgns:Response_MsgObj>
  </jbi:part>
</jbi:message>
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:msgns0="yahoonews" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
    <xsl:output method="xml" indent="yes" encoding="UTF-8"/>
    <xsl:template match="/">
        <jbi:message xmlns:msgns="xsltnewscounter" type="msgns:Response_Msg" version="1.0" xmlns:jbi="http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper">
            <jbi:part>
                <msgns:Response_MsgObj>
                    <xsl:apply-templates/>
                </msgns:Response_MsgObj>
            </jbi:part>
        </jbi:message>
    </xsl:template>
    
    <xsl:template match="jbi:message">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="jbi:part">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="rss">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="channel">
        <items><xsl:value-of select="count(item)"/></items>
    </xsl:template>
</xsl:stylesheet>