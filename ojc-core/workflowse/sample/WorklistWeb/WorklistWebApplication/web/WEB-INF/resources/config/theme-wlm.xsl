<!--
    Copyright (C) 2006 Orbeon, Inc.

    This program is free software; you can redistribute it and/or modify it under the terms of the
    GNU Lesser General Public License as published by the Free Software Foundation; either version
    2.1 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU Lesser General Public License for more details.

    The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
-->
<xsl:stylesheet version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:f="http://orbeon.org/oxf/xml/formatting"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:xforms="http://www.w3.org/2002/xforms"
    xmlns:xxforms="http://orbeon.org/oxf/xml/xforms"
    xmlns:version="java:org.orbeon.oxf.common.Version">

    <!-- Get generic templates from plain theme -->
    <xsl:import href="theme-plain.xsl"/>

    <!-- This contains some useful request information -->
    <xsl:variable name="request" select="doc('input:request')" as="document-node()"/>

    <!-- List of applications -->
    <xsl:variable name="applications" select="doc('../apps-list.xml')" as="document-node()"/>
    <!-- Current application id -->
    <xsl:variable name="current-application-id" select="tokenize(doc('input:request')/*/request-path, '/')[2]" as="xs:string"/>
    <!-- Source viewer application id if relevant -->
    <xsl:variable name="is-source-viewer" select="$current-application-id = 'source-viewer'" as="xs:boolean"/>
    <xsl:variable name="source-viewer-application-id" select="if ($is-source-viewer) then tokenize(doc('input:request')/*/request-path, '/')[3] else ()" as="xs:string?"/>
    <!-- Try to obtain a meaningful title for the example -->
    <xsl:variable name="title" select="if (/xhtml:html/xhtml:head/xhtml:title != '')
                                       then /xhtml:html/xhtml:head/xhtml:title
                                       else if (/xhtml:html/xhtml:body/xhtml:h1)
                                            then (/xhtml:html/xhtml:body/xhtml:h1)[1]
                                            else '[Untitled]'" as="xs:string"/>
    <!-- Orbeon Forms version -->
    <xsl:variable name="orbeon-forms-version" select="version:getVersion()" as="xs:string"/>

    <!-- - - - - - - Themed page template - - - - - - -->
    <xsl:template match="/">
        <xhtml:html>
            <xhtml:head>
                <xhtml:title>Orbeon Forms Example Applications - <xsl:value-of select="$title"/></xhtml:title>
                <!-- Standard scripts/styles -->
                <!-- NOTE: The XForms engine may place additional scripts and stylesheets here as needed -->
                <xhtml:link rel="stylesheet" href="../config/theme/orbeon.css" type="text/css"/>
                <xhtml:link rel="stylesheet" href="../config/theme/xforms.css" type="text/css"/>
                <xhtml:link rel="stylesheet" href="../config/theme/xforms-widgets.css" type="text/css"/>
                <!-- Handle head elements -->
                <xsl:for-each select="/xhtml:html/xhtml:head/(xhtml:meta | xhtml:link | xhtml:style | xhtml:script)">
                    <xsl:element name="xhtml:{local-name()}" namespace="{namespace-uri()}">
                        <xsl:copy-of select="@*"/>
                        <xsl:apply-templates/>
                    </xsl:element>
                </xsl:for-each>
                <!-- Orbeon Forms version -->
                <xhtml:meta name="generator" content="Orbeon Forms {$orbeon-forms-version}"/>
            </xhtml:head>
            <xhtml:body bgcolor="white">
                <!-- Copy body attributes -->
                <xsl:apply-templates select="/xhtml:html/xhtml:body/@*"/>

                <xhtml:table id="main" width="100%" border="0" cellpadding="0" cellspacing="0">
                    
                    <xhtml:tr>
                        
                        <xhtml:td id="maincontent" valign="top" width="99%">
                            <xhtml:div class="maincontent">
                                <!-- Title -->
                                <xhtml:h1>
                                    <!-- Title -->
                                    <xsl:value-of select="$title"/>
                                </xhtml:h1>
                                <!-- Body -->
                                <xhtml:div id="mainbody">
                                    <xsl:apply-templates select="/xhtml:html/xhtml:body/node()"/>
                                </xhtml:div>
                            </xhtml:div>
                        </xhtml:td>
                    </xhtml:tr>
                </xhtml:table>
                
            </xhtml:body>
        </xhtml:html>
    </xsl:template>

</xsl:stylesheet>
