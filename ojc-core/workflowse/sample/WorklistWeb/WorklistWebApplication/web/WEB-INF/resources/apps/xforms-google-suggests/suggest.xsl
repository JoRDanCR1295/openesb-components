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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        version="2.0">

    <xsl:variable name="query" as="xs:string" select="doc('input:instance')/query"/>
    <xsl:template match="/">
        <suggestions xsl:version="2.0">
            <xsl:if test="normalize-space($query) != ''">
                <!-- Get back Google response -->
                <xsl:variable name="google-response" as="element(html)" select="
                        doc(concat('http://www.google.com/complete/search?qu=', escape-html-uri($query), '&amp;js=false'))/html"/>
                <!-- Extract the interesting part of the JavaScript -->
                <xsl:variable name="suggestions-list" as="xs:string" select="
                        substring-before(substring-after($google-response/head/script, 'new Array(&quot;'), '&quot;), new Array(')"/>
                <!-- Generate on <suggestion> element for each suggestion we get from Google -->
                <xsl:for-each select="tokenize($suggestions-list, '&quot;, &quot;')">
                    <suggestion>
                        <xsl:value-of select="."/>
                    </suggestion>
                </xsl:for-each>
            </xsl:if>
        </suggestions>
    </xsl:template>
</xsl:stylesheet>
