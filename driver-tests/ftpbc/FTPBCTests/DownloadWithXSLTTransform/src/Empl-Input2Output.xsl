<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ns2="http://sun.com/UploadTest/EmplOutput">
    <xsl:template match="/">
        <xsl:element name="EmployeeOutput" namespace="http://sun.com/DownloadTest/EmplOutput">
            <xsl:element name="EmployeeNumber">
                <xsl:value-of select="/ns2:EmployeeOutput/EmpId"/>
            </xsl:element>
            <xsl:element name="LastName">
                <xsl:value-of select="/ns2:EmployeeOutput/LName"/>
            </xsl:element>
            <xsl:element name="FirstName">
                <xsl:value-of select="/ns2:EmployeeOutput/FName"/>
            </xsl:element>
            <xsl:element name="JobTitle">
                <xsl:value-of select="/ns2:EmployeeOutput/Title"/>
            </xsl:element>
            <xsl:element name="Pay">
                <xsl:value-of select="( /ns2:EmployeeOutput/Amt + 1000)"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
