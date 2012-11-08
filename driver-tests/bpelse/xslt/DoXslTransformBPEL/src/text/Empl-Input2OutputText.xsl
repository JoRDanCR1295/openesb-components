<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:ns2="http://sun.com/EmplInput">
    <xsl:output method="text"/>
    <xsl:param name="var1">Bad Monkey</xsl:param>
    <xsl:template match="/">
        <xsl:value-of select="ns2:EmployeeInput/EmployeeNumber"/>,
        <xsl:value-of select="ns2:EmployeeInput/LastName"/>,
        <xsl:value-of select="ns2:EmployeeInput/FirstName"/>,
        <xsl:value-of select="concat(ns2:EmployeeInput/JobTitle, ' (aka ', $var1, ')')"/>,
        <xsl:value-of select="( ns2:EmployeeInput/HoursWorked * ns2:EmployeeInput/PayRate)"/>
    </xsl:template>
</xsl:stylesheet>
