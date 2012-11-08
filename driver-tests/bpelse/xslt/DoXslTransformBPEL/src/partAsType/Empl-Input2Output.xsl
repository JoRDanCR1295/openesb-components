<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:ns2="http://sun.com/EmplInput">
    <!-- global parameter with default value not matching passed-in value -->
    <xsl:param name="var1">Bad Monkey</xsl:param>
    <!-- Matching anonymous element because input document is undefined, per BPEL spec -->
    <xsl:template match="/*">
        <part1>
            <xsl:element name="EmpId">
                <xsl:value-of select="EmployeeNumber"/>
            </xsl:element>
            <xsl:element name="LName">
                <xsl:value-of select="LastName"/>
            </xsl:element>
            <xsl:element name="FName">
                <xsl:value-of select="FirstName"/>
            </xsl:element>
            <xsl:element name="Title">
                <xsl:value-of select="concat(JobTitle, ' (aka ', $var1, ')')"/>
            </xsl:element>
            <xsl:element name="Amt">
                <xsl:value-of select="( HoursWorked * PayRate)"/>
            </xsl:element>
        </part1>
    </xsl:template>
</xsl:stylesheet>
