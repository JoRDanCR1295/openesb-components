<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:ns2="http://sun.com/EmplInputQual"
                xmlns:ns1="http://sun.com/EmplOutputQual">
    <!-- global parameter with default value not matching passed-in value -->
    <xsl:param name="var1">Bad Monkey</xsl:param>
    <xsl:template match="/">
        <ns1:EmployeeOutput>
            <ns1:EmpId>
                <xsl:value-of select="ns2:EmployeeInput/ns2:EmployeeNumber"/>
            </ns1:EmpId>
            <ns1:LName>
                <xsl:value-of select="ns2:EmployeeInput/ns2:LastName"/>
            </ns1:LName>
            <ns1:FName>
                <xsl:value-of select="ns2:EmployeeInput/ns2:FirstName"/>
            </ns1:FName>
            <ns1:Title>
                <xsl:value-of select="concat(ns2:EmployeeInput/ns2:JobTitle, ' (aka ', $var1, ')')"/>
            </ns1:Title>
            <ns1:Amt>
                <xsl:value-of select="( ns2:EmployeeInput/ns2:HoursWorked * ns2:EmployeeInput/ns2:PayRate)"/>
            </ns1:Amt>
        </ns1:EmployeeOutput>
    </xsl:template>
</xsl:stylesheet>
