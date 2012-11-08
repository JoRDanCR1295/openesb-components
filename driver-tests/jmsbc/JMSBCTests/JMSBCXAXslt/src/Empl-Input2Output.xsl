<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:ns2="http://sun.com/JMSBCXATest/EmplInput">
    <xsl:template match="/">
        <xsl:element name="EmployeeOutput" namespace="http://sun.com/JMSBCXATest/EmplOutput">
            <xsl:element name="EmpId">
                <xsl:value-of select="/ns2:EmployeeInput/EmployeeNumber"/>
            </xsl:element>
            <xsl:element name="LName">
                <xsl:value-of select="/ns2:EmployeeInput/LastName"/>
            </xsl:element>
            <xsl:element name="FName">
                <xsl:value-of select="/ns2:EmployeeInput/FirstName"/>
            </xsl:element>
            <xsl:element name="Title">
                <xsl:value-of select="/ns2:EmployeeInput/JobTitle"/>
            </xsl:element>
            <xsl:element name="Amt">
                <xsl:value-of select="( /ns2:EmployeeInput/HoursWorked * /ns2:EmployeeInput/PayRate )"/>
            </xsl:element>
        </xsl:element>
    </xsl:template>
</xsl:stylesheet>
