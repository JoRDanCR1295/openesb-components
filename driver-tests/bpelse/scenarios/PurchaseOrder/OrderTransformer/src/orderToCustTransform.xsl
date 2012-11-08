<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
    xmlns:ns="http://xml.netbeans.org/schema/orders">
    <xsl:output method="xml" version="1.0" encoding="UTF-8"/>
    <xsl:template match="ns:Orders">
        <CustomerHistoryEntries xmlns="http://xml.netbeans.org/schema/custhistory"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://xml.netbeans.org/schema/custhistory custhistory.xsd">
            <xsl:apply-templates/>
        </CustomerHistoryEntries>
    </xsl:template>
    <xsl:template match="ns:Order">
        <CustomerHistoryEntry xmlns="http://xml.netbeans.org/schema/custhistory">
            <CustomerNumber>
                <xsl:apply-templates select="./ns:OrderHeader/ns:CUST_NO"/>
            </CustomerNumber>
            <OrderLookupInfo>
                <xsl:apply-templates select="./ns:OrderKey"/>
                <xsl:apply-templates select="./ns:OrderHeader/ns:PURCH_ORD_NO"/>
                <xsl:apply-templates select="./ns:OrderItems/ns:item/ns:ITM_NUMBER"/>
                <xsl:apply-templates select="./ns:OrderText"/>        
            </OrderLookupInfo>
        </CustomerHistoryEntry>
    </xsl:template>
    <xsl:template match="ns:CUST_NO">
        <xsl:value-of select="."/>
    </xsl:template>
    <xsl:template match="ns:OrderKey">
        <OrderNumber xmlns="http://xml.netbeans.org/schema/custhistory">
            <xsl:value-of select="."/>
        </OrderNumber>
    </xsl:template>
    <xsl:template match="ns:PURCH_ORD_NO">
        <PURCH_ORD_NO xmlns="http://xml.netbeans.org/schema/custhistory">
            <xsl:value-of select="."/>
        </PURCH_ORD_NO>
    </xsl:template>
    <xsl:template match="ns:ITM_NUMBER">
        <ITM_NUMBER xmlns="http://xml.netbeans.org/schema/custhistory">
            <xsl:value-of select="."/>
        </ITM_NUMBER>
    </xsl:template>
    <xsl:template match="ns:OrderText">
        <OrderText xmlns="http://xml.netbeans.org/schema/custhistory">
            <xsl:value-of select="."/>
        </OrderText>
    </xsl:template>
    
</xsl:stylesheet>
