<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:imolaejb="uri://schemas.imola.it/jbi/wsdl-extensions/ejb/">

<!-- sets the application server -->
<xsl:variable name="corbaName" select="document('../corbaNameConfiguration.xml')/corbaNames/websphere6"/>


<!--  Modify the imolaejb address -->
<xsl:template match="imolaejb:address">
	<imolaejb:address localizationType="corbaname">
		<xsl:attribute name="name">
			<xsl:value-of select="$corbaName"/>
		</xsl:attribute>
	</imolaejb:address>  
</xsl:template>

<!-- identity -->
<xsl:template match="@*|*">
	<xsl:copy>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>