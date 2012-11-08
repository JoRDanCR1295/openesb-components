<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:imolaejb="uri://schemas.imola.it/jbi/wsdl-extensions/ejb/">

<!--  Modify the imolaejb orb -->
<xsl:template match="imolaejb:binding">
<imolaejb:binding>
	<imolaejb:orb>
		<imolaejb:property name="org.omg.CORBA.ORBClass" value="org.jacorb.orb.ORB"/>
		<imolaejb:property name="org.omg.CORBA.ORBSingletonClass" value="org.jacorb.orb.ORBSingleton"/>		
	</imolaejb:orb>
</imolaejb:binding>		
</xsl:template>

<!-- identity -->
<xsl:template match="@*|*">
	<xsl:copy>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>

            