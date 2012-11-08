<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:imolacorba="uri://schemas.imola.it/jbi/wsdl-extensions/corba/">

<!--  Modify the imolacorba orb -->
<xsl:template match="imolacorba:orb">
	<imolacorba:orb>
		<imolacorba:property name="org.omg.CORBA.ORBClass" value="com.sun.corba.ee.impl.orb.ORBImpl"/>			
		<imolacorba:property name="org.omg.CORBA.ORBInitialPort" value="1050"/>
		<imolacorba:property name="org.omg.CORBA.ORBInitialHost" value="localhost"/>					
	</imolacorba:orb>	
</xsl:template>

<!-- identity -->
<xsl:template match="@*|*">
	<xsl:copy>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>