<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:imolacorba="uri://schemas.imola.it/jbi/wsdl-extensions/corba/">

<!--  Modify the imolaejb orb -->
<xsl:template match="imolacorba:orb">
	<imolacorba:orb>
		<imolacorba:property name="org.omg.CORBA.ORBClass" value="org.jacorb.orb.ORB"/>
		<imolacorba:property name="org.omg.CORBA.ORBSingletonClass" value="org.jacorb.orb.ORBSingleton"/>
    <imolacorba:property name="ORBInitRef.NameService" value="corbaloc::localhost:1050/NameService"/>		
	</imolacorba:orb>
</xsl:template>

<!-- identity -->
<xsl:template match="@*|*">
	<xsl:copy>
		<xsl:apply-templates select="@*|node()"/>
	</xsl:copy>
</xsl:template>

</xsl:stylesheet>

            