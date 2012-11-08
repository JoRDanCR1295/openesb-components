<!DOCTYPE stylesheet [
  <!ENTITY tab "<xsl:text>&#9;</xsl:text>">
  <!ENTITY cr "<xsl:text>&#xA;</xsl:text>">
]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:jbi="http://java.sun.com/xml/ns/jbi"
    exclude-result-prefixes="jbi">
    <xsl:template match="/">
<project name="cdk-compiler" default="compile-project" basedir="..">&cr;&tab;
    <property environment="env"/>&cr;&tab;
    <property name="gf.home" value="${{env.JV_GFBASE}}"/>&cr;&tab;
    <property name="ojc" value="${{env.JV_SRCROOT}}"/>&cr;&tab;
    <property name="jbi" value="${{basedir}}/jbiadapter"/>&cr;&tab;
    <property name="pkg" value="${{basedir}}/packaging"/>&cr;&tab;
    <property file="${{basedir}}/build.properties"/>&cr;&tab;
    
    <xsl:param name="compile.depends"/>&cr;
    &tab;
    <path id="proj.classpath">&cr;&tab;&tab;
        <fileset dir="${{pkg}}">&cr;&tab;&tab;&tab;
            <include name="**/*.jar"/>&cr;&tab;&tab;
        </fileset>&cr;&tab;
    </path>&cr;
    &cr;&tab;
    <xsl:comment>Generates component installer jar</xsl:comment>&cr;&tab;
    <target name="compile-project" depends="clean,{$compile.depends}"/>&cr;
    &cr;&tab;
    <target name="clean">&cr;&tab;&tab;
        <delete dir="${{dist}}"/>&cr;&tab;
    </target>&cr;
    &cr;&tab;
    <target name="jbiadapter">&cr;&tab;&tab;
        <echo>${basedir}</echo>&cr;&tab;&tab;
        <mkdir dir="${{jbi}}/bld"/>&cr;&tab;&tab;
        <mkdir dir="${{jbi}}/bld/classes"/>&cr;&tab;&tab;
        <javac srcdir="${{jbi}}/src" destdir="${{jbi}}/bld/classes">&cr;&tab;&tab;&tab;
            <classpath refid="proj.classpath"/>&cr;&tab;&tab;
        </javac>&cr;&tab;&tab;
        <copy todir="${{jbi}}/bld/classes">&cr;&tab;&tab;&tab;
            <fileset dir="${{jbi}}/src">&cr;&tab;&tab;&tab;&tab;
                <exclude name="**/*.java"/>&cr;&tab;&tab;&tab;
            </fileset>&cr;&tab;&tab;
        </copy>&cr;&tab;&tab;
        <jar destfile="${{jbi}}/bld/${{%PROJ_SHORT_LC%}}jbiadapter.jar" basedir="${{jbi}}/bld/classes"/>&cr;&tab;&tab;
        <copy todir="${{dist}}/lib" file="${{jbi}}/bld/${{%PROJ_SHORT_LC%}}jbiadapter.jar">&cr;&tab;&tab;&tab;
            <xsl:comment>Rename jbiadapter jar to coincide with OJC convention</xsl:comment>&cr;&tab;&tab;&tab;
            <mapper type="glob" from="${{%PROJ_SHORT_LC%}}*" to="${{%PROJ_NAME%}}-*"/>&cr;&tab;&tab;
        </copy>&cr;&tab;
    </target>&cr;
&cr;&tab;
    <target name="packaging">&cr;&tab;&tab;
        <echo>DIST: ${dist}</echo>&cr;&tab;&tab;
        <mkdir dir="${{dist}}/META-INF"/>&cr;&tab;&tab;
        <copy todir="${{dist}}/META-INF" file="${{pkg}}/src/jbi.xml"/>&cr;&tab;&tab;
        <copy todir="${{dist}}">&cr;&tab;&tab;&tab;
            <fileset dir="${{pkg}}">&cr;&tab;&tab;&tab;&tab;
                <include name="**/*.jar"/>&cr;&tab;&tab;&tab;&tab;
                <exclude name="**/jbi.jar"/>&cr;&tab;&tab;&tab;&tab;
                <exclude name="**/bld/**"/>&cr;&tab;&tab;&tab;
            </fileset>&cr;&tab;&tab;
        </copy>&cr;&tab;&tab;
        <jar destfile="${{pkg}}/bld/${{%PROJ_NAME%}}-installer.jar" basedir="${{dist}}"/>&cr;&tab;
    </target>&cr;
    &cr;
    <!--
    <xsl:for-each select="project/modules/module[text() != 'jbiadapter' and text() != 'packaging']">
        <xsl:variable name="modName" select="text()"/>&tab;
        <target name="{$modName}"/>&cr;
    </xsl:for-each>
    -->
</project>&cr;
    </xsl:template>
</xsl:stylesheet>
