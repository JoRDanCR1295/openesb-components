<?xml version="1.0" encoding="UTF-8"?>
<!--
The contents of this file are subject to the terms of the Common Development
and Distribution License (the License). You may not use this file except in
compliance with the License.

You can obtain a copy of the License at http://www.netbeans.org/cddl.html
or http://www.netbeans.org/cddl.txt.

When distributing Covered Code, include this CDDL Header Notice in each file
and include the License file at http://www.netbeans.org/cddl.txt.
If applicable, add the following below the CDDL Header, with the fields
enclosed by brackets [] replaced by your own identifying information:
"Portions Copyrighted [year] [name of copyright owner]"

The Original Software is NetBeans. The Initial Developer of the Original
Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
Microsystems, Inc. All Rights Reserved.
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:p="http://www.netbeans.org/ns/project/1"
                xmlns:xalan="http://xml.apache.org/xslt"
                xmlns:jbiproject="http://www.netbeans.org/ns/jbi-component-project/1"
                xmlns:projdeps="http://www.netbeans.org/ns/ant-project-references/1"
                xmlns:projdeps2="http://www.netbeans.org/ns/ant-project-references/2"
                exclude-result-prefixes="xalan p projdeps projdeps2">
<!-- XXX should use namespaces for NB in-VM tasks from ant/browsetask and debuggerjpda/ant (Ant 1.6.1 and higher only) -->
    <xsl:output method="xml" indent="yes" encoding="UTF-8" xalan:indent-amount="4"/>
    <xsl:template match="/">

        <xsl:comment><![CDATA[
*** GENERATED FROM project.xml - DO NOT EDIT  ***
***         EDIT ../build.xml INSTEAD         ***

For the purpose of easier reading the script
is divided into following sections:

  - initialization
  - compilation
  - jar
  - execution
  - debugging
  - javadoc
  - junit compilation
  - junit execution
  - junit debugging
  - cleanup

]]></xsl:comment>

        <xsl:variable name="name" select="/p:project/p:configuration/jbiproject:data/jbiproject:name"/>
        <!-- Synch with build-impl.xsl: -->
        <xsl:variable name="codename" select="translate($name, ' ', '_')"/>
        <project name="{$codename}-impl">
            <xsl:attribute name="default">default</xsl:attribute>
            <xsl:attribute name="basedir">..</xsl:attribute>

            <import file="jbiadmin-impl.xml"/>
            
            <target name="default">
                <xsl:attribute name="depends">test,jar,javadoc</xsl:attribute>
                <xsl:attribute name="description">Build and test whole project.</xsl:attribute>
            </target>

            <xsl:comment> 
    ======================
    INITIALIZATION SECTION 
    ======================
    </xsl:comment>

            <target name="-pre-init">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-init-private">
                <xsl:attribute name="depends">-pre-init</xsl:attribute>
                <property file="nbproject/private/private.properties"/>
            </target>

            <target name="-init-user">
                <xsl:attribute name="depends">-pre-init,-init-private</xsl:attribute>
                <property file="${{user.properties.file}}"/>
                <xsl:comment> The two properties below are usually overridden </xsl:comment>
                <xsl:comment> by the active platform. Just a fallback. </xsl:comment>
                <property name="default.javac.source" value="1.4"/>
                <property name="default.javac.target" value="1.4"/>
            </target>

            <target name="-init-project">
                <xsl:attribute name="depends">-pre-init,-init-private,-init-user</xsl:attribute>
                <property file="nbproject/project.properties"/>
            </target>

            <target name="-do-init">
                <xsl:attribute name="depends">-pre-init,-init-private,-init-user,-init-project,-init-macrodef-property</xsl:attribute>
                <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                    <jbiproject:property name="platform.home" value="platforms.${{platform.active}}.home"/>
                    <jbiproject:property name="platform.bootcp" value="platforms.${{platform.active}}.bootclasspath"/>
                    <jbiproject:property name="platform.compiler" value="platforms.${{platform.active}}.compile"/>
                    <jbiproject:property name="platform.javac.tmp" value="platforms.${{platform.active}}.javac"/>
                    <condition property="platform.javac" value="${{platform.home}}/bin/javac">
                        <equals arg1="${{platform.javac.tmp}}" arg2="$${{platforms.${{platform.active}}.javac}}"/>
                    </condition>
                    <property name="platform.javac" value="${{platform.javac.tmp}}"/>
                    <jbiproject:property name="platform.java.tmp" value="platforms.${{platform.active}}.java"/>
                    <condition property="platform.java" value="${{platform.home}}/bin/java">
                        <equals arg1="${{platform.java.tmp}}" arg2="$${{platforms.${{platform.active}}.java}}"/>
                    </condition>
                    <property name="platform.java" value="${{platform.java.tmp}}"/>
                    <jbiproject:property name="platform.javadoc.tmp" value="platforms.${{platform.active}}.javadoc"/>
                    <condition property="platform.javadoc" value="${{platform.home}}/bin/javadoc">
                        <equals arg1="${{platform.javadoc.tmp}}" arg2="$${{platforms.${{platform.active}}.javadoc}}"/>
                    </condition>
                    <property name="platform.javadoc" value="${{platform.javadoc.tmp}}"/>
                    <condition property="platform.invalid" value="true">
                        <or>
                            <contains string="${{platform.javac}}" substring="$${{platforms."/>
                            <contains string="${{platform.java}}" substring="$${{platforms."/>
                            <contains string="${{platform.javadoc}}" substring="$${{platforms."/>
                        </or>
                    </condition>
                    <fail unless="platform.home">Must set platform.home</fail>
                    <fail unless="platform.bootcp">Must set platform.bootcp</fail>
                    <fail unless="platform.java">Must set platform.java</fail>
                    <fail unless="platform.javac">Must set platform.javac</fail>
                    <fail if="platform.invalid">Platform is not correctly set up</fail>
                </xsl:if>
                <available file="${{manifest.file}}" property="manifest.available"/>
                <condition property="manifest.available+main.class">
                    <and>
                        <isset property="manifest.available"/>
                        <isset property="main.class"/>
                        <not>
                            <equals arg1="${{main.class}}" arg2="" trim="true"/>
                        </not>
                    </and>
                </condition>
                <condition property="manifest.available+main.class+mkdist.available">
                    <and>
                        <istrue value="${{manifest.available+main.class}}"/>
                        <isset property="libs.CopyLibs.classpath"/>
                    </and>
                </condition>
                <xsl:call-template name="createRootAvailableTest">
                    <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                    <xsl:with-param name="propName">have.tests</xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="createRootAvailableTest">
                    <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                    <xsl:with-param name="propName">have.sources</xsl:with-param>
                </xsl:call-template>
                <condition property="netbeans.home+have.tests">
                    <and>
                        <isset property="netbeans.home"/>
                        <isset property="have.tests"/>
                    </and>
                </condition>
                <condition property="no.javadoc.preview">
                    <isfalse value="${{javadoc.preview}}"/>
                </condition>
                <property name="run.jvmargs" value=""/>
                <property name="javac.compilerargs" value=""/>
                <property name="work.dir" value="${{basedir}}"/>
                <condition property="no.deps">
                    <and>
                        <istrue value="${{no.dependencies}}"/>
                    </and>
                </condition>
                <property name="javac.debug" value="true"/>
                <property name="javadoc.preview" value="true"/>
            </target>

            <target name="-post-init">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-init-check">
                <xsl:attribute name="depends">-pre-init,-init-private,-init-user,-init-project,-do-init</xsl:attribute>
                <!-- XXX XSLT 2.0 would make it possible to use a for-each here -->
                <!-- Note that if the properties were defined in project.xml that would be easy -->
                <!-- But required props should be defined by the AntBasedProjectType, not stored in each project -->
                <xsl:call-template name="createSourcePathValidityTest">
                    <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                </xsl:call-template>
                <xsl:call-template name="createSourcePathValidityTest">
                    <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                </xsl:call-template>
                <fail unless="build.dir">Must set build.dir</fail>
                <fail unless="dist.dir">Must set dist.dir</fail>
                <fail unless="build.classes.dir">Must set build.classes.dir</fail>
                <fail unless="dist.javadoc.dir">Must set dist.javadoc.dir</fail>
                <fail unless="build.test.classes.dir">Must set build.test.classes.dir</fail>
                <fail unless="build.test.results.dir">Must set build.test.results.dir</fail>
                <fail unless="build.classes.excludes">Must set build.classes.excludes</fail>
                <fail unless="dist.jar">Must set dist.jar</fail>
            </target>

            <target name="-init-macrodef-property">
                <macrodef>
                    <xsl:attribute name="name">property</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">name</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">value</xsl:attribute>
                    </attribute>
                    <sequential>
                        <property name="@{{name}}" value="${{@{{value}}}}"/>
                    </sequential>
                  </macrodef>
            </target>
            
            <target name="-init-macrodef-javac">
                <macrodef>
                    <xsl:attribute name="name">javac</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">srcdir</xsl:attribute>
                        <xsl:attribute name="default">
                            <xsl:call-template name="createPath">
                                <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                            </xsl:call-template>
                        </xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">destdir</xsl:attribute>
                        <xsl:attribute name="default">${build.classes.dir}</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">classpath</xsl:attribute>
                        <xsl:attribute name="default">${javac.classpath}</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">debug</xsl:attribute>
                        <xsl:attribute name="default">${javac.debug}</xsl:attribute>
                    </attribute>
                    <element>
                        <xsl:attribute name="name">customize</xsl:attribute>
                        <xsl:attribute name="optional">true</xsl:attribute>
                    </element>
                    <sequential>
                        <javac>
                            
                            <xsl:attribute name="srcdir">@{srcdir}</xsl:attribute>
                            <xsl:attribute name="destdir">@{destdir}</xsl:attribute>
                            <xsl:attribute name="debug">@{debug}</xsl:attribute>
                            <xsl:attribute name="deprecation">${javac.deprecation}</xsl:attribute>
                            <xsl:if test ="not(/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform/@explicit-source-supported ='false')">                            
                                <xsl:attribute name="source">${javac.source}</xsl:attribute>
                                <xsl:attribute name="target">${javac.target}</xsl:attribute>
                                <xsl:attribute name="fork">yes</xsl:attribute>
                            </xsl:if>                            
                            <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                                <xsl:attribute name="fork">yes</xsl:attribute>
                                <xsl:attribute name="executable">${platform.javac}</xsl:attribute>
                                <xsl:attribute name="tempdir">${java.io.tmpdir}</xsl:attribute> <!-- XXX cf. #51482, Ant #29391 -->
                            </xsl:if>
                            <xsl:attribute name="includeantruntime">false</xsl:attribute>
                            <classpath>
                                <path path="@{{classpath}}"/>
                            </classpath>
                            <compilerarg line="${{javac.compilerargs}}"/>
                            <customize/>
                        </javac>
                    </sequential>
                 </macrodef>
            </target>

            <target name="-init-macrodef-junit">
                <macrodef>
                    <xsl:attribute name="name">junit</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">includes</xsl:attribute>
                        <xsl:attribute name="default">**/*Test.java</xsl:attribute>
                    </attribute>
                    <sequential>
                        <junit>
                            <xsl:attribute name="showoutput">true</xsl:attribute>
                            <xsl:attribute name="fork">true</xsl:attribute>
                            <xsl:attribute name="dir">${basedir}</xsl:attribute> <!-- #47474: match <java> --> 
                            <xsl:attribute name="failureproperty">tests.failed</xsl:attribute>
                            <xsl:attribute name="errorproperty">tests.failed</xsl:attribute>
                            <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                                <xsl:attribute name="jvm">${platform.java}</xsl:attribute>
                            </xsl:if>
                            <batchtest todir="${{build.test.results.dir}}">
                                <xsl:call-template name="createFilesets">
                                    <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                                    <xsl:with-param name="includes">@{includes}</xsl:with-param>
                                </xsl:call-template>
                            </batchtest>
                            <classpath>
                                <path path="${{run.test.classpath}}"/>
                            </classpath>
                            <syspropertyset>
                                <propertyref prefix="test-sys-prop."/>
                                <mapper type="glob" from="test-sys-prop.*" to="*"/>
                            </syspropertyset>
                            <formatter type="brief" usefile="false"/>
                            <formatter type="xml"/>
                            <jvmarg line="${{run.jvmargs}}"/>
                        </junit>
                    </sequential>
                </macrodef>
            </target>

            <target name="-init-macrodef-nbjpda">
                <macrodef>
                    <xsl:attribute name="name">nbjpdastart</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">name</xsl:attribute>
                        <xsl:attribute name="default">${main.class}</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">classpath</xsl:attribute>
                        <xsl:attribute name="default">${debug.classpath}</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">stopclassname</xsl:attribute>
                        <xsl:attribute name="default"></xsl:attribute>
                    </attribute>
                    <sequential>
                        <nbjpdastart transport="dt_socket" addressproperty="jpda.address" name="@{{name}}" stopclassname="@{{stopclassname}}">
                            <classpath>
                                <path path="@{{classpath}}"/>
                            </classpath>
                            <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                                <bootclasspath>
                                    <path path="${{platform.bootcp}}"/>
                                </bootclasspath>
                            </xsl:if>
                        </nbjpdastart>
                    </sequential>
                </macrodef>
                <macrodef>
                    <xsl:attribute name="name">nbjpdareload</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">dir</xsl:attribute>
                        <xsl:attribute name="default">${build.classes.dir}</xsl:attribute>
                    </attribute>
                    <sequential>
                        <nbjpdareload>
                            <fileset includes="${{fix.includes}}*.class" dir="@{{dir}}"/>
                        </nbjpdareload>
                    </sequential>
                </macrodef>
            </target>

            <target name="-init-macrodef-debug">
                <macrodef>
                    <xsl:attribute name="name">debug</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">classname</xsl:attribute>
                        <xsl:attribute name="default">${main.class}</xsl:attribute>
                    </attribute>
                    <attribute>
                        <xsl:attribute name="name">classpath</xsl:attribute>
                        <xsl:attribute name="default">${debug.classpath}</xsl:attribute>
                    </attribute>
                    <element>
                        <xsl:attribute name="name">customize</xsl:attribute>
                        <xsl:attribute name="optional">true</xsl:attribute>
                    </element>
                    <sequential>
                        <java fork="true" classname="@{{classname}}">
                            <xsl:attribute name="dir">${work.dir}</xsl:attribute>
                            <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                                <xsl:attribute name="jvm">${platform.java}</xsl:attribute>
                            </xsl:if>
                            <jvmarg value="-Xdebug"/>
                            <jvmarg value="-Xnoagent"/>
                            <jvmarg value="-Djava.compiler=none"/>
                            <jvmarg value="-Xrunjdwp:transport=dt_socket,address=${{jpda.address}}"/>
                            <jvmarg line="${{run.jvmargs}}"/>
                            <classpath>
                                <path path="@{{classpath}}"/>
                            </classpath>
                            <syspropertyset>
                                <propertyref prefix="run-sys-prop."/>
                                <mapper type="glob" from="run-sys-prop.*" to="*"/>
                            </syspropertyset>
                            <customize/>
                        </java>
                    </sequential>
                </macrodef>
            </target>
            
            <target name="-init-macrodef-java">
                <macrodef>
                    <xsl:attribute name="name">java</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <attribute>
                        <xsl:attribute name="name">classname</xsl:attribute>
                        <xsl:attribute name="default">${main.class}</xsl:attribute>
                    </attribute>
                    <element>
                        <xsl:attribute name="name">customize</xsl:attribute>
                        <xsl:attribute name="optional">true</xsl:attribute>
                    </element>
                    <sequential>
                        <java fork="true" classname="@{{classname}}">
                            <xsl:attribute name="dir">${work.dir}</xsl:attribute>
                            <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                                <xsl:attribute name="jvm">${platform.java}</xsl:attribute>
                            </xsl:if>
                            <jvmarg line="${{run.jvmargs}}"/>
                            <classpath>
                                <path path="${{run.classpath}}"/>
                            </classpath>
                            <syspropertyset>
                                <propertyref prefix="run-sys-prop."/>
                                <mapper type="glob" from="run-sys-prop.*" to="*"/>
                            </syspropertyset>
                            <customize/>
                        </java>
                    </sequential>
                </macrodef>
            </target>

            <target name="-init-presetdef-jar">
                <presetdef>
                    <xsl:attribute name="name">jar</xsl:attribute>
                    <xsl:attribute name="uri">http://www.netbeans.org/ns/jbi-component-project/1</xsl:attribute>
                    <jar jarfile="${{dist.jar}}" compress="${{jar.compress}}">
                        <jbiproject:fileset dir="${{build.classes.dir}}"/>
                        <!-- XXX should have a property serving as the excludes list -->
                    </jar>
                </presetdef>
            </target>
            
            <target name="init">
                <xsl:attribute name="depends">-pre-init,-init-private,-init-user,-init-project,-do-init,-post-init,-init-check,-init-macrodef-property,-init-macrodef-javac,-init-macrodef-junit,-init-macrodef-nbjpda,-init-macrodef-debug,-init-macrodef-java,-init-presetdef-jar</xsl:attribute>
            </target>

            <xsl:comment>
    ===================
    COMPILATION SECTION
    ===================
    </xsl:comment>

            <xsl:call-template name="deps.target">
                <xsl:with-param name="targetname" select="'deps-jar'"/>
                <xsl:with-param name="type" select="'jar'"/>
            </xsl:call-template>
                        
            <target name="-pre-pre-compile">
                <xsl:attribute name="depends">init,deps-jar</xsl:attribute>
                <mkdir dir="${{build.classes.dir}}"/>
            </target>

            <target name="-pre-compile">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-do-compile">
                <xsl:attribute name="depends">init,deps-jar,-pre-pre-compile,-pre-compile</xsl:attribute>
                <xsl:attribute name="if">have.sources</xsl:attribute>
                <jbiproject:javac/>
                <copy todir="${{build.classes.dir}}">
                    <xsl:call-template name="createFilesets">
                        <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                        <xsl:with-param name="excludes">${build.classes.excludes}</xsl:with-param>
                    </xsl:call-template>
                </copy>
            </target>

            <target name="-post-compile">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="compile">
                <xsl:attribute name="depends">init,deps-jar,-pre-pre-compile,-pre-compile,-do-compile,-post-compile</xsl:attribute>
                <xsl:attribute name="description">Compile project.</xsl:attribute>
            </target>

            <target name="-pre-compile-single">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-do-compile-single">
                <xsl:attribute name="depends">init,deps-jar,-pre-pre-compile</xsl:attribute>
                <fail unless="javac.includes">Must select some files in the IDE or set javac.includes</fail>
                <jbiproject:javac>
                    <customize>
                        <patternset includes="${{javac.includes}}"/>
                    </customize>
                </jbiproject:javac>
            </target>

            <target name="-post-compile-single">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="compile-single">
                <xsl:attribute name="depends">init,deps-jar,-pre-pre-compile,-pre-compile-single,-do-compile-single,-post-compile-single</xsl:attribute>
            </target>

            <xsl:comment>
    ====================
    JAR BUILDING SECTION
    ====================
    </xsl:comment>

            <target name="-pre-pre-jar">
                <xsl:attribute name="depends">init</xsl:attribute>
                <dirname property="dist.jar.dir" file="${{dist.jar}}"/>
                <mkdir dir="${{dist.jar.dir}}"/>

                <dirname property="build.predist.jar.dir" file="${{build.predist.jar}}"/>
                <mkdir dir="${{build.predist.dir}}"/>
                
            </target>

            <target name="-pre-jar">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>
<!-- JBIFIX 
            <target name="-do-jar-without-manifest">
                <xsl:attribute name="depends">init,compile,-pre-pre-jar,-pre-jar</xsl:attribute>
                <xsl:attribute name="unless">manifest.available</xsl:attribute>
                <jbiproject:jar/>
            </target>

            <target name="-do-jar-with-manifest">
                <xsl:attribute name="depends">init,compile,-pre-pre-jar,-pre-jar</xsl:attribute>
                <xsl:attribute name="if">manifest.available</xsl:attribute>
                <xsl:attribute name="unless">manifest.available+main.class</xsl:attribute>
                <jbiproject:jar manifest="${{manifest.file}}"/>
            </target>

            <target name="-do-jar-with-mainclass">
                <xsl:attribute name="depends">init,compile,-pre-pre-jar,-pre-jar</xsl:attribute>
                <xsl:attribute name="if">manifest.available+main.class</xsl:attribute>
                <xsl:attribute name="unless">manifest.available+main.class+mkdist.available</xsl:attribute>
                <jbiproject:jar manifest="${{manifest.file}}">
                    <jbiproject:manifest>
                        <jbiproject:attribute name="Main-Class" value="${{main.class}}"/>
                    </jbiproject:manifest>
                </jbiproject:jar>
                <echo>To run this application from the command line without Ant, try:</echo>
                <property name="build.classes.dir.resolved" location="${{build.classes.dir}}"/>
                <property name="dist.jar.resolved" location="${{dist.jar}}"/>
                <pathconvert property="run.classpath.with.dist.jar">
                    <path path="${{run.classpath}}"/>
                    <map from="${{build.classes.dir.resolved}}" to="${{dist.jar.resolved}}"/>
                </pathconvert>
                <echo><xsl:choose>
                        <xsl:when test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">${platform.java}</xsl:when>
                        <xsl:otherwise>java</xsl:otherwise>
                    </xsl:choose> -cp "${run.classpath.with.dist.jar}" ${main.class}</echo>
            </target>
-->            
            <target name="-do-jar-with-libraries">
                <xsl:attribute name="depends">init,compile,-pre-pre-jar,-pre-jar</xsl:attribute>
                <property name="build.classes.dir.resolved" location="${{build.classes.dir}}"/>
                <property name="libs.jbi.classpath.resolved" location="${{libs.jbi.classpath}}"/>
                <pathconvert property="run.classpath.without.build.classes.dir">
                    <path path="${{run.classpath}}"/>
                    <map from="${{build.classes.dir.resolved}}" to=""/>
                    <map from="${{libs.jbi.classpath.resolved}}" to=""/>
                </pathconvert>      
                <jar destfile="${{build.predist.jar}}" >
                    <fileset dir="${{build.classes.dir}}"/>
                </jar>
                <taskdef classname="com.sun.jbi.cdk.ant.CopyFiles" name="copyfiles" classpath="${{libs.jbi-cdk-ant-tasks.classpath}}"/>
                <copyfiles todir="${{build.predist.dir}}/lib" files="${{jbi.component.lib.classpath}}" />
                
            </target>

            <target name="-do-jbi-comp-zip">
                <xsl:attribute name="depends">init,compile,-pre-pre-jar,-pre-jar,-do-jar-with-libraries</xsl:attribute>
                <zip destfile="${{dist.jar}}" >
                    <zipfileset dir="${{conf.dir}}" />
                    <zipfileset file="${{build.predist.jar}}" />
                    <zipfileset dir="${{build.predist.dir}}/lib" prefix="lib" />
                </zip>                
            </target>
            
            <target name="-post-jar">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="jar">
                <xsl:attribute name="depends">init,compile,-pre-jar,-do-jar-with-libraries,-do-jbi-comp-zip,-post-jar</xsl:attribute>
                <xsl:attribute name="description">Build JAR.</xsl:attribute>
            </target>

            <xsl:comment>
    =================
    EXECUTION SECTION
    =================
    </xsl:comment>

            <target name="run">
                <xsl:attribute name="depends">init,compile</xsl:attribute>
                <xsl:attribute name="description">Run a main class.</xsl:attribute>
                <jbiproject:java>
                    <customize>
                        <arg line="${{application.args}}"/>
                    </customize>
                </jbiproject:java>
            </target>

            <target name="run-single">
                <xsl:attribute name="depends">init,compile-single</xsl:attribute>
                <fail unless="run.class">Must select one file in the IDE or set run.class</fail>
                <jbiproject:java classname="${{run.class}}"/>
            </target>

            <xsl:comment>
    =================
    DEBUGGING SECTION
    =================
    </xsl:comment>

            <target name="-debug-start-debugger">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <jbiproject:nbjpdastart name="${{debug.class}}"/>
            </target>

            <target name="-debug-start-debuggee">
                <xsl:attribute name="depends">init,compile</xsl:attribute>
                <jbiproject:debug>
                    <customize>
                        <arg line="${{application.args}}"/>
                    </customize>
                </jbiproject:debug>
            </target>

            <target name="debug">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,compile,-debug-start-debugger,-debug-start-debuggee</xsl:attribute>
                <xsl:attribute name="description">Debug project in IDE.</xsl:attribute>
            </target>

            <target name="-debug-start-debugger-stepinto">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <jbiproject:nbjpdastart stopclassname="${{main.class}}"/>
            </target>

            <target name="debug-stepinto">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,compile,-debug-start-debugger-stepinto,-debug-start-debuggee</xsl:attribute>
            </target>

            <target name="-debug-start-debuggee-single">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,compile-single</xsl:attribute>
                <fail unless="debug.class">Must select one file in the IDE or set debug.class</fail>
                <jbiproject:debug classname="${{debug.class}}"/>
            </target>

            <target name="debug-single">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,compile-single,-debug-start-debugger,-debug-start-debuggee-single</xsl:attribute>
            </target>

            <target name="-pre-debug-fix">
                <xsl:attribute name="depends">init</xsl:attribute>
                <fail unless="fix.includes">Must set fix.includes</fail>
                <property name="javac.includes" value="${{fix.includes}}.java"/>
            </target>

            <target name="-do-debug-fix">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,-pre-debug-fix,compile-single</xsl:attribute>
                <jbiproject:nbjpdareload/>
            </target>

            <target name="debug-fix">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,-pre-debug-fix,-do-debug-fix</xsl:attribute>
            </target>

            <target name="connect-debugger" unless="is.debugged">
                <nbjpdaconnect address="${{jpda.address}}" host="${{jpda.host}}" name="${{name}}" transport="${{jpda.transport}}">
                    <classpath>
                        <path path="${{debug.classpath}}"/>
                    </classpath>
                    <sourcepath>
                        <path path="${{web.docbase.dir}}"/>
                    </sourcepath>
                </nbjpdaconnect>
            </target>    
            
            <xsl:comment>
    ===============
    JAVADOC SECTION
    ===============
    </xsl:comment>

            <target name="-javadoc-build">
                <xsl:attribute name="depends">init</xsl:attribute>
                <mkdir dir="${{dist.javadoc.dir}}"/>
                <!-- XXX do an up-to-date check first -->
                <javadoc>
                    <xsl:attribute name="destdir">${dist.javadoc.dir}</xsl:attribute>
                    <xsl:attribute name="source">${javac.source}</xsl:attribute>
                    <xsl:attribute name="notree">${javadoc.notree}</xsl:attribute>
                    <xsl:attribute name="use">${javadoc.use}</xsl:attribute>
                    <xsl:attribute name="nonavbar">${javadoc.nonavbar}</xsl:attribute>
                    <xsl:attribute name="noindex">${javadoc.noindex}</xsl:attribute>
                    <xsl:attribute name="splitindex">${javadoc.splitindex}</xsl:attribute>
                    <xsl:attribute name="author">${javadoc.author}</xsl:attribute>
                    <xsl:attribute name="version">${javadoc.version}</xsl:attribute>
                    <xsl:attribute name="windowtitle">${javadoc.windowtitle}</xsl:attribute>
                    <xsl:attribute name="private">${javadoc.private}</xsl:attribute>
                    <xsl:attribute name="additionalparam">${javadoc.additionalparam}</xsl:attribute>
                    <xsl:attribute name="failonerror">true</xsl:attribute> <!-- #47325 -->
                    <xsl:attribute name="useexternalfile">true</xsl:attribute> <!-- #57375, requires Ant >=1.6.5 -->
                    <xsl:if test="/p:project/p:configuration/jbiproject:data/jbiproject:explicit-platform">
                        <xsl:attribute name="executable">${platform.javadoc}</xsl:attribute>
                    </xsl:if>                                                        
                    <classpath>
                        <path path="${{javac.classpath}}"/>
                    </classpath>
                    <sourcepath>
                        <xsl:call-template name="createPathElements">
                            <xsl:with-param name="locations" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                        </xsl:call-template>
                    </sourcepath>
                    <xsl:call-template name="createPackagesets">
                            <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                            <xsl:with-param name="includes">*/**</xsl:with-param>
                   </xsl:call-template>
                    <xsl:call-template name="createFilesets">
                            <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:source-roots"/>
                            <xsl:with-param name="includes">*.java</xsl:with-param>
                   </xsl:call-template>
                </javadoc>
            </target>

            <target name="-javadoc-browse">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="unless">no.javadoc.preview</xsl:attribute>
                <xsl:attribute name="depends">init,-javadoc-build</xsl:attribute>
                <nbbrowse file="${{dist.javadoc.dir}}/index.html"/>
            </target>

            <target name="javadoc">
                <xsl:attribute name="depends">init,-javadoc-build,-javadoc-browse</xsl:attribute>
                <xsl:attribute name="description">Build Javadoc.</xsl:attribute>
            </target>

            <xsl:comment>
    =========================
    JUNIT COMPILATION SECTION
    =========================
    </xsl:comment>

            <target name="-pre-pre-compile-test">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile</xsl:attribute>
                <mkdir dir="${{build.test.classes.dir}}"/>
            </target>

            <target name="-pre-compile-test">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-do-compile-test">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile,-pre-pre-compile-test,-pre-compile-test</xsl:attribute>
                <xsl:element name="jbiproject:javac">
                    <xsl:attribute name="srcdir">
                        <xsl:call-template name="createPath">
                            <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                        </xsl:call-template>
                    </xsl:attribute>
                    <xsl:attribute name="destdir">${build.test.classes.dir}</xsl:attribute>
                    <xsl:attribute name="debug">true</xsl:attribute>
                    <xsl:attribute name="classpath">${javac.test.classpath}</xsl:attribute>
                </xsl:element>
                <copy todir="${{build.test.classes.dir}}">
                    <xsl:call-template name="createFilesets">
                        <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                        <xsl:with-param name="excludes">**/*.java</xsl:with-param>
                    </xsl:call-template>
                </copy>
            </target>

            <target name="-post-compile-test">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="compile-test">
                <xsl:attribute name="depends">init,compile,-pre-pre-compile-test,-pre-compile-test,-do-compile-test,-post-compile-test</xsl:attribute>
            </target>

            <target name="-pre-compile-test-single">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="-do-compile-test-single">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile,-pre-pre-compile-test,-pre-compile-test-single</xsl:attribute>
                <fail unless="javac.includes">Must select some files in the IDE or set javac.includes</fail>
                <xsl:element name="jbiproject:javac">
                    <xsl:attribute name="srcdir">
                        <xsl:call-template name="createPath">
                            <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                        </xsl:call-template>
                    </xsl:attribute>
                    <xsl:attribute name="destdir">${build.test.classes.dir}</xsl:attribute>
                    <xsl:attribute name="debug">true</xsl:attribute>
                    <xsl:attribute name="classpath">${javac.test.classpath}</xsl:attribute>
                    <customize>
                        <patternset includes="${{javac.includes}}"/>
                    </customize>
                </xsl:element>
                <copy todir="${{build.test.classes.dir}}">
                    <xsl:call-template name="createFilesets">
                        <xsl:with-param name="roots" select="/p:project/p:configuration/jbiproject:data/jbiproject:test-roots"/>
                        <xsl:with-param name="excludes">**/*.java</xsl:with-param>
                    </xsl:call-template>
                </copy>
            </target>

            <target name="-post-compile-test-single">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="compile-test-single">
                <xsl:attribute name="depends">init,compile,-pre-pre-compile-test,-pre-compile-test-single,-do-compile-test-single,-post-compile-test-single</xsl:attribute>
            </target>

            <xsl:comment>
    =======================
    JUNIT EXECUTION SECTION
    =======================
    </xsl:comment>

            <target name="-pre-test-run">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <mkdir dir="${{build.test.results.dir}}"/>
                <antcall inheritall="true" target="run-deploy-test-sa" />
            </target>

            <target name="-do-test-run">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test,-pre-test-run</xsl:attribute>
                <jbiproject:junit/>
            </target>

            <target name="-post-test-run">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test,-pre-test-run,-do-test-run</xsl:attribute>
                <antcall inheritall="true" target="run-undeploy-test-sa" />
                <fail if="tests.failed">Some tests failed; see details above.</fail>
            </target>

            <target name="test-report">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <!-- TBD
                <junitreport todir="${{build.test.results.dir}}">
                    <fileset dir="${{build.test.results.dir}}">
                        <include name="TEST-*.xml"/>
                    </fileset>
                    <report format="noframes" todir="${{build.test.results.dir}}"/>
                </junitreport>
                -->
            </target>

            <target name="-test-browse">
                <xsl:attribute name="if">netbeans.home+have.tests</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <!-- TBD
                <nbbrowse file="${{build.test.results.dir}}/junit-noframes.html"/>
                -->
            </target>

            <target name="test">
                <xsl:attribute name="depends">init,compile-test,-pre-test-run,-do-test-run,test-report,-post-test-run,-test-browse</xsl:attribute>
                <xsl:attribute name="description">Run unit tests.</xsl:attribute>
            </target>

            <target name="-pre-test-run-single">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init</xsl:attribute>
                <mkdir dir="${{build.test.results.dir}}"/>
            </target>

            <target name="-do-test-run-single">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test-single,-pre-test-run-single</xsl:attribute>
                <fail unless="test.includes">Must select some files in the IDE or set test.includes</fail>
                <jbiproject:junit includes="${{test.includes}}"/>
            </target>

            <target name="-post-test-run-single">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test-single,-pre-test-run-single,-do-test-run-single</xsl:attribute>
                <fail if="tests.failed">Some tests failed; see details above.</fail>
            </target>

            <target name="test-single">
                <xsl:attribute name="depends">init,compile-test-single,-pre-test-run-single,-do-test-run-single,-post-test-run-single</xsl:attribute>
                <xsl:attribute name="description">Run single unit test.</xsl:attribute>
            </target>

            <xsl:comment>
    =======================
    JUNIT DEBUGGING SECTION
    =======================
    </xsl:comment>

            <target name="-debug-start-debuggee-test">
                <xsl:attribute name="if">have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test</xsl:attribute>
                <fail unless="test.class">Must select one file in the IDE or set test.class</fail>
                <jbiproject:debug classname="junit.textui.TestRunner" classpath="${{debug.test.classpath}}">
                    <customize>
                        <arg line="${{test.class}}"/>
                    </customize>
                </jbiproject:debug>
            </target>

            <target name="-debug-start-debugger-test">
                <xsl:attribute name="if">netbeans.home+have.tests</xsl:attribute>
                <xsl:attribute name="depends">init,compile-test</xsl:attribute>
                <jbiproject:nbjpdastart name="${{test.class}}" classpath="${{debug.test.classpath}}"/>
            </target>

            <target name="debug-test">
                <xsl:attribute name="depends">init,compile-test,-debug-start-debugger-test,-debug-start-debuggee-test</xsl:attribute>
            </target>

            <target name="-do-debug-fix-test">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,-pre-debug-fix,compile-test-single</xsl:attribute>
                <jbiproject:nbjpdareload dir="${{build.test.classes.dir}}"/>
            </target>

            <target name="debug-fix-test">
                <xsl:attribute name="if">netbeans.home</xsl:attribute>
                <xsl:attribute name="depends">init,-pre-debug-fix,-do-debug-fix-test</xsl:attribute>
            </target>
            
    <xsl:comment>
    ===============
    CLEANUP SECTION
    ===============
    </xsl:comment>

            <xsl:call-template name="deps.target">
                <xsl:with-param name="targetname" select="'deps-clean'"/>
            </xsl:call-template>

            <target name="-do-clean">
                <xsl:attribute name="depends">init</xsl:attribute>
                <delete dir="${{build.dir}}"/>
                <delete dir="${{dist.dir}}"/>
                <!-- XXX explicitly delete all build.* and dist.* dirs in case they are not subdirs -->
            </target>

            <target name="-post-clean">
                <xsl:comment> Empty placeholder for easier customization. </xsl:comment>
                <xsl:comment> You can override this target in the ../build.xml file. </xsl:comment>
            </target>

            <target name="clean">
                <xsl:attribute name="depends">init,deps-clean,-do-clean,-post-clean</xsl:attribute>
                <xsl:attribute name="description">Clean build products.</xsl:attribute>
            </target>

        </project>

        <!-- TBD items:

        Could pass <propertyset> to run, debug, etc. under Ant 1.6,
        optionally, by doing e.g.

          <propertyset>
            <propertyref prefix="sysprop."/>
            <mapper type="glob" from="sysprop.*" to="*"/>
          </propertyset>

        Now user can add to e.g. project.properties e.g.:
          sysprop.org.netbeans.modules.javahelp=0
        to simulate
          -Dorg.netbeans.modules.javahelp=0

        -->

    </xsl:template>

    <!---
    Generic template to build subdependencies of a certain type.
    Feel free to copy into other modules.
    @param targetname required name of target to generate
    @param type artifact-type from project.xml to filter on; optional, if not specified, uses
                all references, and looks for clean targets rather than build targets
    @return an Ant target which builds (or cleans) all known subprojects
    -->
    <xsl:template name="deps.target">
        <xsl:param name="targetname"/>
        <xsl:param name="type"/>
        <target name="{$targetname}">
            <xsl:attribute name="depends">init</xsl:attribute>
            <xsl:attribute name="unless">no.deps</xsl:attribute>
            
            <xsl:variable name="references2" select="/p:project/p:configuration/projdeps2:references"/>
            <xsl:for-each select="$references2/projdeps2:reference[not($type) or projdeps2:artifact-type = $type]">
                <xsl:variable name="subproj" select="projdeps2:foreign-project"/>
                <xsl:variable name="subtarget">
                    <xsl:choose>
                        <xsl:when test="$type">
                            <xsl:value-of select="projdeps2:target"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="projdeps2:clean-target"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                <xsl:variable name="script" select="projdeps2:script"/>
                <xsl:choose>
                    <xsl:when test="projdeps2:properties">
                        <ant target="{$subtarget}" inheritall="false" antfile="{$script}">
                            <xsl:for-each select="projdeps2:properties/projdeps2:property">
                                <property name="{@name}" value="{.}"/>
                            </xsl:for-each>
                        </ant>
                    </xsl:when>
                    <xsl:otherwise>
                        <ant target="{$subtarget}" inheritall="false" antfile="{$script}"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>
            
            <xsl:variable name="references" select="/p:project/p:configuration/projdeps:references"/>
            <xsl:for-each select="$references/projdeps:reference[not($type) or projdeps:artifact-type = $type]">
                <xsl:variable name="subproj" select="projdeps:foreign-project"/>
                <xsl:variable name="subtarget">
                    <xsl:choose>
                        <xsl:when test="$type">
                            <xsl:value-of select="projdeps:target"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:value-of select="projdeps:clean-target"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                <xsl:variable name="script" select="projdeps:script"/>
                <ant target="{$subtarget}" inheritall="false" antfile="${{project.{$subproj}}}/{$script}"/>
            </xsl:for-each>
            
        </target>
    </xsl:template>
    
    <xsl:template name="createRootAvailableTest">
        <xsl:param name="roots"/>
        <xsl:param name="propName"/>
        <xsl:element name="condition">
            <xsl:attribute name="property"><xsl:value-of select="$propName"/></xsl:attribute>
            <or>
                <xsl:for-each select="$roots/jbiproject:root">
                    <xsl:element name="available">
			        <xsl:attribute name="file"><xsl:text>${</xsl:text><xsl:value-of select="@id"/><xsl:text>}</xsl:text></xsl:attribute>
		            </xsl:element>
                </xsl:for-each>
            </or>
        </xsl:element>
    </xsl:template>

    <xsl:template name="createSourcePathValidityTest">
        <xsl:param name="roots"/>
        <xsl:for-each select="$roots/jbiproject:root">
            <xsl:element name="fail">
			    <xsl:attribute name="unless"><xsl:value-of select="@id"/></xsl:attribute>
                <xsl:text>Must set </xsl:text><xsl:value-of select="@id"/>
		    </xsl:element>
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="createFilesets">
        <xsl:param name="roots"/>
        <xsl:param name="includes"/>
        <xsl:param name="excludes"/>
        <xsl:for-each select="$roots/jbiproject:root">
            <xsl:element name="fileset">
                <xsl:attribute name="dir"><xsl:text>${</xsl:text><xsl:value-of select="@id"/><xsl:text>}</xsl:text></xsl:attribute>
                <xsl:if test="$includes">
                    <xsl:attribute name="includes"><xsl:value-of select="$includes"/></xsl:attribute>
                </xsl:if>
                <xsl:if test="$excludes">
                    <xsl:attribute name="excludes"><xsl:value-of select="$excludes"/></xsl:attribute>
                </xsl:if>
            </xsl:element>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="createPackagesets">
        <xsl:param name="roots"/>
        <xsl:param name="includes"/>
        <xsl:param name="excludes"/>
        <xsl:for-each select="$roots/jbiproject:root">
            <xsl:element name="packageset">
                <xsl:attribute name="dir"><xsl:text>${</xsl:text><xsl:value-of select="@id"/><xsl:text>}</xsl:text></xsl:attribute>
                <xsl:if test="$includes">
                    <xsl:attribute name="includes"><xsl:value-of select="$includes"/></xsl:attribute>
                </xsl:if>
                <xsl:if test="$excludes">
                    <xsl:attribute name="excludes"><xsl:value-of select="$excludes"/></xsl:attribute>
                </xsl:if>
            </xsl:element>
        </xsl:for-each>
    </xsl:template>        

    <xsl:template name="createPathElements">
        <xsl:param name="locations"/>
        <xsl:for-each select="$locations/jbiproject:root">
            <xsl:element name="pathelement">
			    <xsl:attribute name="location"><xsl:text>${</xsl:text><xsl:value-of select="@id"/><xsl:text>}</xsl:text></xsl:attribute>
		    </xsl:element>
        </xsl:for-each>
    </xsl:template>

	<xsl:template name="createPath">
		<xsl:param name="roots"/>
		<xsl:for-each select="$roots/jbiproject:root">
		    <xsl:if test="position() != 1">
			    <xsl:text>:</xsl:text>
		    </xsl:if>
		    <xsl:text>${</xsl:text>
		    <xsl:value-of select="@id"/>
		    <xsl:text>}</xsl:text>
		</xsl:for-each>						
	</xsl:template>

</xsl:stylesheet>
