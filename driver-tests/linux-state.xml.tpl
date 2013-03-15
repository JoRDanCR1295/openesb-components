<?xml version="1.0" encoding="UTF-8"?> 
<!--
  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
  
  Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
  
  The contents of this file are subject to the terms of either the GNU General Public
  License Version 2 only ("GPL") or the Common Development and Distribution
  License("CDDL") (collectively, the "License"). You may not use this file except in
  compliance with the License. You can obtain a copy of the License at
  http://www.netbeans.org/cddl-gplv2.html or nbbuild/licenses/CDDL-GPL-2-CP. See the
  License for the specific language governing permissions and limitations under the
  License.  When distributing the software, include this License Header Notice in
  each file and include the License file at nbbuild/licenses/CDDL-GPL-2-CP.  Sun
  designates this particular file as subject to the "Classpath" exception as provided
  by Sun in the GPL Version 2 section of the License file that accompanied this code.
  If applicable, add the following below the License Header, with the fields enclosed
  by brackets [] replaced by your own identifying information:
  "Portions Copyrighted [year] [name of copyright owner]"
  
  Contributor(s):
  
  The Original Software is NetBeans. The Initial Developer of the Original Software
  is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun Microsystems, Inc. All
  Rights Reserved.
  
  If you wish your version of this file to be governed by only the CDDL or only the
  GPL Version 2, indicate your decision by adding "[Contributor] elects to include
  this software in this distribution under the [CDDL or GPL Version 2] license." If
  you do not indicate a single choice of license, a recipient has the option to
  distribute your version of this file under either the CDDL, the GPL Version 2 or
  to extend the choice of license to its licensees as provided above. However, if you
  add GPL Version 2 code and therefore, elected the GPL Version 2 license, then the
  option applies only if the new code is made subject to such option by the copyright
  holder.
--> 
<state xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="state-file.xsd">
<components>
        <product platform="windows linux solaris-sparc solaris-x86 macosx-ppc macosx-x86 aix" status="to-be-installed" uid="openesb-ide" version="#OPENESB_IDE_VERSION#">
            <properties>
                <property name="installation.location.windows">$N{install}/netbeans</property>
                <property name="minimum.jdk.version">1.5.0.6</property>
                <property name="minimum.jdk.version.windows-ia64">1.5.0</property>
                <property name="preferred.jdk.version.macosx">1.6.0.23.0</property>
                <property name="maximum.jdk.version">10.0.0</property>
                <property name="jdk.location">/home/hudson/jdk1.6.0_23</property>
                <property name="minimum.jdk.version.linux-ppc64">1.5.0</property>
                <property name="minimum.jdk.version.aix">1.5.0</property>
                <property name="start.menu.shortcut.location">current.user</property>
                <property name="installation.location.macosx">$N{install}/netbeans.app</property>
                <property name="minimum.jdk.version.linux-ppc">1.5.0</property>
                <property name="installation.location">/home/hudson/jobs/Driver-test/workspace/openesbv2.3/netbeans</property>
                <property name="desktop.shortcut.location">current.user</property>
            </properties>
        </product>
        <product platform="linux" status="to-be-installed" uid="glassfish" version="#GLASSFISH_VERSION#">
            <properties>
                <property name="password">adminadmin</property>
                <property name="jmx.admin.port">8686</property>
                <property name="username">admin</property>
                <property name="vendor.jdk.allowed.pattern">Sun Microsystems.*</property>
                <property name="jms.port">7676</property>
                <property name="maximum.jdk.version">1.6.99</property>
                <property name="minimum.jdk.version">1.5.0</property>
                <property name="jdk.location">/home/hudson/jdk1.6.0_23</property>
                <property name="installation.location">/home/hudson/jobs/Driver-test/workspace/openesbv2.3/glassfish</property>
                <property name="http.port">18080</property>
                <property name="https.port">18181</property>
                <property name="preferred.jdk.version.macosx">1.6.0.23.0</property>
                <property name="admin.port">4848</property>
                <property name="iiop.port">3100</property>
                <property name="vendor.jdk.allowed.pattern.aix">IBM.*</property>
                <property name="iiop.mutualauth.port">3920</property>
                <property name="iiop.ssl.port">3820</property>
                <property name="vendor.jdk.allowed.pattern.macosx">Apple.*</property>
            </properties>
        </product>
        <product platform="windows linux solaris-sparc solaris-x86 macosx-ppc macosx-x86 aix" status="to-be-installed" uid="openesb-core" version="#OPENESB_CORE_VERSION#">
            <properties>
                <property name="installation.location">/home/hudson/jobs/Driver-test/workspace/openesbv2.3/glassfish/addons/jbi-components</property>
            </properties>
        </product>
        <product platform="windows linux solaris-sparc solaris-x86 macosx-ppc macosx-x86 aix" status="to-be-installed" uid="openesb-components" version="#OPENESB_COMPONENTS_VERSION#">
            <properties>
                <property name="installation.location">/home/hudson/jobs/Driver-test/workspace/openesbv2.3/glassfish/addons/jbi-components</property>
            </properties>
        </product>
    </components>
</state>