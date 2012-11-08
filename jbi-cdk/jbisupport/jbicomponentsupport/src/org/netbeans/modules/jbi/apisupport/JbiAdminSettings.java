/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import org.netbeans.modules.j2ee.deployment.devmodules.api.Deployment;
import org.netbeans.modules.j2ee.deployment.devmodules.api.J2eePlatform;
import org.netbeans.modules.j2ee.deployment.plugins.api.InstanceProperties;
import org.netbeans.modules.jbi.apisupport.common.DOMUtil;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 *
 * @author chikkala
 */
public interface JbiAdminSettings {
    
    public static final String UNKNOWN_SERVER_ID = "unknown_server_id";
    
    String getDisplayName();
    boolean isRemote();
    String getAppServerId();
    String getAppServerHome();
    String getJbiHome();
    String getHost();
    String getPort();
    String getJMXRMIPort();
    String getUsername();
    String getPassword();
    boolean isOpenESB2x();
    
    public static class JbiAdminSettingsImpl implements JbiAdminSettings {
        
        private String mDisplayName = "Unknown Server";
        private String mAppServerId = UNKNOWN_SERVER_ID;
        private String mAppServerHome = "C:/Sun/Appserver";
        private String mJbiHome = "C:/Sun/Appserver/jbi";
        private String mHost = "localhost";
        private String mPort = "8686";
        private String mUsername = "admin";
        private String mPassword = "adminadmin";
        
        private String mJMXRMIPort = "8686";
        
        private boolean mRemote = false;
        private boolean mOpenESB2x = false;
        
        /** Creates a new instance of JbiServer */
        protected JbiAdminSettingsImpl(String appServerId) {
            this.mAppServerId = appServerId;
            initSettings();
        }
        
        public String getAppServerId() {
            return this.mAppServerId;
        }
        
        protected void setAppServerId(String id) {
            this.mAppServerId = id;
        }
        
        public String getDisplayName() {
            return this.mDisplayName;
        }
        
        public void setDisplayName(String displayName) {
            this.mDisplayName = displayName;
        }
        
        public String getAppServerHome() {
            return this.mAppServerHome;
        }
        
        public void setAppServerHome(String appServerHome) {
            this.mAppServerHome = appServerHome;
        }
        
        public String getJbiHome() {
            return this.mJbiHome;
        }
        
        public void setJbiHome(String jbiHome) {
            this.mJbiHome = jbiHome;
        }
        
        public String getHost() {
            return this.mHost;
        }
        
        public void setHost(String host) {
            this.mHost = host;
        }
        
        public String getPort() {
            return this.mPort;
        }
        
        public void setPort(String port) {
            this.mPort = port;
        }
        
        public String getJMXRMIPort() {
            return this.mJMXRMIPort;
        }
        
        public void setJMXRMIPort(String port) {
            this.mJMXRMIPort = port;
        }
        
        public String getUsername() {
            return this.mUsername;
        }
        
        public void setUsername(String username) {
            this.mUsername = username;
        }
        
        public String getPassword() {
            return this.mPassword;
        }
        
        public void setPassword(String password) {
            this.mPassword = password;
        }
        
        public void setRemote(boolean remote) {
            this.mRemote = remote;
        }
        
        public boolean isRemote() {
            return this.mRemote;
        }

        public void setOpenESB2x(boolean isOpenESB2x) {
            this.mOpenESB2x = isOpenESB2x;
        }
        
        public boolean isOpenESB2x() {
            return this.mOpenESB2x;
        }
        
        public String toString() {
            return this.getDisplayName();
        }
        
        protected void initSettings() {
            // findAppServerDetails();
            // String instanceId = findSunAppServerInstance();
            String instanceId = getAppServerId();
            if (instanceId == null || UNKNOWN_SERVER_ID.equalsIgnoreCase(instanceId)) {
                return ; // use defaults
            }
            InstanceProperties instProps = InstanceProperties.getInstanceProperties(instanceId);
            if (instProps == null ) {
                this.setAppServerId(UNKNOWN_SERVER_ID);
                return; // use defaults
            }
            
            Deployment deployment = Deployment.getDefault();
            J2eePlatform jeePlatform = deployment.getJ2eePlatform(instanceId);
            File[] roots = jeePlatform.getPlatformRoots();
            
            String displayName = instProps.getProperty("displayName");
            String url = instProps.getProperty("url");  // "[D:\Sun\gfv2\glassfish]deployer:Sun:AppServer::localhost:4848"
            String username = instProps.getProperty("username");
            String password = instProps.getProperty("password");
            
            // parse url for home, host, port
            String asHome = getAppServerHomeFromUrl(url);
            if ( asHome == null && roots.length > 0 ) {
                // fall back to the platform roots
                asHome = roots[0].getAbsolutePath();
            }
            String host = getAppServerAdminHostFromUrl(url);
            String port = getAppServerAdminPortFromUrl(url);
            
            String jbiHome = null;
            boolean remote = false;
            boolean openESB2x = false;
            
            // check if the instance is configured as a remote instance.
            String domain = instProps.getProperty("DOMAIN");
            String domainLocation = instProps.getProperty("LOCATION");
            // if there is no domain name, it is assume that the instance is configured for remote admin
            if ( domain == null ) {
                remote = true;
            }
            
            if ( isOpenESB2x(asHome)) {
                // this is openesb 2.0 or later.
                openESB2x = true;
                jbiHome = (new File(asHome + "/jbi")).getAbsolutePath();
            } else {
                // get the jbi home, port from the domain xml if the instance is not created for remote.
                jbiHome = new File(asHome, "addons/jbi").getAbsolutePath();  //default
                port = "8686"; // default
                if ( domain != null ){  // then get the jbi home and port from the domain xml
                    Document domainXmlDoc = getDomainXmlDoc(domainLocation, domain);
                    jbiHome = getJbiHomeFromDomainXml(domainXmlDoc);
                    port = getJMXRMIPortFromDomainXml(domainXmlDoc);
                }
            }            
            
            this.setOpenESB2x(openESB2x);
            this.setRemote(remote);
            
            this.setDisplayName(displayName);
            this.setAppServerHome(asHome);
            this.setJbiHome(jbiHome);
            
            this.setHost(host);
            this.setPort(port);
            this.setUsername(username);
            this.setPassword(password);
            
            try {
                if ( domain != null ){  // then get the jmx rmi port for test clients
                    String jmxRmiPort = null;
                    Document domainXmlDoc = getDomainXmlDoc(domainLocation, domain);
                    jmxRmiPort = getJMXRMIPortFromDomainXml(domainXmlDoc);
                    if ( jmxRmiPort != null ) {
                        this.setJMXRMIPort(jmxRmiPort);
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }            
            
        }
        
        /**
         * return true if the $ashome/jbi/lib/jbi-ant-tasks.jar is present or the jbi dir is at the root of ashome.
         * this check will tell whether we should fall back to the old jbi home settings or not for openesb1.0
         */
        private boolean isOpenESB2x(String asHome) {
            File jbiHome = new File(asHome + "jbi");
            File antTasksJar = new File(asHome + "/jbi/lib/jbi-ant-tasks.jar");
            return (antTasksJar.exists() || jbiHome.exists());
        }
        
        private String getAppServerHomeFromUrl(String url) {
            // URL "[D:\Sun\gfv2\glassfish]deployer:Sun:AppServer::localhost:4848"
            // appserver home is the value within []
            String home = null;
            int beginIdx = url.indexOf('[');
            int endIdx = url.indexOf(']');
            if ( beginIdx >= 0 && endIdx >= 0 ) {
                home = url.substring(beginIdx+1, endIdx);
            }
            return home;
        }
        
        private String getAppServerAdminHostFromUrl(String url) {
            // URL "[D:\Sun\gfv2\glassfish]deployer:Sun:AppServer::localhost:4848"
            // appserver admin host is the value within :: and :
            String host = "localhost";
            int beginIdx = url.indexOf("::", 0);
            if ( beginIdx >= 0 ) {
                beginIdx += 2;
            }
            int endIdx = url.indexOf(':', beginIdx);
            
            if ( beginIdx >= 0 && endIdx >= 0 ) {
                host = url.substring(beginIdx, endIdx);
            }
            return host;
        }
        
        private String getAppServerAdminPortFromUrl(String url) {
            // URL "[D:\Sun\gfv2\glassfish]deployer:Sun:AppServer::localhost:4848"
            // appserver admin port is the value after last occurence of :
            // appserver admin host is the value within :: and :
            String port = "8686";
            int beginIdx = url.lastIndexOf(':');
            
            if ( beginIdx >= 0 ) {
                port = url.substring(beginIdx + 1);
            }
            return port;
        }
        
        private Document getDomainXmlDoc(String domainLocation, String domain) {
            Document xmlDoc = null;
            
            File domainXmlFile = new File(domainLocation, domain + "/config/domain.xml");
            try {
                xmlDoc = DOMUtil.UTIL.buildDOMDocument(new FileReader(domainXmlFile), false);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
            if ( xmlDoc != null ) {
//                StringWriter writer = new StringWriter();
//                try {
//                    DOMUtil.UTIL.DOM2String(xmlDoc, writer );
//                } catch (Exception ex) {
//                    ex.printStackTrace();
//                }
//                System.out.println("########## DOMAIN.XML Content ###########");
//                System.out.println(writer.getBuffer().toString());
                
            } else {
                System.out.println("Can not read the domain xml " + domainXmlFile.getAbsolutePath());
            }
            return xmlDoc;
        }
        
        private String getJbiHomeFromDomainXml(Document domainXmlDoc) {
            String jbiHome = null;
            // XPath xpath = XPathFactory.newInstance().newXPath();
            NodeList list = domainXmlDoc.getElementsByTagName("lifecycle-module");
            if ( list.getLength() > 0 ) {
                Util.logDebug("### Lifecycle modules found " + list.getLength());
            } else {
                Util.logDebug("### Lifecycle modules NOT FOUND");
            }
            for ( int i=0; i < list.getLength(); ++i) {
                Element el = (Element) list.item(i);
                String name = el.getAttribute("name");
                if ( name != null && name.equalsIgnoreCase("JBIFramework")) {
                    // found the jbi lifecycle module node
                    NodeList propList = DOMUtil.UTIL.getChildElements(el, "property");
                    for ( int j=0; j < propList.getLength(); ++j) {
                        Element propEl = (Element) propList.item(j);
                        String propName = propEl.getAttribute("name");
                        if ( propName != null && propName.equalsIgnoreCase("com.sun.jbi.home")) {
                            jbiHome = propEl.getAttribute("value");
                            break;
                        }
                    }
                    break;
                }
            }
            return jbiHome;
        }
        
        private String getJMXRMIPortFromDomainXml(Document domainXmlDoc) {
            String jbiPort = null;
            
            NodeList list = domainXmlDoc.getElementsByTagName("jmx-connector");
            for ( int i=0; i < list.getLength(); ++i) {
                Element el = (Element) list.item(i);
                String protocol = el.getAttribute("protocol");
                if ( protocol != null && protocol.equalsIgnoreCase("rmi_jrmp")) {
                    // found The JSR 160 "system-jmx-connector"
                    jbiPort = el.getAttribute("port");
                    break;
                }
            }
            return jbiPort;
        }
        
        private static boolean isSunAppServer(String instanceId) {
            return (instanceId.contains("]deployer:Sun:AppServer::"));
        }
        
        private static String[] findSunAppServerInstances() {
            List list = new ArrayList();
            String[] instances = InstanceProperties.getInstanceList();
            for ( int i=0; i < instances.length; ++i) {
                if ( isSunAppServer(instances[i])) {
                    list.add(instances[i]);
                }
            }
            return (String[]) list.toArray(new String[list.size()]);
        }
        
        private static String findSunAppServerInstance() {
            String[] sunInstances = findSunAppServerInstances();
            // InstanceProperties foundInstanceProps = null;
            // for now pick the first server instance that you get
            //TODO find the started server instance or add project customizer to pick the target server
            if ( sunInstances.length > 0 ) {
                return sunInstances[0];
                // foundInstanceProps = InstanceProperties.getInstanceProperties(sunInstances[0]);
            } else {
                return null;
            }
        }
        
        private static void findAppServerDetails() {
            
// Instance : [D:\export\demo\test\javaeesdk]deployer:Sun:AppServer::localhost:4848
//  Property: password Value : adminadmin
//  Property: username Value : admin
//  Property: url Value : [D:\export\demo\test\javaeesdk]deployer:Sun:AppServer::localhost:4848
//  Property: LOCATION Value : D:\export\demo\test\javaeesdk\domains
//  Property: httpportnumber Value : 4848
//  Property: DOMAIN Value : domain1
//  Property: displayName Value : JavaEESDK
// Platform Root : D:\export\demo\test\javaeesdk
//  LOCATION and DOMAIN values will be null for the remote host. you have to parse the url value to get the host and port.
            
            // find the running sun appserver with jbi installed
            // check the defult appserver that is sun appserver
            Deployment deployment = Deployment.getDefault();
            String[] serverInstanceIDs = deployment.getServerInstanceIDs();
            Util.logDebug("####### Server Instance IDs ############");
            for ( int i =0; i < serverInstanceIDs.length; ++i ) {
                System.out.println(serverInstanceIDs[i]);
            }
            
            String[] instances = InstanceProperties.getInstanceList();
            
            System.out.println("####### Instances ############");
            for ( int i =0; i < instances.length; ++i ) {
                String instanceId = instances[i];
                System.out.println(" Instance : " + instanceId);
                InstanceProperties instanceProps = InstanceProperties.getInstanceProperties(instanceId);
                Enumeration propsEnum = instanceProps.propertyNames();
                for ( ;propsEnum.hasMoreElements();) {
                    String prop = (String) propsEnum.nextElement();
                    System.out.println("  Property: " + prop + " Value : " + instanceProps.getProperty(prop));
                }
                J2eePlatform jeePlatform = deployment.getJ2eePlatform(instanceId);
                File[] roots = jeePlatform.getPlatformRoots();
                for ( int j=0; j < roots.length; ++j ) {
                    System.out.println(" Platform Root : " + roots[j].getAbsolutePath());
                }
                
            }
        }
        
        /**
         * returns the JbiAdminSettings for appServerId or null if not found.
         */
        public static JbiAdminSettings getJbiAdminSettings(String appServerId) {
            if ( appServerId == null || appServerId.trim().length() <= 0 ) {
                return null;
            }
            JbiAdminSettingsImpl impl = new JbiAdminSettingsImpl(appServerId);
            return impl;
        }
        /**
         * returns a first occurence of the JbiAdminSettings
         */
        public static JbiAdminSettings getDefaultJbiAdminSettings() {
            JbiAdminSettingsImpl impl = new JbiAdminSettingsImpl(UNKNOWN_SERVER_ID);
            return impl;
        }
        
        public static JbiAdminSettings getJbiAdminSettings() {
            JbiAdminSettings jbiAdminSettings = null;
            String instanceId = findSunAppServerInstance();
            jbiAdminSettings = getJbiAdminSettings(instanceId);
            
            if ( jbiAdminSettings == null) {
                jbiAdminSettings = getDefaultJbiAdminSettings();
            }
            return jbiAdminSettings;
        }
        
        public static JbiAdminSettings[] getAllJbiAdminSettings() {
            List list = new ArrayList();
            list.add(getDefaultJbiAdminSettings());
            String[] appServerIds = findSunAppServerInstances();
            for ( int i=0; i < appServerIds.length; ++i ) {
                String appServerId = appServerIds[i];
                Util.logDebug("## Adding jbi admin for AppServer ID " + appServerId);
                JbiAdminSettings jbiAdminSettings = getJbiAdminSettings(appServerId);
                if ( jbiAdminSettings != null ) {
                    Util.logDebug("### Adding Jbi Admin Setting for " + jbiAdminSettings.toString());
                } else {
                    Util.logDebug("### Jbi Admin is NULL in All JBI Settings");
                }
                list.add(jbiAdminSettings);
            }
            Util.logDebug("List of JBI Admins found " + list.size());
            return (JbiAdminSettings[]) list.toArray(new JbiAdminSettings[list.size()]);
        }
        
    }
    
}
