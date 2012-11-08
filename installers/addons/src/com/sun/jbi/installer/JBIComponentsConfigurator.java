/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JBIComponentsConfigurator.java - ver 1.1 - 01/04/2006
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.installer;

import com.sun.appserv.addons.InstallationContext;
import com.sun.appserv.addons.ConfigurationContext;
import com.sun.appserv.addons.ConfigurationContext.ConfigurationType;
import com.sun.appserv.addons.AddonVersion;
import com.sun.appserv.addons.AddonException;
import com.sun.appserv.addons.Configurator;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Logger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ResourceBundle;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarException;
import java.util.jar.JarFile;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.OutputKeys;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.InputSource;
import org.xml.sax.EntityResolver;
import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;


public class JBIComponentsConfigurator implements Configurator {

    private String mAntScriptName = null;
    private String mAntBaseDir = null;
    private Logger mLogger = null;
    private Properties mCmdLineParams = new java.util.Properties();
    private ResourceBundle resourceBundle = null;

    public void configure(ConfigurationContext cc) throws AddonException {
        setResourceBundle();
        mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsConfigurator");
        mLogger.info("Configuring jbi components...");

        String appServerInstallRoot = cc.getInstallationContext().
            getInstallationDirectory().getAbsolutePath();
        String jbiDomainRoot = cc.getDomainDirectory().getAbsolutePath();
        ConfigurationType ct = cc.getConfigurationType();
        boolean isDAS = false;
        if (ct == ConfigurationType.DAS) {
            isDAS = true;
        }

        String timeStamp = "";
        try {
            String pattern = "yyyyMMddHHmm";
            String underscore = "_";
            SimpleDateFormat df = new SimpleDateFormat(pattern);
            String result = df.format(new Date());
            timeStamp = underscore + result;
        }
        catch (Exception e) {
            //Here we do not do anything and the timestamp will be an empty string
        }

        try {
            mAntBaseDir = System.getProperty("java.io.tmpdir") +
                File.separator + "jbi_components_configurator" + timeStamp;


            String jbiConfigJar = appServerInstallRoot + File.separator +
                "lib" + File.separator +
                "addons" + File.separator +
                InstallConstants.JBI_COMPONENTS_CONFIGURATOR_JAR;

            File baseDir = new File(mAntBaseDir);
            if(!baseDir.exists())
            {
                baseDir.mkdirs();
            }
            mAntScriptName = mAntBaseDir +
                File.separator + InstallConstants.JBI_CONFIGURE_SCRIPT;

            JarFactory jrFctry = new JarFactory(mAntBaseDir);
            jrFctry.unJar(new File(jbiConfigJar));
            AntRunner ant = new AntRunner();

            // Get the component names from the jbi.xml of the jar file
            String encoderlibName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" + 
                File.separator + "encoderlib.jar");
            String saxonlibName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" + 
                File.separator + "saxonlib.jar");
            String wsdlextlibName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" + 
                File.separator + "wsdlextlib.jar");
            String bpelseName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" + 
                File.separator + "bpelserviceengine.jar");
            String xsltseName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "xsltserviceengine.jar");
            String filebcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "filebc.jar");
            String jmsbcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "jmsbc.jar");
            String sqlseName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "sqlse.jar");
                
            /*
            String iepseName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "iepserviceengine.jar");
            String jdbcbcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "jdbcbc.jar");
            String smtpbcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "smtpbc.jar");
            String ftpbcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "ftpbc.jar");
            String mqbcName = getComponentName(appServerInstallRoot + File.separator +
                "addons" + File.separator + "jbi-components" +  
                File.separator + "mqbc.jar");
            */

            mCmdLineParams.setProperty(InstallConstants.IS_DAS, (isDAS ? "true" : "false"));



            mCmdLineParams.setProperty(InstallConstants.ENCODERLIB_NAME, encoderlibName);
            mCmdLineParams.setProperty(InstallConstants.SAXONLIB_NAME, saxonlibName);
            mCmdLineParams.setProperty(InstallConstants.WSDLEXTLIB_NAME, wsdlextlibName);
            mCmdLineParams.setProperty(InstallConstants.BPELSE_NAME, bpelseName);
            mCmdLineParams.setProperty(InstallConstants.XSLTSE_NAME, xsltseName);
            mCmdLineParams.setProperty(InstallConstants.FILEBC_NAME, filebcName);
            mCmdLineParams.setProperty(InstallConstants.JMSBC_NAME, jmsbcName);
            mCmdLineParams.setProperty(InstallConstants.SQLSE_NAME, sqlseName);
            
            /*
            mCmdLineParams.setProperty(InstallConstants.IEPSE_NAME, iepseName);
            mCmdLineParams.setProperty(InstallConstants.JDBCBC_NAME, jdbcbcName);
            mCmdLineParams.setProperty(InstallConstants.SMTPBC_NAME, smtpbcName);
            mCmdLineParams.setProperty(InstallConstants.FTPBC_NAME, ftpbcName);
            mCmdLineParams.setProperty(InstallConstants.MQBC_NAME, mqbcName);
						*/



            mCmdLineParams.setProperty(InstallConstants.ENCODERLIB_JAR_NAME, InstallConstants.ENCODERLIB_JAR);
            mCmdLineParams.setProperty(InstallConstants.SAXONLIB_JAR_NAME, InstallConstants.SAXONLIB_JAR);
            mCmdLineParams.setProperty(InstallConstants.WSDLEXTLIB_JAR_NAME, InstallConstants.WSDLEXTLIB_JAR);
            mCmdLineParams.setProperty(InstallConstants.BPELSE_JAR_NAME, InstallConstants.BPELSE_JAR);
            mCmdLineParams.setProperty(InstallConstants.XSLTSE_JAR_NAME, InstallConstants.XSLTSE_JAR);
            mCmdLineParams.setProperty(InstallConstants.FILEBC_JAR_NAME, InstallConstants.FILEBC_JAR);
            mCmdLineParams.setProperty(InstallConstants.JMSBC_JAR_NAME, InstallConstants.JMSBC_JAR);
						mCmdLineParams.setProperty(InstallConstants.SQLSE_JAR_NAME, InstallConstants.SQLSE_JAR);
            
            /*
            mCmdLineParams.setProperty(InstallConstants.IEPSE_JAR_NAME, InstallConstants.IEPSE_JAR);
            mCmdLineParams.setProperty(InstallConstants.JDBCBC_JAR_NAME, InstallConstants.JDBCBC_JAR);
            mCmdLineParams.setProperty(InstallConstants.SMTPBC_JAR_NAME, InstallConstants.SMTPBC_JAR);
            mCmdLineParams.setProperty(InstallConstants.FTPBC_JAR_NAME, InstallConstants.FTPBC_JAR);
            mCmdLineParams.setProperty(InstallConstants.MQBC_JAR_NAME, InstallConstants.MQBC_JAR);
						*/

            mCmdLineParams.setProperty(InstallConstants.AS_INSTALL_DIR, appServerInstallRoot);
            mCmdLineParams.setProperty(InstallConstants.AS_DOMAIN_ROOT, jbiDomainRoot);

            String logFile = jbiDomainRoot + File.separator +
                "logs" + File.separator +
                "jbi-components-configurator.log";

            if (!ant.runAnt(appServerInstallRoot, mAntScriptName,
                InstallConstants.JBI_CONFIGURE_TARGET, mCmdLineParams, logFile))
            {
                mLogger.severe(resourceBundle.getString(
                    "Unable-to-execute-ant-script ") + mAntScriptName);
            }
            deleteDirAndContents(baseDir, true);
            // edit domain.xml for iepse to configure its databse
            //    configDomainXml(jbiDomainRoot);
            // 

        }
        catch(Exception ex) {
            mLogger.severe(
                resourceBundle.getString("configuration-failed") + ex.getMessage());
            throw new AddonException(resourceBundle.getString("configuration-failed") + ex.getMessage());
        }
        mLogger.info(resourceBundle.getString("configuration-successful"));
    }


    /**
     * This method updates the domain xml file
     * for IEPSE to configure its database.
     */
    public boolean configDomainXml(String domainRoot) {

        try {
            String domainXmlfile = domainRoot +
                File.separator + "config" + 
                File.separator + "domain.xml";
            Document document = getDocument(domainXmlfile);
            NodeList resourcesList = document.getElementsByTagName("resources");
            //as there will be only one node named 'resources' so we take the first one.
                Node resources = resourcesList.item(0);
            if (resources == null) {
                return false;
            }
            // adding jdbc-resource for iepse
                NodeList jdbcResourceList = document.getElementsByTagName("jdbc-resource");
            if (!nodeExists(jdbcResourceList, "pool-name", "iepseDBPool")) {
                Element jdbcResourceNode = document.createElement("jdbc-resource");
                jdbcResourceNode.setAttribute("enabled", "true");
                jdbcResourceNode.setAttribute("jndi-name", "jdbc/iepseDB");
                jdbcResourceNode.setAttribute("object-type", "user");
                jdbcResourceNode.setAttribute("pool-name", "iepseDBPool");
                resources.appendChild(jdbcResourceNode);
            }

            // adding jdbc-connection-pool for iepse
                NodeList jdbcConnPoolList = document.getElementsByTagName("jdbc-connection-pool");
            if (!nodeExists(jdbcConnPoolList, "name", "iepseDBPool")) {
                Element jdbcConnPoolNode = document.createElement("jdbc-connection-pool");
                jdbcConnPoolNode.setAttribute("allow-non-component-callers", "false");
                jdbcConnPoolNode.setAttribute("associate-with-thread", "false");
                jdbcConnPoolNode.setAttribute("connection-creation-retry-attempts", "0");
                jdbcConnPoolNode.setAttribute("connection-creation-retry-interval-in-seconds", "10");
                jdbcConnPoolNode.setAttribute("connection-leak-reclaim", "false");
                jdbcConnPoolNode.setAttribute("connection-leak-timeout-in-seconds", "0");
                jdbcConnPoolNode.setAttribute("connection-validation-method", "auto-commit");
                jdbcConnPoolNode.setAttribute("datasource-classname", "org.apache.derby.jdbc.ClientDataSource");
                jdbcConnPoolNode.setAttribute("fail-all-connections", "false");
                jdbcConnPoolNode.setAttribute("idle-timeout-in-seconds", "300");
                jdbcConnPoolNode.setAttribute("is-connection-validation-required", "false");
                jdbcConnPoolNode.setAttribute("is-isolation-level-guaranteed", "false");
                jdbcConnPoolNode.setAttribute("lazy-connection-association", "false");
                jdbcConnPoolNode.setAttribute("lazy-connection-enlistment", "false");
                jdbcConnPoolNode.setAttribute("match-connections", "false");
                jdbcConnPoolNode.setAttribute("max-connection-usage-count", "0");
                jdbcConnPoolNode.setAttribute("max-pool-size", "32");
                jdbcConnPoolNode.setAttribute("max-wait-time-in-millis", "60000");
                jdbcConnPoolNode.setAttribute("name", "iepseDBPool");
                jdbcConnPoolNode.setAttribute("non-transactional-connections", "false");
                jdbcConnPoolNode.setAttribute("pool-resize-quantity", "2");
                jdbcConnPoolNode.setAttribute("res-type", "javax.sql.DataSource");
                jdbcConnPoolNode.setAttribute("statement-timeout-in-seconds", "-1");
                jdbcConnPoolNode.setAttribute("steady-pool-size", "8");
                jdbcConnPoolNode.setAttribute("validate-atmost-once-period-in-seconds", "0");
                jdbcConnPoolNode.setAttribute("wrap-jdbc-objects", "false");

                Element property = document.createElement("property");
                property.setAttribute("name", "connectionAttributes");
                property.setAttribute("value", ";create=true");
                jdbcConnPoolNode.appendChild(property);

                property = document.createElement("property");
                property.setAttribute("name", "PortNumber");
                property.setAttribute("value", "1527");
                jdbcConnPoolNode.appendChild(property);

                property = document.createElement("property");
                property.setAttribute("name", "Password");
                property.setAttribute("value", "iepseDB");
                jdbcConnPoolNode.appendChild(property);

                property = document.createElement("property");
                property.setAttribute("name", "DatabaseName");
                property.setAttribute("value", "iepseDB");
                jdbcConnPoolNode.appendChild(property);

                property = document.createElement("property");
                property.setAttribute("name", "serverName");
                property.setAttribute("value", "localhost");
                jdbcConnPoolNode.appendChild(property);

                property = document.createElement("property");
                property.setAttribute("name", "User");
                property.setAttribute("value", "iepseDB");
                jdbcConnPoolNode.appendChild(property);

                resources.appendChild(jdbcConnPoolNode);
            }

            NodeList serverList = document.getElementsByTagName("server");
            Element server = getElementFromList(serverList, "name", "server");

            NodeList resourceRefList = document.getElementsByTagName("resource-ref");
            if (!nodeExists(resourceRefList, "ref", "jdbc/iepseDB")) {
                Element resourceRefNode = document.createElement("resource-ref");
                resourceRefNode.setAttribute("enabled", "true");
                resourceRefNode.setAttribute("ref", "jdbc/iepseDB");
                if (server != null) {
                    server.appendChild(resourceRefNode);
                }
            }

            TransformerFactory xformFactory = TransformerFactory.newInstance();
            Transformer transformer = xformFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");

            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                document.getDoctype().getPublicId());
            transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                document.getDoctype().getSystemId());

            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");

            Source input = new DOMSource(document);
            Result output = new StreamResult(new File(domainXmlfile));
            transformer.transform(input, output);
            return true;
        }
        catch (Exception ex) {
            ex.printStackTrace();
            return false;
        }
    }

    /**
     * This method updates the domain xml file
     * for IEPSE to unconfigure its database.
     */
    public boolean unconfigDomainXml(String domainRoot)
        {
        try {
            String domainXmlfile = domainRoot +
                File.separator + "config" + 
                File.separator + "domain.xml";
            Document document = getDocument(domainXmlfile);

            NodeList resourcesList = document.getElementsByTagName("resources");
            Node resources = resourcesList.item(0);
            resourcesList = null;
            if (resources.hasChildNodes()) {
                NodeList resourceList = resources.getChildNodes();
                for(int i = 0; i < resourceList.getLength(); i++){
                    Node node = (Node) resourceList.item(i);
                    if (node.getNodeType() == Node.ELEMENT_NODE) {
                        Element res = (Element) node;
                        if (res.getAttribute("pool-name").equals("iepseDBPool") ||
                            res.getAttribute("name").equals("iepseDBPool")) {
                            resources.removeChild(res);
                        }
                    }
                }
            }

            NodeList serverList = document.getElementsByTagName("server");
            Element server = getElementFromList(serverList, "name", "server");

            if (server != null) {
                if (server.hasChildNodes()) {
                    NodeList refList = server.getChildNodes();
                    for(int i = 0; i < refList.getLength(); i++){
                        Node node = (Node) refList.item(i);
                        if (node.getNodeType() == Node.ELEMENT_NODE) {
                            Element ref = (Element) node;
                            if (ref.getAttribute("ref").equals("jdbc/iepseDB")) {
                                server.removeChild(ref);
                            }
                        }
                    }
                }
            }



            TransformerFactory xformFactory = TransformerFactory.newInstance();
            Transformer transformer = xformFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");

            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                document.getDoctype().getPublicId());
            transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                document.getDoctype().getSystemId());
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");

            Source input = new DOMSource(document);
            Result output = new StreamResult(new File(domainXmlfile));
            transformer.transform(input, output);
            return true;

        }
        catch (Exception ex) {
            ex.printStackTrace();
            return false;
        }
    }


    /**
     * This method is to get the element from the node list.
     */
    public Element getElementFromList(NodeList nodeList, String attr, String attrValue) {
        for(int i = 0; i < nodeList.getLength(); i++){
            Node node = (Node) nodeList.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                Element e = (Element) node;
                if (e.getAttribute(attr).equals(attrValue)) {
                    return e;
                }
            }
        }
        return null;
    }


    /**
     * This method decides the node exists or not.
     */
    public boolean nodeExists(NodeList nodeList, String attr, String attrValue) {
        for(int i = 0; i < nodeList.getLength(); i++){
            Node node = (Node) nodeList.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                Element e = (Element) node;
                if (e.getAttribute(attr).equals(attrValue)) {
                    return true;
                }
            }
        }
        return false;
    }


    /** 
                 * This method is used to find/get the component name in the META-INF\jbi.xml of the component jar fi
        le.
                 */
    public String getComponentName(String jarFileName) {
        String componentName = null;
        try {
            JarFile jarFile = new JarFile(jarFileName);
            if (jarFile != null) {
                JarEntry entry = jarFile.getJarEntry(InstallConstants.JBI_FILE_NAME);
                if (entry != null) {
                    InputStream is = jarFile.getInputStream(entry);
                    InputStreamReader isr = new InputStreamReader(is);
                    BufferedReader reader = new BufferedReader(isr);
                    String line;
                    while ((line = reader.readLine()) != null) {
                        int idx1 = line.indexOf("<name>");
                        if (idx1 == -1) {
                            continue;
                        }
                        int idx2 = line.indexOf("</name>");
                        componentName = line.substring(idx1 + 6, idx2);
                        break;
                    }
                    reader.close();
                    jarFile.close();
                } else {
                }
            } else {
            }
        }
        catch (Exception e) {
            mLogger.severe(
                resourceBundle.getString("get-component-name-failed") + e.getMessage());
            e.printStackTrace();
        }
        if (componentName == null) {
            mLogger.severe(
                resourceBundle.getString("get-component-name-failed") + "jar file is " + jarFileName);
        }
        return componentName;
    }

    private boolean deleteDirAndContents(File path, boolean recursive) {
        if(path.exists()) {
            File[] files = path.listFiles();
            for(int i=0; i<files.length; i++) {
                if(files[i].isDirectory()) {
                    if (recursive) {
                        deleteDirAndContents(files[i], recursive);
                    }
                } else {
                    boolean removed = files[i].delete();
                    if (!removed) {
                        files[i].deleteOnExit();
                    }
                }
            }
        }
        boolean deleted = path.delete();
        if (!deleted) {
            path.deleteOnExit();
        }
        return true;
    }


    public void unconfigure(ConfigurationContext cc) throws AddonException
        {
        String appServerInstallRoot = cc.getInstallationContext().
            getInstallationDirectory().getAbsolutePath();
        String jbiDomainRoot = cc.getDomainDirectory().getAbsolutePath();
        ConfigurationType ct = cc.getConfigurationType();
        boolean isDAS = false;
        if (ct == ConfigurationType.DAS) {
            isDAS = true;
        }

        setResourceBundle();
        mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsConfigurator");
        mLogger.info("Unconfiguring the addon jbi components...");


        // Removing the created directories for every component installed
            try {
            File repositoryRoot = new File(jbiDomainRoot + File.separator + "jbi" + 
                File.separator + "components");
            if (repositoryRoot !=  null) {
                File[] subdir = repositoryRoot.listFiles();
                if (subdir != null) {
                    for (int i=0; i < subdir.length; i++) {
                        if (subdir[i].getName().indexOf(InstallConstants.BPELSE) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.XSLTSE) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.FILEBC) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.JMSBC) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.SQLSE) != -1 ) {
                            /*
                            subdir[i].getName().indexOf(InstallConstants.JDBCBC) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.FTPBC) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.MQBC) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.IEPSE) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.SMTPBC) != -1) {
                            */
                            deleteDirAndContents(subdir[i], true);
                        }
                    }
                }
            }

            repositoryRoot = new File(jbiDomainRoot + File.separator + "jbi" + 
                File.separator + "shared-libraries");
            if (repositoryRoot !=  null) {
                File[] subdir = repositoryRoot.listFiles();
                if (subdir != null) {
                    for (int i=0; i < subdir.length; i++) {
                        if (subdir[i].getName().indexOf(InstallConstants.ENCODER) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.WSDLEXT) != -1 ||
                            subdir[i].getName().indexOf(InstallConstants.SAXON) != -1 ) {
                            deleteDirAndContents(subdir[i], true);
                        }
                    }
                }
            }
            // unconfigure domain.xml for iepse
            //    unconfigDomainXml(jbiDomainRoot);

            // remove the installed databases for iepse
            /*
            File dbDir = new File(System.getProperty("user.home") + File.separator + ".netbeans-derby");
            if (dbDir != null) {
                deleteDirAndContents(dbDir, true);
            }
            dbDir = new File(appServerInstallRoot + File.separator + "databases" + File.separator + "iepseDB");
            deleteDirAndContents(dbDir, true);

            File dbFile = new File(appServerInstallRoot + File.separator + "databases" + File.separator + "iepseDB.zip");
            dbFile.delete();

            dbFile = new File(appServerInstallRoot + File.separator + "databases" + File.separator + "derby.log");
            dbFile.delete();

            dbFile = new File(appServerInstallRoot + File.separator + "databases" + File.separator + "derby.properties");
            dbFile.delete();
            */

        }
        catch (Exception ex) {
            mLogger.severe(
                resourceBundle.getString("unconfiguration-failed") + ex.getMessage());
            throw new AddonException(resourceBundle.getString("unconfiguration-failed") + ex.getMessage());
        }

        /*
                                 * Removing the component entries in jbi-registry.xml
                                 */
        String jbiRegistryfile = jbiDomainRoot + File.separator +
            "jbi" + File.separator + "config" + 
            File.separator + "jbi-registry.xml";
        try {
            Document document = getDocument(jbiRegistryfile);
            NodeList componentList = document.getElementsByTagName("components");
            Node comps = componentList.item(0);
            componentList = null;
            if (comps.hasChildNodes()) {
                componentList = comps.getChildNodes();
                for(int i = 0; i < componentList.getLength(); i++){
                    Node node = (Node) componentList.item(i);
                    if (node.getNodeType() == Node.ELEMENT_NODE) {
                        Element comp = (Element) node;
                        if (comp.getAttribute("file-name").equals(InstallConstants.BPELSE_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.XSLTSE_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.FILEBC_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.JMSBC_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.SQLSE_JAR) || 
                            comp.getAttribute("file-name").equals(InstallConstants.SAXONLIB_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.WSDLEXTLIB_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.ENCODERLIB_JAR)) {
                            /*
                            comp.getAttribute("file-name").equals(InstallConstants.JDBCBC_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.FTPBC_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.MQBC_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.IEPSE_JAR) ||
                            comp.getAttribute("file-name").equals(InstallConstants.SMTPBC_JAR)) {
                            */
                            comps.removeChild(comp);
                        }
                    }
                }
            }

            NodeList slsList = document.getElementsByTagName("shared-libraries");
            Node sls = slsList.item(0);
            slsList = null;
            if (sls.hasChildNodes()) {
                NodeList slList = sls.getChildNodes();
                for(int i = 0; i < slList.getLength(); i++){
                    Node node = (Node) slList.item(i);
                    if (node.getNodeType() == Node.ELEMENT_NODE) {
                        Element sl = (Element) node;
                        if (sl.getAttribute("file-name").equals(InstallConstants.ENCODERLIB_JAR) ||
                            sl.getAttribute("file-name").equals(InstallConstants.WSDLEXTLIB_JAR) ||
                            sl.getAttribute("file-name").equals(InstallConstants.SAXONLIB_JAR)) {
                            sls.removeChild(sl);
                        }
                    }
                }
            }

            NodeList serverList = document.getElementsByTagName("server");
            if (serverList.getLength() > 0) {
                for(int i = 0; i < serverList.getLength(); i++){
                    Node server = (Node) serverList.item(i);
                    NodeList compRefList = server.getChildNodes();
                    for (int j = 0; j < compRefList.getLength(); j++) {
                        Node node = (Node) compRefList.item(j);
                        if (node.getNodeType() == Node.ELEMENT_NODE) {
                            Element compRef = (Element) node;
                            String compNameRef = compRef.getAttribute("name-ref");
                            // either DAS or not, remove the components ref from all server nodes
                                if (compNameRef.indexOf(InstallConstants.BPELSE) != -1 ||
                                compNameRef.indexOf(InstallConstants.XSLTSE) != -1 ||
                                compNameRef.indexOf(InstallConstants.FILEBC) != -1 ||
                                compNameRef.indexOf(InstallConstants.JMSBC) != -1 ||
                                compNameRef.indexOf(InstallConstants.SQLSE) != -1 ||
                                compNameRef.indexOf(InstallConstants.SAXON) != -1 ||
                                compNameRef.indexOf(InstallConstants.WSDLEXT) != -1 ||
                                compNameRef.indexOf(InstallConstants.ENCODER) != -1 ) {
                                /*
                                compNameRef.indexOf(InstallConstants.JDBCBC) != -1 ||
                                compNameRef.indexOf(InstallConstants.FTPBC) != -1 ||
                                compNameRef.indexOf(InstallConstants.MQBC) != -1 ||
                                compNameRef.indexOf(InstallConstants.IEPSE) != -1 ||
                                compNameRef.indexOf(InstallConstants.SMTPBC) != -1) {
                                */
                                server.removeChild(compRef);
                            }
                        }
                    }
                }
            }

            TransformerFactory xformFactory = TransformerFactory.newInstance();
            Transformer transformer = xformFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            /*
                                                transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                                                        document.getDoctype().getPublicId());
                                                transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                                                        document.getDoctype().getSystemId());
                                                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION
                , "no");
                                                */
            Source input = new DOMSource(document);
            Result output = new StreamResult(new File(jbiRegistryfile));
            transformer.transform(input, output);

        }
        catch (Exception ex) {
            mLogger.severe(
                resourceBundle.getString("unconfiguration-failed") + ex.getMessage());
            throw new AddonException(resourceBundle.getString("unconfiguration-failed") + ex.getMessage());
        }
        mLogger.info(resourceBundle.getString("unconfiguration-successful"));
    }

    /**
     * This method is used during an upgrade.
     */
    public void upgrade (ConfigurationContext cc, AddonVersion ver) {
    }

    /**
     * This method is used to parse the JBI registry file and return a document.
     */
    public static Document getDocument(String jbiRegistryFile) {
        DocumentBuilderFactory factory =
            DocumentBuilderFactory.newInstance();
        factory.setValidating(false);
        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            builder.setEntityResolver(new EntityResolver(){
                public InputSource resolveEntity(
                    String publicId,
                    String systemId) throws SAXException, IOException
                    {
                    StringReader reader =
                        new StringReader(
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                        InputSource source = new InputSource(reader);
                        source.setPublicId(publicId);
                        source.setSystemId(systemId);
                        return source;
                }
            }
            );
            Document document = builder.parse(new File(jbiRegistryFile));
            return document;
        }
        catch (SAXException sxe) {
            sxe.printStackTrace();
            return null;
        }
        catch (ParserConfigurationException pce) {
            pce.printStackTrace();
            return null;
        }
        catch (IOException ioe) {
            ioe.printStackTrace();
            return null;
        }
    }

    public void enable(ConfigurationContext cc) throws AddonException {
        throw new AddonException("Not implemented yet");
    }

    public void disable(ConfigurationContext cc) throws AddonException {
        throw new AddonException("Not implemented yet");
    }

    private void setLogger() {
        mLogger = Logger.getLogger("com.sun.jbi.installer.JBIComponentsConfigurator");
    }

    private Logger getLogger() {
        return mLogger;
    }

    private void setResourceBundle() {
        resourceBundle =
            ResourceBundle.getBundle(InstallConstants.RESOURCE_BUNDLE);
    }

    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }

}
