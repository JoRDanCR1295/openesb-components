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
 * @(#)ConfigPersistence.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.crl.util.I18n;

/**
 * Handles Configuration Persistence
 * @author Sun Microsystems
 */
public class ConfigPersistence {
    /**
     * Default file name of the persistent configuration properties file
     */
    public static final String PERSISTENT_CONFIG_FILE_NAME = "config.properties";

    /**
     * Element name expected for the jbi.xml extension element defining default configuration
     */
    public static final String INSTALLATION_CONFIGURATION_EXTENSION_ELEMENT = "Configuration";

    private static final Logger mLogger = 
    		Logger.getLogger(ConfigPersistence.class.getPackage().getName());

    /**
     * Parse the installation configuration extensions to get the default configuration
     * This expects an extension with an element name "Configuration" that contains elements named
     * according to the configuration properties with a text value representing the default, e.g.
     * <code>
     *  <prefix:Configuration>
     *      <prefix:ConfigPropertyName1>ConfigPropertyValue1</config:ConfigPropertyName1>
     *      <prefix:ConfigPropertyName2>ConfigPropertyValue2</config:ConfigPropertyName2>
     *  </prefix:Configuration>
     * <code>
     * @param jbiConfigurationExtension the jbi.xml configuration extension document fragment
     * @return the extracted default configuration properties
     * @throws JBIException if the configuration could not be parsed
     */
    public static Properties parseDefaultConfig(DocumentFragment jbiConfigurationExtension) throws JBIException {
        Properties defaultConfig = new Properties();

        // If there are Configuration defaults in the jbi.xml, extract them
        try {
            if (jbiConfigurationExtension != null) {
                NodeList nodes = jbiConfigurationExtension.getChildNodes();
                for (int nodeCount = 0; nodeCount < nodes.getLength(); nodeCount++) {
                    Node aNode = nodes.item(nodeCount);
                    if (aNode != null && aNode.getNodeType() == Node.ELEMENT_NODE) {
						// Beware that the node prefix, localname and namespace URI can be null for this document fragment
						// Therefore Look for the local name by parsing
						String qualifiedName = aNode.getNodeName();
						String elementName = qualifiedName;
						if (qualifiedName != null) {
							int colonPos = qualifiedName.indexOf(':');
							if (colonPos > -1) {
								elementName = qualifiedName.substring(colonPos + 1);
							}
						}
						// See whether there is a "Configuration" element defined
						if (elementName != null && elementName.equals(INSTALLATION_CONFIGURATION_EXTENSION_ELEMENT)) {
							if (mLogger.isLoggable(Level.FINE)) {
								mLogger.fine("CRL-3001: Default configuration located in installation extension.");
							}
							NodeList elemChildren = aNode.getChildNodes();
							for (int elemChildCount = 0; elemChildCount < elemChildren.getLength(); elemChildCount++) {
								Node elemChild = elemChildren.item(elemChildCount);
								// Get the name of the element ant the text content
								if (elemChild.getNodeType() == Node.ELEMENT_NODE) {
									String propNodeName = elemChild.getNodeName();
									if (propNodeName != null) {
										int propColonPos = propNodeName.indexOf(':');
										if (propColonPos > -1) {
											propNodeName = propNodeName.substring(propColonPos + 1);
										}
									}
									String textVal = elemChild.getTextContent();
									if (mLogger.isLoggable(Level.FINER)) {
										mLogger.finer("CRL-2001: Configuration default for property "+ 
														propNodeName + " set to: " + textVal);
									}
									defaultConfig.setProperty(propNodeName, textVal);
								}
							}
						}
					} 
                    else if (mLogger.isLoggable(Level.FINE)) {
						mLogger.fine("CRL-3002: No installation extension details defined.");
					}
                }
            }
        } 
        catch (Exception ex) {
        	String msg = I18n.loc("CRL-6001: Failed to acquire default configuration from jbi descriptor: {0}",
        						ex.getMessage());
        	mLogger.warning(msg);
        	throw new JBIException(msg, ex);
        }
        
        if (mLogger.isLoggable(Level.CONFIG)) {
        	mLogger.config(I18n.loc("CRL-4001: Parsed default configuration properties: {0}", 
        						  String.valueOf(defaultConfig)));
        }
        
        return defaultConfig;
    }


    /**
     * Persist the "initial" configuration from an installer extension MBean
     * @param mbServer mbean server reference
     * @param installerExtName the name of the installer extension MBean for which to save its settings
     * @param workspaceRoot the workspace root directory of the component
     * @param defaultConfig default properties or null if there are none. These could come from the jbi xml configuration extension. @see #parseDefaultConfig(DocumentFragment)
     * If the same configurable setting is set via the installer extension MBean, that value takes precedence and overrides this default.
     * @throws JBIException if the configuration could not be saved
     */
    public static void persistInitialConfig(MBeanServer mbServer, 
    										ObjectName installerExtName, 
    										String workspaceRoot, 
    										Properties defaultConfig) throws JBIException {
        Properties persistentConfig = defaultConfig;
        if (persistentConfig == null) {
            persistentConfig = new Properties();
        }

        // Get the values from the Installer Extension MBean, override defaults if present.
        try {
            MBeanInfo mbInfo = mbServer.getMBeanInfo(installerExtName);
            MBeanAttributeInfo[] attrs = mbInfo.getAttributes();

            for (int attrCount = 0, n = attrs.length; attrCount < n; attrCount++) {
                String attributeName = attrs[attrCount].getName();
                Object attr = mbServer.getAttribute(installerExtName, attributeName);
                // Only save attributes that have a value set.
                if (attr != null) {
                    persistentConfig.setProperty(attributeName, attr.toString());
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer("CRL-2002: Override default for "+ 
                        			  attributeName +": "+ attr.toString());
                    }
                }
            }

            persistConfig(workspaceRoot, persistentConfig);
        } 
        catch (JMException ex) {
        	String msg = I18n.loc("CRL-6002: Failed to retrieve configuration settings from MBean {0}: {1}",
        						installerExtName, ex.getMessage());
        	mLogger.warning(msg);
            throw new JBIException(msg, ex);
        }
    }

    /**
     * Persist the configuration properties provided - typically from the runtime configuration
     *
     * @param workspaceRoot the workspace root directory of the component
     * @param persistentConfig the configuration properties to save
     * @throws JBIException if the configuration could not be saved
     */
    public static void persistConfig(String workspaceRoot, Properties persistentConfig) throws JBIException {
        File persistentConfigName = new File(workspaceRoot, PERSISTENT_CONFIG_FILE_NAME);
        try {
            synchronized (ConfigPersistence.class) {
                OutputStream os = new FileOutputStream(persistentConfigName);
                persistentConfig.store(os, null);
                os.close();
            }
        } 
        catch (IOException ex) {
            String msg = I18n.loc("CRL-6003: Failed to persist configuration settings to file {0}: {1}",
            		 			persistentConfigName, ex.getMessage());
            mLogger.warning(msg);
            throw new JBIException(msg, ex);
        }
    }

    /**
     * Load the configuration properties
     * @param workspaceRoot the workspace root directory of the component
     * @throws JBIException if the configuration could not be loaded
     * @return the configuration properties
     */
    public static Properties loadConfig(String workspaceRoot) throws JBIException {
        Properties persistentConfig = new Properties();
        File persistentConfigName = new File(workspaceRoot, PERSISTENT_CONFIG_FILE_NAME);
        try {
            synchronized (ConfigPersistence.class) {
                InputStream is = new FileInputStream(persistentConfigName);
                persistentConfig.load(is);
                is.close();
            }
        } 
        catch (IOException ex) {
            String msg = I18n.loc("CRL-6004: Failed to load configuration {0}: {1}",
            					persistentConfigName, ex.getMessage());
            mLogger.warning(msg);
        	throw new JBIException(msg, ex);
        }

        return persistentConfig;
    }
}
