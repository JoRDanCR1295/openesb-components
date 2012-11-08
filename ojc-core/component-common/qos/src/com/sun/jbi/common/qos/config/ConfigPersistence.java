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

package com.sun.jbi.common.qos.config;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;

import org.w3c.dom.Document;

import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * Utility to aid in the storing and loading of component configuration.
 * 
 * @author Kevan Simpson
 */
public class ConfigPersistence {
    private static Logger mLogger = Logger.getLogger(ConfigPersistence.class.getName());
    
    /** The name of the file to which component configuration is persisted. */
    public static final String CONFIG_PERSISTENCE_FILE = "config.properties";
    /** The name of the file to which application configuration is persisted. */
    public static final String APPLICATION_PERSISTENCE_FILE = "config-app-persistence.xml";
    
    /**
     * Loads application configuration values from {@link #APPLICATION_PERSISTENCE_FILE}.
     * 
     * @param config The component configuration to load. 
     * @param workspaceRoot The workspace root of the component.
     * @throws DeploymentException if an error occurs loading configuration.
     */
    public static void loadApplicationConfig(ComponentConfig config,
                                             String workspaceRoot) throws DeploymentException {
        loadApplicationConfig(config, workspaceRoot, APPLICATION_PERSISTENCE_FILE);
    }

    /**
     * Loads application configuration values from the specified file.
     * 
     * @param config The component configuration to load. 
     * @param workspaceRoot The workspace root of the component.
     * @param filename The persistence file.
     * @throws DeploymentException if an error occurs loading configuration.
     */
    private static void loadApplicationConfig(ComponentConfig config,
                                              String workspaceRoot,
                                              String filename) throws DeploymentException {
        if (config != null && !Util.isEmpty(workspaceRoot) && !Util.isEmpty(filename)) {
            if (config.supportsAppConfigs() || config.supportsAppVars()) {
                File file = new File(workspaceRoot, filename);
                if (file == null || !file.exists()) {
                    mLogger.warning(I18n.loc(
                            "QOS-6053: Component Application Configuration file {0} does not exist!", file));
                    return;
                }
                
                try {
                    AppPersistenceParser apParser = new AppPersistenceParser(config);
                    Document doc = XmlUtil.readXml(file);
                    if (doc != null) {
                    	apParser.parse(doc.getDocumentElement());
                    }
                }
                catch (Exception e) {
                  String msg = I18n.loc("QOS-6054: Failed to load application configuration {0}: {1}",
                                        file, e.getMessage());
                  mLogger.warning(msg);
                  throw new DeploymentException(msg, e);
                }
            }
        }
    }

    /**
     * Loads component property values from {@link #CONFIG_PERSISTENCE_FILE}.
     * 
     * @param config The component configuration to load. 
     * @param workspaceRoot The workspace root of the component.
     * @throws DeploymentException if an error occurs loading configuration.
     */
    public static void loadConfig(ComponentConfig config, 
                                  String workspaceRoot) throws DeploymentException {
        loadConfig(config, workspaceRoot, CONFIG_PERSISTENCE_FILE);
    }

    /**
     * Loads component property values from the specified file.
     * 
     * @param config The component configuration to load. 
     * @param workspaceRoot The workspace root of the component.
     * @param filename The persistence file.
     * @throws DeploymentException if an error occurs loading configuration.
     */
    private static void loadConfig(ComponentConfig config,
                                   String workspaceRoot, 
                                   String filename) throws DeploymentException {
        if (!Util.isEmpty(workspaceRoot) && !Util.isEmpty(filename)) {
            Properties props = new Properties();
            File file = new File(workspaceRoot, filename);
            if (file == null || !file.exists()) {
                mLogger.warning(I18n.loc(
                        "QOS-6029: Component Configuration file {0} does not exist!", file));
                return;
            }
            
            try {
                synchronized (ConfigPersistence.class) {
                    InputStream is = new FileInputStream(file);
                    props.load(is);
                    is.close();
                }
            } 
            catch (IOException ex) {
                String msg = I18n.loc("QOS-6016: Failed to load configuration {0}: {1}",
                                      file, ex.getMessage());
                mLogger.warning(msg);
                throw new DeploymentException(msg, ex);
            }

            toConfig(config, props);
        }
    }

    /**
     * Persists application configurations and variables to a file 
     * ({@link #APPLICATION_PERSISTENCE_FILE}).
     * 
     * @param config The component configuration to persist.
     * @param workspaceRoot The workspace root of the component.
     * @throws DeploymentException if an error occurs persisting.
     */
    public static void persistApplicationConfig(ComponentConfig config, String workspaceRoot) 
            throws DeploymentException {
        persistApplicationConfig(config, workspaceRoot, APPLICATION_PERSISTENCE_FILE);
    }

    /**
     * Persists application configurations and variables to the specified file. 
     * 
     * @param config The component configuration to persist.
     * @param workspaceRoot The workspace root of the component.
     * @param filename The persisted file.
     * @throws DeploymentException if an error occurs persisting.
     */
    private static void persistApplicationConfig(ComponentConfig config, 
                                                 String workspaceRoot,
                                                 String filename) throws DeploymentException {
        if (config != null && !Util.isEmpty(workspaceRoot) && !Util.isEmpty(filename)) {
            if (config.supportsAppConfigs() || config.supportsAppVars()) {
                try {
                    String xml = AppPersistenceParser.toAppXml(config);
                    synchronized (ConfigPersistence.class) {
                        BufferedWriter wr = new BufferedWriter(new FileWriter(
                                new File(workspaceRoot, filename)));
                        wr.write(xml);
                        wr.flush();
                        wr.close();
                    }
                } 
                catch (IOException ex) {
                    String msg = I18n.loc(
                            "QOS-6055: Failed to persist application configuration settings to file {0}: {1}",
                            String.valueOf(filename), ex.getMessage());
                    mLogger.warning(msg);
                    throw new DeploymentException(msg, ex);
                }
            }
        }
    }

    /**
     * Persists the specified component configuration in the workspace using 
     * the default filename, {@link #CONFIG_PERSISTENCE_FILE}.
     * 
     * @param config A component's configuration.
     * @param workspaceRoot The component's workspace root.
     * @throws DeploymentException if an error occurs persisting configuration.
     */
    public static void persistConfig(ComponentConfig config, String workspaceRoot) 
            throws DeploymentException {
        persistConfig(config, workspaceRoot, CONFIG_PERSISTENCE_FILE);
    }
    
    /**
     * Persists the specified component configuration in the workspace using
     * the specified filename.
     * 
     * @param config A component's configuration.
     * @param workspaceRoot The component's workspace root.
     * @param filename The filename to which configuration is persisted.
     * @throws DeploymentException if an error occurs persisting configuration.
     */
    private static void persistConfig(ComponentConfig config, 
                                      String workspaceRoot, String filename) 
            throws DeploymentException {
        if (config != null && !Util.isEmpty(workspaceRoot) && !Util.isEmpty(filename)) {
            try {
                Properties props = toProperties(config);
                synchronized (ConfigPersistence.class) {
                    OutputStream os = new FileOutputStream(
                            new File(workspaceRoot, filename));
                    props.store(os, null);
                    os.close();
                }
            } 
            catch (IOException ex) {
                String msg = I18n.loc(
                        "QOS-6017: Failed to persist configuration settings to file {0}: {1}",
                        String.valueOf(filename), ex.getMessage());
                mLogger.warning(msg);
                throw new DeploymentException(msg, ex);
            }
        }
    }

    // package access for testing, can be made public if need arises...
    static void toConfig(ComponentConfig config, Properties props) {
        if (props != null) {
            Set<Object> keySet = new TreeSet<Object>(props.keySet());
            for (Object obj : keySet) {
                String key = (String) obj;
                int ix = key.lastIndexOf(".");
                if (ix > 0) {
                    try {
                        int index = Integer.parseInt(key.substring(ix + 1));
                        Property p = config.getProperty(key.substring(0, ix));
                        p.insertValue(props.getProperty(key), index);
                        config.addProperty(p);  // may not be added yet...
                        continue;
                    }
                    catch (NumberFormatException nfe) {
                        // oops... assume key is not indexed
                        // fall through and assume single value
                    }
                }

                // single value
                Property p = config.getProperty(key);
                p.setValue(props.getProperty(key));

                if (p.getType() == null) {  // not defined in component desc
                    config.addProperty(p);
                }
            }
        }
    }
    
    // package access for testing, can be made public if need arises...
    static Properties toProperties(ComponentConfig config) {
        Properties props = new Properties();
        if (config != null) {
            Set<Property> set = config.propertySet();
            String val = null;
            for (Property p : set) {
                
                if (p.count() > 1) {
                    List<String> values = p.values();
                    for (int i = 0, n = values.size(); i < n; i++) {
                        val = values.get(i);    // null-safe Properties#HashTable
                        if (val != null) {
                            props.setProperty(p.getName() +"."+ i, val);
                        }
                    }
                }
                else {  // 0-1 value, no index
                    val = p.getValue(); // null-safe Properties#HashTable
                    if (val != null) {
                        props.setProperty(p.getName(), val);
                    }
                }
            }
        }
        return props;
    }
}
