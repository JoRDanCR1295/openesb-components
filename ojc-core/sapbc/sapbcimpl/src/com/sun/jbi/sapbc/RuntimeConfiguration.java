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
 * @(#)RuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

import java.io.BufferedReader;
import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.Notification;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanException;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ListenerNotFoundException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;

/**
 * This is the runtime configuration MBean. 
 * It allows configurations to be changed at run-time
 *
 * @author aegloff
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {
    private static final Messages mMessages =
            Messages.getMessages(RuntimeConfiguration.class);
    
    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
    public static final String ENVIRONMENT_VARIABLES= "EnvironmentVariables";
    
    // Configuration validation settings
    private static long MIN_THREADS = 1;
    private static long MAX_THREADS = 10000;
    
    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";
    
    // Configuration file name for environment variables
    private static final String PERSIST_ENVVAR_CONFIG_FILE_NAME = "EnvVarConfig.properties";
    
    // Component configurations
    Properties mConfig;
    
    // Global environment configurations
    Map mEnvVarMap;
    CompositeType mEnvVarRowType = null;
    TabularType mEnvVarTabularType = null;
    
    // Component work root
    String mWorkspaceRoot;
    String mConfigSchema;
    String mConfigData;
    
    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
    
    public RuntimeConfiguration(String workspaceRoot, String configSchema, String configData) throws JBIException, OpenDataException {
        mWorkspaceRoot = workspaceRoot;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        mEnvVarMap = loadEnvironmentVariableConfig(workspaceRoot);
        mConfigSchema = configSchema;
        mConfigData = configData;
    }

    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;
        
        // Validate the attribute value
        Integer newVal = null;
        Integer oldVal = getThreads();
        
        try {
            newVal = val; 
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(
                    mMessages.getString(
                            "RuntimeConfiguration.Invalid_arg",
                            new Object[] {
                                    attrName, ex.getMessage()
                    }));
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(
                mMessages.getString(
                        "RuntimeConfiguration.Invalid_attr", 
                        new Object[] {
                                newVal,
                                attrName,
                                new Long(MIN_THREADS),
                                new Long(MAX_THREADS)}));
        }
        
        // Apply and save the changes
        mConfig.put(CONFIG_THREADS, val.toString()); 
        persistConfiguration();
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("RuntimeConfiguration.Attr_changed");
        String attrType = Integer.class.getName();
        Notification notif = new AttributeChangeNotification(
                this, 
                seqNo, 
                System.currentTimeMillis(), 
                msg, 
                attrName, 
                attrType, 
                oldVal, 
                newVal);
        broadcasterSupport.sendNotification(notif);
    }
    
    public TabularData getEnvironmentVariables() throws OpenDataException {
        TabularData tabularData = createEnvironmentVariableTabularStructure();
        for (Iterator iter = mEnvVarMap.keySet().iterator(); iter.hasNext(); ) {
            String name = (String) iter.next();
            String value = (String) mEnvVarMap.get(name);
            Object[] data = new Object[] {name, value};
            
            CompositeData rowData = new CompositeDataSupport(mEnvVarRowType,
                    new String[] { "name", "value" },
                    data);
            tabularData.put(rowData);
        }
        
        return tabularData;
    }
    
    public Map retrieveEnvVariablesMap() {
        return mEnvVarMap;
    }
    
    public void updateEnvVariablesMap(Map envVarMap) throws MBeanException {
        mEnvVarMap = envVarMap;
        persistEnvVariableConfiguration();
    }
    
    public void setEnvironmentVariables(TabularData val) throws InvalidAttributeValueException, OpenDataException, MBeanException {
        String attrName = ENVIRONMENT_VARIABLES;
        String mapKey = null;
        TabularData newVal = val;
        List items = new ArrayList();
        CompositeType rowType = val.getTabularType().getRowType();
        
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(
                    mMessages.getString("RuntimeConfiguration.Invalid_arg", new Object[] {attrName, ex.getMessage()}));
        }
        
        // Clear the old environment configurations
        mEnvVarMap.clear();
        
        for (Iterator rowTypeItems = rowType.keySet().iterator(); rowTypeItems.hasNext(); ) {
            String item = (String)(rowTypeItems).next();
            items.add(item);
        }
        
        int itemSize = items.size();
        if (itemSize <= 0) {
            throw new InvalidAttributeValueException(mMessages.getString("RuntimeConfiguration.Invalid_Item_Size", itemSize));
        } else if (items.size() > 2) {
            throw new InvalidAttributeValueException(mMessages.getString("RuntimeConfiguration.Invalid_Item_Size", itemSize));
        }
        
        // getting the row index
        List rowIndex = val.getTabularType().getIndexNames();
        int rowIndexSize = rowIndex.size();
        if (rowIndexSize > 1) {
            throw new InvalidAttributeValueException(mMessages.getString("RuntimeConfiguration.Invalid_RowIndex_Size", rowIndexSize));
        } else if (rowIndex.size() <= 0) {
            throw new InvalidAttributeValueException(mMessages.getString("RuntimeConfiguration.Invalid_RowIndex_Size", rowIndexSize));
        } else {
            mapKey = (String) rowIndex.get(0);
            if (mapKey == null || "".equals(mapKey)) {
                throw new InvalidAttributeValueException(mMessages.getString("RuntimeConfiguration.Invalid_RowIndex_Key", mapKey));
            }
        }
        
        for (Iterator dataIter = val.values().iterator(); dataIter.hasNext(); ) {
            CompositeData rowData = (CompositeData) dataIter.next();
            
            String name = null;
            String value = null;
            for (ListIterator<String> itemIt = items.listIterator(); itemIt.hasNext(); ) {
                String item = itemIt.next();
                if (mapKey.equals(item)) {
                    name = (String)rowData.get(item);
                } else {
                    value = (String) rowData.get(item);
                }
            }
            mEnvVarMap.put(name, value);
        }
        
        
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("RuntimeConfiguration.Attr_changed");
        String attrType = TabularData.class.getName();
        TabularData oldVal = getEnvironmentVariables();
        Notification notif = new AttributeChangeNotification(this,
                seqNo,
                System.currentTimeMillis(),
                msg,
                attrName,
                attrType,
                oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);
        
        // persist the env variable properties
        persistEnvVariableConfiguration();
    }
    
    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            throw new MBeanException(
                    ex,
                    mMessages.getString(
                            "RuntimeConfiguration.Failed_persist",
                            new Object[] { mWorkspaceRoot, ex.getMessage() })
                    );
        }
    }
    void persistEnvVariableConfiguration() throws MBeanException {
        // Persist the changed configuration
        PrintWriter writer = null;
        try {
            File envVarPersistFileName = new File(mWorkspaceRoot, PERSIST_ENVVAR_CONFIG_FILE_NAME);
            writer = new PrintWriter(envVarPersistFileName);
            String key = null;
            String value = null;
            for (Iterator iter = mEnvVarMap.keySet().iterator(); iter.hasNext(); ) {
                key = (String) iter.next();
                value = (String) mEnvVarMap.get(key);
                writer.println(key + "=" + (value != null ? value : ""));
            }
            writer.close();
        } catch (IOException ex) {
            throw new MBeanException(ex, mMessages.getString("RuntimeConfiguration.Failed_persist_env_var",
                    new Object[] {mWorkspaceRoot, ex.getMessage()}));
        } catch (Exception ee) {
            throw new MBeanException(ee, mMessages.getString("RuntimeConfiguration.Failed_persist_env_var",
                    new Object[] {mWorkspaceRoot, ee.getMessage()}));
        } finally {
            if ( writer != null ) {
                try {
                    writer.close();
                } catch (Exception e) {
                    //
                }
            }
        }
    }
    public TabularData createEnvironmentVariableTabularStructure() throws OpenDataException {
        TabularData tabularData = null;
        
        String[] envVarRowAttrNames = { "name", "value" };
        String[] envVarRowAttrDesc = { "Environment variable name", "Environment variable value" };
        OpenType[] envVarRowAttrTypes = { SimpleType.STRING, SimpleType.STRING };
        String[] envVarRowIndex = { "name" };
        
        if (mEnvVarRowType == null) {
            mEnvVarRowType = new CompositeType("NameValuePair",
                    "Environment variable name and value pair",
                    envVarRowAttrNames,
                    envVarRowAttrDesc,
                    envVarRowAttrTypes);
        }
        
        if (mEnvVarTabularType == null) {
            mEnvVarTabularType = new TabularType("EnvironmentVariableList",
                    "List of environment name and value pairs",
                    mEnvVarRowType,
                    envVarRowIndex);
        }
        
        tabularData = new TabularDataSupport(mEnvVarTabularType);
        
        return tabularData;
    }
    
    public Map loadEnvironmentVariableConfig(String workspaceRoot) throws JBIException {
        Properties persistedConfig = new Properties();
        Map envVarMap = new HashMap();
        
        File envVarPersistFileName = new File(workspaceRoot, PERSIST_ENVVAR_CONFIG_FILE_NAME);
        if (!envVarPersistFileName.exists()) {
            return envVarMap;
        }
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(envVarPersistFileName));
            String line = null;
            int index = 0;
            String key = null;
            String value = null;
            while ( (line = reader.readLine()) != null ) {
                if ( line.trim().length() == 0 )
                    continue;
                if ( (index=line.indexOf("=")) > 0 ) {
                    key = line.substring(0, index).trim();
                    if ( key != null && key.length() > 0 ) {
                        value = (index == line.length() - 1 ) ? "" : line.substring(index+1).trim();
                        persistedConfig.put(key, value);
                    } else {
                        // malformed line
                    }
                } else {
                    // invalid line - skip
                }
            }
            reader.close();
            
        } catch (IOException ex) {
            throw new JBIException(mMessages.getString("RuntimeConfiguration.Failed_loading_env_vars", new Object[] {envVarPersistFileName, ex}));
        } finally {
            if ( reader != null )
                try {
                    reader.close();
                } catch (Exception e) {
                    //
                }
        }
        
        // load the persisted environment variable configurations in the map
        for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
            String name = (String) e.nextElement();
            String value = persistedConfig.getProperty(name);
            envVarMap.put(name, value);
        }
        
        return envVarMap;
    }
    
    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(
                    new String[] { AttributeChangeNotification.ATTRIBUTE_CHANGE },
                    AttributeChangeNotification.class.getName(), 
                    mMessages.getString("RuntimeConfiguration.Attr_changed"))
        };
    }

    public void addNotificationListener(NotificationListener listener, 
                                        NotificationFilter filter, 
                                        Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) 
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, 
                                           NotificationFilter filter, 
                                           Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    
    public String retrieveConfigurationDisplaySchema() {
        return mConfigSchema;
    }

    public String retrieveConfigurationDisplayData() {
        return mConfigData;
    }
}
