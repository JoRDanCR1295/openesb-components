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

package com.sun.jbi.swiftbc;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;

import javax.jbi.JBIException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 * 
 * @author Nageswara rao
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {

    private static final Messages mMessages = Messages.getMessages(com.sun.jbi.swiftbc.RuntimeConfiguration.class);

    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
	public static final String ENVIRONMENT_VARIABLES= "EnvironmentVariables";


    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";


	 // Configuration file name for environment variables
    private static final String PERSIST_ENVVAR_CONFIG_FILE_NAME = "EnvVarConfig.properties";

    // Configuration validation settings
    long MIN_THREADS = 1;

    long MAX_THREADS = 10000;

    // Configuration
    Properties mConfig;

	// Global environment configurations
    Map mEnvVarMap;
    CompositeType mEnvVarRowType = null;
    TabularType mEnvVarTabularType = null;

    String mWorkspaceRoot;

    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    /** Creates a new instance of RuntimeConfiguration */
    public RuntimeConfiguration(String workspaceRoot) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
		mEnvVarMap = loadEnvironmentVariableConfig(workspaceRoot);
		restore(mConfig);
    }

    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }
    
  
  
      
    /**
     * setter for number of Threads
     *
     * @param val number of threads
     */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_arg", new Object[] { attrName,
                    ex.getMessage() }));
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_attr", new Object[] { newVal,
                    attrName, new Long(MIN_THREADS), new Long(MAX_THREADS) }));
        }

        String oldValue = getThreads().toString();
        
        // Apply and save the changes
        mConfig.put(CONFIG_THREADS, val.toString());
        persistConfiguration();
        saveAndNotifyListners("RTC.Threads_Attribute_changed", CONFIG_THREADS, oldValue, val.toString());
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
                mMessages.getString("RTC_Invalid_arg", new Object[] {attrName, ex.getMessage()}));
        }
        
        // Clear the old environment configurations
        mEnvVarMap.clear();
        
        for (Iterator rowTypeItems = rowType.keySet().iterator(); rowTypeItems.hasNext(); ) {
            String item = (String)(rowTypeItems).next();
            items.add(item);
        }
        
        int itemSize = items.size();
        if (itemSize <= 0) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_Item_Size", itemSize));
        } else if (items.size() > 2) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_Item_Size", itemSize));
        }
        
        // getting the row index
        List rowIndex = val.getTabularType().getIndexNames();
        int rowIndexSize = rowIndex.size();
        if (rowIndexSize > 1) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_RowIndex_Size", rowIndexSize));
        } else if (rowIndex.size() <= 0) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_RowIndex_Size", rowIndexSize));
        } else {
            mapKey = (String) rowIndex.get(0);
            if (mapKey == null || "".equals(mapKey)) {
                throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_RowIndex_Key", mapKey));
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
        String msg = mMessages.getString("RTC_Attr_changed");
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
            throw new MBeanException(ex, mMessages.getString("RTC_Failed_persist", new Object[] { mWorkspaceRoot,
                    ex.getMessage() }));
        }
    }

	void persistEnvVariableConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            File envVarPersistFileName = new File(mWorkspaceRoot, PERSIST_ENVVAR_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(envVarPersistFileName);
            for (Iterator iter = mEnvVarMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String value = (String) mEnvVarMap.get(key);
               String prop = key + "=" + value + "\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (IOException ex) {
            throw new MBeanException(ex, mMessages.getString("RTC_Failed_persist_env_var", 
                                     new Object[] {mWorkspaceRoot, ex.getMessage()}));
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
        
        try {
            InputStream is = new FileInputStream(envVarPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String value = persistedConfig.getProperty(name);
                envVarMap.put(name, value);
            }
        } catch (IOException ex) {
            throw new JBIException("Failed to load configuration " + envVarPersistFileName + " : " + ex.getMessage(), ex);
        }
        
        return envVarMap;
    }


    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] { new MBeanNotificationInfo(
                new String[] { AttributeChangeNotification.ATTRIBUTE_CHANGE },
                AttributeChangeNotification.class.getName(), mMessages.getString("RTC_Attr_changed")) };
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback)
            throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }
    /**
     * DOCUMENT ME!
     *
     * @param propFile DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean restore(Properties mConfig) {
       
        try {
            // TODO do something here..

        } catch (Exception ex) {
            return false;
        }

        return true;
    }

	 private void saveAndNotifyListners(String msgId, String attrName, String oldVal, String newVal) {
      // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString(msgId);
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

    public Properties getProperties() {
        return mConfig;
    }
}
