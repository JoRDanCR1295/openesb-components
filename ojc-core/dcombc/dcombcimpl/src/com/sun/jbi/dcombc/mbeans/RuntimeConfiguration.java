/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.mbeans;

import java.util.Properties;
import java.util.Map;
import java.util.List;
import java.util.Iterator;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Enumeration;
import java.util.logging.Level;
import java.util.logging.Logger;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.jbi.JBIException;

import javax.management.Notification;
import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.MBeanNotificationInfo;
import javax.management.MBeanException;
import javax.management.NotificationBroadcasterSupport;
import javax.management.ListenerNotFoundException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.configuration.ConfigPersistence;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 * 
 * @author Chandrakanth Belde
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(RuntimeConfiguration.class);

    private static final Logger mLogger = Messages.getLogger(RuntimeConfiguration.class);

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";

	// Configuration file name for environment variables
    private static final String PERSIST_ENVVAR_CONFIG_FILE_NAME = "EnvVarConfig.properties";

	// Attribute names
    public static final String ENVIRONMENT_VARIABLES= "EnvironmentVariables";

	public static final String CONFIG_THREADS = "Threads";

    // Configuration validation settings
    private long MIN_THREADS = 1;

    private long MAX_THREADS = 10000;

    // Configuration
    private Properties mConfig;

    private String mWorkspaceRoot;

	// Global environment configurations
    private Map mEnvVarMap;

	private CompositeType mEnvVarRowType = null;
    
	private TabularType mEnvVarTabularType = null;
    // Use delegation to support notification
    
	private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    /** 
	 * Creates a new instance of InstallerExt 
	 *
	 * @param location of the workspace
	 */
    public RuntimeConfiguration(String workspaceRoot) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
		// Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        mEnvVarMap = loadEnvironmentVariableConfig(workspaceRoot);
    }

    /**
     * Initializes the RuntimeConfiguration instance
	 *
     * @throws JBIException if failed to initialize
     */
    public void initialize() throws JBIException {
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(mWorkspaceRoot);
    }

    /**
     * Get the no of threads configured
     *
	 * @return no of threads
     */
    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }

    /**
     * Set the no of Threads
	 * 
     * @param no of threads to assign
     */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "RuntimeConfiguration.INVALID_ARGUMENT", new Object[] { attrName, ex });

            String errMsg = mMessages.getString("RuntimeConfiguration.INVALID_ARGUMENT", new Object[] { attrName, ex });
            throw new InvalidAttributeValueException(errMsg);
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            mLogger.log(Level.SEVERE, "RuntimeConfiguration.INVALID_VALUE", new Object[] { newVal, attrName,
                    new Long(MIN_THREADS), new Long(MAX_THREADS) });

            String errMsg = mMessages.getString("RuntimeConfiguration.INVALID_VALUE", new Object[] { newVal, attrName,
                    new Long(MIN_THREADS), new Long(MAX_THREADS) });
            throw new InvalidAttributeValueException(errMsg);
        }

        // Apply and save the changes
        mConfig.put(CONFIG_THREADS, val.toString());
        persistConfiguration();

        // Notify listeners of this change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Integer oldVal = getThreads();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

    private void persistConfiguration() throws MBeanException {
        // Persist the changed configuration
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            mLogger.log(Level.SEVERE, "RuntimeConfiguration.PERSIST_FAILED", new Object[] { mWorkspaceRoot, ex });

            String errMsg = mMessages.getString("RuntimeConfiguration.PERSIST_FAILED", new Object[] { mWorkspaceRoot,
                    ex });
            throw new MBeanException(ex, errMsg);
        }
    }

	public void persistEnvVariableConfiguration() throws MBeanException {
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

	/**
     * Support notifying about config changes
	 *
     * @return MBeanNotificationInfo[]
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] { new MBeanNotificationInfo(
                new String[] { AttributeChangeNotification.ATTRIBUTE_CHANGE },
                AttributeChangeNotification.class.getName(), "Attribute changed") };
    }

    /**
     * Add Notification Listener to RuntimeConfiguration
	 *
     * @param NotificationListener
     * @param NotificationFilter
     * @param Object
     */
    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /**
     * Remove already added notification listener
	 *
     * @param NotificationListener
     */
    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    /**
     * Remove already added notification listener
     *
	 * @param NotificationListener
     * @param NotificationFilter
     * @param Object
     * @throws ListenerNotFoundException
     */
    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback)
            throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
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
}
