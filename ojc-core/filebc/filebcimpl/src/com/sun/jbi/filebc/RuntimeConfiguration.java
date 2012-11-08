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
package com.sun.jbi.filebc;

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;
import java.io.BufferedReader;
import java.util.logging.Level;
import java.util.logging.Logger;
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
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

/**
 * This is the runtime configuration MBean.
 * It allows configurations to be changed at run-time
 *
 * @author Sherry Weng
 * @author Nitin Nahata
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {

    private static final Messages mMessages =
            Messages.getMessages(RuntimeConfiguration.class);
    private static final Logger mLogger =
            Messages.getLogger(RuntimeConfiguration.class);    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
    public static final String CONFIG_IB_WORKER_THREADS = "IBWorkerThreads";
    public static final String ENVIRONMENT_VARIABLES = "EnvironmentVariables";    // Configuration validation settings
    private static long MIN_THREADS = 1;
    private static long MAX_THREADS = 10000;    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";    // Configuration file name for environment variables
    private static final String DEFAULT_IB_WORKER_THREADS = "5"; 
    private static final String PERSIST_ENVVAR_CONFIG_FILE_NAME = "EnvVarConfig.properties";    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";    // Application configuration row fields
    private static final String APPLICATION_CONFIG_ROW_KEY = "configurationName";
    public static final String APPLICATION_CONFIG_PROPERTY_FILEDIR = "fileDirectory";
    public static final String APPLICATION_CONFIG_PROPERTY_RELATIVEPATH = "relativePath";
    public static final String APPLICATION_CONFIG_PROPERTY_PATHRELATIVETO = "pathRelativeTo";
    public static final String APPLICATION_CONFIG_PROPERTY_LOCKNAME = "lockName";
    public static final String APPLICATION_CONFIG_PROPERTY_WORKAREA = "workArea";
    public static final String APPLICATION_CONFIG_PROPERTY_SEQNAME = "seqName";
    public static final String APPLICATION_CONFIG_PROPERTY_PERSIST_BASE = "persistenceBaseLoc";    // Component configurations
    public static final String APPLICATION_CONFIG_PROPERTY_REURSIVE = "recursive";
    public static final String APPLICATION_CONFIG_PROPERTY_EXCLUDE_REGEX = "recursiveExclude";
    Properties mConfig;    // Global environment configurations
    Map mEnvVarMap;
    CompositeType mEnvVarRowType = null;
    TabularType mEnvVarTabularType = null;    // Component work root
    String mWorkspaceRoot;    // Key store utility for password encryption
    private KeyStoreUtilClient mKeyStoreUtilClient;    // Global application configurations
    private static String[] mAppConfigRowAttrNames = {
        APPLICATION_CONFIG_ROW_KEY,
        ApplicationConfigurationObject.FILEDIR,
        ApplicationConfigurationObject.RELATIVEPATH,
        ApplicationConfigurationObject.PATHRELATIVETO,
        ApplicationConfigurationObject.LOCKNAME,
        ApplicationConfigurationObject.WORKAREA,
        ApplicationConfigurationObject.SEQNAME,
        ApplicationConfigurationObject.PERSISTBASELOC,
        ApplicationConfigurationObject.RECURSIVE,
        ApplicationConfigurationObject.EXCLUDE_REGEX
    };
    private static String[] mAppConfigAttrDesc = {
        "Application Configuration Name",
        ApplicationConfigurationObject.FILEDIR_DESC,
        ApplicationConfigurationObject.RELATIVEPATH_DESC,
        ApplicationConfigurationObject.PATHRELATIVETO_DESC,
        ApplicationConfigurationObject.LOCKNAME_DESC,
        ApplicationConfigurationObject.WORKAREA_DESC,
        ApplicationConfigurationObject.SEQNAME_DESC,
        ApplicationConfigurationObject.PERSIST_BASE_LOC_DESC,
        ApplicationConfigurationObject.RECURSIVE_DESC,
        ApplicationConfigurationObject.EXCLUDE_REGEX_DESC
    };
    private static OpenType[] mAppConfigAttrTypes = {
        SimpleType.STRING,
        SimpleType.STRING,
        SimpleType.BOOLEAN,
        SimpleType.STRING,
        SimpleType.STRING,
        SimpleType.STRING,
        SimpleType.STRING,
        SimpleType.STRING,
        SimpleType.BOOLEAN,
        SimpleType.STRING
    };
    private Map mAppVarMap;
    private Map<String, ApplicationConfigurationObject> mAppConfigMap;
    private CompositeType mAppVarRowType = null;
    private CompositeType mAppConfigRowType = null;
    private TabularType mAppVarTabularType = null;
    private TabularType mAppConfigTabularType = null;    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keystoreUtilClient) throws JBIException,
            OpenDataException, MBeanException {
        mWorkspaceRoot = workspaceRoot;

        mKeyStoreUtilClient = keystoreUtilClient;

        // Load the persisted configuration
        try {
            mConfig = ConfigPersistence.loadConfig(workspaceRoot);
            mAppConfigRowType = createApplicationConfigurationCompositeType();
            mAppConfigTabularType = createApplicationConfigurationTabularType();
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
            mAppVarMap = loadApplicationVariablesConfig(workspaceRoot);
            mAppConfigMap = loadApplicationConfiguration(workspaceRoot);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FILEBC-E01210.Faile_to_construct_composite_data_structures", e.getLocalizedMessage()));
        }
    }

    /**
     * Get the CompositeType definition for the components application configuration 
     *
     * @return the CompositeType for the components application configuration.
     */
    public CompositeType queryApplicationConfigurationType() {
        return mAppConfigRowType;
    }

    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }

    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(
                    mMessages.getString("RTC_Invalid_arg", new Object[]{attrName, ex.getMessage()}));
        }
        if (newVal.intValue() < MIN_THREADS ||
                newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(
                    mMessages.getString("RTC_Invalid_attr",
                    new Object[]{newVal, attrName, new Long(MIN_THREADS), new Long(MAX_THREADS)}));
        }

        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("RTC_Attr_changed");
        String attrType = Integer.class.getName();
        Integer oldVal = getThreads();
        Notification notif = new AttributeChangeNotification(this,
                seqNo,
                System.currentTimeMillis(),
                msg,
                attrName,
                attrType,
                oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);

        // Apply and save the changes
        mConfig.put(CONFIG_THREADS, val.toString());
        persistConfiguration();
    }
    
    public Integer getIBWorkerThreads() {
        String val = mConfig.getProperty(CONFIG_IB_WORKER_THREADS, DEFAULT_IB_WORKER_THREADS);
        return Integer.valueOf(val);
    }

    public void setIBWorkerThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_IB_WORKER_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(
                    mMessages.getString("RTC_Invalid_arg", new Object[]{attrName, ex.getMessage()}));
        }
        if (newVal.intValue() < MIN_THREADS ||
                newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(
                    mMessages.getString("RTC_Invalid_attr",
                    new Object[]{newVal, attrName, new Long(MIN_THREADS), new Long(MAX_THREADS)}));
        }

        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString("RTC_Attr_changed");
        String attrType = Integer.class.getName();
        Integer oldVal = getThreads();
        Notification notif = new AttributeChangeNotification(this,
                seqNo,
                System.currentTimeMillis(),
                msg,
                attrName,
                attrType,
                oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);

        // Apply and save the changes
        mConfig.put(CONFIG_IB_WORKER_THREADS, val.toString());
        persistConfiguration();
		System.out.println("IBWorkerThreads Set");
    }

    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            throw new MBeanException(ex, mMessages.getString("RTC_Failed_persist",
                    new Object[]{mWorkspaceRoot, ex.getMessage()}));
        }
    }

    public TabularData createEnvironmentVariableTabularStructure() throws OpenDataException {
        TabularData tabularData = null;

        String[] envVarRowAttrNames = {"name", "value", "type"};
        String[] envVarRowAttrDesc = {"Environment variable name", "Environment variable value", "Environment variable type"};
        OpenType[] envVarRowAttrTypes = {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING};
        String[] envVarRowIndex = {"name"};

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

    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[]{new MBeanNotificationInfo(
                    new String[]{AttributeChangeNotification.ATTRIBUTE_CHANGE},
                    AttributeChangeNotification.class.getName(),
                    mMessages.getString("RTC_Attr_changed"))
                };
    }

    public void addNotificationListener(NotificationListener listener,
            NotificationFilter filter,
            Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener)
            throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener,
            NotificationFilter filter,
            Object handback) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }

    /*** begin - 
     * Interface methods for Application Configuration and Application Variables support 
     * ***/
    /**
     * Add an application configuration. The configuration name is a part of the CompositeData.
     * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
     *
     * @param name - configuration name, must match the value of the field "name" in the namedConfig
     * @param appConfig - application configuration composite 
     * @throws MBeanException if the application configuration cannot be added.
     */
    public void addApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {
        if (mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01211.Application_config_name_already_exists", name)));
        }

        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != mAppConfigRowAttrNames.length) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01212.Invalid_Item_Size_for_app_config", new Object[]{name, rowType.keySet().size()}));
        }

        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        }

        ApplicationConfigurationObject aco = getApplicationConfigurationObject(name, appConfig);
        mAppConfigMap.put(name, aco);

        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01205.New_application_configuration_added", new Object[]{name, aco.toString()}));
        }
        persistApplicationConfigurationObjects();

    }

    /**
     * Delete an application configuration. 
     *
     * @param name - identification of the application configuration to be deleted
     * @throws MBeanException if the configuration cannot be deleted.
     */
    public void deleteApplicationConfiguration(String name) throws MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01215.Application_configuration_does_not_exist_for_delete", name)));
        }

        mAppConfigMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01206.Application_configuration_deleted", name));
        }
        persistApplicationConfigurationObjects();
    }

    /**
     * Update a application configuration. The configuration name is a part of the CompositeData.
     * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
     *
     * @param name - configuration name, must match the value of the field "configurationName" in the appConfig
     * @param appConfig - application configuration composite
     * @throws MBeanException if there are errors encountered when updating the configuration.
     */
    public void setApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01216.Application_configuration_does_not_exist_for_set", name)));
        }

        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != mAppConfigRowAttrNames.length) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01212.Invalid_Item_Size_for_app_config", new Object[]{name, rowType.keySet().size()}));
        }

        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        }

        ApplicationConfigurationObject aco = getApplicationConfigurationObject(name, appConfig);
        mAppConfigMap.put(name, aco);

        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01207.Application_configuration_updated", new Object[]{name, aco.toString()}));
        }
        persistApplicationConfigurationObjects();
    }

    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a 
     *         component keyed by the configuration name. 
     */
    public TabularData getApplicationConfigurations() {
        TabularData tabularData = new TabularDataSupport(mAppConfigTabularType);
        for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext();) {
            String name = (String) iter.next();
            ApplicationConfigurationObject aco = (ApplicationConfigurationObject) mAppConfigMap.get(name);
            Object[] data = new Object[]{name,
                aco.getFileDirectory(),
                aco.getRelativePath(),
                aco.getPathRelativeTo(),
                aco.getLockName(),
                aco.getWorkArea(),
                aco.getSeqName(),
                aco.getPersistenceBaseLoc(),
                aco.getRecursive(),
                aco.getRecursiveExclude()
            };

            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                        mAppConfigRowAttrNames,
                        data);

                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("FILEBC-E01217.Unable_to_construct_composite_data_for_app_config"), e);
            }
        }

        return tabularData;
    }

    /**
     * This operation adds a new application variable. If a variable with the same name 
     * already exists, the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variable to the 
     *         component. 
     */
    public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01218.Application_variable_name_already_exists", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        }

        String appVarValue = (String) appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String) appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }

        if (appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }

        mAppVarMap.put(name, new String[]{appVarValue, appVarType});
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01203.New_application_variable_added", new Object[]{name, appVarValue}));
        }
        persistApplicationVariablesConfig();

    }

    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
    public void deleteApplicationVariable(String name) throws MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01223.Application_variable_does_not_exist_for_delete", name)));
        }

        mAppVarMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01204.Application_variable_deleted", name));
        }
        persistApplicationVariablesConfig();
    }

    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01224.Application_variable_does_not_exist_for_set", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        }

        String appVarValue = (String) appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String) appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }

        if (appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }

        mAppVarMap.put(name, new String[]{appVarValue, appVarType});
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FILEBC-C01202.Application_variable_updated", new Object[]{name, appVarValue}));
        }

        // persist the application variable properties
        persistApplicationVariablesConfig();
    }

    public TabularData getApplicationVariables() {
        TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext();) {
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata[0];
            String type = metadata[1];
            Object[] data = ("PASSWORD".equals(type)) ? new Object[]{name, "*******", type} : new Object[]{name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                        new String[]{APPLICATION_VARIABLES_ROW_KEY,
                            APPLICATION_VARIABLES_VALUE_FIELD,
                            APPLICATION_VARIABLES_TYPE_FIELD
                        },
                        data);

                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("FILEBC-E01225.Unable_to_construct_composite_data_for_app_variable"), e);
            }
        }

        return tabularData;
    }

    /**
     * helper to convert app config values read from persistence
     * in string form into appropriate type objects
     * @param parmKey
     * @param value
     * @return
     */
    private Object getAppConfigParmObject(String parmKey, String value) {
        OpenType ot = mAppConfigRowType.getType(parmKey);
        Object result = value;
        if (ot == null) {
            // give warning
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, "Can not find open type info for Application Configuration parameter: " + parmKey + ", check and make sure the ApplicationConfigurations.properties has valid entries.");
            }
        } else {
            if (ot.getTypeName().equalsIgnoreCase(String.class.getName())) {
                // already a string, leave as is
            } else if (ot.getTypeName().equalsIgnoreCase(Integer.class.getName())) {
                try {
                    result = new Integer(value);
                } catch (NumberFormatException e) {
                    result = new Integer(-1);
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, "Invalid parameter value for FTPBC Application Configuration parameter: " + parmKey + ", open type name: " + ot.getTypeName() + " integer required.");
                    }
                }
            } else if (ot.getTypeName().equalsIgnoreCase(Boolean.class.getName())) {
                // 
                result = new Boolean(value);
            } else {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, "Invalid open type for FTPBC Application Configuration parameter: " + parmKey + ", open type name: " + ot.getTypeName());
                }
            }
        }
        return result;
    }

    /*** end - 
     * Interface methods for Application Configuration and Application Variables support 
     * ***/
    private ApplicationConfigurationObject getApplicationConfigurationObject(String name, CompositeData appConfig) throws InvalidAttributeValueException {
        ApplicationConfigurationObject aco = (ApplicationConfigurationObject) mAppConfigMap.get(name);

        String fileDir = (String) appConfig.get(ApplicationConfigurationObject.FILEDIR);

        if (fileDir == null && aco != null && aco.getFileDirectory() != null) {
            if (aco == null || aco.getFileDirectory() == null || aco.getFileDirectory().trim().length() == 0) {
                throw new InvalidAttributeValueException(mMessages.getString("FILEBC-E01214.Invalid_app_config_composite_data_null_dir", name));
            }
        }

        return new ApplicationConfigurationObject(appConfig, aco);
    }

    private CompositeType createApplicationVariableCompositeType() throws OpenDataException {
        if (mAppVarRowType != null) {
            return mAppVarRowType;
        }

        String[] appVarRowAttrNames = {APPLICATION_VARIABLES_ROW_KEY, APPLICATION_VARIABLES_VALUE_FIELD, APPLICATION_VARIABLES_TYPE_FIELD};
        String[] appVarRowAttrDesc = {"Application Variable Name", "Application Variable Value", "Application Variable Type"};
        OpenType[] appVarRowAttrTypes = {SimpleType.STRING, SimpleType.STRING, SimpleType.STRING};

        mAppVarRowType = new CompositeType("ApplicationVariables",
                "Application Variable Composite Data",
                appVarRowAttrNames,
                appVarRowAttrDesc,
                appVarRowAttrTypes);

        return mAppVarRowType;
    }

    private TabularType createApplicationVariableTabularType() throws OpenDataException {
        if (mAppVarTabularType != null) {
            return mAppVarTabularType;
        }

        if (mAppVarRowType == null) {
            mAppVarRowType = createApplicationVariableCompositeType();
        }

        mAppVarTabularType = new TabularType("ApplicationVariableList",
                "List of Application Variables",
                mAppVarRowType,
                new String[]{APPLICATION_VARIABLES_ROW_KEY});

        return mAppVarTabularType;
    }

    private CompositeType createApplicationConfigurationCompositeType() throws OpenDataException {
        if (mAppConfigRowType != null) {
            return mAppConfigRowType;
        }

        mAppConfigRowType = new CompositeType("AppliationConfigurationObject",
                "Application Configuration Composite Data",
                mAppConfigRowAttrNames,
                mAppConfigAttrDesc,
                mAppConfigAttrTypes);

        return mAppConfigRowType;
    }

    private TabularType createApplicationConfigurationTabularType() throws OpenDataException {
        if (mAppConfigTabularType != null) {
            return mAppConfigTabularType;
        }

        if (mAppConfigRowType == null) {
            mAppConfigRowType = createApplicationConfigurationCompositeType();
        }

        mAppConfigTabularType = new TabularType("ApplicationConfigurationObjectList",
                "List of Application Configuration Objects",
                mAppConfigRowType,
                new String[]{APPLICATION_CONFIG_ROW_KEY});

        return mAppConfigTabularType;
    }

    private void persistApplicationVariablesConfig() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appVarPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appVarPersistFileName);
            for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext();) {
                String key = (String) iter.next();
                String[] metadata = (String[]) mAppVarMap.get(key);
                String value = metadata[0];
                String type = metadata[1];
                if (type.equals("PASSWORD")) {
                    value = mKeyStoreUtilClient.encrypt(value);
                }
                String prop = (value != null) ? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
                os.write(prop.getBytes());
            }
            os.close();

        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FILEBC-E01207.Failed_to_persist_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
    }

    private Map loadApplicationVariablesConfig(String workspaceRoot) throws MBeanException {
        Map appVarMap = new HashMap();

        File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!appVarPersistFileName.exists()) {
            return appVarMap;
        }

        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(appVarPersistFileName));
            String line = null;
            int index = 0;
            String key = null;
            String value = null;
            String varDesc = null;
            String type = null;
            while ((line = reader.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                if ((index = line.indexOf("=")) > 0) {
                    key = line.substring(0, index).trim();
                    if (key != null && key.length() > 0) {
                        varDesc = (index == line.length() - 1) ? "" : line.substring(index + 1).trim();
                        // if value has env var type info:
                        // e.g. my_var_1=abc123{PASSWORD}
                        int startIndex = varDesc.indexOf("{");
                        if (startIndex < 0) {
                            // no type desc {} - assume type as String
                            value = varDesc;
                            type = "STRING";
                        } else {
                            value = (startIndex == 0) ? null : varDesc.substring(0, startIndex);
                            type = varDesc.substring(startIndex + 1, varDesc.length() - 1);
                        }
                        if (type.equals("PASSWORD")) {
                            value = mKeyStoreUtilClient.decrypt(value);
                        }
                        appVarMap.put(key, new String[]{value, type});
                    } else {
                        // malformed line
                    }
                } else {
                    // invalid line - skip
                }
            }
            reader.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FILEBC-E01208.Failed_to_load_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                    //
                }
            }
        }

        return appVarMap;
    }

    public Map retrieveApplicationVariablesMap() {
        return mAppVarMap;
    }

    public void updateApplicationVariablesMap(Map appVarMap) throws MBeanException {
        mAppVarMap = appVarMap;
        persistApplicationVariablesConfig();
    }

    public Map retrieveApplicationConfigurationsMap() {
        return mAppConfigMap;
    }

    public void updateApplicationConfigurationsMap(Map appConfigMap) throws MBeanException {
        mAppConfigMap = appConfigMap;
        persistApplicationConfigurationObjects();
    }

    private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appConfigPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext();) {
                String key = (String) iter.next();
                ApplicationConfigurationObject aco = (ApplicationConfigurationObject) mAppConfigMap.get(key);

                String val = aco.getFileDirectory();
                String propKey = key.concat(".").concat(ApplicationConfigurationObject.FILEDIR);
                String prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getRelativePath().toString();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.RELATIVEPATH);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getPathRelativeTo();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.PATHRELATIVETO);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getLockName();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.LOCKNAME);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getWorkArea();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.WORKAREA);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getSeqName();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.SEQNAME);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getPersistenceBaseLoc();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.PERSISTBASELOC);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getRecursive().toString();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.RECURSIVE);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());

                val = aco.getRecursiveExclude();
                propKey = key.concat(".").concat(ApplicationConfigurationObject.EXCLUDE_REGEX);
                prop = (val != null) ? propKey + "=" + val + "\n" : propKey + "=\n";
                os.write(prop.getBytes());
            }

            os.close();

        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FILEBC-E01229.Failed_to_persist_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }
    }

    private Map loadApplicationConfiguration(String workspaceRoot) throws MBeanException {
        Map appConfigMap = new HashMap<String, ApplicationConfigurationObject>();
        Map tempMap = new HashMap<String, Map>();

        File appConfigPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        if (!appConfigPersistFileName.exists()) {
            // no persisted app cfg file, return a blank map
            return appConfigMap;
        }

        BufferedReader reader = null;

        Map appConfigParms = null;

        try {
            reader = new BufferedReader(new FileReader(appConfigPersistFileName));
            String line = null;
            int index = 0;
            String key = null;
            String parmKey = null;
            String value = null;
            while ((line = reader.readLine()) != null) {
                if (line.trim().length() == 0) {
                    continue;
                }
                if ((index = line.indexOf("=")) > 0) {
                    key = line.substring(0, index).trim();
                    if (index < line.length()) {
                        value = line.substring(index + 1);
                        value = value.trim();
                    }

                    // key is the app config name
                    // the value is a set of parms:
                    // for example:
                    // app key : myAppConfig 
                    // myAppConfig1.fileDirectory = c:\temp
                    // myAppConfig1.pathRelative = true

                    // extract the appConfig key
                    index = key.lastIndexOf(".");
                    if (index > 0 && index < key.length()) {
                        parmKey = key.substring(index + 1);
                        key = key.substring(0, index);
                        appConfigParms = (Map) tempMap.get(key);
                        if (appConfigParms == null) {
                            appConfigParms = new HashMap();
                            tempMap.put(key, appConfigParms);
                        }
                        if (parmKey != null && parmKey.length() > 0) {
                            Object realValue = getAppConfigParmObject(parmKey, value);
                            appConfigParms.put(parmKey, realValue);
                        } else {
                            // malformed line - missing parm name in app config key part - skip
                        }
                    } else {
                        // malformed key, the key should like: <app_cfg_name>.<app_cfg_parm_name>
                    }
                } else {
                    // invalid line - skip
                }
            }

            reader.close();

            // now convert the parm set into ApplicationConfigurationObject
            Iterator it = tempMap.keySet().iterator();
            while (it.hasNext()) {
                String acoKey = (String) it.next();
                Map parms = (Map) tempMap.get(acoKey);
                ApplicationConfigurationObject aco = new ApplicationConfigurationObject(parms);
                appConfigMap.put(acoKey, aco);
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FILEBC-E01230.Failed_to_load_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        return appConfigMap;
    }

    public void dump(StringBuffer msgBuf) {
        msgBuf.append(CONFIG_THREADS);
        msgBuf.append(": ").append(getThreads()).append('\n');

        try {
            //TabularData envVars = getEnvironmentVariables();
            TabularData envVars = getApplicationVariables();
            // do an explicit cast to make java6 compiler happy ---
            Collection<CompositeData> vars = (Collection<CompositeData>) envVars.values();
            for (CompositeData data : vars) {
                Object name = data.get("name");
                Object value = data.get("value");
                if (name != null) {
                    msgBuf.append(name.toString()).append(": ").append('\n');
                    msgBuf.append(value == null ? "(null)" : value.toString()).append('\n');
                }
            }
        } catch (Exception ex) {
            Logger.getLogger(RuntimeConfiguration.class.getName()).log(Level.SEVERE, ex.getLocalizedMessage(), ex);
        }
    }
}
