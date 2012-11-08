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

package com.sun.jbi.imsbc.mbeans;

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

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.HashMap;
import java.util.Enumeration;
import java.util.Collections;
import java.util.Collection;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.LinkedList;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.util.AlertsUtil;
import com.sun.jbi.imsbc.IMSBindingComponent;


/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 * 
 * @author Sun Microsystems
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {

    private static final Messages mMessages = Messages.getMessages(RuntimeConfiguration.class);

    private static final Logger mLogger = Messages.getLogger(RuntimeConfiguration.class);

// Configuration files' names
    public static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
    public static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    
    // Row and table metadata
    public static final TabularType APPCONFIG_TABULAR_TYPE;
    public static final CompositeType APPCONFIG_ROW_TYPE;
    public static final CompositeType APPVAR_ROW_TYPE;
    public static final TabularType APPVAR_TABULAR_TYPE;
    
    // Application Configuration
    private static final ApplicationConfigurationField APP_CONFIG_NAME;
    private static final ApplicationConfigurationField APP_CONFIG_IMS_SERVER_URL;
    private static final ApplicationConfigurationField[] APP_CONFIG_FIELDS = {
            APP_CONFIG_NAME = new AppConfigNameField(),
            APP_CONFIG_IMS_SERVER_URL = new AppConfigImsServerUrlField(),
    };
    private static final int APP_CONFIG_ROW_SIZE = APP_CONFIG_FIELDS.length;
    
    // Application Variables
    private static final String APP_VAR_NAME = "name";
    private static final String APP_VAR_VALUE = "value";
    private static final String APP_VAR_TYPE = "type";
    private static final String[] APP_VAR_FIELDS = {
            APP_VAR_NAME,
            APP_VAR_VALUE,
            APP_VAR_TYPE
    };
    
    private static final int APP_VAR_COUNT = APP_VAR_FIELDS.length;    	
	
	// Attribute names
    public static final String CONFIG_THREADS = "Threads";

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "10";

    // Configuration validation settings
    long MIN_THREADS = 1;

    long MAX_THREADS = 65535;

    // Configuration
    Properties mConfig;
    String mWorkspaceRoot;

    private KeyStoreUtilClient mKeySupport;

    // Application Configuration Object store
    private final Map<String, Collection<ApplicationConfigurationField>> mAppConfigMap;
    
    // Application Variables store
    private final Map<String, String[]> mAppVarMap;	

    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    static {
        try {
            // Call order is important
            APPCONFIG_ROW_TYPE = createApplicationConfigurationCompositeType();
            APPCONFIG_TABULAR_TYPE = createApplicationConfigurationTabularType();
            APPVAR_ROW_TYPE = createApplicationVariableCompositeType();
            APPVAR_TABULAR_TYPE = createApplicationVariableTabularType();
        } catch (OpenDataException e) {
            throw new RuntimeException(mMessages.getString("IMSBC-E01104.RuntimeConfigMbeanCreateFailedMetadata"), e);
        }
    }

    public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keySupport) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        mKeySupport = keySupport;
        
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        try {
            mAppConfigMap = loadApplicationConfigurations(workspaceRoot);
        } catch (MBeanException e) {
				String errMsg = mMessages.getString("IMSBC-E01105.AppCfgFailedLoad", e.getLocalizedMessage());
				mLogger.log(Level.WARNING, errMsg);
				AlertsUtil.getAlerter().critical(errMsg, 
								IMSBindingComponent.SHORT_DISPLAY_NAME, 
								null, 
								AlertsUtil.getServerType(),
								AlertsUtil.COMPONENT_TYPE_BINDING,
								NotificationEvent.OPERATIONAL_STATE_RUNNING, 
								NotificationEvent.EVENT_TYPE_ALERT,
								"IMSBC-E01105");
				throw new JBIException(errMsg);
        }    
        try {
		    mAppVarMap = loadApplicationVariables(workspaceRoot);
        } catch (MBeanException e) {
				String errMsg = mMessages.getString("IMSBC-E01136.AppVarFailedLoad", e.getLocalizedMessage());
				mLogger.log(Level.WARNING, errMsg);
				AlertsUtil.getAlerter().critical(errMsg, 
								IMSBindingComponent.SHORT_DISPLAY_NAME, 
								null, 
								AlertsUtil.getServerType(),
								AlertsUtil.COMPONENT_TYPE_BINDING,
								NotificationEvent.OPERATIONAL_STATE_RUNNING, 
								NotificationEvent.EVENT_TYPE_ALERT,
								"IMSBC-E01136");
				throw new JBIException(errMsg);
        }  
    }

    /**
     * Initializes the RuntimeConfiguration instance
     * @throws JBIException if failed to initialize
     */
    public void initialize() throws JBIException {
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(mWorkspaceRoot);
    }

    /**
     * Get the no of threads configured
     * @return Integer
     */
    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }

    /**
     * Set the no of Threads
     * @param Integer
     */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E001101.Invalid_Argument", new Object[] { attrName, ex }));

            String errMsg = mMessages.getString("IMSBC-E001101.Invalid_Argument", new Object[] { attrName, ex });
            throw new InvalidAttributeValueException(errMsg);
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E001102.Invalid_Value", new Object[] { newVal, attrName,
                    new Long(MIN_THREADS), new Long(MAX_THREADS) }));

            String errMsg = mMessages.getString("IMSBC-E001102.Invalid_Value", new Object[] { newVal, attrName,
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
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E001103.Persist_Failed", new Object[] { mWorkspaceRoot, ex }));

            String errMsg = mMessages.getString("IMSBC-E001103.Persist_Failed", new Object[] { mWorkspaceRoot,
                    ex });
            throw new MBeanException(ex, errMsg);
        }
    }

    public Map<String, String[]> retrieveApplicationVariablesMap() {
        synchronized (mAppVarMap) {
            return Collections.unmodifiableMap(mAppVarMap);
        }
    }

    public Map<String, Collection<ApplicationConfigurationField>> retrieveApplicationConfigurationsMap() {
        synchronized (mAppConfigMap) {
            return Collections.unmodifiableMap(mAppConfigMap);
        }
    }
    
    public int countVariables() {
        synchronized (mAppVarMap) {
            return mAppVarMap.size();
        }
    }

    public void updateApplicationVariablesMap(Map<String, String[]> map)
            throws MBeanException {
        synchronized (mAppVarMap) {
            mAppVarMap.clear();
            mAppVarMap.putAll(map);
            persistAppVarConfig();
        }
    }

    /**
     * This operation adds a new applicationvariable. If a variable already exists with
     * the same name as that specified then the operation fails.
     *
     * @param name   - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws javax.management.MBeanException
     *          if an error occurs in adding the application variables to the
     *          component.
     */
    public void addApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        
        synchronized (mAppVarMap) {
            if (mAppVarMap.containsKey(name)) {
                throw new MBeanException(new Exception(mMessages.getString(
                        "IMSBC-E01112.AppVarNameAlreadyExists",
                        name)));
            }

            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01113.AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }
        
            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01114.AppVarMissingField",
                                new Object[] {name, APP_VAR_NAME})));
            }
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01118.AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }

            String appVarValue = (String) appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01117.AppVarValueNull",
                                new Object[] {name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String) appVar.get(APP_VAR_TYPE);
            if (appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01117.AppVarValueNull",
                                new Object[] {name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "IMSBC-E01120.AppCfgAdded",
                        new Object[]{name, appVarValue}));
            }
            persistAppVarConfig();
        }
    }

    /**
     * This operation sets an application variable. If a variable does not exist with
     * the same name, its an error.
     *
     * @param name   - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws javax.management.MBeanException
     *          if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        
        synchronized (mAppVarMap) {
            if (!mAppVarMap.containsKey(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01116.AppVarExistenceFailedUpdate",
                                name)));
            }
        
            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01113.AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }

            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01114.AppVarMissingField",
                                new Object[]{name, APP_VAR_NAME})));
            } 
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01118.AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }
        
            String appVarValue = (String)appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01117.AppVarValueNull",
                                new Object[]{name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String)appVar.get(APP_VAR_TYPE);
            if ( appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01117.AppVarValueNull",
                                new Object[]{name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "IMSBC-E01121.AppVarUpdated",
                        new Object[]{name, appVarValue}));
            }
            persistAppVarConfig();
        }
    }

    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws javax.management.MBeanException
     *          on errors.
     */
    public void deleteApplicationVariable(String name) throws MBeanException {
        synchronized (mAppVarMap) {
            if (!mAppVarMap.containsKey(name)) {
                throw new MBeanException(new Exception(mMessages.getString(
                        "IMSBC-E01115.AppVarExistenceFailedDelete", name)));
            }
            mAppVarMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "IMSBC-E01122.AppVarDeleted", name));
            }
            persistAppVarConfig();
        }
    }

    /**
     * Get the Application Variable set for a component.
     *
     * @return a TabularData which has all the applicationvariables set on the component.
     */
    public TabularData getApplicationVariables() {
        TabularData tabularData = new TabularDataSupport(APPVAR_TABULAR_TYPE);
        
        synchronized (mAppVarMap) {
            for (String name : mAppVarMap.keySet()) {
                String[] metadata = mAppVarMap.get(name);
                
                assert(metadata.length == 2);
                
                Object[] data = new Object[3];
                data[2] = metadata[1];
                data[1] = "PASSWORD".equals(data[2]) ? "*******" : metadata[0];
                data[0] = name;
                
                try {
                    CompositeData rowData = new CompositeDataSupport(
                            APPVAR_ROW_TYPE, APP_VAR_FIELDS, data);
                    tabularData.put(rowData);
                } catch (OpenDataException e) {
                    throw new RuntimeException(mMessages.getString(
                            "IMSBC-E01119.AppVarTabularCreateFailed"), e);
                }
            }
        }
        return tabularData;
    }

    /**
     * Get the CompositeType definition for the components application configuration
     * 
     * @return the CompositeType for the components application configuration.
     */
    public CompositeType queryApplicationConfigurationType() {
        return APPCONFIG_ROW_TYPE;
    }

    /**
     * Add an application configuration.
     * 
     * @param name Configuration name, must match the value of the "name"
     *             attribute of a declared property (see jbi.xml in the
     *             mqbc/packaging source tree)
     *
     * @param appConfig Application configuration composite
     *
     * @throws javax.management.MBeanException if the application configuration
     *         cannot be added.
     *
     * @see jbi.xml under mqbc/packaging source tree
     */
    public void addApplicationConfiguration(String name, CompositeData appConfig)
            throws MBeanException {
        
        assert(appConfig != null);
        
        // Check for configuration name collision
        Object[] validationResult = APP_CONFIG_NAME.validate(name);
        if (validationResult != null) {
            if (validationResult[0] != null) {
                name = validationResult[0].toString();
            } else {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01106.AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                        mMessages.getString("MSBC_E01107.AppCfgNameAlreadyExists",
                                name)));
            }
            
            addAppCfg(name, appConfig);
            
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "IMSBC-E01120.AppCfgAdded", new Object[]{name}));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, "IMSBC-E01123.AppCfgPersistFailed", e);
            }
        }
    }

    /**
     * Inserts a new Application Configuration object into the internal ACO map.
     * @param name   Identification of the Application Configuration object.
     * @param config Data of the Application Configuration object.
     * @throws MBeanException for any exception
     */
    private void addAppCfg(String name, CompositeData config)
            throws MBeanException {
        
        assert(name != null);
        assert(config != null);
        
        // Check row width
        CompositeType rowType = config.getCompositeType();
        if (rowType.keySet().size() != APP_CONFIG_ROW_SIZE) {
            throw new MBeanException(new InvalidAttributeValueException(mMessages.getString(
                    "IMSBC-E01108.AppCfgRowSizeInvalid",
                    new Object[]{name, APP_CONFIG_ROW_SIZE, rowType.keySet().size()})));
        }

        // Validate fields
        Collection<ApplicationConfigurationField> appConfig =
                new LinkedList<ApplicationConfigurationField>();
        for (ApplicationConfigurationField appcfgField: APP_CONFIG_FIELDS) {
            if (!config.containsKey(appcfgField.fieldId)) {
                throw new MBeanException(new InvalidAttributeValueException(mMessages.getString(
                        "IMSBC-E01109.AppCfgRowMissingField",
                        new String[] {name, appcfgField.fieldId})));
            }
            
            ApplicationConfigurationField newField;
            Object data = config.get(appcfgField.fieldId);
            Object[] validation = appcfgField.validate(data);
            
            // No validation errors
            if (validation == null) {
                newField = (ApplicationConfigurationField) appcfgField.clone();
                newField.setValue(data);
                appConfig.add(newField);
            }
            else {
                // There are errors
                if (validation[0] == null) {
                    throw new MBeanException(new InvalidAttributeValueException(
                            mMessages.getString(
                                    "IMSBC-E01106.AppCfgValidationFailed",
                                    new Object[]{name, appcfgField.fieldId, validation[1]})));
                }
                
                // No errors, but there are corrective values
                else {
                    data = validation[0];
                    newField = (ApplicationConfigurationField) appcfgField.clone();
                    newField.setValue(data);
                    appConfig.add(newField);
                }
            }
        }
        
        // Save configuration
        synchronized (mAppConfigMap) {
            mAppConfigMap.put(name, appConfig);
        }
    }

    /**
     * Delete an application configuration.
     *
     * @param name - identification of the application configuration to be deleted
     * @throws javax.management.MBeanException
     *          if the configuration cannot be deleted.
     */
    public void deleteApplicationConfiguration(String name)
            throws MBeanException {
        
        // Check for configuration name existence
        Object[] validationResult = APP_CONFIG_NAME.validate(name);
        if (validationResult != null) {
            if (validationResult[0] != null) {
                name = validationResult[0].toString();
            } else {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01106.AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (!mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                        mMessages.getString("IMSBC-E01110.AppCfgExistenceFailedDelete",
                                name)));
            }
        
            // Remove application configuration entry
            mAppConfigMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString("IMSBC-E01124.AppCfgDeleted", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, "IMSBC-E01123.AppCfgPersistFailed", e);
            }
        }
    }

    /**
     * Update a application configuration. The configuration name is a part of the CompositeData.
     * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
     *
     * @param name      - configuration name, must match the value of the field "configurationName" in the appConfig
     * @param appConfig - application configuration composite
     * @throws javax.management.MBeanException
     *          if there are errors encountered when updating the configuration.
     */
    public void setApplicationConfiguration(String name,
                                           CompositeData appConfig)
            throws MBeanException {
        
        // Check for configuration name existence
        Object[] validationResult = APP_CONFIG_NAME.validate(name);
        if (validationResult != null) {
            if (validationResult[0] != null) {
                name = validationResult[0].toString();
            } else {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "IMSBC-E01106.AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new IllegalArgumentException(
                    mMessages.getString("IMSBC-E01111.AppCfgExistenceFailedUpdate",
                            name)));
        }
        
        addAppCfg(name, appConfig);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString(
                    "IMSBC-E01125.AppCfgUpdated", new Object[]{name}));
        }
        
        // Persist configuration
        try {
            persistAppConfigObjects();
        } catch (IOException e) {
            mLogger.log(Level.WARNING, "IMSBC-E01123.AppCfgPersistFailed", e);
        }
    }

    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a
     *         component keyed by the configuration name.
     */
    public TabularData getApplicationConfigurations() {
        TabularData tabularData = new TabularDataSupport(APPCONFIG_TABULAR_TYPE);
        synchronized (mAppConfigMap) {
            for (Collection<ApplicationConfigurationField> fields: mAppConfigMap.values()) {
                Collection<String> names = new LinkedList<String>();
                Collection<Object> values = new LinkedList<Object>();
                for (ApplicationConfigurationField field: fields) {
                    Object value = field.getValue();
                    names.add(field.fieldId);
                    values.add(value);
                }
                try {
                    CompositeData rowData = new CompositeDataSupport(
                            APPCONFIG_ROW_TYPE,
                            names.toArray(new String[names.size()]),
                            values.toArray());
                    tabularData.put(rowData);
                } catch (OpenDataException e) {
                    throw new RuntimeException(mMessages.getString(
                            "IMSBC-E01126.AppCfgTabularDataError"), e);
                }

            }
        }
        return tabularData;
    }

    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(
                new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, 
                AttributeChangeNotification.class.getName(), 
                "Attribute changed")};
    }
    
    public void addNotificationListener(NotificationListener listener, 
            NotificationFilter filter, 
            Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }
    
    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }
    
    public void removeNotificationListener(NotificationListener listener, 
            NotificationFilter filter, 
            Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }

    /**
     * This method is used at construction time to initialize the type
     * describing the composition of an application configuration.  
     * @throws OpenDataException
     */
    private static CompositeType createApplicationConfigurationCompositeType()
            throws OpenDataException {
        String[] names = new String[APP_CONFIG_ROW_SIZE];
        String[] descs = new String[APP_CONFIG_ROW_SIZE];
        SimpleType[] types = new SimpleType[APP_CONFIG_ROW_SIZE];
        for (int i = 0; i < APP_CONFIG_ROW_SIZE; ++i) {
            names[i] = APP_CONFIG_FIELDS[i].fieldId;
            descs[i] = APP_CONFIG_FIELDS[i].fieldDesc;
            types[i] = APP_CONFIG_FIELDS[i].fieldType;
        }
        return new CompositeType("ApplicationConfigurationObject",
                "Application Configuration Composite Data",
                names, descs, types);
    }

    /**
     * This method is used at construction time to initialize tabular type
     * information for application configuration objects.  It's important
     * that {@link #createApplicationConfigurationCompositeType()} is called
     * first before this method.
     * 
     * @throws OpenDataException
     * 
     * @return TabularType describing the application configuration.
     */
    private static TabularType createApplicationConfigurationTabularType()
            throws OpenDataException {
        return new TabularType("ApplicationConfigurationObjectList",
                "List of Application Configuration Objects",
                APPCONFIG_ROW_TYPE,
                new String[] { APP_CONFIG_NAME.fieldId });
    }

    /**
     * This method is used at construction time to initialize the type
     * describing the composition of an application variable.  
     * @throws OpenDataException
     */
    private static CompositeType createApplicationVariableCompositeType()
            throws OpenDataException {
        String[] appVarRowAttrNames = {
                APP_VAR_NAME, APP_VAR_VALUE, APP_VAR_TYPE
        };
        String[] appVarRowAttrDesc = {
                "Application Variable Name",
                "Application Variable Value",
                "Application Variable Type"
        };
        OpenType[] appVarRowAttrTypes = {
                SimpleType.STRING, SimpleType.STRING, SimpleType.STRING
        };

        return new CompositeType("ApplicationVariables",
            "Application Variable Composite Data",
            appVarRowAttrNames,
            appVarRowAttrDesc,
            appVarRowAttrTypes);
    }

    /**
     * This method is used at construction time to initialize tabular type
     * information for application variables.  It's important
     * that {@link #createApplicationConfigurationCompositeType()} is called
     * first before this method.
     * 
     * @throws OpenDataException
     */
    private static TabularType createApplicationVariableTabularType() throws OpenDataException {
        return new TabularType("ApplicationVariableList",
                "List of Application Variables",
                APPVAR_ROW_TYPE,
                new String[] { APP_VAR_NAME } );
    }

    private void persistAppVarConfig() throws MBeanException {
        try {
            File file = new File(mWorkspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            BufferedWriter writer = new BufferedWriter(new FileWriter(file));
            synchronized (mAppVarMap) {
                for (String key : mAppVarMap.keySet()) {
                    String[] metadata = mAppVarMap.get(key);
                    String value = metadata[0];
                    String type = metadata[1];

                /*    if (type.equals("PASSWORD")) {
                        if (mKeySupport != null) {
                            value = mKeySupport.encrypt(value);
                        }
                    }*/
                    String prop = (value != null)
                            ? key + "=" + value + "{" + type + "}\n"
                            : key + "={" + type + "}\n";
                    writer.write(prop);
                } 
                writer.flush();
                writer.close();
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString(
                    "IMSBC-E01127.AppVarPersistWriteFailed",
                    PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        } 
    }
    
    private void persistAppConfigObjects() throws IOException {
        File file = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(file));
            final String LINEFEED = System.getProperty("line.separator", "\n");
            synchronized (mAppConfigMap) {
                for (Map.Entry<String, Collection<ApplicationConfigurationField>> set
                        : mAppConfigMap.entrySet()) {
                    String key = set.getKey();
                    writer.write("[" + key + "]");
                    writer.write(LINEFEED);
                
                    Collection<ApplicationConfigurationField> config = set.getValue();
                    for (ApplicationConfigurationField field: config) {
                        writer.write(field.fieldId + "=" + field.getValue());
                        writer.write(LINEFEED);
                    }
                }
                writer.flush();
            }
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e1) {
                    mLogger.log(Level.WARNING,
                            mMessages.getString(
                                    "IMSBC-E01128.AppCfgFileCloseFailed",
                                    file.getAbsolutePath()), e1);
                }
            }
        }
    }

    private Map loadApplicationVariables(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map<String, String[]> appVarMap = new HashMap<String, String[]>();
        
        File file = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!file.exists()) {
            return appVarMap;
        }
        
        try {
            InputStream is = new FileInputStream(file);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String metadata = persistedConfig.getProperty(name);
                int startIndex = metadata.indexOf("{");
                String value = (startIndex == 0) ? null : metadata.substring(0, startIndex);
                String type = metadata.substring(startIndex + 1, metadata.length() -1);
             /*   if (type.equals("PASSWORD")) {
                    if (mKeySupport != null) {
                        try {
                            value = mKeySupport.decrypt(value);
                        } catch (Exception e1) {
                            throw new MBeanException(e1, mMessages.getString(
                                    "IMSBC_E01129.AppVarLoadDecryptFailed",
                                    name
                            ));
                        }
                    }
                }*/
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (IOException ex) {
            throw new MBeanException(ex, mMessages.getString(
                    "IMSBC_E01130.AppVarPersistLoadFailed",
                    PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
        
        return appVarMap;
    }

	private Map loadApplicationConfigurations(String workspaceRoot) throws MBeanException {
        Map<String, Collection<ApplicationConfigurationField>> appConfigMap =
                new HashMap<String, Collection<ApplicationConfigurationField>>();
        
        final int READAHEAD_LIMIT = 100;
        File file;
        BufferedReader reader;
        
        file = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            return appConfigMap;
        }
        
        // read application configuration objects (ACO)
        try {
            String acoName = null;
            Collection<ApplicationConfigurationField> fields =
                    new LinkedList<ApplicationConfigurationField>();
            
            while (true) {
                String line = reader.readLine();
                if (line == null) {
                    break;
                }
                
                // found a [id] block that marks the beginning of an ACO
                reader.mark(READAHEAD_LIMIT);
                String name = readStanzaHead(reader);
                if (name != null) {
                    // Begin ACO scan
                    if (acoName == null) {
                        acoName = name;
                        readStanzaTail(fields, reader);
                    }
                    
                    Collection<ApplicationConfigurationField> oldFields =
                            appConfigMap.put(acoName, fields);
                    if (oldFields != null && !oldFields.isEmpty()) {
                        mLogger.warning(mMessages.getString(
                                "IMSBC-E01131.AppCfgPersistLoadOverride",
                                name));
                    }
                    mLogger.info(mMessages.getString(
                            "IMSBC-E01132.AppCfgPersistLoaded", acoName));
                    
                    // Was already scanning an ACO, finish up
                    if (acoName != null) {
                        reader.reset();
                        fields.clear();
                        acoName = null;
                    }
                }
            }
        } catch (IOException e) {
            throw new MBeanException(e, mMessages.getString(
                    "IMSBC-E01133.AppCfgPersistLoadFailed",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        try {
            reader.close();
        } catch (IOException e) {
            throw new MBeanException(e, mMessages.getString(
                    "IMSBC-E01128.AppCfgFileCloseFailed",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        return appConfigMap; 
    }

    // pattern to match '[' id ']'
    private static final Pattern cStanzaHeadPattern =
            Pattern.compile("^\\s*\\[\\s*(\\w+)\\s*\\]\\s*$");

/**
     * Parses a line from the reader that follow the format '[' id ']',
     * and returns the portion denoted by <code>id</code>.
     * <p/>
     * Lines are read from the reader until a line that matches the format is
     * found, or until the end of the input is reached.  Lines encountered
     * that do not match the format are discarded.
     * 
     * @return String corresponding to the value that is the subject of the
     *         pattern described above, or <code>null</code> if no matching
     *         lines are found.
     * 
     * @throws IOException if an I/O exception occurs
     */
    private String readStanzaHead(BufferedReader reader) throws IOException {
        String ret = null;
        String line;
        do {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Looking for stanza start");
            }
            
            line = reader.readLine();
            if (line != null) {
                
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer("Read line: " + line);
                }
                
                Matcher matcher = cStanzaHeadPattern.matcher(line);
                if (matcher.matches()) {
                    ret = matcher.group(1);
                    assert (!"".equals(ret));
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer("Stanza start: '" + ret + "'");
                    }
                    break;
                } else {
                    // Warn that content was skipped
                    mLogger.log(Level.WARNING, mMessages.getString(
                            "IMSBC-E01134.AppCfgPersistLoadIgnored", line));
                }
            }
        } while (line != null);
        
        return ret;
    }

    // pattern to match id '=' value
    private static final Pattern cStanzaTailPattern =
            Pattern.compile("^\\s*(\\w+)\\s*=\\s*(-?\\w*)\\s*$");
    
    /**
     * Parses lines from the input that follows the format id '=' value. For
     * every id, finds a matching field listed in {@link #APP_CONFIG_FIELDS},
     * then assigns the value to that field.  The objects in APP_CONFIG_FIELDS
     * are not changed; copies of matching fields are used.
     * <p/>
     * Lines are read from the input source until the beginning of a new stanza
     * is detected. See {@link #readStanzaHead(java.io.BufferedReader)}.
     * 
     * @param fields A collection to which are added the fields corresponding
     *               to the parsed lines.
     * @param reader Input source 
     */
    private void readStanzaTail(Collection<ApplicationConfigurationField> fields,
                                BufferedReader reader) throws IOException {

        String line;
        final int READAHEAD_LIMIT = 1000;

        assert (fields.isEmpty());

        do {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Looking for stanza tail");
            }
            
            reader.mark(READAHEAD_LIMIT);
            line = reader.readLine();
            if (line != null) {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer("Read line: " + line);
                }
                Matcher matcher = cStanzaTailPattern.matcher(line);
                if (matcher.matches()) {
                    String name = matcher.group(1);
                    String value = matcher.group(2);
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer("Found property: " + name + ", value: " + value);
                    }
                    
                    // Accept only properties that match known fields
                    for (ApplicationConfigurationField f : APP_CONFIG_FIELDS) {
                        if (f.fieldId.equals(name)) {
                            if (mLogger.isLoggable(Level.FINEST)) {
                                mLogger.finest("Property " + name + " accepted");
                            }
                            f = (ApplicationConfigurationField) f.clone();
                            f.fromString(value);
                            fields.add(f);
                            break;
                        }
                    }
                }
                // If the line does not specifies a property, check
                // first for a possible new stanza before discarding the line.
                else {
                    Matcher headMatcher = cStanzaHeadPattern.matcher(line);
                    // New stanza; put it back in the reader and stop
                    if (headMatcher.matches()) {
                        if (mLogger.isLoggable(Level.FINER)) {
                            mLogger.fine("New stanza start found; read finished");
                        }
                        reader.reset();
                        line = null;
                    } else {
                        // Warn that content was skipped
                        mLogger.log(Level.WARNING, mMessages.getString(
                                "MQBC_ACFG0108_AppCfgPersistLoadIgnored", line));
                    }
                }
            }
        } while (line != null);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("Stanza complete, " + fields.size() + " fields read");
        }
    }      

    public void dump(StringBuffer msgBuf) {
        msgBuf.append(CONFIG_THREADS + " (max outbound threads)");
        msgBuf.append(": ").append(getThreads()).append('\n');
        
        try {
            // TabularData envVars = getEnvironmentVariables();
            TabularData envVars = getApplicationVariables();
            // do an explicit cast to make java6 compiler happy ---
            Collection<CompositeData> vars = (Collection<CompositeData>)envVars.values();
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