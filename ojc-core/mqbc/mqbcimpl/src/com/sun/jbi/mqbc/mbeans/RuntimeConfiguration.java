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

package com.sun.jbi.mqbc.mbeans;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.mqbc.I18n;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 * 
 * @author Rulong Chen
 * @author Noel.Ang@sun.com
 */
public class RuntimeConfiguration
        implements RuntimeConfigurationMBean, NotificationEmitter {

    // Logger
    private final Logger mLogger = Logger.getLogger(getClass().getName());
    
    // Configuration files' names
    static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
    static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    
    // Row and table metadata
    static final TabularType APPCONFIG_TABULAR_TYPE;
    static final CompositeType APPCONFIG_ROW_TYPE;
    static final CompositeType APPVAR_ROW_TYPE;
    static final TabularType APPVAR_TABULAR_TYPE;
    
    // Application Configuration
    private static final ApplicationConfigurationField APP_CONFIG_NAME;
    private static final ApplicationConfigurationField APP_CONFIG_QMGR_HOST;
    private static final ApplicationConfigurationField APP_CONFIG_QMGR_PORT;
    private static final ApplicationConfigurationField APP_CONFIG_QMGR_NAME;
    private static final ApplicationConfigurationField APP_CONFIG_USERNAME;
    private static final ApplicationConfigurationField APP_CONFIG_PASSWORD;
    private static final ApplicationConfigurationField APP_CONFIG_CIPHER;
    private static final ApplicationConfigurationField APP_CONFIG_SSLPEER;
    private static final ApplicationConfigurationField[] APP_CONFIG_FIELDS = {
            APP_CONFIG_NAME = new AppConfigNameField(),
            APP_CONFIG_QMGR_HOST = new AppConfigQmgrHostField(),
            APP_CONFIG_QMGR_PORT = new AppConfigQmgrPortField(),
            APP_CONFIG_QMGR_NAME = new AppConfigQmgrNameField(),
            APP_CONFIG_USERNAME = new AppConfigUsernameField(),
            APP_CONFIG_PASSWORD = new AppConfigPasswordField(),
            APP_CONFIG_CIPHER = new AppConfigCipherSuiteNameField(),
            APP_CONFIG_SSLPEER = new AppConfigSslPeerNameField(),
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
    
    // Path to component workspace
    private final String mWorkspaceRoot;
    
    // Storage for
    // Runtime properties
    // Application Configuration
    // Application Variables
    private final Properties mConfig;
    private final Map<String, Collection<ApplicationConfigurationField>> mAppConfigMap;
    private final Map<String, String[]> mAppVarMap;
    
    // Constraints and defaults
    private final String DEFAULT_OUTBOUND_THREADS = "10";
    private final String DEFAULT_SEPARATE_TRX_BRANCHES = "true";
    private final int CONSTRAINTS_MIN_OUTBOUND_THREADS = 1;
    private final int CONSTRAINTS_MAX_OUTBOUND_THREADS = 65535;
    
    // Use delegation to support notification
    private final NotificationBroadcasterSupport broadcasterSupport =
            new NotificationBroadcasterSupport();
    
    private KeyStoreUtilClient mKeySupport;
    
    // Runtime properties
    public static final String OUTBOUND_THREADS_PROPERTY = "OutboundThreads";
    public static final String SEPARATE_TRX_BRANCHES_PROPERTY = "SeparateTrxBranches";

    static {
        try {
            // Call order is important
            APPCONFIG_ROW_TYPE = createApplicationConfigurationCompositeType();
            APPCONFIG_TABULAR_TYPE = createApplicationConfigurationTabularType();
            APPVAR_ROW_TYPE = createApplicationVariableCompositeType();
            APPVAR_TABULAR_TYPE = createApplicationVariableTabularType();
        } catch (OpenDataException e) {
            throw new RuntimeException(I18n.msg("0800: Failed to initialize" 
                    + " runtime configuration MBean metadata"), e);
        }
    }
    
    public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keySupport) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        mKeySupport = keySupport;
        
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        try {
            mAppConfigMap = loadApplicationConfigurations(workspaceRoot);
            mAppVarMap = loadApplicationVariables(workspaceRoot);
        } catch (MBeanException e) {
            throw new JBIException(I18n.msg(
                    "0801: Failed to load configuration/variables from workspace"), e);
        }
    }
    
    
    public Integer getOutboundThreads() {
        String val = mConfig.getProperty(OUTBOUND_THREADS_PROPERTY,
                DEFAULT_OUTBOUND_THREADS);
        return Integer.valueOf(val);
    }
    
    public void setOutboundThreads(Integer newVal) throws InvalidAttributeValueException, MBeanException {
        if (newVal == null) {
            throw new InvalidAttributeValueException("Value is null");
        } else {
            if (newVal < CONSTRAINTS_MIN_OUTBOUND_THREADS
                    || newVal > CONSTRAINTS_MAX_OUTBOUND_THREADS) {
                throw new InvalidAttributeValueException(I18n.msg(
                        "0802: Invalid value {0}"
                                + " for configuration parameter {1},"
                                + " valid range is {2} thru {3}",
                        newVal, OUTBOUND_THREADS_PROPERTY,
                        CONSTRAINTS_MIN_OUTBOUND_THREADS,
                        CONSTRAINTS_MAX_OUTBOUND_THREADS));
            }
        }
        
        // Prepare notification of change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Integer.class.getName();
        Integer oldVal = getOutboundThreads();
        Notification notif = new AttributeChangeNotification(this,
                seqNo,
                System.currentTimeMillis(),
                msg, OUTBOUND_THREADS_PROPERTY,
                attrType,
                oldVal,
                newVal);
        
        // Apply, notify, save
        synchronized (mConfig) {
            mConfig.setProperty(OUTBOUND_THREADS_PROPERTY, newVal.toString());
            broadcasterSupport.sendNotification(notif);
            persistEnvConfig();
        }
    }
    
    public Boolean getSeparateTrxBranches() {
        String val = mConfig.getProperty(SEPARATE_TRX_BRANCHES_PROPERTY,
                DEFAULT_SEPARATE_TRX_BRANCHES
        );
        return Boolean.valueOf(val);
    }
    
    public void setSeparateTrxBranches(Boolean newVal)
            throws
            InvalidAttributeValueException,
            MBeanException {
        
        if (newVal == null) {
            throw new InvalidAttributeValueException("Value is null");
        }
        
        // Prepare notification of change
        long seqNo = 0;
        String msg = "Attribute changed";
        String attrType = Boolean.class.getName();
        Boolean oldVal = getSeparateTrxBranches();
        Notification notif = new AttributeChangeNotification(this,
                seqNo,
                System.currentTimeMillis(),
                msg, SEPARATE_TRX_BRANCHES_PROPERTY,
                attrType,
                oldVal,
                newVal);
        
        // Apply, notify, save
        synchronized (mConfig) {
            mConfig.setProperty(SEPARATE_TRX_BRANCHES_PROPERTY, newVal.toString());
            broadcasterSupport.sendNotification(notif);
            persistEnvConfig();
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0803: Cannot add application variable {0}"
                                + " - variable already exists",
                        name)));
            }

            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0804: Cannot add application variable {0}"
                                + " - {1} fields expected, {2} received",
                        name,
                        APP_VAR_COUNT,
                        rowType.keySet().size())));
            }
        
            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0805: Cannot add application variable {0}"
                                + " - field {1} is missing",
                        name,
                        APP_VAR_NAME)));
            }
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0806: Cannot add application variable {0}"
                                + " - its name does not match its"
                                + " {1} value of {2}",
                        name,
                        APP_VAR_NAME,
                        appVarName)));
            }

            String appVarValue = (String) appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0807: Cannot add application variable {0} - {1} is null",
                        name,
                        APP_VAR_VALUE)));
            }
        
            String appVarType = (String) appVar.get(APP_VAR_TYPE);
            if (appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0807: Cannot add application variable {0} - {1} is null",
                        name,
                        APP_VAR_TYPE)));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG,
                        I18n.msg(
                                "0808: Added application variable {0} - value: {1}",
                                name,
                                appVarValue));
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0809: Cannot update non-existent application variable {0}",
                        name)));
            }
        
            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0804: Cannot add application variable {0}"
                                + " - {1} fields expected, {2} received",
                        name,
                        APP_VAR_COUNT,
                        rowType.keySet().size())));
            }

            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0805: Cannot add application variable {0}"
                                + " - field {1} is missing",
                        name,
                        APP_VAR_NAME)));
            } 
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0806: Cannot add application variable {0}"
                                + " - its name does not match its"
                                + " {1} value of {2}",
                        name,
                        APP_VAR_NAME,
                        appVarName)));
            }
        
            String appVarValue = (String)appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0807: Cannot add application variable {0} - {1} is null",
                        name,
                        APP_VAR_VALUE)));
            }
        
            String appVarType = (String)appVar.get(APP_VAR_TYPE);
            if ( appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0807: Cannot add application variable {0} - {1} is null",
                        name,
                        APP_VAR_TYPE)));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG,
                        I18n.msg(
                                "0810: Updated application variable {0} - value: {1}",
                                name,
                                appVarValue));
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0811: Cannot delete non-existent application variable {0}",
                        name)));
            }
            mAppVarMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "0812: Deleted application variable {0}", name));
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
                    throw new RuntimeException(I18n.msg(
                            "0834: Failed to initialize application variables row structure"),
                            e);
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0813: Validation failed for application configuration {0}: {1}",
                        name,
                        validationResult[1])));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0814: Cannot add application configuration {0}"
                                + " - configuration already exists",
                        name)));
            }
            
            addAppCfg(name, appConfig);
            
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "0815: Added application configuration {0}", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING,
                        I18n.msg(
                                "0816: Failed to persist application configurations"),
                        e);
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
            throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                    "0817: Cannot add application configuration {0}"
                            + " - {1} fields expected, {2} received",
                    name,
                    APP_CONFIG_ROW_SIZE,
                    rowType.keySet().size())));
        }

        // Validate fields
        Collection<ApplicationConfigurationField> appConfig =
                new LinkedList<ApplicationConfigurationField>();
        for (ApplicationConfigurationField appcfgField: APP_CONFIG_FIELDS) {
            if (!config.containsKey(appcfgField.fieldId)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0818: Cannot add application configuration {0}"
                                + " - field {1} is missing",
                        name,
                        appcfgField.fieldId)));
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
                    throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                            "0819: Validation failed for application configuration {0}, field {1}: {2}",
                            name,
                            appcfgField.fieldId,
                            validation[1])));
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0813: Validation failed for application configuration {0}: {1}",
                        name,
                        validationResult[1])));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (!mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0820: Cannot delete non-existent application configuration {0}",
                        name)));
            }
        
            // Remove application configuration entry
            mAppConfigMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg("0821: Deleted application configuration {0}", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING,
                        I18n.msg(
                                "0816: Failed to persist application configurations"),
                        e);
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
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "0813: Validation failed for application configuration {0}: {1}",
                        name,
                        validationResult[1])));
            }
        }
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                    "0822: Cannot update non-existent application configuration {0}",
                    name)));
        }
        
        addAppCfg(name, appConfig);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, I18n.msg(
                    "0823: Updated application configuration {0}", name));
        }
        
        // Persist configuration
        try {
            persistAppConfigObjects();
        } catch (IOException e) {
            mLogger.log(Level.WARNING,
                    I18n.msg(
                            "0816: Failed to persist application configurations"),
                    e);
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
                    throw new RuntimeException(I18n.msg(
                            "0824: Failed to initialize application configuration row structure"),
                            e);
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

    private void persistEnvConfig() throws MBeanException {
        // Persist the changed configuration
        try {
            synchronized (mConfig) {
                ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
            }
        } catch (JBIException ex) {
            throw new MBeanException(ex,
                    I18n.msg("0825: Failed to persist configurations to {0}",
                            mWorkspaceRoot));
        }
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
                    if (type.equals("PASSWORD")) {
                        if (mKeySupport != null) {
                            value = mKeySupport.encrypt(value);
                        }
                    }
                    String prop = (value != null)
                            ? key + "=" + value + "{" + type + "}\n"
                            : key + "={" + type + "}\n";
                    writer.write(prop);
                } 
                writer.flush();
                writer.close();
            }
        } catch (Exception ex) {
            throw new MBeanException(ex,
                    I18n.msg("0826: Failed to persist application variables to {0}",
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
                            I18n.msg(
                                    "0827: I/O error occured trying to close store file {0}",
                                    file.getAbsolutePath()),
                            e1);
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
                if (type.equals("PASSWORD")) {
                    if (mKeySupport != null) {
                        try {
                            value = mKeySupport.decrypt(value);
                        } catch (Exception e1) {
                            throw new MBeanException(e1,
                                    I18n.msg(
                                            "0828: Failed to decrypt password data"
                                                    + " from loaded application variable {0}",
                                            name));
                        }
                    }
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (IOException ex) {
            throw new MBeanException(ex, I18n.msg(
                    "0830: Failed to load application variables from file {0}",
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
                        mLogger.warning(I18n.msg(
                                "0831: Loaded application configuration {0}," 
                                        + " but loaded fields overwrote" 
                                        + " existing data in memory",
                                name));
                    }
                    mLogger.info(I18n.msg(
                            "0832: Loaded application configuration {0}", acoName));
                    
                    // Was already scanning an ACO, finish up
                    if (acoName != null) {
                        reader.reset();
                        fields.clear();
                        acoName = null;
                    }
                }
            }
        } catch (IOException e) {
            throw new MBeanException(e, I18n.msg(
                    "0833: Failed to load application configurations from {0}",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        try {
            reader.close();
        } catch (IOException e) {
            throw new MBeanException(e,
                    I18n.msg(
                            "0827: I/O error occured trying to close store file {0}",
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
            if (mLogger.isLoggable(Level.FINER)) {
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
                    mLogger.log(Level.WARNING,
                            I18n.msg("0829: Skipped line not matching expected "
                                    + "configuration file format(s): {0}",
                                    line));
                }
            }
        } while (line != null);
        
        return ret;
    }

    // pattern to match id '=' value
    private static final Pattern cStanzaTailPattern =
            Pattern.compile("^\\s*(\\w+)\\s*=\\s*(.*?)\\s*$");
    
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
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.finer("Looking for stanza tail");
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
                            mLogger.finer("New stanza start found; read finished");
                        }
                        reader.reset();
                        line = null;
                    } else {
                        // Warn that content was skipped
                        mLogger.log(Level.WARNING,
                                I18n.msg("0829: Skipped line not matching expected "
                                        + "configuration file format(s): {0}",
                                        line));
                    }
                }
            }
        } while (line != null);
        
        if (mLogger.isLoggable(Level.FINER)) {
            mLogger.finer("Stanza complete, " + fields.size() + " fields read");
        }
    }
}
