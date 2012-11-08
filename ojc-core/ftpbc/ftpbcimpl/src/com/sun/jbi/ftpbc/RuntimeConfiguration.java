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
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;

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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {

    private static final Messages mMessages =
            Messages.getMessages(RuntimeConfiguration.class);
    private static final Logger mLogger =
            Messages.getLogger(RuntimeConfiguration.class);
    // Configuration file name for app variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
    // Application configuration row fields
    private static final String APP_CFG_NAME = "configurationName";
    // app config parms
    public static final String FTP_HOST_NAME = "Host";
    public static final String FTP_HOST_PORT = "Port";
    public static final String FTP_USER_ID = "User";
    public static final String FTP_PASSWORD = "Password";
    public static final String FTP_DIR_LIST_STYLE = "DirListStyle";
    public static final String FTP_USE_USER_DEFINED_DIR_LIST_STYLE = "UseUserDefinedDirListStyle";
    public static final String FTP_USER_DEFINED_DIR_LIST_STYLE = "UserDefinedDirListStyle";
    public static final String FTP_USER_DEFINED_DIR_LIST_STYLE_CFG = "UserDefinedDirListStyleConfig";
    public static final String FTP_SEC_TYPE = "SecuredType";
    public static final String FTP_ENABLE_CCC = "EnableCCC";
    public static final String FTP_KEY_STORE = "KeyStore";
    public static final String FTP_KEY_STORE_PASSWORD = "KeyStorePassword";
    public static final String FTP_KEY_ALIAS = "KeyAlias";
    public static final String FTP_KEY_PASSWORD = "KeyPassword";
    public static final String FTP_TRUST_STORE = "TrustStore";
    public static final String FTP_TRUST_STORE_PASSWORD = "TrustStorePassword";
    public static final String FTP_PERSISTENCE_BASE_DIR = "PersistenceBaseLoc";
    private static final String[] columns = new String[]{
        APP_CFG_NAME,
        FTP_HOST_NAME,
        FTP_HOST_PORT,
        FTP_USER_ID,
        FTP_PASSWORD,
        FTP_DIR_LIST_STYLE,
        FTP_USE_USER_DEFINED_DIR_LIST_STYLE,
        FTP_USER_DEFINED_DIR_LIST_STYLE,
        FTP_USER_DEFINED_DIR_LIST_STYLE_CFG,
        FTP_SEC_TYPE,
        FTP_ENABLE_CCC,
        FTP_KEY_STORE,
        FTP_KEY_STORE_PASSWORD,
        FTP_KEY_ALIAS,
        FTP_KEY_PASSWORD,
        FTP_TRUST_STORE,
        FTP_TRUST_STORE_PASSWORD,
        FTP_PERSISTENCE_BASE_DIR
    };
    private static final List colsEnrypted = Arrays.asList(new String[]{
                FTP_PASSWORD,
                FTP_KEY_STORE_PASSWORD,
                FTP_KEY_PASSWORD,
                FTP_TRUST_STORE_PASSWORD
            });
    // Configuration 
    private Properties mConfig;
    private String mWorkspaceRoot;
    // Global application configurations
    private Map mAppVarMap;
    private Map mAppConfigMap;
    private CompositeType mAppVarRowType;
    private CompositeType mAppConfigRowType;
    private TabularType mAppVarTabularType;
    private TabularType mAppConfigTabularType;
    // Use delegation to support notification
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
    // Key store utility for password encryption
    private KeyStoreUtilClient mKeyStoreUtilClient;

    public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keystoreUtilClient) throws MBeanException {
        mWorkspaceRoot = workspaceRoot;
        mKeyStoreUtilClient = keystoreUtilClient;

        // Load the persisted configuration
        try {
            mAppConfigRowType = createApplicationConfigurationCompositeType();
            mAppConfigTabularType = createApplicationConfigurationTabularType();
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
            mConfig = ConfigPersistence.loadConfig(workspaceRoot);
            mAppVarMap = loadApplicationVariablesConfig(workspaceRoot);
            mAppConfigMap = loadApplicationConfiguration(workspaceRoot);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FTPBC-E01210.Faile_to_construct_composite_data_structures", e.getLocalizedMessage()));
        }

    }

    public Integer getOutboundThreads() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_OUTBOUND_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }

    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001039.RTC_Invalid_arg", new Object[]{FTPBCComponentContext.CONFIG_OUTBOUND_THREADS, ex.getMessage()}));
        }
        if (newVal.intValue() < FTPBCComponentContext.MIN_OUTBOUND_THREADS ||
                newVal.intValue() > FTPBCComponentContext.MAX_OUTBOUND_THREADS) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001040.RTC_Invalid_attr",
                    new Object[]{newVal, FTPBCComponentContext.CONFIG_OUTBOUND_THREADS, new Long(FTPBCComponentContext.MIN_OUTBOUND_THREADS), new Long(FTPBCComponentContext.MAX_OUTBOUND_THREADS)}));
        }

        persistAndNotify(FTPBCComponentContext.CONFIG_OUTBOUND_THREADS, Integer.class.getName(), newVal, getOutboundThreads());
    }

    /**
     * ftp proxy settings
     */
    public Integer getInvokeTimeout() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT, DEFAULT_INVOKE_TIMEOUT);
        return Integer.valueOf(val);
    }

    public void setInvokeTimeout(Integer val) throws InvalidAttributeValueException, MBeanException {
        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001039.RTC_Invalid_arg", new Object[]{FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT, ex.getMessage()}));
        }

        if (newVal.intValue() <= 0) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001040.RTC_Invalid_attr",
                    new Object[]{newVal, FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT, new Long(1000), new Long(999999999)}));
        }

        persistAndNotify(FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT, Integer.class.getName(), newVal, getInvokeTimeout());
    }

    public Boolean getUseProxy() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_USE_PROXY, "false");
        return Boolean.valueOf(val);
    }

    public void setUseProxy(Boolean val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_USE_PROXY, Boolean.class.getName(), val, getUseProxy());
    }

    public String getProxyURL() {
        return mConfig.getProperty(FTPBCComponentContext.CONFIG_PROXY_URL, "");
    }

    public void setProxyURL(String val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_PROXY_URL, String.class.getName(), val, getProxyURL());
    }

    /**
     * for passive ftp
     */
    public Boolean getUsePassiveFTP() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_USE_PASSIVE_FTP, "true");
        return Boolean.valueOf(val);
    }

    public void setUsePassiveFTP(Boolean val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_USE_PASSIVE_FTP, Boolean.class.getName(), val, getUsePassiveFTP());
    }

    public String getProxyUserID() {
        return mConfig.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_ID, "");
    }

    public void setProxyUserID(String val) throws InvalidAttributeValueException,
            MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_PROXY_USR_ID, String.class.getName(), val, getProxyUserID());
    }

    public String getProxyUserPassword() throws Exception {
        try {
            String base64Encoded = mConfig.getProperty(FTPBCComponentContext.CONFIG_PROXY_USR_PASSWD);
            if (base64Encoded == null || base64Encoded.length() == 0) {
                return "";
            } else {
                return mKeyStoreUtilClient.decrypt(base64Encoded);
            }
        } catch (Exception ex) {
            throw new Exception(ex);
        }
    }

    public void setProxyUserPassword(String val) throws InvalidAttributeValueException,
            MBeanException {
        try {
            String oldVal = getProxyUserPassword();
            String base64Encoded = mKeyStoreUtilClient.encrypt(val);
            persistAndNotify(FTPBCComponentContext.CONFIG_PROXY_USR_PASSWD, String.class.getName(), base64Encoded, oldVal);
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
    }

    private Object[] exractParametersByOrder(HashMap parms) {
        Object[] p = new Object[columns.length];
        for (int i = 0; i < columns.length; i++) {
            p[i] = parms.get(columns[i]);
        }
        return p;
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

    private HashMap mergeParameters(HashMap existingParms, CompositeData config) {
        Object[] values = config.getAll(columns);
        for (int i = 0; i < columns.length; i++) {
            if (columns[i] != null && values[i] != null) {
                existingParms.put(columns[i], values[i]);
            }
        }
        return existingParms;
    }

    private void persistAndNotify(String attrName, String attrType, Object newVal, Object oldVal)
            throws InvalidAttributeValueException, MBeanException {
        if (attrName == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001047.RTC_Attr_set_null_property_name"));
        }
        if (newVal == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001048.RTC_Attr_set_null_property_value", attrName));
        }
        mConfig.put(attrName, newVal.toString());
        persistConfiguration();
        Notification notif = new AttributeChangeNotification(this,
                0,
                System.currentTimeMillis(),
                mMessages.getString(
                // break to avoid processing by logging tool
                "FTPBC-C001999.RTC_Attr_changed"),
                attrName,
                attrType,
                oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);
    }

    /**
     * Get the CompositeType definition for the components application configuration
     *
     * @return the CompositeType for the components application configuration.
     */
    public CompositeType queryApplicationConfigurationType() {
        return mAppConfigRowType;
    }

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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01211.Application_config_name_already_exists", name)));
        }

        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != columns.length) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01212.Invalid_Item_Size_for_app_config", new Object[]{name, rowType.keySet().size(), columns.length}));
        }

        if (!appConfig.containsKey(APP_CFG_NAME)) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        }

        HashMap appConfigParms = getParameters(appConfig);

        String err = validateAppConfig(appConfigParms);

        if (err != null && err.trim().length() > 0) {
            throw new InvalidAttributeValueException(err);
        }

        mAppConfigMap.put(name, appConfigParms);

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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01215.Application_configuration_does_not_exist_for_delete", name)));
        }

        mAppConfigMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FTPBC-C01206.Application_configuration_deleted", name));
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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01216.Application_configuration_does_not_exist_for_set", name)));
        }

        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != columns.length) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01212.Invalid_Item_Size_for_app_config", new Object[]{name, rowType.keySet().size(), columns.length}));
        }

        if (!appConfig.containsKey(APP_CFG_NAME)) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01213.Invalid_key_for_composite_data_for_app_config", name));
        }

        HashMap existingParms = (HashMap) mAppConfigMap.get(name);
        HashMap parms = mergeParameters(existingParms, appConfig);

        String err = validateAppConfig(parms);

        if (err != null && err.trim().length() > 0) {
            throw new InvalidAttributeValueException(err);
        }

        mAppConfigMap.put(name, parms);

        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FTPBC-C01207.Application_configuration_updated", new Object[]{name}));
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
            HashMap parms = (HashMap) mAppConfigMap.get(name);

            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                        columns,
                        exractParametersByOrder(parms));

                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("FTPBC-E01217.Unable_to_construct_composite_data_for_app_config"), e);
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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01218.Application_variable_name_already_exists", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        }

        String appVarValue = (String) appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String) appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }

        if (appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }

        mAppVarMap.put(name, new String[]{appVarValue, appVarType});
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FTPBC-C01203.New_application_variable_added", new Object[]{name, appVarValue}));
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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01223.Application_variable_does_not_exist_for_delete", name)));
        }

        mAppVarMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FTPBC-C01204.Application_variable_deleted", name));
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
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E01224.Application_variable_does_not_exist_for_set", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01220.Invalid_key_for_composite_data_for_app_variable", name));
        }

        String appVarValue = (String) appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String) appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01221.Invalid_app_variable_composite_data_no_value_field", name));
        }

        if (appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E01222.Invalid_app_variable_composite_data_no_type_field", name));
        }

        mAppVarMap.put(name, new String[]{appVarValue, appVarType});
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("FTPBC-C01202.Application_variable_updated", new Object[]{name, appVarValue}));
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
                            APPLICATION_VARIABLES_TYPE_FIELD},
                        data);

                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("FTPBC-E01225.Unable_to_construct_composite_data_for_app_variable"), e);
            }
        }

        return tabularData;
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

    private CompositeType createApplicationVariableCompositeType() throws OpenDataException {
        if (mAppVarRowType != null) {
            return mAppVarRowType;
        }

        String[] appVarRowAttrNames = {
            APPLICATION_VARIABLES_ROW_KEY,
            APPLICATION_VARIABLES_VALUE_FIELD,
            APPLICATION_VARIABLES_TYPE_FIELD};
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

        String[] appConfigRowAttrNames = {
            APP_CFG_NAME,
            FTP_HOST_NAME,
            FTP_HOST_PORT,
            FTP_USER_ID,
            FTP_PASSWORD,
            FTP_DIR_LIST_STYLE,
            FTP_USE_USER_DEFINED_DIR_LIST_STYLE,
            FTP_USER_DEFINED_DIR_LIST_STYLE,
            FTP_USER_DEFINED_DIR_LIST_STYLE_CFG,
            FTP_SEC_TYPE,
            FTP_ENABLE_CCC,
            FTP_KEY_STORE,
            FTP_KEY_STORE_PASSWORD,
            FTP_KEY_ALIAS,
            FTP_KEY_PASSWORD,
            FTP_TRUST_STORE,
            FTP_TRUST_STORE_PASSWORD,
            FTP_PERSISTENCE_BASE_DIR
        };
        String[] appConfigAttrDesc = {
            getDisplayName(APP_CFG_NAME),
            getDisplayName(FTP_HOST_NAME),
            getDisplayName(FTP_HOST_PORT),
            getDisplayName(FTP_USER_ID),
            getDisplayName(FTP_PASSWORD),
            getDisplayName(FTP_DIR_LIST_STYLE),
            getDisplayName(FTP_USE_USER_DEFINED_DIR_LIST_STYLE),
            getDisplayName(FTP_USER_DEFINED_DIR_LIST_STYLE),
            getDisplayName(FTP_USER_DEFINED_DIR_LIST_STYLE_CFG),
            getDisplayName(FTP_SEC_TYPE),
            getDisplayName(FTP_ENABLE_CCC),
            getDisplayName(FTP_KEY_STORE),
            getDisplayName(FTP_KEY_STORE_PASSWORD),
            getDisplayName(FTP_KEY_ALIAS),
            getDisplayName(FTP_KEY_PASSWORD),
            getDisplayName(FTP_TRUST_STORE),
            getDisplayName(FTP_TRUST_STORE_PASSWORD),
            getDisplayName(FTP_PERSISTENCE_BASE_DIR)
        };

        OpenType[] appConfigAttrTypes = {
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.INTEGER,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.BOOLEAN,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.BOOLEAN,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING,
            SimpleType.STRING
        };

        mAppConfigRowType = new CompositeType("AppliationConfigurationObject",
                "Application Configuration Composite Data",
                appConfigRowAttrNames,
                appConfigAttrDesc,
                appConfigAttrTypes);

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
                new String[]{APP_CFG_NAME});

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
            throw new MBeanException(ex, mMessages.getString("FTPBC-E01207.Failed_to_persist_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
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
                            value = (startIndex == 0) ? "" : varDesc.substring(0, startIndex);
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
            throw new MBeanException(ex, mMessages.getString("FTPBC-E01208.Failed_to_load_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
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

    private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appConfigPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext();) {
                String key = (String) iter.next();
                HashMap values = (HashMap) mAppConfigMap.get(key);
                Set keys = values.keySet();
                Iterator it = keys.iterator();
                while (it.hasNext()) {
                    String parmKey = (String) it.next();
                    Object parmValue = values.get(parmKey);
                    if (colsEnrypted.contains(parmKey)) {
                        parmValue = mKeyStoreUtilClient.encrypt(parmValue != null ? parmValue.toString() : "");
                    }
                    String prop = (parmValue != null) ? key.concat(".").concat(parmKey) + "=" + parmValue.toString() + "\n" : key.concat(".").concat(parmKey) + "=\n";
                    os.write(prop.getBytes());
                }
            }
            os.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FTPBC-E01229.Failed_to_persist_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }
    }

    private Map loadApplicationConfiguration(String workspaceRoot) throws MBeanException {
        Map appConfigMap = new HashMap();

        File appConfigPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        if (!appConfigPersistFileName.exists()) {
            // no persisted app configs yet, return a blank map
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
                    // app key : myAppConfig 
                    // myAppConfig1.HOST = lobster.stc.com
                    // myAppConfig1.PORT = 21
                    // myAppConfig1.USER = anonymous
                    // myAppConfig1.PASSWORD = abc@yahoo.com

                    // extract the appConfig key
                    index = key.indexOf(".");
                    if (index > 0 && index < key.length()) {
                        parmKey = key.substring(index + 1);
                        key = key.substring(0, index);
                        appConfigParms = (Map) appConfigMap.get(key);
                        if (appConfigParms == null) {
                            appConfigParms = new HashMap();
                            appConfigMap.put(key, appConfigParms);
                        }
                        if (parmKey != null && parmKey.length() > 0) {
                            if (colsEnrypted.contains(parmKey)) {
                                value = mKeyStoreUtilClient.decrypt(value);
                            }
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
        } catch (Exception ex) {
            throw new MBeanException(ex, mMessages.getString("FTPBC-E01230.Failed_to_load_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                    //
                }
            }
        }

        return appConfigMap;
    }

    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            throw new MBeanException(ex, mMessages.getString("FTPBC-E01209.Failed_to_persist_mbean_config", mWorkspaceRoot));
        }
    }

    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[]{new MBeanNotificationInfo(new String[]{AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }

    public void dump(StringBuffer msgBuf) {
        msgBuf.append(FTPBCComponentContext.CONFIG_OUTBOUND_THREADS).append(": ").append(getOutboundThreads()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_INVOKE_TIMEOUT).append(": ").append(getInvokeTimeout()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_USE_PROXY).append(": ").append(getUseProxy()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_PROXY_URL).append(": ").append(getProxyURL()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_PROXY_USR_ID).append(": ").append(getProxyUserID()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_PROXY_USR_PASSWD).append(": ").append("********").append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_USE_PASSIVE_FTP).append(": ").append(getUsePassiveFTP()).append("\n");

        msgBuf.append(FTPBCComponentContext.CONFIG_CONN_MAX_IDLE_TIMEOUT).append(": ").append(getConnectionMaxIdleTimeout() != null ? getConnectionMaxIdleTimeout().toString() : "").append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_POOL_MIN_SZ).append(": ").append(getConnectionPoolMinSize()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_POOL_MAX_SZ).append(": ").append(getConnectionPoolMaxSize()).append("\n");
        msgBuf.append(FTPBCComponentContext.CONFIG_ENABLE_NM_PROPS).append(": ").append(getEnableNMProps()).append("\n");

//      now obtain the parameters as JVM properties
//      open esb issue #: 1793
//        msgBuf.append(FTPBCComponentContext.CONFIG_ENABLE_CLUSTER_AWARE).append(": ").append(getEnableClusterAware()).append("\n");
//        msgBuf.append(FTPBCComponentContext.CONFIG_TOKEN_PERSIST_URL).append(": ").append(getTokenPersistenceURL()).append("\n");
//        msgBuf.append(FTPBCComponentContext.CONFIG_DB_DRV_CLASS).append(": ").append(getTokenDBJDBCDriverClass()).append("\n");

        msgBuf.append(CONFIG_APPLICATON_VARIABLES).append(": { ");
        for (Iterator it = mAppVarMap.keySet().iterator(); it.hasNext();) {
            String name = (String) it.next();
            msgBuf.append('[').append(name).append(',');
            String[] valueType = (String[]) mAppVarMap.get(name);
            if ("PASSWORD".equals(valueType[1])) {
                msgBuf.append("*******").append(']');
            } else {
                msgBuf.append(valueType[0]).append(']');
            }
        }
        msgBuf.append(" }\n");
        msgBuf.append(CONFIG_APPLICATION_CONFIGURATIONS).append(": { ");
        for (Iterator it = mAppConfigMap.keySet().iterator(); it.hasNext();) {
            String name = (String) it.next();
            msgBuf.append('[').append("\n");
            Map parms = (Map) mAppConfigMap.get(name);
            if (parms != null) {
                Set parmKeys = parms.keySet();
                Iterator it2 = parmKeys.iterator();
                while (it2.hasNext()) {
                    String pKey = (String) it2.next();
                    Object value = parms.get(pKey);
                    if (value != null) {
                        if (colsEnrypted.contains(pKey)) {
                            value = "********";
                        }
                        msgBuf.append(name).append(".").append(pKey).append(":").append(value.toString()).append("\n");
                    } else {
                        msgBuf.append(name).append(".").append(pKey).append(":").append("\n");
                    }
                }
            }
            msgBuf.append(']');
        }
        msgBuf.append(" }");
    }

    private HashMap getParameters(CompositeData config) {
        HashMap parms = new HashMap();
        Object[] values = config.getAll(columns);
        for (int i = 0; i < columns.length; i++) {
            parms.put(columns[i], values[i]);
        }
        return parms;
    }

    private String validateAppConfig(HashMap parms) {
        StringBuffer errMsg = new StringBuffer();
        String appCfgName = (String) parms.get(APP_CFG_NAME);
        String parm = (String) parms.get(FTP_HOST_NAME);
        if (parm == null || parm.length() == 0) {
            // if app config is used
            // host is mandatory
            errMsg.append("Application Configuration Parameter :" + FTP_HOST_NAME + " not specified for :" + appCfgName);
            errMsg.append("\n");
        }
        if (parms.get(FTP_HOST_PORT) == null) {
            // if app config is used
            // port is mandatory
            errMsg.append("Application Configuration Parameter :" + FTP_HOST_PORT + " not specified for :" + appCfgName);
            errMsg.append("\n");
        }

        parm = (String) parms.get(FTP_USER_ID);
        if (parm == null || parm.length() == 0) {
            // if app config is used
            // user is mandatory
            errMsg.append("Application Configuration Parameter :" + FTP_USER_ID + " not specified for :" + appCfgName);
            errMsg.append("\n");
        }

        // validate if use user defined dir list style is enabled

        parm = (String) parms.get(FTP_SEC_TYPE);
        String secType = parm;
        if (parm == null || parm.length() == 0) {
            errMsg.append("Application Configuration Parameter :" + FTP_SEC_TYPE + " not specified for :" + appCfgName);
            errMsg.append("\n");
        } else {
            if (!parm.equals(FtpFileConfigConstants.FTP_SECURE_NONE)) {
                // need at least kstore or tstore specified
                // kstore and tstore can fall back to each other
                boolean hasStore = false;
                parm = (String) parms.get(FTP_KEY_STORE);
                if (parm == null || parm.length() == 0) {
                } else {
                    parm = (String) parms.get(FTP_KEY_STORE_PASSWORD);
                    if (parm == null || parm.length() == 0) {
                        errMsg.append("Application Configuration Parameter :" + FTP_KEY_STORE_PASSWORD + " not specified for :" + appCfgName);
                        errMsg.append("\n");
                    }
                    hasStore = true;
                }
                parm = (String) parms.get(FTP_TRUST_STORE);
                if (parm == null || parm.length() == 0) {
                } else {
                    // if tstore is specified - a password must also be specified
                    parm = (String) parms.get(FTP_TRUST_STORE_PASSWORD);
                    if (parm == null || parm.length() == 0) {
                        errMsg.append("Application Configuration Parameter :" + FTP_TRUST_STORE_PASSWORD + " not specifiedfor :" + appCfgName);
                        errMsg.append("\n");
                    }
                    hasStore = true;
                }

                if (!hasStore) {
                    errMsg.append("When Application Configuration Parameter " + FTP_SEC_TYPE + " = " + secType + " key store and/or trust store must be specified.");
                    errMsg.append("\n");
                }
            }
        }
        return errMsg.toString();
    }

    private String getDisplayName(String key) {
        return mMessages.getString(key);
    }

    public Integer getConnectionPoolMinSize() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_POOL_MIN_SZ, "0");
        return Integer.valueOf(val);
    }

    public void setConnectionPoolMinSize(Integer val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_POOL_MIN_SZ, Integer.class.getName(), val, getConnectionPoolMinSize());
    }

    public Integer getConnectionPoolMaxSize() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_POOL_MAX_SZ, "1");
        return Integer.valueOf(val);
    }

    public void setConnectionPoolMaxSize(Integer val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_POOL_MAX_SZ, Integer.class.getName(), val, getConnectionPoolMaxSize());
    }

    public Integer getConnectionMaxIdleTimeout() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_CONN_MAX_IDLE_TIMEOUT, "1000");
        return Integer.valueOf(val);
    }

    public void setConnectionMaxIdleTimeout(Integer val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_CONN_MAX_IDLE_TIMEOUT, Integer.class.getName(), val, getConnectionMaxIdleTimeout());
    }

    public Boolean getEnableNMProps() {
        String val = mConfig.getProperty(FTPBCComponentContext.CONFIG_ENABLE_NM_PROPS, "true");
        return Boolean.valueOf(val);
    }

    public void setEnableNMProps(Boolean val) throws InvalidAttributeValueException, MBeanException {
        persistAndNotify(FTPBCComponentContext.CONFIG_ENABLE_NM_PROPS, Boolean.class.getName(), val, getEnableNMProps());
    }
}
