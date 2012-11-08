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

package com.sun.jbi.ldapbc;

import java.util.Properties;
import java.util.Map;
import java.util.HashMap;
import java.util.Enumeration;
import java.util.Collections;
import java.util.Collection;
import java.util.LinkedList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.logging.Logger;
import java.util.logging.Level;
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
import com.sun.jbi.configuration.ConfigPersistence;
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
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.ldapbc.configuration.*;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 *
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean,
    NotificationEmitter {

	// Message bundle
    private static final Messages mMessages =
            Messages.getMessages(RuntimeConfiguration.class);
    private static Logger mLogger = Messages.getLogger(RuntimeConfiguration.class);

    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
	public static final String CONFIG_MAX_RETRIES_COUNT = "RetryCount";
	public static final String CNFIG_RETRY_INTERVAL = "RetryInterval";
	public static final String CONFIG_RECOURCE_ACTION = "RecoveryType";

	public static final String CONFIG_ALLOW_DYNAMIC_ENDPOINT = "AllowDynamicEndpoint";

	public static final String CONFIG_ALLOW_CONNECTION_POOLING = "AllowConnectionPooling";
	public static final String CONFIG_CONNPOOL_PREF_SIZE = "ConnectionPoolPrefSize";
	public static final String CONFIG_CONNPOOL_MAX_SIZE = "ConnectionPoolMaxSize";
	public static final String CONFIG_CONNPOOL_MAX_IDLE_TIME = "ConnectionMaxIdleTimeout";
	public static final String CONFIG_CONNPOOL_PROTOCOL = "ConnectionProtocol";
	public static final String CONFIG_CONNPOOL_AUTHENTICATION = "ConnectionAuthentication";
   
	// Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";
	private static final String DEFAULT_MAX_RETRY_COUNT = "0";
	private static final String DEFAULT_RETRY_INTERVAL = "1000";
	private static final String DEFAULT_RECOVERY_TYPE = "ERROR";

	private static final String DEFAULT_PREF_POOL_SIZE = "1";
	private static final String DEFAULT_MAX_POOL_SIZE = "10";
	private static final String DEFAULT_CONN_MAX_IDLE_TIMEOUT = "300";
	private static final String DEFAULT_PROTOCOL = "plain ssl";
	private static final String DEFAULT_AUTHENTICATION = "none simple";

    // Configuration validation settings    
    long MIN_THREADS = 1;
    long MAX_THREADS = 10000;

    // Configuration 
    Properties mConfig;
    String mWorkspaceRoot;

    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
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
    private static final ApplicationConfigurationField APP_CONFIG_LOCATION;
    private static final ApplicationConfigurationField APP_CONFIG_PRINCIPAL;
    private static final ApplicationConfigurationField APP_CONFIG_CREDENTIAL;
    private static final ApplicationConfigurationField APP_CONFIG_SSL_CON_TYPE;
    private static final ApplicationConfigurationField APP_CONFIG_AUTHENTICATION_TYPE;
	private static final ApplicationConfigurationField APP_CONFIG_PROTOCOL;
    private static final ApplicationConfigurationField APP_CONFIG_TRUSTSTORE;
    private static final ApplicationConfigurationField APP_CONFIG_TRUSTSTORE_PASSWORD;
	private static final ApplicationConfigurationField APP_CONFIG_TRUSTSTORE_TYPE;
    private static final ApplicationConfigurationField APP_CONFIG_KEYSTORE;
    private static final ApplicationConfigurationField APP_CONFIG_KEYSTORE_PASSWORD;
    private static final ApplicationConfigurationField APP_CONFIG_KEYSTORE_USERNAME;
    private static final ApplicationConfigurationField APP_CONFIG_KEYSTORE_TYPE;
    private static final ApplicationConfigurationField APP_CONFIG_KEYSTORE_TLS_SECURITY;
    private static final ApplicationConfigurationField[] APP_CONFIG_FIELDS = {
            APP_CONFIG_NAME = new AppConfigNameField(),
            APP_CONFIG_LOCATION = new AppConfigLocationField(),
            APP_CONFIG_PRINCIPAL = new AppConfigPrincipalField(),
            APP_CONFIG_CREDENTIAL = new AppConfigCredentialField(),
            APP_CONFIG_SSL_CON_TYPE = new AppConfigSSLTypeField(),
            APP_CONFIG_AUTHENTICATION_TYPE = new AppConfigAuthenticationField(),
			APP_CONFIG_PROTOCOL = new AppConfigProtocolField() ,
			APP_CONFIG_TRUSTSTORE = new AppConfigTrustStoreField() ,
			APP_CONFIG_TRUSTSTORE_PASSWORD = new AppConfigTrustStorePwdField(),
			APP_CONFIG_TRUSTSTORE_TYPE = new AppConfigTrustStoreTypeField(),
			APP_CONFIG_KEYSTORE = new AppConfigKeystoreField(),
			APP_CONFIG_KEYSTORE_PASSWORD = new AppConfigKeystorePwdField(),
			APP_CONFIG_KEYSTORE_USERNAME = new AppConfigKeystoreUNameField(),
			APP_CONFIG_KEYSTORE_TYPE = new AppConfigKeystoreTypeField(),
			APP_CONFIG_KEYSTORE_TLS_SECURITY = new AppConfigTLSSecurityField(),	 	
			
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
    
	 // Application Configuration Object store
    private final Map<String, Collection<ApplicationConfigurationField>> mAppConfigMap;
    // Application Variables store
    private final Map<String, String[]> mAppVarMap;

	 private KeyStoreUtilClient mKeySupport;

	 static {
        try {
            // Call order is important
            APPCONFIG_ROW_TYPE = createApplicationConfigurationCompositeType();
            APPCONFIG_TABULAR_TYPE = createApplicationConfigurationTabularType();
            APPVAR_ROW_TYPE = createApplicationVariableCompositeType();
            APPVAR_TABULAR_TYPE = createApplicationVariableTabularType();
        } catch (OpenDataException e) {
            throw new RuntimeException(mMessages.getString("LDAPBC_ACFG0201_RuntimeConfigMbeanCtorFailedMetadata"), e);
        }
    }
    /** Creates a new instance of InstallerExt */
    public RuntimeConfiguration(final String workspaceRoot, KeyStoreUtilClient keyStoreUtilClient) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        mKeySupport = keyStoreUtilClient;
		try {
            mAppConfigMap = loadApplicationConfigurations(workspaceRoot);
            mAppVarMap = loadApplicationVariables(workspaceRoot);
        } catch (MBeanException e) {
            throw new JBIException(mMessages.getString(
                    "LDAPBC_ACFG0200_RuntimeConfigMbeanCtorFailedDataLoad"), e);
        }
    }

    /**
     * @return
     */
    public Integer getThreads() {
        final String val = mConfig.getProperty(CONFIG_THREADS, RuntimeConfiguration.DEFAULT_THREADS);

        return Integer.valueOf(val);
    }

	public Integer getRetryCount() {
        final String val = mConfig.getProperty(CONFIG_MAX_RETRIES_COUNT, RuntimeConfiguration.DEFAULT_MAX_RETRY_COUNT);

        return Integer.valueOf(val);
    }


	public Integer getRetryInterval() {
        final String val = mConfig.getProperty(CNFIG_RETRY_INTERVAL, RuntimeConfiguration.DEFAULT_RETRY_INTERVAL);

        return Integer.valueOf(val);
    }

	public String  getRecoveryType() {
        final String val = mConfig.getProperty(CONFIG_RECOURCE_ACTION, RuntimeConfiguration.DEFAULT_RECOVERY_TYPE);

        return val;
    }

    /**
    /**
     * This method returns Whether Dynamic Endpoint is enabled or not 
     * @returns String
     */
	 public Boolean getAllowDynamicEndpoint() {
	        String allowDynEndpt = mConfig.getProperty(CONFIG_ALLOW_DYNAMIC_ENDPOINT, "false");
	        return Boolean.valueOf(allowDynEndpt);
	 }

    /**
     * This method returns whether Connection Pooling is enabled or not
     * @returns Boolean
     */
	public Boolean getAllowConnectionPooling() {
        final String allowConnPool = mConfig.getProperty(CONFIG_ALLOW_CONNECTION_POOLING, "false");
        return Boolean.valueOf(allowConnPool);
    }

    /**
     * This method returns Connection Pool Minimum Size
     * @returns Integer
     */
	public Integer getConnectionPoolPrefSize() {
        final String prefSize = mConfig.getProperty(CONFIG_CONNPOOL_PREF_SIZE, RuntimeConfiguration.DEFAULT_PREF_POOL_SIZE);
        return Integer.valueOf(prefSize);
    }

    /**
     * This method returns Connection Pool Maximum Size
     * @returns Integer
     */
	public Integer getConnectionPoolMaxSize() {
        final String maxSize = mConfig.getProperty(CONFIG_CONNPOOL_MAX_SIZE, RuntimeConfiguration.DEFAULT_MAX_POOL_SIZE);
        return Integer.valueOf(maxSize);
    }

    /**
     * This method returns Connection Pool Maximum Idle Timeout 
     * @returns Integer
     */
	public Integer getConnectionMaxIdleTimeout() {
        final String maxIdleTime = mConfig.getProperty(CONFIG_CONNPOOL_MAX_IDLE_TIME, RuntimeConfiguration.DEFAULT_CONN_MAX_IDLE_TIMEOUT);
        return Integer.valueOf(maxIdleTime);
    }

    /**
     * This method returns Connection Pool Protocol
     * @returns String
     */
	public String getConnectionProtocol() {
        final String protocol = mConfig.getProperty(CONFIG_CONNPOOL_PROTOCOL, RuntimeConfiguration.DEFAULT_PROTOCOL);
        return protocol;
    }

    /**
     * This method returns Connection Pool Authentication 
     * @returns String
     */
	public String getConnectionAuthentication() {
        final String auth = mConfig.getProperty(CONFIG_CONNPOOL_AUTHENTICATION, RuntimeConfiguration.DEFAULT_AUTHENTICATION);
        return auth;
    }
	 
    /**
     *
     */
    public void setThreads(final Integer val)
        throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }

        final String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;

        try {
            newVal = val;
        } catch (final Exception ex) {
            throw new InvalidAttributeValueException(
                "Invalid argument for setting attribute " + attrName + " :" +
                ex.getMessage());
        }

        if ((newVal.intValue() < MIN_THREADS) ||
                (newVal.intValue() > MAX_THREADS)) {
            throw new InvalidAttributeValueException("A value of " + newVal +
                " is not valid for attribute " + attrName +
                ". The valid range is " + MIN_THREADS + " - " + MAX_THREADS);
        }

        String oldValue = getThreads().toString();
        
       mConfig.setProperty(CONFIG_THREADS, newVal.toString());
       saveAndNotifyListners("Threads_Attribute_changed", CONFIG_THREADS, oldValue, newVal.toString());
       persistConfiguration();

    }

	 /**
     *
     */
    public void setRetryCount(final Integer val)
        throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }

        final String attrName = CONFIG_MAX_RETRIES_COUNT;

        // Validate the attribute value
        Integer newVal = null;

        try {
            newVal = val;
        } catch (final Exception ex) {
            throw new InvalidAttributeValueException(
                "Invalid argument for setting attribute " + attrName + " :" +
                ex.getMessage());
        }

     
        String oldValue = getRetryCount().toString();
        
       mConfig.setProperty(CONFIG_MAX_RETRIES_COUNT, newVal.toString());
       saveAndNotifyListners("Retry_Count_Attribute_Changed", CONFIG_MAX_RETRIES_COUNT, oldValue, newVal.toString());
       persistConfiguration();

    }

	
	 /**
     *
     */
    public void setRetryInterval(final Integer val)
        throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }

        final String attrName = CNFIG_RETRY_INTERVAL;

        // Validate the attribute value
        Integer newVal = null;

        try {
            newVal = val;
        } catch (final Exception ex) {
            throw new InvalidAttributeValueException(
                "Invalid argument for setting attribute " + attrName + " :" +
                ex.getMessage());
        }

     
        String oldValue = getRetryInterval().toString();
        
       mConfig.setProperty(CNFIG_RETRY_INTERVAL, newVal.toString());
       saveAndNotifyListners("Retry_Interval_Attribute_Changed", CNFIG_RETRY_INTERVAL, oldValue, newVal.toString());
       persistConfiguration();

    }

	public void setRecoveryType(String  val)
        throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }

        final String attrName = CONFIG_RECOURCE_ACTION;

        // Validate the attribute value
        String newVal = null;

        try {
            newVal = val;
        } catch (final Exception ex) {
            throw new InvalidAttributeValueException(
                "Invalid argument for setting attribute " + attrName + " :" +
                ex.getMessage());
        }

     
        String oldValue = getRecoveryType();
        
       mConfig.setProperty(CONFIG_RECOURCE_ACTION, newVal);
       saveAndNotifyListners("Retry_Interval_Attribute_Changed", CONFIG_RECOURCE_ACTION, oldValue, newVal);
       persistConfiguration();

    }

    /**
	 * This method sets whether Dynamic Endpoint is enabled or not
     * @param Boolean
     */
    public void setAllowDynamicEndpoint(final Boolean dynEndpoint) throws InvalidAttributeValueException, MBeanException {
        if (dynEndpoint == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0210_AllowDynamicEndpointIsNull"));
        }

        final String attrName = CONFIG_ALLOW_DYNAMIC_ENDPOINT;

        // Validate the new Allow Dynamic Endpoint value
        Boolean newDynEndpoint = null;

        try {
        	newDynEndpoint = dynEndpoint;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        Boolean oldDynEndpoint = getAllowDynamicEndpoint();
        
        mConfig.setProperty(CONFIG_ALLOW_DYNAMIC_ENDPOINT, newDynEndpoint.toString());
        saveAndNotifyListners("Allow_Dynamic_Endpoint_Attribute_Changed", CONFIG_ALLOW_DYNAMIC_ENDPOINT, oldDynEndpoint, newDynEndpoint);
        persistConfiguration();
    }

    /**
	 * This method sets whether Connection Pooling is enabled or not
     * @param Boolean
     */
    public void setAllowConnectionPooling(final Boolean connPool) throws InvalidAttributeValueException, MBeanException {
        if (connPool == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0208_AllowConnPoolIsNull"));
        }

        final String attrName = CONFIG_ALLOW_CONNECTION_POOLING;

        // Validate the new Allow Connection Pooling value
        Boolean newConnPool = null;

        try {
        	newConnPool = connPool;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        Boolean oldConnPool = getAllowConnectionPooling();
        
        mConfig.setProperty(CONFIG_ALLOW_CONNECTION_POOLING, newConnPool.toString());
        saveAndNotifyListners("Allow_Conn_Pool_Attribute_Changed", CONFIG_ALLOW_CONNECTION_POOLING, oldConnPool, newConnPool);
        persistConfiguration();
    }

    /**
	 * This method sets Connection Pool Minimum Size
     * @param Integer
     */
    public void setConnectionPoolPrefSize(final Integer prefSize) throws InvalidAttributeValueException, MBeanException {
        if (prefSize == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0204_ConnPoolPrefSizeIsNull"));
        }

        final String attrName = CONFIG_CONNPOOL_PREF_SIZE;

        // Validate the new Connection Pool Min Size value
        Integer newPrefSize = null;

        try {
            newPrefSize = prefSize;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        String oldPrefSize = getConnectionPoolPrefSize().toString();
        
        mConfig.setProperty(CONFIG_CONNPOOL_PREF_SIZE, newPrefSize.toString());
        saveAndNotifyListners("Conn_Pool_Pref_Size_Attribute_Changed", CONFIG_CONNPOOL_PREF_SIZE, oldPrefSize, newPrefSize.toString());
        persistConfiguration();
    }

    /**
	 * This method sets Connection Pool Maximum Size
     * @param Integer
     */
    public void setConnectionPoolMaxSize(final Integer maxSize) throws InvalidAttributeValueException, MBeanException {
        if (maxSize == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0205_ConnPoolMaxSizeIsNull"));
        }

        final String attrName = CONFIG_CONNPOOL_MAX_SIZE;

        // Validate the new Connection Pool Max Size value
        Integer newMaxSize = null;

        try {
            newMaxSize = maxSize;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        String oldMaxSize = getConnectionPoolMaxSize().toString();
        
        mConfig.setProperty(CONFIG_CONNPOOL_MAX_SIZE, newMaxSize.toString());
        saveAndNotifyListners("Conn_Pool_Max_Size_Attribute_Changed", CONFIG_CONNPOOL_MAX_SIZE, oldMaxSize, newMaxSize.toString());
        persistConfiguration();
    }

    /**
	 * This method sets Connection Pool Maximum Idle Timeout
     * @param Integer
     */
    public void setConnectionMaxIdleTimeout(final Integer maxIdleTime) throws InvalidAttributeValueException, MBeanException {
        if (maxIdleTime == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0207_ConnPoolMaxIdleTimeoutIsNull"));
        }

        final String attrName = CONFIG_CONNPOOL_MAX_IDLE_TIME;

        // Validate the new Connection Pool Max Idle Timeout value
        Integer newMaxIdleTime = null;

        try {
            newMaxIdleTime = maxIdleTime;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        String oldMaxIdleTime = getConnectionMaxIdleTimeout().toString();
        
        mConfig.setProperty(CONFIG_CONNPOOL_MAX_IDLE_TIME, newMaxIdleTime.toString());
        saveAndNotifyListners("Conn_Pool_Max_Idle_Timeout_Attribute_Changed", CONFIG_CONNPOOL_MAX_IDLE_TIME, oldMaxIdleTime, newMaxIdleTime.toString());
        persistConfiguration();
    }

    /**
	 * This method sets Connection Pool Protocol
     * @param String
     */
    public void setConnectionProtocol(final String protocol) throws InvalidAttributeValueException, MBeanException {
        if (protocol == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0208_ConnPoolProtocolIsNull"));
        }

        final String attrName = CONFIG_CONNPOOL_PROTOCOL;

        // Validate the new Connection Protocol value
        String newProtocol = null;

        try {
            newProtocol = protocol;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        String oldProtocol = getConnectionProtocol();
        
        mConfig.setProperty(CONFIG_CONNPOOL_PROTOCOL, newProtocol);
        saveAndNotifyListners("Conn_Pool_Protocol_Attribute_Changed", CONFIG_CONNPOOL_PROTOCOL, oldProtocol, newProtocol);
        persistConfiguration();
    }

    /**
	 * This method sets Connection Pool Authentication
     * @param String
     */
    public void setConnectionAuthentication(final String auth) throws InvalidAttributeValueException, MBeanException {
        if (auth == null) {
            throw new InvalidAttributeValueException( mMessages.getString("LDAPBC_ACFG0209_ConnPoolAuthenticationIsNull"));
        }

        final String attrName = CONFIG_CONNPOOL_AUTHENTICATION;

        // Validate the new Connection Authentication value
        String newAuth = null;

        try {
            newAuth = auth;
        } catch (final Exception ex) {
			throw new InvalidAttributeValueException(
								mMessages.getString("LDAPBC_ACFG0206_Invalid_Attribute_Value") + attrName + " :" +
								ex.getMessage());
        }

        String oldAuth = getConnectionAuthentication();
        
        mConfig.setProperty(CONFIG_CONNPOOL_AUTHENTICATION, newAuth);
        saveAndNotifyListners("Conn_Pool_Authentication_Attribute_Changed", CONFIG_CONNPOOL_AUTHENTICATION, oldAuth, newAuth);
        persistConfiguration();
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
                        "LDAPBC_ACFG0006_AppVarNameAlreadyExists",
                        name)));
            }

            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0007_AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }
        
            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0008_AppVarMissingField",
                                new Object[] {name, APP_VAR_NAME})));
            }
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0012_AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }

            String appVarValue = (String) appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0011_AppVarValueNull",
                                new Object[] {name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String) appVar.get(APP_VAR_TYPE);
            if (appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0011_AppVarValueNull",
                                new Object[] {name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "LDAPBC_ACFG0112_AppVarAdded",
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
                                "LDAPBC_ACFG0010_AppVarExistenceFailedUpdate",
                                name)));
            }
        
            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0007_AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }

            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0008_AppVarMissingField",
                                new Object[]{name, APP_VAR_NAME})));
            } 
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0012_AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }
        
            String appVarValue = (String)appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0011_AppVarValueNull",
                                new Object[]{name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String)appVar.get(APP_VAR_TYPE);
            if ( appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        mMessages.getString(
                                "LDAPBC_ACFG0011_AppVarValueNull",
                                new Object[]{name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "LDAPBC_ACFG0114_AppVarUpdated",
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
                        "LDAPBC_ACFG0009_AppVarExistenceFailedDelete", name)));
            }
            mAppVarMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "LDAPBC_ACFG0113_AppVarDeleted", name));
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
                            "LDAPBC_ACFG0013_AppVarTabularCreateFailed"), e);
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
     *             LDAPBC/packaging source tree)
     *
     * @param appConfig Application configuration composite
     *
     * @throws javax.management.MBeanException if the application configuration
     *         cannot be added.
     *
     * @see jbi.xml under LDAPBC/packaging source tree
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
                                "LDAPBC_ACFG0000_AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                        mMessages.getString("LDAPBC_ACFG0001_AppCfgNameAlreadyExists",
                                name)));
            }
            
            addAppCfg(name, appConfig);
            
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString(
                        "LDAPBC_ACFG0100_AppCfgAdded", new Object[]{name}));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, "LDAPBC_ACFG0102_AppCfgPersistFailed", e);
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
                    "LDAPBC_ACFG0002_AppCfgRowSizeInvalid",
                    new Object[]{name, APP_CONFIG_ROW_SIZE, rowType.keySet().size()})));
        }

        // Validate fields
        Collection<ApplicationConfigurationField> appConfig =
                new LinkedList<ApplicationConfigurationField>();
        for (ApplicationConfigurationField appcfgField: APP_CONFIG_FIELDS) {
            if (!config.containsKey(appcfgField.fieldId)) {
                throw new MBeanException(new InvalidAttributeValueException(mMessages.getString(
                        "LDAPBC_ACFG0003_AppCfgRowMissingField",
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
                                    "LDAPBC_ACFG0000_AppCfgValidationFailed",
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
                                "LDAPBC_ACFG0000_AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (!mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                        mMessages.getString("LDAPBC_ACFG0004_AppCfgExistenceFailedDelete",
                                name)));
            }
        
            // Remove application configuration entry
            mAppConfigMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, mMessages.getString("LDAPBC_ACFG0103_AppCfgDeleted", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, "LDAPBC_ACFG0102_AppCfgPersistFailed", e);
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
                                "LDAPBC_ACFG0000_AppCfgValidationFailed",
                                new Object[]{name, name, validationResult[1]})));
            }
        }
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new IllegalArgumentException(
                    mMessages.getString("LDAPBC_ACFG0005_AppCfgExistenceFailedUpdate",
                            name)));
        }
        
        addAppCfg(name, appConfig);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString(
                    "LDAPBC_ACFG0104_AppCfgUpdated", new Object[]{name}));
        }
        
        // Persist configuration
        try {
            persistAppConfigObjects();
        } catch (IOException e) {
            mLogger.log(Level.WARNING, "LDAPBC_ACFG0102_AppCfgPersistFailed", e);
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
                            "LDAPBC_ACFG0105_AppCfgTabularDataError"), e);
                }

            }
        }
        return tabularData;
    }
    /**
     *
     * @throws MBeanException
     */
    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration        
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (final JBIException ex) {
            throw new MBeanException(ex,
                "Failed to persist configuration to " + mWorkspaceRoot + ": " +
                ex.getMessage());
        }
    }

    /**
     *
     * @return
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(new String[] {
                    AttributeChangeNotification.ATTRIBUTE_CHANGE
                }, AttributeChangeNotification.class.getName(),
                "Attribute changed")
        };
    }

    /**
     *
     * @param listener
     * @param filter
     * @param handback
     */
    public void addNotificationListener(final NotificationListener listener,
        final NotificationFilter filter, final Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /**
     *
     * @param listener
     * @throws ListenerNotFoundException
     */
    public void removeNotificationListener(final NotificationListener listener)
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    /**
     *
     * @param listener
     * @param filter
     * @param handback
     * @throws ListenerNotFoundException
     */
    public void removeNotificationListener(final NotificationListener listener,
        final NotificationFilter filter, final Object handback)
        throws ListenerNotFoundException {
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
            throw new MBeanException(ex, mMessages.getString(
                    "LDAPBC_ACFG0111_AppVarPersistWriteFailed",
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
                        // Not a good way to check fields.  Should add interface for pwd field or check the type.
                        if (field instanceof AppConfigKeystorePwdField || field instanceof AppConfigTrustStorePwdField ||
                            field instanceof AppConfigCredentialField) {
                            try {
                                String val = (String)field.getValue();
                                if (!val.equals("")) {
                                    writer.write(field.fieldId + "=" + mKeySupport.encrypt(val));
                                } else {
                                    writer.write(field.fieldId + "=" + val);
                                }
                            } catch (Exception ex) {
                                mLogger.log(Level.WARNING,
                                            mMessages.getString("LDAPBC_ACFG0118_AppCFGPersistEncryptFailed",
                                                                field.fieldId));
                            }
                        } else {
                            writer.write(field.fieldId + "=" + field.getValue());
                        }
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
                                    "LDAPBC_ACFG0101_AppCfgFileCloseFailed",
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
                if (type.equals("PASSWORD")) {
                    if (mKeySupport != null) {
                        try {
                            value = mKeySupport.decrypt(value);
                        } catch (Exception e1) {
                            throw new MBeanException(e1, mMessages.getString(
                                    "LDAPBC_ACFG0115_AppVarLoadDecryptFailed",
                                    name
                            ));
                        }
                    }
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (IOException ex) {
            throw new MBeanException(ex, mMessages.getString(
                    "LDAPBC_ACFG0106_AppVarPersistLoadFailed",
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
                String name = readStanzaHead(reader,line);
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
                                "LDAPBC_ACFG0110_AppCfgPersistLoadOverride",
                                name));
                    }
                    mLogger.info(mMessages.getString(
                            "LDAPBC_ACFG0109_AppCfgPersistLoaded", acoName));
                    
                    // Was already scanning an ACO, finish up
                    if (acoName != null) {
                        reader.reset();
                        //fields.clear();
                        acoName = null;
                    }
                }
            }
        } catch (IOException e) {
            throw new MBeanException(e, mMessages.getString(
                    "LDAPBC_ACFG0107_AppCfgPersistLoadFailed",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        try {
            reader.close();
        } catch (IOException e) {
            throw new MBeanException(e, mMessages.getString(
                    "LDAPBC_ACFG0101_AppCfgFileCloseFailed",
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
    private String readStanzaHead(BufferedReader reader, String line) throws IOException {
        String ret = null;
        String lineRead = line;
        do {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Looking for stanza start");
            }
            
            if (lineRead != null) {
                
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer("Read line: " + lineRead);
                }
                
                Matcher matcher = cStanzaHeadPattern.matcher(lineRead);
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
                            "LDAPBC_ACFG0108_AppCfgPersistLoadIgnored", lineRead));
                }
                
            }
            lineRead = reader.readLine();
        } while (lineRead != null);
        
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
                            if (f instanceof AppConfigKeystorePwdField || f instanceof AppConfigTrustStorePwdField ||
                                f instanceof AppConfigCredentialField) {
                                try {
                                    if (value.equals("")) {
                                        f.fromString(value);
                                    } else {
                                        f.fromString(mKeySupport.decrypt(value));
                                    }
                                } catch (Exception ex) {
                                    mLogger.log(Level.WARNING,
                                                mMessages.getString("LDAPBC_ACFG0117_AppCFGLoadDecryptFailed",
                                                                    f.fieldId));
                                }
                            } else {
                                f.fromString(value);
                            }
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
                                "LDAPBC_ACFG0108_AppCfgPersistLoadIgnored", line));
                    }
                }
            }
        } while (line != null);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("Stanza complete, " + fields.size() + " fields read");
        }
    }
    private void saveAndNotifyListners(String msgId, String attrName, Object oldVal, Object newVal) {
        // Notify listeners of this change
        long seqNo = 0;
        String msg = mMessages.getString(msgId);
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

}
