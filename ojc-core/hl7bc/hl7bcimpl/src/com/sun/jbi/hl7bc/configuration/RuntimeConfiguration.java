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

package com.sun.jbi.hl7bc.configuration;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
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

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.hl7bc.I18n;

/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 * 
 * @author Nageswara rao
 */
public class RuntimeConfiguration implements RuntimeConfigurationMBean, NotificationEmitter {

    private static final Logger mLogger = Logger.getLogger(RuntimeConfiguration.class.getName());
    
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
    private static final ApplicationConfigurationField APP_CONFIG_HOSTNAME;
    private static final ApplicationConfigurationField APP_CONFIG_PORT;
    private static final ApplicationConfigurationField APP_CONFIG_VALIDATE_MSH;
    private static final ApplicationConfigurationField APP_CONFIG_ACK_MODE;
    private static final ApplicationConfigurationField APP_CONFIG_LLP_TYPE;
    private static final ApplicationConfigurationField APP_CONFIG_START_BLOCK_CHAR;
    private static final ApplicationConfigurationField APP_CONFIG_END_BLOCK_CHAR;
    private static final ApplicationConfigurationField APP_CONFIG_END_DATA_CHAR;
    private static final ApplicationConfigurationField APP_CONFIG_HLLP_CHECKSUM;
    private static final ApplicationConfigurationField APP_CONFIG_MLLPV2_RETRIES_COUNT;
    private static final ApplicationConfigurationField APP_CONFIG_MLLPV2_RETRY_INTERVAL;
    private static final ApplicationConfigurationField APP_CONFIG_MLLPV2_TIME_TO_WAIT_FOR_ACKNAK;
    private static final ApplicationConfigurationField APP_CONFIG_SEQUENCE_NUMBER_ENABLED;
    private static final ApplicationConfigurationField APP_CONFIG_PROCESSING_ID;
    private static final ApplicationConfigurationField APP_CONFIG_VERSION_ID;
    private static final ApplicationConfigurationField APP_CONFIG_FIELD_SEPARATOR;
    private static final ApplicationConfigurationField APP_CONFIG_ENCODING_CHARS;
    private static final ApplicationConfigurationField APP_CONFIG_SENDING_APPLICATION;
    private static final ApplicationConfigurationField APP_CONFIG_SENDING_FACILITY;
    private static final ApplicationConfigurationField APP_CONFIG_SFT_ENABLED;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_VENDOR_ORG;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_CERTIFIED_VERSION;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_PROD_NAME;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_BINARY_ID;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_PROD_INFO;
    private static final ApplicationConfigurationField APP_CONFIG_SOFT_INSTALL_DATE;
    private static final ApplicationConfigurationField APP_CONFIG_JOURNALLING_MESSAGE;
    private static final ApplicationConfigurationField APP_CONFIG_TCP_ROLE;
    
    private static final ApplicationConfigurationField[] APP_CONFIG_FIELDS = {
            APP_CONFIG_NAME = new AppConfigNameField(),
            APP_CONFIG_HOSTNAME = new AppConfigrHostNameField(),
            APP_CONFIG_PORT = new AppConfigPortField(),
            APP_CONFIG_VALIDATE_MSH = new AppConfigValidateMSHField(),
            APP_CONFIG_ACK_MODE = new AppConfigAckModeField(),
            APP_CONFIG_LLP_TYPE = new AppConfigLLPTypeField(),
            APP_CONFIG_START_BLOCK_CHAR = new AppConfigSBCharField(),
            APP_CONFIG_END_BLOCK_CHAR = new AppConfigEBCharField(),
            APP_CONFIG_END_DATA_CHAR = new AppConfigEDCharField(),
            APP_CONFIG_HLLP_CHECKSUM = new AppConfigHLLPChecksumField(),
            APP_CONFIG_MLLPV2_RETRIES_COUNT = new AppConfigMLLPv2RetriesOnNakField(),
            APP_CONFIG_MLLPV2_RETRY_INTERVAL = new AppConfigMLLPV2RetryInterval(),
            APP_CONFIG_MLLPV2_TIME_TO_WAIT_FOR_ACKNAK = new AppConfigMLLPV2TimeToWaitForAckNak(),
            APP_CONFIG_SEQUENCE_NUMBER_ENABLED = new AppConfigSeqNumEnabledField(),
            APP_CONFIG_PROCESSING_ID = new AppConfigProcessingIdField(),
            APP_CONFIG_VERSION_ID = new AppConfigVersionIdField(),
            APP_CONFIG_FIELD_SEPARATOR = new AppConfigFieldSeparator(),
            APP_CONFIG_ENCODING_CHARS = new AppConfigEncodingCharactersField(),
            APP_CONFIG_SENDING_APPLICATION = new AppConfigSendingApplicationField(),
            APP_CONFIG_SENDING_FACILITY = new AppConfigSendingFacilityField(),
            APP_CONFIG_SFT_ENABLED = new AppConfigSFTEnabledField(),
            APP_CONFIG_SOFT_VENDOR_ORG = new AppConfigSoftVendorOrgField(),
            APP_CONFIG_SOFT_CERTIFIED_VERSION = new AppConfigSoftVersionOrReleaseNumField(),
            APP_CONFIG_SOFT_PROD_NAME = new AppConfigSoftProductNameField(),
            APP_CONFIG_SOFT_BINARY_ID = new AppConfigSoftBinaryId(),
            APP_CONFIG_SOFT_PROD_INFO = new AppConfigSoftProdInfoField(),
            APP_CONFIG_SOFT_INSTALL_DATE = new AppConfigSoftInstallDateField(),
			APP_CONFIG_JOURNALLING_MESSAGE = new AppConfigJournallingField(),
			APP_CONFIG_TCP_ROLE = new AppConfigTcpRoleField(),
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
    // Attribute names
    public static final String CONFIG_INBOUNDREPLY_THREADS = "InboundReplyThreads";
    
    public static final String CONFIG_ALLOW_DYN_EP = "AllowDynamicEndpoint";
    public static final String CONFIG_CREATESNEWCONNECTION_NAME = "AlwaysCreatesNewConnection";
    public static final String CONFIG_DEFAULT_MAX_TIME_TO_WAIT_FOR_A_RESPONSE = "DefaultMaxTimeToWaitForAResponse";

    public static final String CONFIG_POOL_MIN_SZ = "ConnectionPoolMinSize";

    public static final String CONFIG_POOL_MAX_SZ = "ConnectionPoolMaxSize";

    public static final String CONFIG_CONN_MAX_IDLE_TIMEOUT = "ConnectionMaxIdleTimeout";

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "16";

   /** Database JNDI Name setting when there is no setting defined in jbi.xml */
    private static final String DB_JNDI_NAME_FACTORYDEFAULT = "jdbc/__default"; //$NON-NLS-1$

   /** Database JNDI Name property tag string pattern */
    String DB_JNDI_NAME = "DatabaseJNDIName"; //$NON-NLS-1$

    // Configuration validation settings
    long MIN_THREADS = 5;

    long MAX_THREADS = 2147483647;

    // Configuration
    private Properties mConfig;
    private String mWorkspaceRoot;
    
// Application Configuration Object store
    private Map<String, Collection<ApplicationConfigurationField>> mAppConfigMap;
    
    // Application Variables store
    private Map<String, String[]> mAppVarMap;
   
    // Use delegation to support notification
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
    
    static {
        try {
            // Call order is important
            APPCONFIG_ROW_TYPE = createApplicationConfigurationCompositeType();
            APPCONFIG_TABULAR_TYPE = createApplicationConfigurationTabularType();
            APPVAR_ROW_TYPE = createApplicationVariableCompositeType();
            APPVAR_TABULAR_TYPE = createApplicationVariableTabularType();
        } catch (OpenDataException e) {
			throw new RuntimeException(I18n.msg("E0102: Failed to create RuntimeConfigurationMBean, metadata initialization failed due to internal inconsistency"), e);
        }
    }

    /** Creates a new instance of RuntimeConfiguration */
    public RuntimeConfiguration(String workspaceRoot) throws JBIException {
        mWorkspaceRoot = workspaceRoot;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        try {
            mAppConfigMap = loadApplicationConfigurations(workspaceRoot);
            mAppVarMap = loadApplicationVariables(workspaceRoot);
        } catch (MBeanException e) {
			throw new JBIException(I18n.msg("E0103: Failed to create RuntimeConfigurationMBean, persisted configuration failed to load."), e);
        }
    }

    public Integer getThreads() {
        String val = mConfig.getProperty(CONFIG_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }

    public Integer getInboundReplyThreads() {
        String val = mConfig.getProperty(CONFIG_INBOUNDREPLY_THREADS, DEFAULT_THREADS);
        return Integer.valueOf(val);
    }
   
    /**
     * getter for JNDI name Does not allow for null value settings. Instead returns FACTORYDEFAULT
     * value.
     * 
     * @return val JNDI name
     */
    public String getDatabaseJNDIName() {
		String val = mConfig.getProperty(DB_JNDI_NAME, DB_JNDI_NAME_FACTORYDEFAULT);
        return val;
    }
    
    public Boolean getAllowDynamicEndpoint() {
        String val = mConfig.getProperty(CONFIG_ALLOW_DYN_EP, "false");
        return Boolean.valueOf(val);
    }

    public Integer getDefaultMaxTimeToWaitForAResponse() {
        String val = mConfig.getProperty(CONFIG_DEFAULT_MAX_TIME_TO_WAIT_FOR_A_RESPONSE, "-1");
        return Integer.valueOf(val);
    }
    
    public void setDefaultMaxTimeToWaitForAResponse(Integer theValue) throws MBeanException {
        if (theValue < -1) {
            theValue = -1;
        }
        String oldValue = getThreads().toString();
        
        String newValue = Integer.toString(theValue);
        mConfig.setProperty(CONFIG_THREADS, newValue);
        saveAndNotifyListners("C0118: DefaultMaxTimeToWaitForAResponse Attribute is changed", CONFIG_DEFAULT_MAX_TIME_TO_WAIT_FOR_A_RESPONSE, oldValue, newValue);
        persistConfiguration();
    }
    
    public Boolean isAlwaysCreatesNewConnEnabled() {
        return getAlwaysCreatesNewConnection();
    }
    
    public Boolean getAlwaysCreatesNewConnection() {
        String val = mConfig.getProperty(CONFIG_CREATESNEWCONNECTION_NAME, "true");
        return Boolean.valueOf(val);
    }

    public Integer getConnectionPoolMinSize() {
        String val = mConfig.getProperty(CONFIG_POOL_MIN_SZ, "0");
        return Integer.valueOf(val);
    }


    public Integer getConnectionPoolMaxSize() {
        String val = mConfig.getProperty(CONFIG_POOL_MAX_SZ, "1");
        return Integer.valueOf(val);
    }


    public Integer getConnectionMaxIdleTimeout() {
        String val = mConfig.getProperty(CONFIG_CONN_MAX_IDLE_TIMEOUT, "1000");
        return Integer.valueOf(val);
    }

    /**
     * setter for number of Threads
     * 
     * @param val number of threads
     */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }
        String attrName = CONFIG_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {			
			throw new InvalidAttributeValueException(I18n.msg("E0104: Invalid argument for setting attribute {0} : {1}",attrName, ex.getMessage()));
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(I18n.msg("E0105: A value of {0} is not valid for attribute {1}. The valid range is {2} - {3}", newVal,
                    attrName, new Long(MIN_THREADS), new Long(MAX_THREADS) ));
        }

        String oldValue = getThreads().toString();
        
       mConfig.setProperty(CONFIG_THREADS, newVal.toString());
       saveAndNotifyListners("C0106: Threads Attribute is changed", CONFIG_THREADS, oldValue, newVal.toString());
       persistConfiguration();

    }

    /**
     * setter for number of Threads
     * 
     * @param val number of threads
     */
    public void setInboundReplyThreads(Integer val) throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Value is null");
        }
        String attrName = CONFIG_INBOUNDREPLY_THREADS;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {			
			throw new InvalidAttributeValueException(I18n.msg("E0104: Invalid argument for setting attribute {0} : {1}",attrName, ex.getMessage()));
        }
        if (newVal.intValue() < MIN_THREADS || newVal.intValue() > MAX_THREADS) {
            throw new InvalidAttributeValueException(I18n.msg("E0105: A value of {0} is not valid for attribute {1}. The valid range is {2} - {3}", newVal,
                    attrName, new Long(MIN_THREADS), new Long(MAX_THREADS) ));
        }

        String oldValue = getInboundReplyThreads().toString();
        
       mConfig.setProperty(CONFIG_INBOUNDREPLY_THREADS, newVal.toString());
       saveAndNotifyListners("C0106: Threads Attribute is changed", CONFIG_INBOUNDREPLY_THREADS, oldValue, newVal.toString());
       persistConfiguration();

    }

    /**
     * setter for JNDI name Does not allow for null value settings. Instead returns FACTORYDEFAULT
     * value.
     * 
     * @param jndiname jndi name
     */
    public void setDatabaseJNDIName(String flag) throws InvalidAttributeValueException, MBeanException {
        if (flag == null) {
            flag = DB_JNDI_NAME_FACTORYDEFAULT;
        }

        String oldValue = getDatabaseJNDIName();
        // Apply and save the changes
        mConfig.setProperty(DB_JNDI_NAME, flag);
        saveAndNotifyListners("C0107: DB_JNDIName Attribute is changed", DB_JNDI_NAME, oldValue, flag);
        persistConfiguration();
        
    }
    
    public void setAllowDynamicEndpoint(Boolean val) throws InvalidAttributeValueException, MBeanException {
        if(val == null) {
            val = Boolean.getBoolean("false");
        }
        String oldValue = getAllowDynamicEndpoint().toString();
        // Apply and save the changes
        mConfig.setProperty(CONFIG_ALLOW_DYN_EP, val.toString());
        saveAndNotifyListners("C0108: AllowDynamicEndpoint Attribute is changed", CONFIG_ALLOW_DYN_EP, oldValue, val.toString());
        persistConfiguration();
    }
    
    public void setAlwaysCreatesNewConnection(Boolean val) throws InvalidAttributeValueException, MBeanException {
        if(val == null) {
            val = Boolean.getBoolean("true");
        }
        String oldValue = getAlwaysCreatesNewConnection().toString();
        // Apply and save the changes
        mConfig.setProperty(CONFIG_CREATESNEWCONNECTION_NAME, val.toString());
        saveAndNotifyListners("C0109: AlwaysCreatesNewConnection Attribute is changed", CONFIG_CREATESNEWCONNECTION_NAME, oldValue, val.toString());
        persistConfiguration();
    }

    /**
     * setter for connection poll min size value
     * 
     * @param val size
     */
    public void setConnectionPoolMinSize(Integer val) throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Connection poll min size value is null");
        }
        String attrName = CONFIG_POOL_MIN_SZ;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {            
            throw new InvalidAttributeValueException(I18n.msg("E0104: Invalid argument for setting attribute {0} : {1}",attrName, ex.getMessage()));
        }

        String oldValue = getConnectionPoolMinSize().toString();
        
       mConfig.setProperty(CONFIG_POOL_MIN_SZ, newVal.toString());
       saveAndNotifyListners("C0110: Connection poll min size value Attribute is changed", CONFIG_POOL_MIN_SZ, oldValue, newVal.toString());
       persistConfiguration();

    }

    /**
     * setter for connection poll max size value
     * 
     * @param val size
     */
    public void setConnectionPoolMaxSize(Integer val) throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Connection poll max size value is null");
        }
        String attrName = CONFIG_POOL_MAX_SZ;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {            
            throw new InvalidAttributeValueException(I18n.msg("E0104: Invalid argument for setting attribute {0} : {1}",attrName, ex.getMessage()));
        }

        String oldValue = getConnectionPoolMaxSize().toString();
        
       mConfig.setProperty(CONFIG_POOL_MAX_SZ, newVal.toString());
       saveAndNotifyListners("C0111: Connection poll max size value Attribute is changed", CONFIG_POOL_MAX_SZ, oldValue, newVal.toString());
       persistConfiguration();

    }


    /**
     * setter for connection max idle timeout value
     * 
     * @param val size
     */
    public void setConnectionMaxIdleTimeout(Integer val) throws InvalidAttributeValueException, MBeanException {
        if (val == null) {
            throw new InvalidAttributeValueException("Connection max idle time out value is null");
        }
        String attrName = CONFIG_CONN_MAX_IDLE_TIMEOUT;

        // Validate the attribute value
        Integer newVal = null;
        try {
            newVal = val;
        } catch (Exception ex) {            
            throw new InvalidAttributeValueException(I18n.msg("E0104: Invalid argument for setting attribute {0} : {1}",attrName, ex.getMessage()));
        }

        String oldValue = getConnectionMaxIdleTimeout().toString();
        
       mConfig.setProperty(CONFIG_CONN_MAX_IDLE_TIMEOUT, newVal.toString());
       saveAndNotifyListners("C0112: Connection max idle timeout value Attribute is changed", CONFIG_CONN_MAX_IDLE_TIMEOUT, oldValue, newVal.toString());
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
     * This operation adds a new applicationvariable. If a variable already exists with the same
     * name as that specified then the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws javax.management.MBeanException if an error occurs in adding the application
     *             variables to the component.
     */
    public void addApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        
        synchronized (mAppVarMap) {
            if (mAppVarMap.containsKey(name)) {
                throw new MBeanException(new Exception(I18n.msg("E0106: Application Variable ''{0}'' already exists.  Choose a different name",name)));
				
            }

            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0107: Rejected Application Variable {0}, format not recognized (tuple size of {1} expected, got {2}).",
                                name, APP_VAR_COUNT,
                                        rowType.keySet().size())));
            }
        
            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0108: Rejected Application Variable {0}, format not recognized; field ''{1}'' missing.",
                                name, APP_VAR_NAME)));
            }
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0109: Rejected Application Variable {0}, name does not match the value of its name field, ''{1}''.",
                                name, appVarName)));
            }

            String appVarValue = (String) appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0110: Rejected Application Variable {0}, because field {1} is and cannot be null.",
                                name, APP_VAR_VALUE)));
            }
        
            String appVarType = (String) appVar.get(APP_VAR_TYPE);
            if (appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0110: Rejected Application Variable {0}, because field {1} is and cannot be null.",
                                name, APP_VAR_TYPE)));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "C0100: Application variable {0} (value: {1}) saved.",
                        name, appVarValue));
            }
            persistAppVarConfig();
        }
    }

    /**
     * This operation sets an application variable. If a variable does not exist with the same name,
     * its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws javax.management.MBeanException if one or more application variables cannot be
     *             deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar)
            throws MBeanException {
        
        synchronized (mAppVarMap) {
            if (!mAppVarMap.containsKey(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0111: Failed to update non-existent Application Variable {0}.",
                                name)));
            }
        
            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0107: Rejected Application Variable {0}, format not recognized (tuple size of {1} expected, got {2}).",
                                 name, APP_VAR_COUNT,
                                        rowType.keySet().size())));
            }

            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                         I18n.msg(
                                "E0108: Rejected Application Variable {0}, format not recognized; field ''{1}'' missing.",
                                 name, APP_VAR_NAME)));
            } 
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                       I18n.msg(
                                "E0109: Rejected Application Variable {0}, name does not match the value of its name field, ''{1}''.",
                                name, appVarName)));
            }
        
            String appVarValue = (String)appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0110: Rejected Application Variable {0}, because field {1} is and cannot be null.",
                                name, APP_VAR_VALUE)));
            }
        
            String appVarType = (String)appVar.get(APP_VAR_TYPE);
            if ( appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        I18n.msg(
                                "E0110: Rejected Application Variable {0}, because field {1} is and cannot be null.",
                                name, APP_VAR_TYPE)));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "C0101: Application variable {0} (new value: {1}) updated.",
                         name, appVarValue));
            }
            persistAppVarConfig();
        }
    }

    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     * 
     * @param name - name of the application variable
     * @throws javax.management.MBeanException on errors.
     */
    public void deleteApplicationVariable(String name) throws MBeanException {
        synchronized (mAppVarMap) {
            if (!mAppVarMap.containsKey(name)) {
                throw new MBeanException(new Exception(I18n.msg(
                        "E0112: Failed to delete non-existent Application Variable {0}.", name)));
            }
            mAppVarMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "C0102: Application variable {0} deleted.", name));
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
                            "E0113: Failed to create Tabular representation for application variables."), e);
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
     * @param name Configuration name, must match the value of the "name" attribute of a declared
     *            property (see jbi.xml in the hl7bc/packaging source tree)
     * @param appConfig Application configuration composite
     * @throws javax.management.MBeanException if the application configuration cannot be added.
     * @see jbi.xml under hl7bc/packaging source tree
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
                        I18n.msg(
                                "E0114: Failed validation for Application Configuration ''{0}'', field ''{1}'': {2}",
                                 name, name, validationResult[1])));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                        I18n.msg("E0115: Application Configuration ''{0}'' already exists. Choose a different name.",
                                name)));
            }
            
            addAppCfg(name, appConfig);
            
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG, I18n.msg(
                        "C0103: Application Configuration {0} saved.", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, I18n.msg("W0100: Failed to persist Application Configuration."), e);
            }
        }
    }

    /**
     * Inserts a new Application Configuration object into the internal ACO map.
     * 
     * @param name Identification of the Application Configuration object.
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
                    "E0116: Rejected Application Configuration {0}, format not recognized; expected {1} rows, got {2}.",
                    name, APP_CONFIG_ROW_SIZE, rowType.keySet().size())));
        }

        // Validate fields
        Collection<ApplicationConfigurationField> appConfig =
                new LinkedList<ApplicationConfigurationField>();
        for (ApplicationConfigurationField appcfgField: APP_CONFIG_FIELDS) {
            if (!config.containsKey(appcfgField.fieldId)) {
                throw new MBeanException(new InvalidAttributeValueException(I18n.msg(
                        "E0117: Rejected Application Configuration {0}, format not recognized; field {1} missing.",
                         name, appcfgField.fieldId)));
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
                            I18n.msg(
                                    "E0114: Failed validation for Application Configuration ''{0}'', field ''{1}'': {2}",
                                     name, appcfgField.fieldId, validation[1])));
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
     * @throws javax.management.MBeanException if the configuration cannot be deleted.
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
                        I18n.msg(
                                "E0114: Failed validation for Application Configuration ''{0}'', field ''{1}'': {2}",
                                 name, name, validationResult[1])));
            }
        }
        
        synchronized (mAppConfigMap) {
            if (!mAppConfigMap.containsKey(name)) {
                throw new MBeanException(new IllegalArgumentException(
                         I18n.msg("E0119: Failed to delete non-existent Application Configuration {0}.",
                                name)));
            }
        
            // Remove application configuration entry
            mAppConfigMap.remove(name);
            if (mLogger.isLoggable(Level.CONFIG)) {
                mLogger.log(Level.CONFIG,  I18n.msg("C0104: Application Configuration {0} deleted.", name));
            }
        
            // Persist configuration
            try {
                persistAppConfigObjects();
            } catch (IOException e) {
                mLogger.log(Level.WARNING, I18n.msg("W0100: Failed to persist Application Configuration."), e);
            }
        }
    }

    /**
     * Update a application configuration. The configuration name is a part of the CompositeData.
     * The itemName for the configuration name is "configurationName" and the type is
     * SimpleType.STRING
     * 
     * @param name - configuration name, must match the value of the field "configurationName" in
     *            the appConfig
     * @param appConfig - application configuration composite
     * @throws javax.management.MBeanException if there are errors encountered when updating the
     *             configuration.
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
                        I18n.msg(
                                "E0114: Failed validation for Application Configuration ''{0}'', field ''{1}'': {2}",
                                 name, name, validationResult[1])));
            }
        }
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new IllegalArgumentException(
                    I18n.msg("E0120: Failed to update non-existent Application Configuration {0}.",
                            name)));
        }
        
        addAppCfg(name, appConfig);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, I18n.msg(
                    "C0105: Application Configuration {0} updated.", name));
        }
        
        // Persist configuration
        try {
            persistAppConfigObjects();
        } catch (IOException e) {
            mLogger.log(Level.WARNING, I18n.msg("W0100: Failed to persist Application Configuration."), e);
        }
    }

    /**
     * Get a Map of all application configurations for the component.
     * 
     * @return a TabularData of all the application configurations for a component keyed by the
     *         configuration name.
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
                            "E0121: Failed to read application configuration tabular data."), e);
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
     * This method is used at construction time to initialize the type describing the composition of
     * an application configuration.
     * 
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
     * This method is used at construction time to initialize tabular type information for
     * application configuration objects. It's important that
     * {@link #createApplicationConfigurationCompositeType()} is called first before this method.
     * 
     * @throws OpenDataException
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
     * This method is used at construction time to initialize the type describing the composition of
     * an application variable.
     * 
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
     * This method is used at construction time to initialize tabular type information for
     * application variables. It's important that
     * {@link #createApplicationConfigurationCompositeType()} is called first before this method.
     * 
     * @throws OpenDataException
     */
    private static TabularType createApplicationVariableTabularType() throws OpenDataException {
        return new TabularType("ApplicationVariableList",
                "List of Application Variables",
                APPVAR_ROW_TYPE,
                new String[] { APP_VAR_NAME } );
    }

    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration
        try {
            ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (JBIException ex) {
            throw new MBeanException(ex, I18n.msg("E0122: Failed to persist configuration to {0}\\: {1}", mWorkspaceRoot,
                    ex.getMessage()));
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
                    String prop = (value != null)
                            ? key + "=" + value + "{" + type + "}\n"
                            : key + "={" + type + "}\n";
                    writer.write(prop);
                } 
                writer.flush();
                writer.close();
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, I18n.msg(
                    "E0123: Failed to persist application variables to {0}",
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
                                    "W0101: Failed to close application configuration store file {0}.",
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
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (IOException ex) {
            throw new MBeanException(ex, I18n.msg(
                    "E0124: Failed to load persisted application variables from {0}",
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
            //Collection<ApplicationConfigurationField> fields =
                   // new LinkedList<ApplicationConfigurationField>();
            
            while (true) {
            Collection<ApplicationConfigurationField> fields =
                    new LinkedList<ApplicationConfigurationField>();
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
                        mLogger.warning(I18n.msg(
                                "W0102: Pre-existing application configuration {0} overwritten during load of persisted data.",
                                name));
                    }
                    mLogger.info(I18n.msg(
                            "I0102: Application Configuration {0} loaded.", acoName));
                    
                    // Was already scanning an ACO, finish up
                    if (acoName != null) {
                        reader.reset();
                        //fields.clear();
                        acoName = null;
                    }
                }
            }
        } catch (IOException e) {
            throw new MBeanException(e, I18n.msg(
                    "E0125: Failed to load persisted application configuration from {0}",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        try {
            reader.close();
        } catch (IOException e) {
            throw new MBeanException(e, I18n.msg(
                    "W0101: Failed to close application configuration store file {0}.",
                    PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }

        return appConfigMap; 
    }
    
    // pattern to match '[' id ']'
    private static final Pattern cStanzaHeadPattern =
            Pattern.compile("^\\s*\\[\\s*(\\w+)\\s*\\]\\s*$");

    /**
     * Parses a line from the reader that follow the format '[' id ']', and returns the portion
     * denoted by <code>id</code>. <p/> Lines are read from the reader until a line that matches
     * the format is found, or until the end of the input is reached. Lines encountered that do not
     * match the format are discarded.
     * 
     * @return String corresponding to the value that is the subject of the pattern described above,
     *         or <code>null</code> if no matching lines are found.
     * @throws IOException if an I/O exception occurs
     */
    private String readStanzaHead(BufferedReader reader, String line) throws IOException {
        String ret = null;
        do {
            if (mLogger.isLoggable(Level.FINE)) {
                //mLogger.fine("Looking for stanza start");
            }
            
            if (line != null) {
                
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer("Read line: " + line);
                }
                
                Matcher matcher = cStanzaHeadPattern.matcher(line);
                if (matcher.matches()) {
                    ret = matcher.group(1);
                    assert (!"".equals(ret));
                    if (mLogger.isLoggable(Level.FINER)) {
                        mLogger.finer("Start: '" + ret + "'");
                    }
                    break;
                } else {
                    // Warn that content was skipped
                    mLogger.log(Level.WARNING, I18n.msg(
                            "W0103: While loading persisted application configuration, unexpected line ignored: {0}", line));
                }
            }
            line = reader.readLine();
        } while (line != null);
        
        return ret;
    }

    // pattern to match id '=' value
    private static final Pattern cStanzaTailPattern =
            Pattern.compile("^\\s*(\\w+)\\s*=\\s*(-?\\w*)\\s*$");
    
    /**
     * Parses lines from the input that follows the format id '=' value. For every id, finds a
     * matching field listed in {@link #APP_CONFIG_FIELDS}, then assigns the value to that field.
     * The objects in APP_CONFIG_FIELDS are not changed; copies of matching fields are used. <p/>
     * Lines are read from the input source until the beginning of a new stanza is detected. See
     * {@link #readStanzaHead(java.io.BufferedReader)}.
     * 
     * @param fields A collection to which are added the fields corresponding to the parsed lines.
     * @param reader Input source
     */
   /* private void readStanzaTail(Collection<ApplicationConfigurationField> fields,
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
                        mLogger.log(Level.WARNING, I18n.msg(
                                "W0103: While loading persisted application configuration, unexpected line ignored: {0}", line));
                    }
                }
            }
        } while (line != null);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("Stanza complete, " + fields.size() + " fields read");
        }
    }*/
    
    private void readStanzaTail(Collection<ApplicationConfigurationField> fields,
                                BufferedReader reader) throws IOException {

        String line;
        final int READAHEAD_LIMIT = 1000;

        assert (fields.isEmpty());
        int index = 0;
        do {
            if (mLogger.isLoggable(Level.FINE)) {
                //mLogger.fine("Looking for stanza tail");
            }
            
            reader.mark(READAHEAD_LIMIT);
            line = reader.readLine();
            if (line != null) {
                if (mLogger.isLoggable(Level.FINER)) {
                    mLogger.finer("Read line: " + line);
                }
                if ((index = line.indexOf("=")) > 0) {
                    String name = line.substring(0, index).trim();
                    String value = null;
                    if (index < line.length()) {
                        value = line.substring(index + 1);
                        value = value.trim();
                    }
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
                            mLogger.fine("New start found; read finished");
                        }
                        reader.reset();
                        line = null;
                    } else {
                        // Warn that content was skipped
                        mLogger.log(Level.WARNING, I18n.msg(
                                "W0103: While loading persisted application configuration, unexpected line ignored: {0}", line));
                    }
                }
            }
        } while (line != null);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine("HL7BC-I0190: "+fields.size() + " Application Configuration fields read");
        }
    }
    
    public void dump(StringBuffer msgBuf) {
        msgBuf.append(CONFIG_THREADS + " (max outbound threads)");
        msgBuf.append(": ").append(getThreads()).append('\n');
        msgBuf.append(CONFIG_INBOUNDREPLY_THREADS + " (max inboundReply threads)");
        msgBuf.append(": ").append(getInboundReplyThreads()).append('\n');
		msgBuf.append(DB_JNDI_NAME + " (Data Source Name)");
		msgBuf.append(": ").append(getDatabaseJNDIName()).append('\n');
        
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

    private void saveAndNotifyListners(String msgId, String attrName, String oldVal, String newVal) {
        // Notify listeners of this change
        long seqNo = 0;
        String msg = I18n.msg(msgId);
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

    public Properties getProperties() {
        return mConfig;
    }

}
