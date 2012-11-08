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
 * @(#)ETLSERuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.etl.mbean;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.NotificationBroadcaster;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularType;
import javax.jbi.JBIException;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.internationalization.Messages;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;

/**
 * @author Sujit Biswas
 * 
 */
public class ETLSERuntimeConfiguration implements ETLSERuntimeConfigurationMBean,
        NotificationBroadcaster {

    // add support for i18n
    // private static final Messages MESSAGES =
    // Messages.getMessages(ETLSERuntimeConfiguration.class);
    private static transient final Logger mLogger = Logger.getLogger(ETLSERuntimeConfiguration.class.getName());
    private static final Messages mMessages = Messages.getMessages(ETLSERuntimeConfiguration.class);
    private static final String MAX_THREAD_COUNT = "MaxThreadCount";
    static final String AXION_DB_WORKING_DIR = "AxiondbWorkingDirectory";
    static final String AXION_DB_DATA_DIR = "AxiondbDataDirectory";
    private static final String EXTERNAL_CON_PROVIDER = "ExternalConnectionProvider";
    private static final Integer DEFAULT_MAX_THREAD_COUNT = Integer.valueOf(10);
    
    
    NotificationBroadcasterSupport notificationBroadcaster = new NotificationBroadcasterSupport();
    String propFile;
    private Integer maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
    String axiondbWorkingDir = System.getProperty("user.home");
    String axiondbDataDirectory = System.getProperty("user.home");
    private String externalProviderClassName = "com.sun.etl.engine.utils.RuntimeConnectionProvider";
    
    private String mConfigurationDisplaySchema;
    private String mConfigurationDisplayData;
    private KeyStoreUtilClient mKeyStoreUtil;
    private static final String PERSIST_ENVVAR_CONFIG_FILE_NAME = "EnvVarConfig.properties";
    //private static final Messages mMessages = Messages.getMessages(ETLSERuntimeConfiguration.class);
    Map mEnvVarMap;
    Map endPointAppConfigMap;
    String mWorkspaceRoot;
    String mConfigSchema;
    String mConfigData;
    Properties mConfig;
    
    private static final String NON_XA_DS_JNDI_NAME = "NonXADsJndiName";
    private static final String XA_DS_JNDI_NAME = "XADsJndiName";
    
    private static final String PERSISTENCE_DB_DRIVER_CLASS = "PersistenceDBDriverClass";
    private static final String PERSISTENCE_DB_URL = "PersistenceDBUrl";
    private static final String PERSISTENCE_DB_USER = "PersistenceDBUser";
    private static final String PERSISTENCE_DB_PASSWORD = "PersistenceDBPassword";
    private static final String RETRY_MAX_COUNT = "RetryMaxCount";
    private static final String RETRY_MAX_INTERVAL = "RetryMaxInterval";
    

    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
    // Application configuration row fields
    private static final String APPLICATION_CONFIG_ROW_KEY = "configurationName";
    private static final String APPLICATION_CONFIG_PROPERTY_URL = "AppDataRoot";
    // Global application configurations
    private Map mAppVarMap;
    private Map mAppConfigMap;
    private CompositeType mAppVarRowType = null;
    private CompositeType mAppConfigRowType = null;
    private TabularType mAppVarTabularType = null;
    private TabularType mAppConfigTabularType = null;

    // Appliation configurations and application variables
    public static final String CONFIG_APPLICATON_VARIABLES = "ApplicationVariables";
    public static final String CONFIG_APPLICATION_CONFIGURATIONS = "ApplicationConfigurations";
    
    private static final String[] AttributeNames = new String[] { APPLICATION_CONFIG_ROW_KEY,"ProjectName","CollaborationName", "DBWorkingDir", "DynamicFlatFile", "DataDir", "SourceConnection1" , "SourceConnection2", "SourceConnection3", 
                "SourceConnection4", "SourceConnection5", "TargetConnection1","TargetConnection2", "TargetConnection3"};

    public String setConfigurationDisplayData() {
        return mConfigurationDisplayData;
    }

    public void setMConfigurationDisplayData(String mConfigurationDisplayData) {
        this.mConfigurationDisplayData = mConfigurationDisplayData;
    }

    public String retrieveConfigurationDisplaySchema() {
        return mConfigurationDisplaySchema;
    }

    public void setConfigurationDisplaySchema(String mConfigurationDisplaySchema) {
        this.mConfigurationDisplaySchema = mConfigurationDisplaySchema;
    }

 
    public ETLSERuntimeConfiguration(String propFile, String workspaceRoot, KeyStoreUtilClient aKeyStoreUtil, String configSchema, String configData) throws JBIException, NotCompliantMBeanException {
        this.propFile = propFile;
        mWorkspaceRoot = workspaceRoot;
        mKeyStoreUtil = aKeyStoreUtil;
        // Load the persisted configuration
        mConfig = ConfigPersistence.loadConfig(workspaceRoot);
        mEnvVarMap = loadEnvironmentVariableConfig(workspaceRoot);
        mConfigSchema = configSchema;
        mConfigData = configData;
        try {
        mAppVarMap = loadApplicationVariablesConfig(workspaceRoot);
        mAppConfigMap = loadApplicationConfiguration(workspaceRoot);
        mLogger.info(mMessages.getString("ETLSE-I0168.restored_AppConfig")); 
        mAppConfigRowType = createApplicationConfigurationCompositeType();
        mAppConfigTabularType = createApplicationConfigurationTabularType();
//        mAppVarRowType = createApplicationVariableCompositeType();
        mAppVarTabularType = createApplicationVariableTabularType();
        endPointAppConfigMap = new HashMap();
        }catch(Exception ex) {
            ex.printStackTrace();
        }
        restore(this.propFile);
    }

    public String getMaxThreadCount() throws InvalidAttributeValueException, MBeanException {
        return maxThreadCount.toString();
    }

    public void setMaxThreadCount(String count) throws InvalidAttributeValueException,
            MBeanException {
        String newValue = count;
        String oldValue = getMaxThreadCount();

        try {
            maxThreadCount = Integer.valueOf(Integer.parseInt(newValue.trim()));
            if (maxThreadCount.intValue() < 1) {
                maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
            }
        } catch (NumberFormatException e) {
            mLogger.log(Level.SEVERE,e.getMessage(), e);
            maxThreadCount = DEFAULT_MAX_THREAD_COUNT;
            e.printStackTrace();
        }

        //save(propFile);
        persistAndNotify(this.MAX_THREAD_COUNT, String.class.getName(), newValue, oldValue);

        long seqNo = 0;
        String msg = "ETLSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, MAX_THREAD_COUNT, attrType, oldValue, newValue);
        notificationBroadcaster.sendNotification(notif);

    }

    /**
     * @return the axiondbDataDirectory
     */
    public String getExternalConnectionProvider() {
        return externalProviderClassName;
    }

    /**
     * @param axiondbDataDirectory
     *            the axiondbDataDirectory to set
     */
    public void setExternalConnectionProvider(String externalProviderClassName) throws InvalidAttributeValueException, MBeanException {

        String newValue = externalProviderClassName;
        String oldValue = this.getExternalConnectionProvider();

        this.externalProviderClassName = externalProviderClassName;

        //save(propFile);
        persistAndNotify(ETLSERuntimeConfiguration.EXTERNAL_CON_PROVIDER, String.class.getName(), newValue, oldValue);

        long seqNo = 0;
        String msg = "MashupSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg,EXTERNAL_CON_PROVIDER , attrType, oldValue, newValue);
        notificationBroadcaster.sendNotification(notif);
    }
    

    public boolean restore(String propFile) {
        File fin = new File(propFile);

        try {
            FileInputStream fis = new FileInputStream(fin);
            Properties prop = new Properties();
            prop.load(fis);
            
            mLogger.fine(mMessages.getString("ETLSE-F0309.properties_after_loading")+ prop);

            setMaxThreadCount(prop.getProperty(MAX_THREAD_COUNT));

           
            if( prop.getProperty(AXION_DB_WORKING_DIR) != null) {
                setAxiondbWorkingDirectory(prop.getProperty(AXION_DB_WORKING_DIR));
            }

           
            if (prop.getProperty(AXION_DB_DATA_DIR) != null) {
                setAxiondbDataDirectory(prop.getProperty(AXION_DB_DATA_DIR));
            }
            
            mLogger.fine(mMessages.getString("ETLSE-F0310.dataDirectory_is_set")+ getAxiondbDataDirectory());
             if (prop.getProperty(EXTERNAL_CON_PROVIDER) == null) {
                            prop.setProperty(EXTERNAL_CON_PROVIDER, this.externalProviderClassName);
                        }
                        this.setExternalConnectionProvider(prop.getProperty(EXTERNAL_CON_PROVIDER));
            
          
            
            if(prop.getProperty(XA_DS_JNDI_NAME) != null) {
                this.setXaDSJndiName(prop.getProperty(XA_DS_JNDI_NAME));
            }
            
             if(prop.getProperty(NON_XA_DS_JNDI_NAME) != null) {
                this.setNonXaDSJndiName(prop.getProperty(NON_XA_DS_JNDI_NAME));
            }
            
            String retryVar = prop.getProperty(RETRY_MAX_COUNT);
            retryVar = (retryVar == null || "".equalsIgnoreCase(retryVar)) ? "0": retryVar;
            this.setRetryMaxCount(Integer.parseInt(retryVar));
            
            String interval = prop.getProperty(RETRY_MAX_INTERVAL);
            interval = (interval == null || "".equalsIgnoreCase(interval)) ? "0":interval;
            
            this.setRetryMaxInterval(Integer.parseInt(interval));
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, ex.getMessage(), ex);
            return false;
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param propFile
     *            DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean save(String propFile) {
        File fin = new File(propFile);
        Properties prop = new Properties();

        try {
            FileInputStream fis = new FileInputStream(fin);
            prop.load(fis);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE,ex.getMessage(), ex);
            return false;
        }

        try {
            prop.setProperty(MAX_THREAD_COUNT, getMaxThreadCount());
            if (getAxiondbWorkingDirectory() != null) {
                prop.setProperty(AXION_DB_WORKING_DIR, getAxiondbWorkingDirectory());
            }
            prop.setProperty(AXION_DB_DATA_DIR, getAxiondbDataDirectory());
            prop.setProperty(NON_XA_DS_JNDI_NAME, this.getNonXaDSJndiName());
            prop.setProperty(XA_DS_JNDI_NAME, this.getXaDSJndiName());
            prop.setProperty(RETRY_MAX_COUNT, String.valueOf(this.getRetryMaxCount()));
        prop.setProperty(EXTERNAL_CON_PROVIDER,getExternalConnectionProvider());

            prop.setProperty(RETRY_MAX_INTERVAL, String.valueOf(this.getRetryMaxInterval()));
         
        } catch (Exception e) {
            mLogger.log(Level.SEVERE,e.getMessage(), e);
            e.printStackTrace();
        }

        File fout = new File(propFile);

        try {
            FileOutputStream fos = new FileOutputStream(fout);
            prop.store(fos, "etlse.properties");
        } catch (FileNotFoundException ex) {
            mLogger.log(Level.SEVERE,ex.getMessage(), ex);
            return false;
        } catch (IOException ex) {
            mLogger.log(Level.SEVERE,ex.getMessage(), ex);
            return false;
        }

        return true;
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter,
            Object handback) throws IllegalArgumentException {
        notificationBroadcaster.addNotificationListener(listener, filter, handback);

    }

    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[]{new MBeanNotificationInfo(
                    new String[]{AttributeChangeNotification.ATTRIBUTE_CHANGE                    },
                    AttributeChangeNotification.class.getName(),
                    "ETLSERuntimeConfiguration.Attribute_changed")
                };
    }

    public void removeNotificationListener(NotificationListener listener)
            throws ListenerNotFoundException {
        notificationBroadcaster.removeNotificationListener(listener);

    }

    /**
     * @return the axiondbInstanceName
     */
    public String getAxiondbDataDirectory() {
        return axiondbDataDirectory;
    }

    /**
     * @param axiondbInstanceName
     *            the axiondbInstanceName to set
     */
    public void setAxiondbDataDirectory(String axiondbDataDirectory) throws InvalidAttributeValueException, MBeanException {
        boolean isValid = true;
        if (axiondbDataDirectory == null || "".equalsIgnoreCase(axiondbDataDirectory)) {
            isValid = false;
        }
        if (isValid) {
            File f = new File(axiondbDataDirectory);
            if (!f.exists()) {
                isValid = false;
            }
        }
        if (!isValid) {
             mLogger.log(Level.CONFIG,mMessages.getString("ETLSE-E0414.inValid_DataDirectory"));
        }else {
        String newValue = axiondbDataDirectory;
        String oldValue = getAxiondbDataDirectory();

        this.axiondbDataDirectory = axiondbDataDirectory;

        //save(propFile);
        persistAndNotify(ETLSERuntimeConfiguration.AXION_DB_DATA_DIR, String.class.getName(), newValue, oldValue);

        long seqNo = 0;
        String msg = "ETLSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, AXION_DB_DATA_DIR, attrType, oldValue, newValue);
        notificationBroadcaster.sendNotification(notif);
    }}

    /**
     * @return the axiondbWorkingDir
     */
    public String getAxiondbWorkingDirectory() {
        
        return axiondbWorkingDir;
    }

    /**
     * @param axiondbWorkingDir
     *            the axiondbWorkingDir to set
     */
    public void setAxiondbWorkingDirectory(String axiondbWorkingDir) throws InvalidAttributeValueException, MBeanException {
        boolean isValid = true;
        if (axiondbWorkingDir == null || "".equalsIgnoreCase(axiondbWorkingDir)) {
            isValid = false;
        }
        if (isValid) {
            File f = new File(axiondbWorkingDir);
            if (!f.exists()) {
                isValid = false;
            }
        }
        if (!isValid) {
            mLogger.log(Level.CONFIG,mMessages.getString("ETLSE-E0414.inValid_DataDirectory"));
        }else {
        String newValue = axiondbWorkingDir;
        String oldValue = getAxiondbWorkingDirectory();

        this.axiondbWorkingDir = axiondbWorkingDir;

        //save(propFile);
        persistAndNotify(ETLSERuntimeConfiguration.AXION_DB_WORKING_DIR, String.class.getName(), newValue, oldValue);
        long seqNo = 0;
        String msg = "ETLSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, AXION_DB_WORKING_DIR, attrType, oldValue, newValue);
        notificationBroadcaster.sendNotification(notif);

    }}

    public String getNonXaDSJndiName() {
        return mConfig.getProperty(this.NON_XA_DS_JNDI_NAME);
    }
    
    public void setNonXaDSJndiName(String val) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getNonXaDSJndiName();
        persistAndNotify(NON_XA_DS_JNDI_NAME, String.class.getName(), val, oldValue );
        long seqNo = 0;
        String msg = "ETLSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, NON_XA_DS_JNDI_NAME, attrType, val, oldValue);
        notificationBroadcaster.sendNotification(notif);
    }
    
     public String getXaDSJndiName() {
        return mConfig.getProperty(this.XA_DS_JNDI_NAME);
    }
    
    public void setXaDSJndiName(String val) throws InvalidAttributeValueException, MBeanException {
        String oldValue = getXaDSJndiName();
        persistAndNotify(XA_DS_JNDI_NAME, String.class.getName(), val, oldValue );
        long seqNo = 0;
        String msg = "ETLSERuntimeConfiguration.Attribute_changed";
        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, XA_DS_JNDI_NAME, attrType, val, oldValue);
        notificationBroadcaster.sendNotification(notif);
    }
    
    
    
    public int getRetryMaxCount() {
        String count = this.mConfig.getProperty(RETRY_MAX_COUNT, "");
        if( count == null || "".equalsIgnoreCase(count)) return 10;
        return Integer.parseInt(count);
    }
    
    public void setRetryMaxCount(int count) throws InvalidAttributeValueException, MBeanException {
        try {
            String oldVal = String.valueOf(this.getRetryMaxCount());
            persistAndNotify(RETRY_MAX_COUNT, String.class.getName(), String.valueOf(count), oldVal);
            long seqNo = 0;
            String msg = "ETLSERuntimeConfiguration.Attribute_changed";
            String attrType = String.class.getName();
            Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, RETRY_MAX_COUNT, attrType, String.valueOf(count), oldVal);
            notificationBroadcaster.sendNotification(notif);
        } catch(Exception ex) {
            throw new MBeanException(ex);
        }
    }
    
    public int getRetryMaxInterval() {
        String interval = this.mConfig.getProperty(RETRY_MAX_INTERVAL, "");
        if(interval == null || "".equalsIgnoreCase(interval)) return 10000;
        return Integer.parseInt(interval);
    }
    
    public void setRetryMaxInterval(int interval) throws InvalidAttributeValueException, MBeanException {
        try {
            persistAndNotify(RETRY_MAX_INTERVAL, String.class.getName(), String.valueOf(interval), String.valueOf(getRetryMaxInterval()));
             long seqNo = 0;
            String msg = "ETLSERuntimeConfiguration.Attribute_changed";
            String attrType = String.class.getName();
            Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), msg, RETRY_MAX_INTERVAL, attrType, String.valueOf(interval), String.valueOf(getRetryMaxInterval()));
            notificationBroadcaster.sendNotification(notif);
        } catch (Exception ex) {
            throw new MBeanException(ex);
        }
    }
    
    
    
    
    
    

    public Map retrieveEnvVariablesMap() {
        return mEnvVarMap;
    }

    public void updateEnvVariablesMap(Map envVarMap) throws MBeanException {
        mEnvVarMap = envVarMap;
        persistEnvVariableConfiguration();
    }

    void persistEnvVariableConfiguration() throws MBeanException {
        // Persist the changed configuration
        PrintWriter writer = null;
        try {
            File envVarPersistFileName = new File(mWorkspaceRoot, PERSIST_ENVVAR_CONFIG_FILE_NAME);
            writer = new PrintWriter(envVarPersistFileName);
            String key = null;
            String[] varDesc = null;
            String value = null;
            String type = null;
            for (Iterator iter = mEnvVarMap.keySet().iterator(); iter.hasNext();) {
                key = (String) iter.next();
                varDesc = (String[]) mEnvVarMap.get(key);
                if (varDesc != null && varDesc.length == 2) {
                    value = varDesc[0];
                    type = varDesc[1];
                    if (type != null && type.equals("PASSWORD")) {
                        if (value == null) {
                            value = "";
                        }
                        value = mKeyStoreUtil.encrypt(value);
                        writer.println(key + "=" + value + "{" + type + "}");
                    } else {
                        if (value != null) {
                            writer.println(key + "=" + value + "{" + (type != null ? type : "STRING") + "}");
                        } else {
                            writer.println(key + "={" + (type != null ? type : "STRING") + "}");
                        }
                    }
                } else {
                    // e.g. - not defined but referenced in WSDL
                    // assume STRING type, no value yet
                    writer.println(key + "={STRING}");
                }
            }
            writer.close();
        } catch (IOException ex) {
            //throw new MBeanException(ex, mMessages.getString("ETLBC-E001045.RTC_Failed_persist_env_var",
            // new Object[]{mWorkspaceRoot, ex.getMessage()}));
        } catch (Exception ee) {
            // throw new MBeanException(ee, mMessages.getString("ETLBC-E001045.RTC_Failed_persist_env_var",
            //    new Object[]{mWorkspaceRoot, ee.getMessage()}));
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e) {
                    //
                }
            }
        }
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
                            value = mKeyStoreUtil.decrypt(value);
                        }
                        envVarMap.put(key, new String[]{value, type});
                    } else {
                        // malformed line
                    }
                } else {
                    // invalid line - skip
                }
            }
            reader.close();

        } catch (Exception ex) {
            //throw new JBIException(mMessages.getString("ETLBC-E001046.RTC_Failed_loading_env_vars_exception", new Object[]{envVarPersistFileName, ex}));
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                    //
                }
            }
        }

        return envVarMap;
    }

    private void persistAndNotify(String attrName, String attrType, Object newVal, Object oldVal)
            throws InvalidAttributeValueException, MBeanException {
        if (attrName == null) {
            // throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001047.RTC_Attr_set_null_property_name"));
        }
        if (newVal == null) {
            // throw new InvalidAttributeValueException(mMessages.getString("FTPBC-E001048.RTC_Attr_set_null_property_value", attrName));
        }
        mConfig.put(attrName, newVal.toString());
        persistConfiguration();
        Notification notif = new AttributeChangeNotification(this,
                0,
                System.currentTimeMillis(),
                "",//mMessages.getString(
                // break to avoid processing by logging tool
                //"FTPBC-C001999.RTC_Attr_changed"),
                attrName,
                attrType,
                oldVal,
                newVal);
        notificationBroadcaster.sendNotification(notif);
    }

    void persistConfiguration() throws MBeanException {
        // Persist the changed configuration
        try {
              ConfigPersistence.persistConfig(mWorkspaceRoot, mConfig);
        } catch (Exception ex) {
            //throw new MBeanException(ex, mMessages.getString("FTPBC-E001041.RTC_Failed_persist", new Object[]{mWorkspaceRoot, ex.getMessage()}));
        }
    }

    public String retrieveConfigurationDisplayData() {
        return mConfigData;
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
            throw new MBeanException(new Exception("Sun-etl-engine.Application_variable_name_already_exists" + name));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException("Sun-etl-engine.Invalid_Item_Size_for_app_variable :" +  rowType.keySet().size());
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException("Sun-etl-engine.Invalid_key_for_composite_data_for_app_variable" + name);
        }

        String appVarValue = (String) appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String) appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException("Sun-etl-engine.Invalid_app_variable_composite_data_no_value_field" + name);
        }

        if (appVarType == null) {
            throw new InvalidAttributeValueException("Sun-etl-engine.Invalid_app_variable_composite_data_no_type_field" + name);
        }

        mAppVarMap.put(name, new String[]{appVarValue, appVarType                });
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, "Sun-etl-engine.New_application_variable_added", new Object[]{name, appVarValue});
        }
        persistApplicationVariablesConfig();
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
//            throw new MBeanException(new Exception(mMessages.getString("INF-E01211.Application_config_name_already_exists", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 7) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01212.Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01213.Invalid_key_for_composite_data_for_app_config", name));
        } 
        
        dump(new StringBuffer());
        mAppConfigMap.put(name, appConfig);
        
        if (mLogger.isLoggable(Level.CONFIG)) {
//            mLogger.log(Level.CONFIG, mMessages.getString("INF-C01205.New_application_configuration_added", new Object[] { name, appConfigValue }));
        }
        persistApplicationConfigurationObjects();
        
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
                    value = mKeyStoreUtil.encrypt(value);
                }
                String prop = (value != null) ? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
                os.write(prop.getBytes());
            }
            os.close();
        } catch (Exception ex) {
            ex.printStackTrace();
            //throw new MBeanException(ex, mMessages.getString("INF-E01207.Failed_to_persist_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
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
     
         /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException {
//        if (!mAppVarMap.containsKey(name)) {
//            throw new MBeanException(new Exception(mMessages.getString("INF-E01224.Application_variable_does_not_exist_for_set", name)));
//        }
//        
//        CompositeType rowType = appVar.getCompositeType();
//        if (rowType.keySet().size() != 3) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01219.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
//        }
//        
//        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01220.Invalid_key_for_composite_data_for_app_variable", name));
//        } 
//        
//        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
//        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
//        
//        if (appVarValue == null) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01221.Invalid_app_variable_composite_data_no_value_field", name));
//        }
//        
//        if ( appVarType == null) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01222.Invalid_app_variable_composite_data_no_type_field", name));
//        }
//        
//        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
//        if (mLogger.isLoggable(Level.CONFIG)) {
//            mLogger.log(Level.CONFIG, mMessages.getString("INF-C01202.Application_variable_updated", new Object[] { name, appVarValue }));
//        }
    
        // persist the application variable properties
        persistApplicationVariablesConfig();
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
//            throw new MBeanException(new Exception(mMessages.getString("INF-E01216.Application_configuration_does_not_exist_for_set", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 7) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01212.Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
//            throw new InvalidAttributeValueException(mMessages.getString("INF-E01213.Invalid_key_for_composite_data_for_app_config", name));
        } 
        
        mAppConfigMap.put(name, appConfig);
        dump(new StringBuffer());
        if (mLogger.isLoggable(Level.CONFIG)) {
//            mLogger.log(Level.CONFIG, mMessages.getString("INF-C01207.Application_configuration_updated", new Object[] { name, appConfigValue }));
        }
        persistApplicationConfigurationObjects();
    }
    
    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
     public void deleteApplicationVariable(String name) throws MBeanException {
//        if (!mAppVarMap.containsKey(name)) {
//            throw new MBeanException(new Exception(mMessages.getString("INF-E01223.Application_variable_does_not_exist_for_delete", name)));
//        }
        
        mAppVarMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
//            mLogger.log(Level.CONFIG, mMessages.getString("INF-C01204.Application_variable_deleted", name));
        }
        persistApplicationVariablesConfig();
    }
     
          /**
      * Delete an application configuration. 
      *
      * @param name - identification of the application configuration to be deleted
      * @throws MBeanException if the configuration cannot be deleted.
      */
    public void deleteApplicationConfiguration(String name) throws MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
//            throw new MBeanException(new Exception(mMessages.getString("INF-E01215.Application_configuration_does_not_exist_for_delete", name)));
        }
        
        mAppConfigMap.remove(name);
        if (mLogger.isLoggable(Level.CONFIG)) {
//            mLogger.log(Level.CONFIG, mMessages.getString("INF-C01206.Application_configuration_deleted", name));
        }
        persistApplicationConfigurationObjects();
    }
    
     public TabularData getApplicationVariables() {
         try {
             if (mAppVarTabularType == null) {
                 mAppVarTabularType = this.createApplicationVariableTabularType();
             }
         } catch (OpenDataException e) {
             e.printStackTrace();
         }

    	TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            Object[] data = ("PASSWORD".equals(type))? new Object[] { name, "*******", type } : new Object[] {name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                                                                 new String[] { APPLICATION_VARIABLES_ROW_KEY , 
                                                                                APPLICATION_VARIABLES_VALUE_FIELD , 
                                                                                APPLICATION_VARIABLES_TYPE_FIELD },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                e.printStackTrace();
//                throw new RuntimeException(mMessages.getString("INF-E01225.Unable_to_construct_composite_data_for_app_variable"), e);
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
    
        private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration  

        try {
            //DBConnectionFactory.setApplicationConfigMap(mAppConfigMap);
            File appConfigPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            Properties configProperties = new Properties();
            configProperties.load(new FileInputStream(appConfigPersistFileName));
            String[] appConfigRowAttrNames = AttributeNames;

            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext();) {
                String key = (String) iter.next();
                CompositeData value = (CompositeData) mAppConfigMap.get(key);
                for (int i = 0; i < appConfigRowAttrNames.length; i++) {
                    String propKey = (i == 0) ? key : key + "." + appConfigRowAttrNames[i];
                    Object entry = value.get(appConfigRowAttrNames[i]);
                    if( entry instanceof String) {
                        configProperties.put(propKey, entry);
                    } else if( entry instanceof Boolean) {
                        Boolean flag = (Boolean)entry;
                        configProperties.put(propKey,String.valueOf(flag));
                    }
                }

            }
            configProperties.store(os,"ApplicationConfiguration");
            os.close();
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new MBeanException(ex, "INF-E01229.Failed_to_persist_application_configurations"); 
        }
    }
        
    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a 
     *         component keyed by the configuration name. 
     */
    public TabularData getApplicationConfigurations() {
        TabularData tabularData = new TabularDataSupport(mAppConfigTabularType);
        if( mAppConfigMap == null ) return tabularData;
        for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            CompositeData value = (CompositeData) mAppConfigMap.get(name);
           
            
            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                                                                 AttributeNames,
                                                                 value.getAll(AttributeNames));
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                e.printStackTrace();
                throw new RuntimeException(mMessages.getString("ETLSE-E0416.Unable_to_construct_composite_data_for_app_config"));
            }
        }
        
        return tabularData;
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
                                             new String[] { APPLICATION_VARIABLES_ROW_KEY } );
        
        return mAppVarTabularType;
    }
    
    private CompositeType createApplicationConfigurationCompositeType() throws OpenDataException {
    	if (mAppConfigRowType != null) {
    	    return mAppConfigRowType;
    	}
    	
        String[] appConfigRowAttrNames = AttributeNames;
        String[] appConfigAttrDesc = { "Application Configuration Name","ProjectName", "ColloborationName", "AxiondbWorkingDirectory", "DynamicFlatFile", "AxiondbDataDirectory", "SourceConnection1", "SourceConnection2","SourceConnection3", "SourceConnection4", "SourceConnection5", 
                                "TargetConnection1", "TargetConnection2", "TargetConnection3"};
        OpenType[] appConfigAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, SimpleType.STRING , SimpleType.STRING, SimpleType.STRING, SimpleType.STRING ,SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, 
                                SimpleType.STRING, SimpleType.STRING, SimpleType.STRING};
        
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
                                                new String[] { APPLICATION_CONFIG_ROW_KEY });
       
       return mAppConfigTabularType;
    }
    
    
        private CompositeType createApplicationVariableCompositeType() throws OpenDataException {
    	if (mAppVarRowType != null) {
    	    return mAppVarRowType;
    	}
    	
        String[] appVarRowAttrNames = { APPLICATION_VARIABLES_ROW_KEY, APPLICATION_VARIABLES_VALUE_FIELD, APPLICATION_VARIABLES_TYPE_FIELD };
        String[] appVarRowAttrDesc = { "Application Variable Name", "Application Variable Value", "Application Variable Type" };
        OpenType[] appVarRowAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING };
        
        mAppVarRowType = new CompositeType("ApplicationVariables",
                                           "Application Variable Composite Data",
                                           appVarRowAttrNames,
                                           appVarRowAttrDesc,
                                           appVarRowAttrTypes);
        
        return mAppVarRowType;
    }
        
    private Map loadApplicationConfiguration(String workspaceRoot) throws MBeanException {

        Properties persistedConfig = new Properties();
 
        List configNames = new ArrayList();
        List row = new ArrayList();
         CompositeData rowData = null;

         
         
        

        File appConfigPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        if (!appConfigPersistFileName.exists()) {
            return this.mAppConfigMap;
        }
        String[] configAttrs = AttributeNames;
        try {
            if (mAppConfigRowType == null) {
            mAppConfigRowType = createApplicationConfigurationCompositeType();
           }
            InputStream is = new FileInputStream(appConfigPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                if(name.indexOf(".") == -1 ) {
                    configNames.add(name);
                }
            }
            
            for(int i=0;i<configNames.size();i++) {
                
                row.clear();
                String configName = (String) configNames.get(i);

              
                for (int j = 0; j < configAttrs.length; j++) {
                    String key = (j == 0) ? configName : configName + "." + configAttrs[j];
                    String value = persistedConfig.getProperty(key);
                    OpenType type = mAppConfigRowType.getType(key);
                                 row.add(value);
                   
                }
                rowData = new CompositeDataSupport(mAppConfigRowType,configAttrs,row.toArray());
                if( this.mAppConfigMap == null ) {
                    this.mAppConfigMap = new HashMap();
                }
                this.mAppConfigMap.put(configName, rowData);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            //throw new MBeanException(ex, mMessages.getString("INF-E01230.Failed_to_load_application_configurations", PERSIST_APPLICATION_CONFIG_FILE_NAME));
        }
        
        return this.mAppConfigMap;
    }
    
        private Map loadApplicationVariablesConfig(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map appVarMap = new HashMap();
        
        File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!appVarPersistFileName.exists()) {
            return appVarMap;
        }
        
        try {
            InputStream is = new FileInputStream(appVarPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String metadata = persistedConfig.getProperty(name);
                int startIndex = metadata.indexOf("{");
                String value = (startIndex == 0)? null : metadata.substring(0, startIndex);
                String type = metadata.substring(startIndex + 1, metadata.length() -1);
                if (type.equals("PASSWORD")) {
                    value = mKeyStoreUtil.decrypt(value);
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (Exception ex) {
            ex.printStackTrace();
//            throw new MBeanException(ex, mMessages.getString("INF-E01208.Failed_to_load_application_variables", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
        
        return appVarMap;
    }
    
        
    public void setAppConfigEndPointMap(Map endPointMap) {
        this.endPointAppConfigMap = endPointMap;
    }
    
    public Map retrieveAppConfigEndPointMap() {
        return this.endPointAppConfigMap;
    }
    
        
    public void dump(StringBuffer msgBuf) {
       if( msgBuf == null ) msgBuf = new StringBuffer();
       if(mAppConfigMap == null) mAppConfigMap =new HashMap();
      
        msgBuf.append(CONFIG_APPLICATION_CONFIGURATIONS).append(": { ");
        for (Iterator it = mAppConfigMap.keySet().iterator(); it.hasNext(); ) {
            String name = (String)it.next();
            msgBuf.append('[').append(name).append(',');
            CompositeData value = (CompositeData) mAppConfigMap.get(name);
            msgBuf.append(value.toString()).append(']');
        }
        msgBuf.append(" }");
        
        mLogger.log(Level.INFO, msgBuf.toString());
    }
}
