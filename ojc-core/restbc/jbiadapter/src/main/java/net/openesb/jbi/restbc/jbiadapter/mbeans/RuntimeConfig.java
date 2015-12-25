package net.openesb.jbi.restbc.jbiadapter.mbeans;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.AttributeChangeNotification;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanNotificationInfo;
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

import net.openesb.jbi.restbc.jbiadapter.I18n;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;

/**
 * RuntimeConfigImpl.java
 *
 * @author Edward Chou
 */
public class RuntimeConfig extends AbstractConfigMBean implements RuntimeConfigMBean, NotificationEmitter {
    
    /*
     * 201-230
     */
    private static final Logger logger = Logger.getLogger(RuntimeConfig.class.getName());
    
    private int nmrThreadPoolSize = 16;
    private int nmrMaxThreadPoolSize = 64;
    private int defaultHttpListenerPort = 9696;
    private int defaultHttpListenerThreads = 32;
    private int defaultHttpsListenerPort = 9697;
    private int defaultHttpsListenerThreads = 32;
    private String truststorePassword = "changeit";
    private String keystorePassword = "changeit";
    private boolean enableHostnameVerifier = false;
    private boolean enableClientAuth = false;
    
    private ComponentContext ctx;
    
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
    private static final String APPLICATION_CONFIG_PROPERTY_URL = "url";
    
    // Global application configurations
    private Map mAppVarMap;
    private Map mAppConfigMap;
    private CompositeType mAppVarRowType = null;
    private CompositeType mAppConfigRowType = null;
    private TabularType mAppVarTabularType = null;
    private TabularType mAppConfigTabularType = null;
    
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
    
    private KeyStoreUtilClient keyStoreUtilClient;
    
    public RuntimeConfig(ComponentContext ctx, ComponentConfig config) throws DeploymentException {
        super(ctx, config);
        
        this.ctx = ctx;
        
        try {
            keyStoreUtilClient = new KeyStoreUtilClient(ctx);
            mAppVarMap = loadApplicationVariablesConfig(ctx.getWorkspaceRoot());
            mAppConfigMap = loadApplicationConfiguration(ctx.getWorkspaceRoot());
            mAppConfigRowType = createApplicationConfigurationCompositeType();
            mAppConfigTabularType = createApplicationConfigurationTabularType();
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
        } catch (Exception e) {
            String msg = I18n.loc("RESTBC-7228: Failed during mbean initialization {0}", e);
            logger.severe(msg);
            throw new DeploymentException(msg, e);
        }
        
    }
    
    /**
     * @return the nmrThreadPoolSize
     */
    public Integer getNmrThreadPoolSize() {
        String strVal = getConfig().getProperty(NMR_THREAD_POOL_SIZE).getValue();
        try {
            nmrThreadPoolSize = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return nmrThreadPoolSize;
    }

    /**
     * @param nmrThreadPoolSize the nmrThreadPoolSize to set
     */
    public void setNmrThreadPoolSize(Integer nmrThreadPoolSize) {
        getConfig().getProperty(NMR_THREAD_POOL_SIZE).setValue(String.valueOf(nmrThreadPoolSize));
        persistCfg();
        this.nmrThreadPoolSize = nmrThreadPoolSize;
    }

    /**
     * @return the nmrMaxThreadPoolSize
     */
    public Integer getNmrMaxThreadPoolSize() {
        String strVal = getConfig().getProperty(NMR_MAX_THREAD_POOL_SIZE).getValue();
        try {
            nmrMaxThreadPoolSize = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return nmrMaxThreadPoolSize;
    }

    /**
     * @param nmrMaxThreadPoolSize the nmrMaxThreadPoolSize to set
     */
    public void setNmrMaxThreadPoolSize(Integer nmrMaxThreadPoolSize) {
        getConfig().getProperty(NMR_MAX_THREAD_POOL_SIZE).setValue(String.valueOf(nmrMaxThreadPoolSize));
        persistCfg();
        this.nmrMaxThreadPoolSize = nmrMaxThreadPoolSize;
    }

    /**
     * @return the defaultHttpListenerPort
     */
    public Integer getDefaultHttpListenerPort() {
        String strVal = getConfig().getProperty(DEFAULT_HTTP_LISTENER_PORT).getValue();
        try {
            defaultHttpListenerPort = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return defaultHttpListenerPort;
    }

    /**
     * @param defaultHttpListenerPort the defaultHttpListenerPort to set
     */
    public void setDefaultHttpListenerPort(Integer defaultHttpListenerPort) {
        getConfig().getProperty(DEFAULT_HTTP_LISTENER_PORT).setValue(String.valueOf(defaultHttpListenerPort));
        persistCfg();
        this.defaultHttpListenerPort = defaultHttpListenerPort;
    }

    /**
     * @return the defaultHttpListenerThreads
     */
    public Integer getDefaultHttpListenerThreads() {
        String strVal = getConfig().getProperty(DEFAULT_HTTP_LISTENER_THREADS).getValue();
        try {
            defaultHttpListenerThreads = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return defaultHttpListenerThreads;
    }

    /**
     * @param defaultHttpListenerThreads the defaultHttpListenerThreads to set
     */
    public void setDefaultHttpListenerThreads(Integer defaultHttpListenerThreads) {
        getConfig().getProperty(DEFAULT_HTTP_LISTENER_THREADS).setValue(String.valueOf(defaultHttpListenerThreads));
        persistCfg();
        this.defaultHttpListenerThreads = defaultHttpListenerThreads;
    }

    /**
     * @return the defaultHttpsListenerPort
     */
    public Integer getDefaultHttpsListenerPort() {
        String strVal = getConfig().getProperty(DEFAULT_HTTPS_LISTENER_PORT).getValue();
        try {
            defaultHttpsListenerPort = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return defaultHttpsListenerPort;
    }

    /**
     * @param defaultHttpsListenerPort the defaultHttpsListenerPort to set
     */
    public void setDefaultHttpsListenerPort(Integer defaultHttpsListenerPort) {
        getConfig().getProperty(DEFAULT_HTTPS_LISTENER_PORT).setValue(String.valueOf(defaultHttpsListenerPort));
        persistCfg();
        this.defaultHttpsListenerPort = defaultHttpsListenerPort;
    }

    /**
     * @return the defaultHttpsListenerThreads
     */
    public Integer getDefaultHttpsListenerThreads() {
        String strVal = getConfig().getProperty(DEFAULT_HTTPS_LISTENER_THREADS).getValue();
        try {
            defaultHttpsListenerThreads = Integer.valueOf(strVal).intValue();
        } catch (Exception e){
            // ignore
        }
        return defaultHttpsListenerThreads;
    }

    /**
     * @param defaultHttpsListenerThreads the defaultHttpsListenerThreads to set
     */
    public void setDefaultHttpsListenerThreads(Integer defaultHttpsListenerThreads) {
        getConfig().getProperty(DEFAULT_HTTPS_LISTENER_THREADS).setValue(String.valueOf(defaultHttpsListenerThreads));
        persistCfg();
        this.defaultHttpsListenerThreads = defaultHttpsListenerThreads;
    }

    /**
     * @return the truststorePassword
     */
    public String getTruststorePassword() {
        truststorePassword = getConfig().getProperty(TRUSTSTORE_PASSWORD).getValue();
        return truststorePassword;
    }

    /**
     * @param truststorePassword the truststorePassword to set
     */
    public void setTruststorePassword(String truststorePassword) {
        getConfig().getProperty(TRUSTSTORE_PASSWORD).setValue(truststorePassword);
        persistCfg();
        this.truststorePassword = truststorePassword;
    }

    /**
     * @return the enableHostnameVerifier
     */
    public Boolean isEnableHostnameVerifier() {
        String strVal = getConfig().getProperty(ENABLE_HOSTNAME_VERIFIER).getValue();
        try {
            enableHostnameVerifier = Boolean.valueOf(strVal).booleanValue();
        } catch (Exception e){
            // ignore
        }
        return enableHostnameVerifier;
    }

    /**
     * @param enableHostnameVerifier the enableHostnameVerifier to set
     */
    public void setEnableHostnameVerifier(Boolean enableHostnameVerifier) {
        getConfig().getProperty(ENABLE_HOSTNAME_VERIFIER).setValue(String.valueOf(enableHostnameVerifier));
        persistCfg();
        this.enableHostnameVerifier = enableHostnameVerifier;
    }
    
    @Override
    public Boolean isEnableClientAuth() {
        String strVal = getConfig().getProperty(ENABLE_CLIENT_AUTH).getValue();
        try {
            enableClientAuth = Boolean.valueOf(strVal);
        } catch (Exception e){
            // ignore
        }
        return enableClientAuth;
    }

    @Override
    public void setEnableClientAuth(Boolean enableClientAuth) {
        getConfig().getProperty(ENABLE_CLIENT_AUTH).setValue(String.valueOf(enableClientAuth));
        persistCfg();
        this.enableClientAuth = enableClientAuth;
    }

    /**
     * @return the keystorePassword
     */
    public String getKeystorePassword() {
        keystorePassword = getConfig().getProperty(KEYSTORE_PASSWORD).getValue();
        return keystorePassword;
    }
    
    /**
     * @param keystorePassword the keystorePassword to set
     */
    public void setKeystorePassword(String keystorePassword) {
        getConfig().getProperty(KEYSTORE_PASSWORD).setValue(keystorePassword);
        persistCfg();
        this.keystorePassword = keystorePassword;
    }


    private void persistCfg(){
        try {
            this.persistConfiguration();
        } catch (Exception e){
            String msg = I18n.loc("RESTBC-7521: Exception while persisting configuration changes.");
            logger.log(Level.SEVERE, msg, e);
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
      * Add an application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "name" in the namedConfig
      * @param appConfig - application configuration composite 
      * @throws MBeanException if the application configuration cannot be added.
      */
    public void addApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {
        if (mAppConfigMap.containsKey(name)) {
            String msg = I18n.loc("RESTBC-7201: Application Config already exists {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 2) {
            String msg = I18n.loc("RESTBC-7202: Invalid item size for Application Config {0} {1}", name, rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            String msg = I18n.loc("RESTBC-7203: Invalid key for composite data for Application Config {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        String appConfigValue = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_URL);
        if (appConfigValue == null) {
            String msg = I18n.loc("RESTBC-7221: Invalid app config composite data null url", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppConfigMap.put(name, appConfigValue);
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESTBC-1201.New Application Cconfiguration added {0}", name);
            logger.finest(msg);
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
            String msg = I18n.loc("RESTBC-7204: Application Configuration does not exist for delete {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        mAppConfigMap.remove(name);
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESTBC-1202: Application Configuration deleted {0}", name);
            logger.finest(msg);
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
            String msg = I18n.loc("RESTBC-7205: Application Configuration does not exist for set {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 2) {
            String msg = I18n.loc("RESTBC-7206: Invalid item size for app config {0} {1}", name, rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            String msg = I18n.loc("RESTBC-7207: Invalid key for composite data for app config", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        String appConfigValue = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_URL);
        if ( appConfigValue == null) {
            String msg = I18n.loc("RESTBC-7221: Invalid app config composite data null url", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppConfigMap.put(name, appConfigValue);
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESETBC-1203: Application Configuration updated {0}", name);
            logger.finest(msg);
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
        for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String value = (String) mAppConfigMap.get(name);
            Object[] data = new Object[] {name, value};
            
            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                                                                 new String[] { APPLICATION_CONFIG_ROW_KEY, APPLICATION_CONFIG_PROPERTY_URL },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                String msg = I18n.loc("RESTBC-7208: Unable to construct composite data for app config {0}", e);
                logger.severe(msg);
                throw new RuntimeException(msg);
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
            String msg = I18n.loc("RESTBC-7209: Application variable name already exists {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            String msg = I18n.loc("RESTBC-7210: Invalid item size for app variable {0}", rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            String msg = I18n.loc("RESTBC-7211: Invalid key for composite data for app variable {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            String msg = I18n.loc("RESTBC-7212: Invalid app variable composite data no value field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (appVarType == null) {
            String msg = I18n.loc("RESTBC-7213: Invalid app variable composite data no type field", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESTBC-1204: New application variable added {0} {1}", name, appVarValue);
            logger.finest(msg);
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
            String msg = I18n.loc("RESTBC-7214: Application variable does not exist for delete {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        mAppVarMap.remove(name);
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESTBC-1205: Application variable deleted {0}", name);
            logger.finest(msg);
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
            String msg = I18n.loc("RESTBC-7215: Application variable does not exist for set {0}", name);
            logger.severe(msg);
            throw new MBeanException(new Exception(msg));
        }
        
        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            String msg = I18n.loc("RESTBC-7216: Invalid item size for app variable", rowType.keySet().size());
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            String msg = I18n.loc("RESTBC-7211: Invalid key for composite data for app variable {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            String msg = I18n.loc("RESTBC-7212: Invalid app variable composite data no value field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        if (appVarType == null) {
            String msg = I18n.loc("RESTBC-7219: Invalid app variable composite data no type field {0}", name);
            logger.severe(msg);
            throw new InvalidAttributeValueException(msg);
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.loc("RESTBC-1206.Application variable updated {0} {1}", name, appVarValue );
            logger.finest(msg);
        }
    
        // persist the application variable properties
        persistApplicationVariablesConfig();
    }
    
    public TabularData getApplicationVariables() {
        TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            Object[] data = ("PASSWORD".equalsIgnoreCase(type) && value != null && !"".equals(value))? 
                            new Object[] { name, "*******", type } : new Object[] {name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                                                                 new String[] { APPLICATION_VARIABLES_ROW_KEY , 
                                                                                APPLICATION_VARIABLES_VALUE_FIELD , 
                                                                                APPLICATION_VARIABLES_TYPE_FIELD },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                String msg = I18n.loc("RESTBC-7220: Unable to construct composite data for app variable", e);
                logger.severe(msg);
                throw new RuntimeException(msg);
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
        
        String[] appConfigRowAttrNames = { APPLICATION_CONFIG_ROW_KEY, APPLICATION_CONFIG_PROPERTY_URL };
        String[] appConfigAttrDesc = { "Application Configuration Name", "URL" };
        OpenType[] appConfigAttrTypes = { SimpleType.STRING, SimpleType.STRING };
        
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
    
    private void persistApplicationVariablesConfig() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appVarPersistFileName = new File(ctx.getWorkspaceRoot(), PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appVarPersistFileName);
            for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String[] metadata = (String[]) mAppVarMap.get(key);
               String value = metadata[0];
               String type = metadata[1];
               if (type.equalsIgnoreCase("PASSWORD")) {
                   value = keyStoreUtilClient.encrypt(value);
               }
               String prop = (value != null)? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (Exception ex) {
            String msg = I18n.loc("RESTBC-7224: Failed to persist application variables {0} {1}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg); 
        } 
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
                if (type.equalsIgnoreCase("PASSWORD")) {
                    value = keyStoreUtilClient.decrypt(value);
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (Exception ex) {
            String msg = I18n.loc("RESTBC-7225: Failed to load application variables {0} {1}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg);
        }
        
        return appVarMap;
    }
    
    private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appConfigPersistFileName = new File(ctx.getWorkspaceRoot(), PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String value = (String) mAppConfigMap.get(key);
               String prop = (value != null)? key + "=" + value + "\n" : key + "=\n";
               os.write(prop.getBytes());
            } 
            os.close();
        } catch (Exception ex) {
            String msg = I18n.loc("RESTBC-7226: Failed to persist application configurations {0} {1}", PERSIST_APPLICATION_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg); 
        } 
    }
    
    private Map loadApplicationConfiguration(String workspaceRoot) throws MBeanException {
        Properties persistedConfig = new Properties();
        Map appConfigMap = new HashMap();
        
        File appConfigPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
        if (!appConfigPersistFileName.exists()) {
            return appConfigMap;
        }
        
        try {
            InputStream is = new FileInputStream(appConfigPersistFileName);
            persistedConfig.load(is);
            is.close();
            
            // load the persisted environment variable configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String value = persistedConfig.getProperty(name);
                appConfigMap.put(name, value);
            }
        } catch (Exception ex) {
            String msg = I18n.loc("RESTBC-7227: Failed to load application configurations {0} {1}", PERSIST_APPLICATION_CONFIG_FILE_NAME, ex);
            logger.severe(msg);
            throw new MBeanException(ex, msg);
        }
        
        return appConfigMap; 
    }
    
    
    
    
    
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }    

}
