 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4corba.jbi.component.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
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

import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.configuration.ConfigPersistence;


/**
 * Runtime configuration MBean, allow configuration to be changed at run-time
 */
@SuppressWarnings("unchecked")
public class RuntimeConfiguration implements RuntimeConfigurationMBean,
    NotificationEmitter {
    private static final Logger LOG = LoggerFactory
            .getLogger(RuntimeConfiguration.class);
    private static final Messages MESSAGES
            = Messages.getMessages(RuntimeConfiguration.class);

    // Attribute names
    public static final String CONFIG_THREADS = "OutboundThreads";

    // Default values in the absence of configuration settings
    private static final String DEFAULT_THREADS = "5";

    // Configuration validation settings    
    long MIN_THREADS = 1;
    long MAX_THREADS = 10000;
    
        
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";
	public static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    
    // Application configuration row fields
    private static final String APPLICATION_CONFIG_ROW_KEY = "configurationName";
    //private static final String APPLICATION_CONFIG_PROPERTY_IDL = "Provider_IDL";
    private static final String APPLICATION_CONFIG_PROPERTY_LOCALIZATION_TYPE = "Localization_Type";
    private static final String APPLICATION_CONFIG_PROPERTY_EXTENSION_NAME = "Name";
    private static final String APPLICATION_CONFIG_PROPERTY_ORB = "ORB";
    
    // Configuration 
    private Properties mConfig=new Properties();
    private String mWorkspaceRoot;
    private String mConfigSchema;
    private String mConfigData;
    
    // Global application configurations
	private Map mAppConfigMap=new HashMap();
    private CompositeType mAppConfigRowType = null;
    private TabularType mAppConfigTabularType = null;

	public static  CompositeType APPVAR_ROW_TYPE;
    public static  TabularType APPVAR_TABULAR_TYPE;

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
	
	// Application Variables store
	private  Map<String, String[]> mAppVarMap = null;	

	private KeyStoreUtilClient mKeySupport;

    // Use delegation to support notification
    NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();

    /** Creates a new instance of InstallerExt */
	public RuntimeConfiguration(final String workspaceRoot,String configSchema, String configData) throws JBIException {
        try {
            mWorkspaceRoot = workspaceRoot;
            mConfigSchema = configSchema;
            mConfigData = configData;
            
            LOG.debug("ConfigurationDisplaySchema:"+mConfigSchema);
            LOG.debug("ConfigurationDisplayData:"+mConfigData);
            
            // Load the persisted configuration

            mConfig = ConfigPersistence.loadConfig(workspaceRoot);
            mAppConfigMap = loadApplicationConfiguration(workspaceRoot);
            mAppConfigRowType = createApplicationConfigurationCompositeType();
            mAppConfigTabularType = createApplicationConfigurationTabularType();
			APPVAR_ROW_TYPE = createApplicationVariableCompositeType();
            APPVAR_TABULAR_TYPE = createApplicationVariableTabularType();
			mAppVarMap = loadApplicationVariables(workspaceRoot);

            
        }catch(Exception e){
            //if we have an error we assume we aren't in openesb
            LOG.warn("unable to load configurations properties we assume we aren't running under OpenESB", new JBIException(MESSAGES.getString("CRB000411_Failed_to_construct_composite_data_structures", e.getLocalizedMessage()), e));
        }
    }

	RuntimeConfiguration(String workspaceRoot, String configSchema) {
        mWorkspaceRoot = workspaceRoot;
        mConfigSchema = configSchema;
        mConfig = new Properties();
        mAppConfigMap = new HashMap();
        //mAppConfigRowType = createApplicationConfigurationCompositeType();
        //mAppConfigTabularType = createApplicationConfigurationTabularType();                
    }

    /**
     * @return
     */
    //@Override
    public Integer getOutboundThreads() {
        final String val = mConfig.getProperty(RuntimeConfiguration.CONFIG_THREADS, RuntimeConfiguration.DEFAULT_THREADS);

        return Integer.valueOf(val);
    }

    /**
     *
     * @param val 
     * @throws javax.management.InvalidAttributeValueException 
     * @throws javax.management.MBeanException 
     */
    //@Override
    public void setOutboundThreads(final Integer val)
        throws InvalidAttributeValueException, MBeanException {
        final String attrName = RuntimeConfiguration.CONFIG_THREADS;

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

        // Apply and save the changes
        mConfig.put(RuntimeConfiguration.CONFIG_THREADS, val.toString());
        persistConfiguration();

        // Notify listeners of this change
        final long seqNo = 0;
        final String msg = "Attribute changed";
        final String attrType = Integer.class.getName();
        final Integer oldVal = getOutboundThreads();
        final Notification notif = new AttributeChangeNotification(this, seqNo,
                System.currentTimeMillis(), msg, attrName, attrType, oldVal,
                newVal);
        broadcasterSupport.sendNotification(notif);
    }

    /**
     *
     * @throws MBeanException
     */
    private void persistConfiguration() throws MBeanException {
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
    //@Override
    public void removeNotificationListener(final NotificationListener listener,
        final NotificationFilter filter, final Object handback)
        throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
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
            throw new MBeanException(new Exception(MESSAGES.getString("CRB000412_Application_config_name_already_exists for ", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 4) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000413_Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000414_Invalid_key_for_composite_data_for_app_config", name));
        } 
        
        String[] appConfigValues = new String[3];
        //appConfigValues[0] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_IDL);
        appConfigValues[0] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_LOCALIZATION_TYPE);
        appConfigValues[1] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_EXTENSION_NAME);
        appConfigValues[2] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_ORB);
        

        if ( appConfigValues[0] == null ||appConfigValues[1] == null || appConfigValues[2] == null) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000415_Invalid_app_config_composite_data_null", name));
        }
        
        if(appConfigValues[2]!=null && !appConfigValues[2].equals("")){
            if(appConfigValues[2].indexOf("=") < 1){
                throw new InvalidAttributeValueException(MESSAGES.getString("CRB000422_Invalid_ORB_value"));
            }
        }
        
        if(appConfigValues[0]!=null && !appConfigValues[0].equals("")){
            if(!appConfigValues[0].equals("NameService") && !appConfigValues[0].equals("corbaloc") 
                    && !appConfigValues[0].equals("corbaname") && !appConfigValues[0].equals("IOR")){
                throw new InvalidAttributeValueException(MESSAGES.getString("CRB000424_Unable_to_add_Localization_type"));
            }
        }
        
    	
        mAppConfigMap.put(name, appConfigValues);
        
        if (LOG.isInfoEnabled()) {
            LOG.info(MESSAGES.getString("CRB000219_New_application_configuration_added", new Object[] { name, appConfigValues }));
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
            throw new MBeanException(new Exception(MESSAGES.getString("CRB000216_Application_configuration_does_not_exist_for_delete", name)));
        }
        
        mAppConfigMap.remove(name);
        if (LOG.isInfoEnabled()) {
            LOG.info(MESSAGES.getString("CRB000420_Application_configuration_deleted", name));
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
            throw new MBeanException(new Exception(MESSAGES.getString("CRB000417_Application_configuration_does_not_exist_for_set", name)));
        }
        
        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != 4) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000413_Invalid_Item_Size_for_app_config", new Object[] { name, rowType.keySet().size() }));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000413_Invalid_key_for_composite_data_for_app_config", name));
        } 
        String[] appConfigValues = new String[3];
        //appConfigValues[0] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_IDL);
        appConfigValues[0] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_LOCALIZATION_TYPE);
        appConfigValues[1] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_EXTENSION_NAME);
        appConfigValues[2] = (String) appConfig.get(APPLICATION_CONFIG_PROPERTY_ORB);
        if ( appConfigValues[0] == null ||appConfigValues[1] == null || appConfigValues[2] == null ) {
            throw new InvalidAttributeValueException(MESSAGES.getString("CRB000415_Invalid_app_config_composite_data_null", name));
        }
        if(appConfigValues[2]!=null && !appConfigValues[2].equals("")){
            if(appConfigValues[2].indexOf("=") < 1){
                throw new InvalidAttributeValueException(MESSAGES.getString("CRB000422_Invalid_ORB_value"));
            }
        }
        
        if(appConfigValues[0]!=null && !appConfigValues[0].equals("")){
            if(!appConfigValues[0].equals("NameService") && !appConfigValues[0].equals("corbaloc") 
                    && !appConfigValues[0].equals("corbaname") && !appConfigValues[0].equals("IOR")){
                throw new InvalidAttributeValueException(MESSAGES.getString("CRB000424_Unable_to_add_Localization_type"));
            }
        }
        
        mAppConfigMap.put(name, appConfigValues);
        if (LOG.isInfoEnabled()) {
            LOG.info(MESSAGES.getString("CRB000421_Application_configuration_updated", new Object[] { name, appConfigValues }));
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
            String[] values = (String[]) mAppConfigMap.get(name);
            Object[] data = new Object[] {name, values[0], values[1], values[2]};
            
            try {
                CompositeData rowData = new CompositeDataSupport(mAppConfigRowType,
                                                                 new String[] { APPLICATION_CONFIG_ROW_KEY,
                                                                 APPLICATION_CONFIG_PROPERTY_LOCALIZATION_TYPE,
                                                                 APPLICATION_CONFIG_PROPERTY_EXTENSION_NAME,
                                                                 APPLICATION_CONFIG_PROPERTY_ORB },
                                                                 data);
                
                tabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(MESSAGES.getString("CRB000418_Unable_to_construct_composite_data_for_app_config"), e);
            }
        }
        
        return tabularData;
    }
    
    public Map retrieveApplicationConfigurationsMap() {
        return mAppConfigMap;
    }
    
    public void updateApplicationConfigurationsMap(Map appConfigMap) throws MBeanException {
        mAppConfigMap = appConfigMap;
        persistApplicationConfigurationObjects();
    }
    
    private CompositeType createApplicationConfigurationCompositeType() throws OpenDataException {
    	if (mAppConfigRowType != null) {
    	    return mAppConfigRowType;
    	}
    	
        String[] appConfigRowAttrNames = { APPLICATION_CONFIG_ROW_KEY, 
                                           APPLICATION_CONFIG_PROPERTY_LOCALIZATION_TYPE,
                                           APPLICATION_CONFIG_PROPERTY_EXTENSION_NAME,
                                           APPLICATION_CONFIG_PROPERTY_ORB };
        String[] appConfigAttrDesc = { "Application Configuration Name", "Corba Localization Type", "Corba Extension Name", "ORB" };
        OpenType[] appConfigAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING, SimpleType.STRING };
        
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
    
    private void persistApplicationConfigurationObjects() throws MBeanException {
        // Persist the changed configuration        
        try {
            File appConfigPersistFileName = new File(mWorkspaceRoot, PERSIST_APPLICATION_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appConfigPersistFileName);
            for (Iterator iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String[] value = (String[]) mAppConfigMap.get(key);
               String prop = (value != null)? key + "=" + value[0] + ";" + value[1] + ";" + 
                              value[2] + "\n" : key + "=\n";
               os.write(prop.getBytes());
               
            } 
            os.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, "Failed to persist application configuration objects"); 
            
        } 
    }
    
    private Map loadApplicationConfiguration(String workspaceRoot) throws JBIException {
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
            
            // load the persisted application configurations in the map
            for (Enumeration e = persistedConfig.propertyNames(); e.hasMoreElements(); ) {
                String name = (String) e.nextElement();
                String value = persistedConfig.getProperty(name);
                if(value!=null){
                    String[] values= value.split(";");
                    // String.split() will truncate trailing empty strings.
                    // Hence ensure that values array has 3 elements.
                    String[] finalValues= new String[3];
                    for(int i=0; i<values.length ; i++){
                        finalValues[i] = values[i];
                    }
                    appConfigMap.put(name, finalValues);
                    
                }else {
                    appConfigMap.put(name, value);
                }
                
            }
        } catch (Exception ex) {
            throw new JBIException("Failed to load persisted application objects", ex);
        }
        
        return appConfigMap; 
    }
    
    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema()  {
        return this.mConfigSchema;
    }
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData() {
        return this.mConfigData;
    }
    
    public Map<String, String[]> retrieveApplicationVariablesMap() {
    	// 20/11/2008: MARCO for the execution outside Glassfish
    	if (mAppVarMap == null) {
    		return new HashMap();
    	}
        synchronized (mAppVarMap) {
            return Collections.unmodifiableMap(mAppVarMap);
        }
    }

	 public int countVariables() {
		// 20/11/2008: MARCO for the execution outside Glassfish
	    if (mAppVarMap == null) {
	    	return 0;
	    }
        synchronized (mAppVarMap) {
            return mAppVarMap.size();
        }
    }

	 public void updateApplicationVariablesMap(Map<String, String[]> map)
            throws MBeanException {
	    // 20/11/2008: MARCO for the execution outside Glassfish
		if (mAppVarMap == null) {
	    		return;
		}
	
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
                throw new MBeanException(new Exception(MESSAGES.getString(
                        "CRB000442_AppVarNameAlreadyExists",
                        name)));
            }

            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000443_AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }
        
            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000444_AppVarMissingField",
                                new Object[] {name, APP_VAR_NAME})));
            }
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000448_AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }

            String appVarValue = (String) appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000447_AppVarValueNull",
                                new Object[] {name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String) appVar.get(APP_VAR_TYPE);
            if (appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000447_AppVarValueNull",
                                new Object[] {name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (LOG.isInfoEnabled()) {
                LOG.info(MESSAGES.getString(
                        "CRB000455_AppVarAdded",
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
                        MESSAGES.getString(
                                "CRB000446_AppVarExistenceFailedUpdate",
                                name)));
            }
        
            CompositeType rowType = appVar.getCompositeType();
            if (rowType.keySet().size() != APP_VAR_COUNT) {
                throw new MBeanException(new InvalidAttributeValueException(
                       MESSAGES.getString(
                                "CRB000443_AppVarRowSizeInvalid",
                                new Object[]{name, APP_VAR_COUNT,
                                        rowType.keySet().size()})));
            }

            if (!appVar.containsKey(APP_VAR_NAME)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000444_AppVarMissingField",
                                new Object[]{name, APP_VAR_NAME})));
            } 
            String appVarName = (String) appVar.get(APP_VAR_NAME);
            if (!appVarName.equals(name)) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000448_AppVarNameMismatch",
                                new Object[] {name, appVarName})));
            }
        
            String appVarValue = (String)appVar.get(APP_VAR_VALUE);
            if (appVarValue == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000447_AppVarValueNull",
                                new Object[]{name, APP_VAR_VALUE})));
            }
        
            String appVarType = (String)appVar.get(APP_VAR_TYPE);
            if ( appVarType == null) {
                throw new MBeanException(new InvalidAttributeValueException(
                        MESSAGES.getString(
                                "CRB000447_AppVarValueNull",
                                new Object[]{name, APP_VAR_TYPE})));
            }
        
            mAppVarMap.put(name, new String[] { appVarValue, appVarType });
            if (LOG.isInfoEnabled()) {
                LOG.info(MESSAGES.getString(
                        "CRB000450_AppVarUpdated",
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
                throw new MBeanException(new Exception(MESSAGES.getString(
                        "CRB000445_AppVarExistenceFailedDelete", name)));
            }
            mAppVarMap.remove(name);
            if (LOG.isInfoEnabled()) {
                LOG.info(MESSAGES.getString(
                        "CRB000451_AppVarDeleted", name));
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
                    throw new RuntimeException(MESSAGES.getString(
                            "CRB000449_AppVarTabularCreateFailed"), e);
                }
            }
        }
        return tabularData;
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
            throw new MBeanException(ex, MESSAGES.getString(
                    "CRB000452_AppVarPersistWriteFailed",
                    PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
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
                            throw new MBeanException(e1, MESSAGES.getString(
                                    "CRB000453_AppVarLoadDecryptFailed",
                                    name
                            ));
                        }
                    }
                }
                appVarMap.put(name, new String[] {value, type});
            }
        } catch (IOException ex) {
            throw new MBeanException(ex, MESSAGES.getString(
                    "CRB000454_AppVarPersistLoadFailed",
                    PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
        
        return appVarMap;
    }

 }
