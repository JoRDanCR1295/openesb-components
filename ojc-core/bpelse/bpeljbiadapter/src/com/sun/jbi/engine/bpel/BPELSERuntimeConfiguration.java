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
 * @(#)BPELSERuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

/*
 * BPELSERuntimeConfigurationMBean.java
 *
 * Created on August 29, 2005, 3:38 PM
 *
 * To change this template, choose Tools | Options and locate the template under
 * the Source Creation and Management node. Right-click the template and choose
 * Open. You can then make changes to the template in the Source Editor.
 */

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
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

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine.TransformEngine;
import com.sun.jbi.engine.bpel.util.I18n;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class BPELSERuntimeConfiguration implements NotificationEmitter, BPELSERuntimeConfigurationMBean {
    private static final Logger LOGGER = Logger.getLogger(BPELSERuntimeConfiguration.class.getName());
    // Application variables row fields
    private static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    private static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    private static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";

    private String workspaceRoot = null;
    
    // Global application configurations
    private Map mAppVarMap;
    private CompositeType mAppVarRowType = null;
    private TabularType mAppVarTabularType = null;

    private ComponentConfig props;
    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();
    
    private Integer mDebugPort;
    private Boolean mDebugEnabled;

    private Boolean mPersistenceEnabled;
    private String mDB_NonXAJNDIName = null;
    private String mDB_XAJNDIName = null;

    private Integer mThreadCount;
    private Integer mEngine_Expiry_Interval;
    private Integer mWaitingRequestLifeSpan;

	private Boolean mMonitoringEnabled;
	private Boolean mKPIEnabled;
    private Boolean mMonitoringVariableEnabled;

    private Boolean mValidationEnabled;

    private TransformEngine mTransformEngine = TransformEngine.XSLT_1_0;

    /**
     * Creates a new BPELSERuntimeConfiguration object.
     *
     * @param workspaceRoot location component workspace root
     * @throws JBIException 
     */
    public BPELSERuntimeConfiguration(String workspaceRoot) throws JBIException {
        this.workspaceRoot = workspaceRoot;
        restore();
    }
     
    public BPELSERuntimeConfiguration(ComponentContext ctx) throws JBIException {
        this.workspaceRoot = ctx.getWorkspaceRoot();
        this.props = ComponentConfig.parse(ctx.getInstallRoot());
        // could also acquire CC-aware logger
        restore();
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getDebugEnabled()
     */
    public Boolean getDebugEnabled() {
        return mDebugEnabled;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getDebugPort()
     */
    public Integer getDebugPort() {
        return mDebugPort;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getPersistenceEnabled()
     */
    public Boolean getPersistenceEnabled() {
        return mPersistenceEnabled;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getDatabaseNonXAJNDIName()
     */
    public String getDatabaseNonXAJNDIName() {
        return mDB_NonXAJNDIName;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getDatabaseXAJNDIName()
     */
    public String getDatabaseXAJNDIName() {
        return mDB_XAJNDIName;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getThreadCount()
     */
    public Integer getThreadCount() {
        return mThreadCount;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getEngineExpiryInterval()
     */
    public Integer getEngineExpiryInterval() {
        return mEngine_Expiry_Interval;
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getWaitingRequestLifeSpan()
     */
    public Integer getWaitingRequestLifeSpan() {
    	return mWaitingRequestLifeSpan;
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getMonitoringEnabled()
     */
    public Boolean getMonitoringEnabled() {
        return mMonitoringEnabled;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getMonitoringVariableEnabled()
     */
    public Boolean getMonitoringVariableEnabled() {
        return mMonitoringVariableEnabled;
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#getKPIEnabled()
     */
    public Boolean getKPIEnabled() {
        return mKPIEnabled;
    }    
    
    /** @return the transformEngine */
	public String getTransformEngine() {
		return mTransformEngine.toString();
	}

	/** @param transformEngine the transformEngine to set */
	public void setTransformEngine(String engine) throws MBeanException, InvalidAttributeValueException {
		try {
			String oldValue = getTransformEngine();
			mTransformEngine = (engine == null) 
					? TransformEngine.XSLT_1_0 : TransformEngine.valueOf(engine);
			
	        saveAndNotifyListners("BPELSERuntimeConfiguration.Attribute_changed", 
	                Engine.TRANSFORM_ENGINE, oldValue, mTransformEngine.toString());
		}
		catch (Exception e) {
			mTransformEngine = TransformEngine.XSLT_1_0;
            String msg = I18n.loc(
            		"BPJBI-6035: Invalid Transform Engine specified - Value must be XSLT_1_0 or XSLT_2_0");
            LOGGER.log(Level.WARNING, msg);
            throw new InvalidAttributeValueException(msg);
		}
	}

	/*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setDebugEnabled(java.lang.Boolean)
     */
    public void setDebugEnabled(Boolean newValue) throws MBeanException {
        if (newValue == null) {
            newValue = Engine.DEBUG_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getDebugEnabled().toString();

        mDebugEnabled = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.Attribute_changed", 
                Engine.DEBUG_ENABLED, oldValue, newValue.toString());
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setDebugPort(java.lang.Integer)
     */
    public void setDebugPort(Integer newValue)  throws MBeanException, InvalidAttributeValueException{
        if (newValue <= 0 || newValue > 65535) {
            String msg = I18n.loc("BPJBI-6016: Invalid Debug Port Number specified - Port Number must be non-negative between 0 and 65535");
            LOGGER.log(Level.WARNING, msg);
            throw new InvalidAttributeValueException(msg);
        }

        String oldValue = getDebugPort().toString();
        mDebugPort = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.Attribute_changed", 
                Engine.DEBUG_PORT, oldValue, newValue.toString());
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setPersistenceEnabled(java.lang.Boolean)
     */
    public void setPersistenceEnabled(Boolean newValue) throws MBeanException {

        if (newValue == null) {
            newValue = Engine.PERSISTENCE_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getPersistenceEnabled().toString();

        mPersistenceEnabled = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.PersistenceEnabled_Attribute_changed",
                Engine.PERSISTENCE_ENABLED, oldValue, newValue.toString());
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setDatabaseNonXAJNDIName(java.lang.String)
     */
    public void setDatabaseNonXAJNDIName(String newValue) throws MBeanException {
        if (newValue == null) {
            newValue = com.sun.jbi.engine.bpel.core.bpel.engine.Engine.DB_NONXA_JNDI_NAME_FACCTORYDEFAULT;
        }

        String oldValue = getDatabaseNonXAJNDIName();

        mDB_NonXAJNDIName = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.DB_JNDIName_Attribute_changed", Engine.DB_NON_XA_JNDI_NAME, oldValue, newValue);
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setDatabaseXAJNDIName(java.lang.String)
     */
    public void setDatabaseXAJNDIName(String newValue) throws InvalidAttributeValueException, MBeanException {
        if (newValue == null) {
            newValue = com.sun.jbi.engine.bpel.core.bpel.engine.Engine.DB_XA_JNDI_NAME_FACCTORYDEFAULT;
        }

        String oldValue = getDatabaseXAJNDIName();

        mDB_XAJNDIName = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.DB_JNDIName_Attribute_changed", Engine.DB_XA_JNDI_NAME, oldValue, newValue);
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setThreadCount(java.lang.Integer)
     */
    public void setThreadCount(Integer newValue) throws MBeanException {
        if (newValue < 1) {
            newValue = Engine.THREAD_COUNT_FACCTORYDEFAULT;
        }

        String oldValue = getThreadCount().toString();
        
        mThreadCount = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.Attribute_changed",
                Engine.THREAD_COUNT, oldValue, newValue.toString());
    }

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setEngineExpiryInterval(java.lang.Integer)
     */
    public void setEngineExpiryInterval(Integer newValue) throws MBeanException {
        if (newValue < 1) {
            newValue = Engine.ENGINE_EXPIRY_INTERVAL_FACCTORYDEFAULT;
        }

        String oldValue = getEngineExpiryInterval().toString();
        mEngine_Expiry_Interval = newValue;
        saveAndNotifyListners(
                "BPELSERuntimeConfiguration.Engine_Expiry_Interval_Attribute_changed",
                Engine.ENGINE_EXPIRY_INTERVAL, oldValue, newValue.toString());
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setWaitingRequestLifeSpan(java.lang.Integer)
     */
    public void setWaitingRequestLifeSpan(Integer newValue) throws MBeanException {
    	if (newValue < 1) {
    		newValue = Engine.WAITING_REQUEST_LIFE_SPAN_FACTORYDEFAULT;
    	}
    	
    	String oldValue = getWaitingRequestLifeSpan().toString();
    	mWaitingRequestLifeSpan = newValue;
    	saveAndNotifyListners("BPELSERuntimeConfiguration.Waiting_Request_Life_Span_changed",
                Engine.WAITING_REQUEST_LIFE_SPAN, oldValue, newValue.toString());
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setMonitoringEnabled(java.lang.Boolean)
     */
    public void setMonitoringEnabled(Boolean newValue) throws MBeanException {
        if (newValue == null) {
            newValue = Engine.MONITOR_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getMonitoringEnabled().toString();

        mMonitoringEnabled = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.MonitoringEnabled_Attribute_changed",
                Engine.MONITOR_ENABLED, oldValue, newValue.toString());
    }    
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setVariableMonitoringEnabled(java.lang.Boolean)
     */
    public void setMonitoringVariableEnabled(Boolean newValue) throws MBeanException {
        if (newValue == null) {
            newValue = Engine.MONITOR_VARIABLE_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getMonitoringVariableEnabled().toString();

        mMonitoringVariableEnabled= newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.VariableMonitoringEnabled_Attribute_changed",
                Engine.MONITOR_VARIABLE_ENABLED, oldValue, newValue.toString());        
    }    

    /*
     * @see com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean#setKPIEnabled(java.lang.Boolean)
     */
    public void setKPIEnabled(Boolean newValue) throws MBeanException {
        if (newValue == null) {
            newValue = Engine.KPI_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getKPIEnabled().toString();

        mKPIEnabled = newValue;
        saveAndNotifyListners("BPELSERuntimeConfiguration.KPIEnabled_Attribute_changed",
                Engine.KPI_ENABLED, oldValue, newValue.toString());
    }  

    private void restore() throws JBIException {
        ConfigPersistence.loadConfig(props, workspaceRoot);

        mDebugEnabled = Boolean.valueOf(props.getProperty(Engine.DEBUG_ENABLED).getValue());
        mDebugPort = Integer.valueOf(props.getProperty(Engine.DEBUG_PORT).getValue());

        mPersistenceEnabled = Boolean.valueOf(props.getProperty(Engine.PERSISTENCE_ENABLED).getValue());
        mDB_NonXAJNDIName = props.getProperty(Engine.DB_NON_XA_JNDI_NAME).getValue();
        mDB_XAJNDIName = props.getProperty(Engine.DB_XA_JNDI_NAME).getValue();

        mThreadCount = Integer.valueOf(props.getProperty(Engine.THREAD_COUNT).getValue());
        mEngine_Expiry_Interval = Integer.valueOf(props.getProperty(
                Engine.ENGINE_EXPIRY_INTERVAL).getValue());
        mWaitingRequestLifeSpan = Integer.valueOf(props.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN).getValue());

        mMonitoringEnabled = Boolean.valueOf(props.getProperty(Engine.MONITOR_ENABLED).getValue());
        mKPIEnabled = Boolean.valueOf(props.getProperty(Engine.KPI_ENABLED).getValue());
        mMonitoringVariableEnabled = Boolean.valueOf(props.getProperty(
                Engine.MONITOR_VARIABLE_ENABLED).getValue());
        
        mTransformEngine = TransformEngine.valueOf(props.getProperty(Engine.TRANSFORM_ENGINE).getValue());
        mValidationEnabled = Boolean.valueOf(props.getProperty(Engine.VALIDATION_ENABLED).getValue());

        // Load the persisted application variables configuration
        try {
            mAppVarMap = loadApplicationVariablesConfig(workspaceRoot);
            mAppVarRowType = createApplicationVariableCompositeType();
            mAppVarTabularType = createApplicationVariableTabularType();
        } catch (MBeanException ex) {
            throw new JBIException(ex);
        } catch (OpenDataException ex) {
            throw new JBIException(I18n.loc("BPJBI-6036: failed to get WSDL Factory", ex.getLocalizedMessage()));
        }
        
    }

    private void persistApplicationVariablesConfig() throws MBeanException {
        // Persist the changed configuration
        try {
            File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
            OutputStream os = new FileOutputStream(appVarPersistFileName);
            for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) {
               String key = (String) iter.next();
               String[] metadata = (String[]) mAppVarMap.get(key);
               String value = metadata[0];
               String type = metadata[1];
               String prop = (value != null)? key + "=" + value + "{" + type + "}\n" : key + "={" + type + "}\n";
               os.write(prop.getBytes());
            }
            os.close();
        } catch (Exception ex) {
            throw new MBeanException(ex, I18n.loc("BPJBI-6038: Failed to persist application variable properties in {0}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }
    }

    private Map loadApplicationVariablesConfig(String workspaceRoot) throws MBeanException {
        Map appVarMap = new HashMap();

        File appVarPersistFileName = new File(workspaceRoot, PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
        if (!appVarPersistFileName.exists()) {
            return appVarMap;
        }

        try {
            BufferedReader in = new BufferedReader(new FileReader(appVarPersistFileName));
            String line;
            while ((line = in.readLine()) != null) {

                //read name
                int index = line.indexOf("=");
                String name = line.substring(0, index);
                String metadata = line.substring(index + 1);

                //read value and type
                index = metadata.indexOf("{");
                String type = metadata.substring(index + 1, metadata.length() - 1);
                String value = (index == 0) ? "" : metadata.substring(0, index);
                appVarMap.put(name, new String[]{value, type});
            }
        } catch (Exception ex) {
            throw new MBeanException(ex, I18n.loc("BPJBI-6037: Failed to load application variable properties from {0}", PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME));
        }

        return appVarMap;
    }

    private void saveAndNotifyListners(String msgId, String attrName, String oldValue, String newValue) throws MBeanException {
        // Notify listeners of this change
        long seqNo = 0;

        String logMsg = I18n.loc(
                        "BPJBI-4009: Configuration change property name [{0}] old value [{1}] new value [{2}]",
                        attrName, oldValue, newValue);
        LOGGER.log(Level.CONFIG, logMsg);

        String attrType = String.class.getName();
        Notification notif = new AttributeChangeNotification(this, seqNo, System.currentTimeMillis(), logMsg, attrName,
                attrType, oldValue, newValue);
        broadcasterSupport.sendNotification(notif);

        //save the configuration to properties file
        try {
            save();
        } catch (JBIException e) {
            throw new MBeanException (e); 
        }
    }

    private void save() throws JBIException {
        props.getProperty(Engine.DEBUG_ENABLED).setValue(mDebugEnabled.toString());
        props.getProperty(Engine.DEBUG_PORT).setValue(mDebugPort.toString());

        props.getProperty(Engine.PERSISTENCE_ENABLED).setValue(mPersistenceEnabled.toString());
        props.getProperty(Engine.DB_NON_XA_JNDI_NAME).setValue(mDB_NonXAJNDIName);
        props.getProperty(Engine.DB_XA_JNDI_NAME).setValue(mDB_XAJNDIName);
        
        props.getProperty(Engine.THREAD_COUNT).setValue(mThreadCount.toString());
        props.getProperty(Engine.ENGINE_EXPIRY_INTERVAL).setValue(mEngine_Expiry_Interval.toString());
        props.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN).setValue(mWaitingRequestLifeSpan.toString());

        props.getProperty(Engine.MONITOR_ENABLED).setValue(mMonitoringEnabled.toString());        
        props.getProperty(Engine.KPI_ENABLED).setValue(mKPIEnabled.toString());               
        props.getProperty(Engine.MONITOR_VARIABLE_ENABLED).setValue(mMonitoringVariableEnabled.toString());

        props.getProperty(Engine.TRANSFORM_ENGINE).setValue(mTransformEngine.toString());

        props.getProperty(Engine.VALIDATION_ENABLED).setValue(mValidationEnabled.toString());

        ConfigPersistence.persistConfig(props, workspaceRoot);
    }
    

    Properties getProperties() {
        Properties prop = new Properties();

        Iterator<Property> propIterator = props.propertySet().iterator();
        while (propIterator.hasNext()) {
            Property componentProp = propIterator.next();
            prop.put(componentProp.getName(), componentProp.getValue());
        }
        return prop;
    }

    /*
     * @see NotificationEmitter#getNotificationInfo()
     */
    public MBeanNotificationInfo[] getNotificationInfo() {
        return new MBeanNotificationInfo[] {
            new MBeanNotificationInfo(
                new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE},
                AttributeChangeNotification.class.getName(), 
                I18n.loc("BPJBI-4015: Attribute changed")
            )
        };
    }

    /*
     * @see NotificationEmitter#addNotificationListener(javax.management.NotificationListener, javax.management.NotificationFilter, java.lang.Object)
     */
    public void addNotificationListener(NotificationListener listener,
            NotificationFilter filter, Object handback) {
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    /*
     * @see NotificationEmitter#removeNotificationListener(javax.management.NotificationListener)
     */
    public void removeNotificationListener(NotificationListener listener)
            throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    /*
     * @see NotificationEmitter#removeNotificationListener(javax.management.NotificationListener, javax.management.NotificationFilter, java.lang.Object)
     */
    public void removeNotificationListener(NotificationListener listener,
            NotificationFilter filter, Object handback)
            throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener, filter,
                handback);
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
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6039: Cannot add application variable named {0} - an application variable with the same name already exists", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6040: Found an invalid number of items {1} in the composite data for application variable {0}. Allowed number of items is 3", rowType.keySet().size())));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6041: The composite data for application variable {0} is not valid: it is not keyed by \"name\"", name)));
        }

        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6042: Found invalid composite data for application variable {0}: composite data does not contain an item for \"value\"", name)));
        }

        if ( appVarType == null) {
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6043: Found invalid composite data for application variable {0}: composite data does not contain an item for \"type\"", name)));
        }

        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        if (LOGGER.isLoggable(Level.CONFIG)) {
            LOGGER.log(Level.CONFIG, I18n.loc("BPJBI-6077: Added a new application variable with name {0}, value {1} ", new Object[] { name, appVarValue }));
        }
        persistApplicationVariablesConfig();

        //TODO add notification
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
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6047: Cannot delete application variable {0} - there is no defined application variable by that name", name)));
        }

        mAppVarMap.remove(name);
        if (LOGGER.isLoggable(Level.CONFIG)) {
            LOGGER.log(Level.CONFIG, I18n.loc("BPJBI-6078: Application variable {0} deleted.", name));
        }
        persistApplicationVariablesConfig();

             //TODO add notification
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
            throw new MBeanException(new Exception(I18n.loc("BPJBI-6045: Cannot update application variable {0} - there is no defined application variable by that name", name)));
        }

        CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(I18n.loc("BPJBI-6040: Found an invalid number of items {1} in the composite data for application variable {0}. Allowed number of items is 3", rowType.keySet().size()));
        }

        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(I18n.loc("BPJBI-6041: The composite data for application variable {0} is not valid: it is not keyed by \"name\"", name));
        }

        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);

        if (appVarValue == null) {
            throw new InvalidAttributeValueException(I18n.loc("BPJBI-6042: Found invalid composite data for application variable {0}: composite data does not contain an item for \"value\"", name));
        }

        if ( appVarType == null) {
            throw new InvalidAttributeValueException(I18n.loc("BPJBI-6043: Found invalid composite data for application variable {0}: composite data does not contain an item for \"type\"", name));
        }

        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        if (LOGGER.isLoggable(Level.CONFIG)) {
            LOGGER.log(Level.CONFIG, I18n.loc("BPJBI-6076: Application variable {0} is updated with a new value {1}.", new Object[] { name, appVarValue }));
        }

        // persist the application variable properties
        persistApplicationVariablesConfig();

        //TODO add notification
    }

    public TabularData getApplicationVariables() {
    	TabularData tabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) {
            String name = (String) iter.next();
            String[] metadata = (String[]) mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            // not sure if we should care about password variables in bpel se
            // for any case leave it here
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
                throw new RuntimeException(I18n.loc("BPJBI-6046: Failed to construct application variable composite data"), e);
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
        //TODO add notification
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

    public void setValidationEnabled(Boolean validationEnabled) throws MBeanException {
        if (validationEnabled == null) {
            validationEnabled = Engine.VALIDATION_ENABLED_FACCTORYDEFAULT;
        }

        String oldValue = getValidationEnabled().toString();

        mValidationEnabled = validationEnabled;
        saveAndNotifyListners("BPELSERuntimeConfiguration.Attribute_changed",
                Engine.VALIDATION_ENABLED, oldValue, validationEnabled.toString());
    }

    public Boolean getValidationEnabled() {
        return mValidationEnabled;
    }
}
