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

package com.sun.jbi.jmsbc.mbeans;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.util.AlertsUtil;

public abstract class RuntimeConfiguration implements RuntimeConfigurationMBean {

    private static final Messages mMessages =
        Messages.getMessages(RuntimeConfiguration.class);
    private static final Logger mLogger =
        Messages.getLogger(RuntimeConfiguration.class);

    // Configuration file name for environment variables
    private static final String PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME = "ApplicationVariables.properties";
    // Configuration file name for application configuration objects
    private static final String PERSIST_APPLICATION_CONFIG_FILE_NAME = "ApplicationConfigurations.properties";

    //Instance variable
	private boolean mHasApplicationVariableChanged = true;
	private boolean mHasApplicationConfigurationChanged = true;
	private String mWorkspaceRoot;
	private KeyStoreUtilClient mKeystoreUtilClient;
	private Map<String, String[]> mAppVarMap;
	private Map<String, Object[]> mAppConfigMap;
    private TabularType mAppConfigTabularType;
    private CompositeType mAppConfigRowType;
    private CompositeType mAppVarRowType;
    private TabularType mAppVarTabularType;
    private TabularData mApplicationConfigurationsTabularData;
    private TabularData mApplicationVariableTabularData;
	private String[] mApplicationConfigRowNames;
	private String[] mApplicationConfigRowDesc;
	private OpenType[] mApplicationConfigRowTypes;
    
    
	public RuntimeConfiguration(String workspaceRoot, KeyStoreUtilClient keystoreUtilClient) throws JBIException{
		mWorkspaceRoot = workspaceRoot;
		mKeystoreUtilClient = keystoreUtilClient;

		// Load the persisted configuration
        try {
        	mAppVarMap = (Map<String, String[]>)loadApplicationVariables();
        }catch(Throwable t){
        	mLogger.log(Level.WARNING, "JMSBC-E0308.load_application_variable_failure", new Object[] {PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, t });
        	AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-E0308.load_application_variable_failure", new Object[] {PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, t }), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-E0308");
        }
        if(mAppVarMap == null){
        	mAppVarMap = new HashMap<String, String[]>();
        }
        
        try {
            mAppConfigMap = (HashMap<String, Object[]>)loadApplicationConfiguration();
        }catch(Throwable t){
        	mLogger.log(Level.WARNING, "JMSBC-E0309.load_application_configuration_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, t });
        	AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-E0309.load_application_configuration_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, t }), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-E0309");
        }
        if(mAppConfigMap == null){
        	mAppConfigMap = new HashMap<String, Object[]>();
        }

        try{
            createApplicationConfigurationTabularType();
            createApplicationVariableCompositeType();
            createApplicationVariableTabularType();
        } catch (Exception e) {
            throw new JBIException(mMessages.getString("JMSBC-E0310.Failed_to_construct_composite_data_structures", e.getLocalizedMessage()), e);
        }
	}
	
	protected Object loadApplicationVariables() throws Exception{
		HashMap<String, String[]> variables = (HashMap<String, String[]>) loadFromFile(PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME);
		if(variables == null)
			return null;
		
		for(String[] value : variables .values()){
			if("PASSWORD".equalsIgnoreCase(value[1])){
				value[0] = decrypt(value[0]);
			}
		}
		return variables;
	}

	protected Object loadApplicationConfiguration() throws Exception{
		return dencryptApplicationConfiguration((Map<String, Object[]>)loadFromFile(PERSIST_APPLICATION_CONFIG_FILE_NAME));
	}

	private Object loadFromFile(String fileName) throws Exception {
		Object result = null;
        File appVarPersistFileName = new File(mWorkspaceRoot, fileName);
        if (!appVarPersistFileName.exists()) {
            return result;
        }
        ObjectInputStream is = null;
        try {
            is = new ObjectInputStream(new FileInputStream(appVarPersistFileName));
            result = is.readObject();
        }finally{
        	closeStreams(is);
        }
        return result;
    }

	private void persistToFile(String fileName, Object object) throws Exception {
		ObjectOutputStream out = null;
		try{
			out = new ObjectOutputStream(new FileOutputStream(new File(mWorkspaceRoot, fileName)));
			out.writeObject(object);
		}finally{
			closeStreams(out);
		}
    }
	
	private void closeStreams(Closeable s){
    	if(s != null){
    		try{s.close();}catch(Throwable e){}
    	}
	}

    protected void createApplicationConfigurationTabularType() throws OpenDataException {
    	if (mAppConfigTabularType != null) {
    	    return;
    	}
    	
        mAppConfigTabularType = new TabularType("ApplicationConfigurationObjectList",
                                                "List of Application Configuration Objects",
                                                getApplicationConfigCompositeType(),
                                                new String[]{APPLICATION_CONFIG_ROW_KEY});
    }
	
    protected void createApplicationVariableCompositeType() throws OpenDataException {
    	if (mAppVarRowType != null) {
    	    return;
    	}
    	
        mAppVarRowType = new CompositeType("ApplicationVariables",
                                           "Application Variable Composite Data",
                                           RuntimeConfigurationMBean.APPLICATION_VARIABLES_ROW_NAMES,
                                           RuntimeConfigurationMBean.APPLICATION_VARIABLES_ROW_NAMES,
                                           RuntimeConfigurationMBean.APPLICATION_VARIABLES_ROW_TYPES);
        
    }

    protected void createApplicationVariableTabularType() throws OpenDataException {
    	if (mAppVarTabularType != null) {
            return;
        }
        if (mAppVarRowType == null) {
            createApplicationVariableCompositeType();
        }
        mAppVarTabularType = new TabularType("ApplicationVariableList",
                                             "List of Application Variables",
                                             mAppVarRowType,
                                             new String[] { APPLICATION_VARIABLES_ROW_KEY } );
    }
    
    public void addApplicationConfiguration(String name, CompositeData appConfig)
			throws InvalidAttributeValueException, MBeanException {
        if (mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0311.Application_config_name_already_exists", name)));
        }
        addUpdateApplicationConfiguration(name, appConfig);
	}

	public void addApplicationVariable(String name, CompositeData appVar)
			throws InvalidAttributeValueException, MBeanException {
        if (mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0312.Application_variable_name_already_exists", name)));
        }
        addUpdateApplicationVariable(name, appVar);
	}

	private void addUpdateApplicationVariable(String name, CompositeData appVar)
			throws InvalidAttributeValueException, MBeanException {
		CompositeType rowType = appVar.getCompositeType();
        if (rowType.keySet().size() != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0313.Invalid_Item_Size_for_app_variable", rowType.keySet().size()));
        }
        
        if (!appVar.containsKey(APPLICATION_VARIABLES_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0314.Invalid_key_for_composite_data_for_app_variable", name));
        } 
        
        String appVarValue = (String)appVar.get(APPLICATION_VARIABLES_VALUE_FIELD);
        String appVarType = (String)appVar.get(APPLICATION_VARIABLES_TYPE_FIELD);
        
        if (appVarValue == null) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0315.Invalid_app_variable_no_value_field", name));
        }
        
        if ( appVarType == null) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0316.Invalid_app_variable_type", name));
        }
        
        mAppVarMap.put(name, new String[] { appVarValue, appVarType });
        mHasApplicationVariableChanged = true;
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("JMSBC-C0306.New_application_variable_added", new Object[] { name, appVarValue }));
        }
        
        persistApplicationVariablesConfig();
	}

	public void deleteApplicationConfiguration(String name)
			throws MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0319.application_config_not_exist", name)));
        }
        mAppConfigMap.remove(name);
        mHasApplicationConfigurationChanged = true;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("JMSBC-C0307.Application_configuration_deleted", name));
        }
        persistApplicationConfigurationObjects();
	}

	public void deleteApplicationVariable(String name) throws MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0320.application_variable_not_exist", name)));
        }
        mAppVarMap.remove(name);
        mHasApplicationVariableChanged = true;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("JMSBC-C0308.Application_variable_deleted", name));
        }
        persistApplicationVariablesConfig();
	}

	public TabularData getApplicationConfigurations() {
		if(!mHasApplicationConfigurationChanged){
			return mApplicationConfigurationsTabularData;
		}
        mApplicationConfigurationsTabularData = new TabularDataSupport(mAppConfigTabularType);
        for (Iterator<String> iter = mAppConfigMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = iter.next();
            Object[] value = mAppConfigMap.get(name);
            value = encryptIfRrequired(value, false);
            Object[] data = new Object[value.length + 1];
            data[0] = name;
            System.arraycopy(value, 0, data, 1, value.length);
            try {
                CompositeData rowData = new CompositeDataSupport(
						getApplicationConfigCompositeType(),
						privateGetApplicationConfigRowNames(), data);

                mApplicationConfigurationsTabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("JMSBC-E0321.application_composite_data_construction_error"), e);
            }
        }
        mHasApplicationConfigurationChanged = false;
        return mApplicationConfigurationsTabularData;
	}

	public TabularData getApplicationVariables() {
		if(!mHasApplicationVariableChanged){
			return mApplicationVariableTabularData;
		}
		
		mApplicationVariableTabularData = new TabularDataSupport(mAppVarTabularType);
        for (Iterator<String> iter = mAppVarMap.keySet().iterator(); iter.hasNext(); ) { 
            String name = (String) iter.next();
            String[] metadata = mAppVarMap.get(name);
            String value = metadata [0];
            String type = metadata [1];
            Object[] data = ("PASSWORD".equals(type))? new Object[] { name, "*******", type } : new Object[] {name, value, type};
            try {
                CompositeData rowData = new CompositeDataSupport(mAppVarRowType,
                                                                 new String[] { APPLICATION_VARIABLES_ROW_KEY , 
                                                                                APPLICATION_VARIABLES_VALUE_FIELD , 
                                                                                APPLICATION_VARIABLES_TYPE_FIELD },
                                                                 data);
                
                mApplicationVariableTabularData.put(rowData);
            } catch (OpenDataException e) {
                throw new RuntimeException(mMessages.getString("JMSBC-E0322.application_composite_data_construction_error"), e);
            }
        }
        
        mHasApplicationVariableChanged = false;
        return mApplicationVariableTabularData;
	}

	public CompositeType queryApplicationConfigurationType() throws OpenDataException {
		return getApplicationConfigCompositeType();
	}

	public Map<String, Object[]> retrieveApplicationConfigurationsMap() {
		return mAppConfigMap;
	}

	public Map<String, String[]> retrieveApplicationVariablesMap() {
		return mAppVarMap;
	}

	public void setApplicationConfiguration(String name, CompositeData appConfig)
			throws InvalidAttributeValueException, MBeanException {
        if (!mAppConfigMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0323.Application_configuration_does_not_exist",  name)));
        }
        addUpdateApplicationConfiguration(name, appConfig);
	}

	private void addUpdateApplicationConfiguration(String name,
			CompositeData appConfig) throws InvalidAttributeValueException, MBeanException {

        CompositeType rowType = appConfig.getCompositeType();
        if (rowType.keySet().size() != privateGetApplicationConfigRowNames().length) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0324.Invalid_item_size_for_application_configuration", name));
        }
        
        if (!appConfig.containsKey(APPLICATION_CONFIG_ROW_KEY)) {
            throw new InvalidAttributeValueException(mMessages.getString("JMSBC-E0325.Invalid_key",name));
        } 

        Object[] arr = appConfig.getAll(privateGetApplicationConfigRowNames());
        Object[] values = new Object[arr.length - 1];
        System.arraycopy(arr, 1, values, 0, values.length);
		validateApplicationConfigData(values);
        mAppConfigMap.put(name, values);
        mHasApplicationConfigurationChanged = true;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, mMessages.getString("JMSBC-C0305.New_application_configuration_added", name));
        }
        persistApplicationConfigurationObjects();
	}

	public void setApplicationVariable(String name, CompositeData appVar)
			throws InvalidAttributeValueException, MBeanException {
        if (!mAppVarMap.containsKey(name)) {
            throw new MBeanException(new Exception(mMessages.getString("JMSBC-E0326.Application_variable_does_not_exist", name)));
        }
        addUpdateApplicationVariable(name, appVar);

	}

	public void updateApplicationConfigurationsMap(Map<String, Object[]> val)
			throws MBeanException {
        mAppConfigMap = val;
        mHasApplicationConfigurationChanged = true;
        persistApplicationConfigurationObjects();
	}

	public void updateApplicationVariablesMap(Map<String, String[]> val) throws MBeanException {
        mAppVarMap = val;
        mHasApplicationVariableChanged = true;
        persistApplicationVariablesConfig();
	}
	
	protected void persistApplicationConfigurationObjects() throws MBeanException{
		Object encryptedCopy = encryptApplicationConfiguration(mAppConfigMap);
        try{
            persistToFile(PERSIST_APPLICATION_CONFIG_FILE_NAME, encryptedCopy);
        }catch(Exception e){
        	String str = mMessages.getString("JMSBC-E0318.persist_application_config_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, e});
        	mLogger.log(Level.WARNING, str);
        	AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-E0318.persist_application_config_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, e}), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-E0318");
        	throw new MBeanException(e, str);
        }
	}

	protected void persistApplicationVariablesConfig() throws MBeanException{
		HashMap<String, String[]> copy = new HashMap<String, String[]>();
		for(Map.Entry<String, String[]> entry : mAppVarMap.entrySet()){
			String[] newValue = new String[2];
			newValue[1] = entry.getValue()[1];
			if("PASSWORD".equalsIgnoreCase(entry.getValue()[1])){
				newValue[0] = encrypt(entry.getValue()[0]);
			}else{
				newValue[0] = entry.getValue()[0];
			}
			copy.put(entry.getKey(), newValue);
		}
        try{
            persistToFile(PERSIST_APPLICATION_VARIABLE_CONFIG_FILE_NAME, copy);
        }catch(Exception e){
        	String str = mMessages.getString("JMSBC-E0317.persist_application_variable_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, e});
        	mLogger.log(Level.WARNING, str);
        	AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-E0317.persist_application_variable_failure", new Object[] { PERSIST_APPLICATION_CONFIG_FILE_NAME, e}), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-E0317");
        	throw new MBeanException(e, str);
        }
	}

	/**
	 * Get Application specific configuration composite type
	 * 
	 * @return
	 * @throws OpenDataException
	 */
	protected CompositeType getApplicationConfigCompositeType() throws OpenDataException{
    	if (mAppConfigRowType != null) {
    	    return mAppConfigRowType;
    	}
        mAppConfigRowType = new CompositeType("AppliationConfigurationObject",
                                      "Application Configuration Composite Data",
                                      privateGetApplicationConfigRowNames(),
                                      privateGetApplicationConfigRowDesc(),
                                      privateGetApplicationConfigRowTypes());
        
        return mAppConfigRowType;
	}
	
	private String[] privateGetApplicationConfigRowNames(){
		if(mApplicationConfigRowNames != null)
			return mApplicationConfigRowNames;
		
		String[] rowNames = getApplicationConfigRowNames();
		mApplicationConfigRowNames = new String[rowNames.length + 1];
		mApplicationConfigRowNames[0] = APPLICATION_CONFIG_ROW_KEY;
		System.arraycopy(rowNames, 0, mApplicationConfigRowNames, 1, rowNames.length);
		return mApplicationConfigRowNames;
	}
	
	private String[] privateGetApplicationConfigRowDesc(){
		if(mApplicationConfigRowDesc != null)
			return mApplicationConfigRowDesc;
		
		String[] rowDescs = getApplicationConfigRowDesc();
		mApplicationConfigRowDesc = new String[rowDescs.length + 1];
		mApplicationConfigRowDesc[0] = APPLICATION_CONFIG_ROW_DESC;
		System.arraycopy(rowDescs, 0, mApplicationConfigRowDesc, 1, rowDescs.length);
		return mApplicationConfigRowDesc;
	}

	private OpenType[] privateGetApplicationConfigRowTypes(){
		if(mApplicationConfigRowTypes != null)
			return mApplicationConfigRowTypes;
		
		OpenType[] rowTypes = getApplicationConfigRowTypes();
		mApplicationConfigRowTypes = new OpenType[rowTypes.length + 1];
		mApplicationConfigRowTypes[0] = APPLICATION_CONFIG_ROW_TYPE;
		System.arraycopy(rowTypes, 0, mApplicationConfigRowTypes, 1, rowTypes.length);
		return mApplicationConfigRowTypes;
	}

	/**
	 * The implementation would return the row names to generate tabular type
	 * 
	 * @return
	 */
	abstract protected String[] getApplicationConfigRowNames();

	/**
	 * The implementation would return the row descriptions to generate tabular type
	 * 
	 * @return
	 */
	abstract protected String[] getApplicationConfigRowDesc();

	/**
	 * The implementation would return the row Types to generate tabular type
	 * 
	 * @return
	 */
	abstract protected OpenType[] getApplicationConfigRowTypes();

	/**
	 * The implementation would generate the application specific data.
	 *  
	 * @param compositeData
	 * @param name
	 * @return
	 * @throws InvalidAttributeValueException
	 */
	abstract protected void validateApplicationConfigData(Object[] objs) throws InvalidAttributeValueException;

	/**
	 * The implementation would generate the application specific data.
	 *  
	 * @param compositeData
	 * @param name
	 * @return
	 * @throws InvalidAttributeValueException
	 */
	abstract protected Object[] encryptIfRrequired(Object[] objs, boolean base64);
	
	abstract protected Object[] decryptIfRrequired(Object[] objs);
	
	protected Object encryptApplicationConfiguration(Map<String, Object[]> map){
		Map<String, Object[]> result = new HashMap<String, Object[]>();
		for(Map.Entry<String, Object[]> entry: map.entrySet()){
			result.put(entry.getKey(), encryptIfRrequired(entry.getValue(), true));
		}
		return result;
	}

	protected Object dencryptApplicationConfiguration(Map<String, Object[]> map){
		if(map == null)
			return null;
		Map<String, Object[]> result = new HashMap<String, Object[]>();
		for(Map.Entry<String, Object[]> entry: map.entrySet()){
			result.put(entry.getKey(), decryptIfRrequired(entry.getValue()));
		}
		return result;
	}
	
	protected String decrypt(String str){
		try {
			str = mKeystoreUtilClient.decrypt(str);
		} catch (Exception e) {}
		return str;
	}
	
	protected String encrypt(String str){
		try {
			str = mKeystoreUtilClient.encrypt(str);
		} catch (Exception e) {}
		return str;
	}
	
	public void dump(StringBuffer msgBuf) {

        for (Iterator<String> it = mAppVarMap.keySet().iterator(); it.hasNext(); ) {
    	    msgBuf.append(CONFIG_APPLICATON_VARIABLES).append(": { ");
            String name = it.next();
            msgBuf.append('[').append(name).append(',');
            String[] valueType = mAppVarMap.get(name);
            if ("PASSWORD".equals(valueType[1])) {
                msgBuf.append("*******").append(']');
            } else {
                msgBuf.append(valueType[0]).append(']');
            }
            msgBuf.append(" }\n");
        }
        for (Iterator<String> it = mAppConfigMap.keySet().iterator(); it.hasNext();) {
	    msgBuf.append(CONFIG_APPLICATION_CONFIGURATIONS).append(": { ");
	    String name = it.next();
	    msgBuf.append('[').append(name).append("=(");
	    Object[] value = mAppConfigMap.get(name);
	    for (int i = 0; i < value.length; ++i) {
		if ((i == 5 || i == 7) && !((value[i] == null) || (value[i].equals("")))) {
		    msgBuf.append("******, ");
		} else {
		    msgBuf.append(value[i] + ", ");
		}
	    }
	    msgBuf.append(')');
	    msgBuf.append(']');
	    msgBuf.append(" }");
	    if (it.hasNext()) {
		msgBuf.append("\n");
	    }
	}
    }

}
