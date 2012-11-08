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
 * @(#)RuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.mbeans;

import java.util.Map;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;

import com.sun.jbi.internationalization.Messages;

public interface RuntimeConfigurationMBean {

    public static final Messages mMessages =
        Messages.getMessages(RuntimeConfigurationMBean.class);
	
    // Application variables row fields
    public static final String APPLICATION_VARIABLES_ROW_KEY = "name";
    public static final String APPLICATION_VARIABLES_VALUE_FIELD = "value";
    public static final String APPLICATION_VARIABLES_TYPE_FIELD = "type";
    public static final String[] APPLICATION_VARIABLES_ROW_NAMES = new String[] {
			APPLICATION_VARIABLES_ROW_KEY, 
			APPLICATION_VARIABLES_VALUE_FIELD,
			APPLICATION_VARIABLES_TYPE_FIELD };

    public static final String[] APPLICATION_VARIABLES_ROW_DESC = new String[] {
    	mMessages.getString("APPLICATION_VARIABLES_ROW_KEY"), 
    	mMessages.getString("APPLICATION_VARIABLES_VALUE_FIELD"),
    	mMessages.getString("APPLICATION_VARIABLES_TYPE_FIELD") };

    public static final OpenType[] APPLICATION_VARIABLES_ROW_TYPES = new OpenType[] {
    	SimpleType.STRING, 
    	SimpleType.STRING,
    	SimpleType.STRING };

	// Application configurations and application variables
    public static final String CONFIG_APPLICATON_VARIABLES = "ApplicationVariables";
    public static final String CONFIG_APPLICATION_CONFIGURATIONS = "ApplicationConfigurations";
	public static final String APPLICATION_CONFIG_ROW_KEY = "configurationName";
	public static final String APPLICATION_CONFIG_ROW_DESC = mMessages.getString("APPLICATION_CONFIG_ROW_KEY"); 
	public static final OpenType APPLICATION_CONFIG_ROW_TYPE = SimpleType.STRING; 
	
	/**
     * This operation adds a new application variable. If a variable with the same name 
     * already exists, the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable composite
     * @throws MBeanException if an error occurs in adding the application variable to the 
     *         component. 
     */
     public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException;
     
    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable composite to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
    public void setApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException; 
     
    /**
     * This operation deletes an application variable, if a variable with the specified name does
     * not exist, it's an error.
     *
     * @param name - name of the application variable
     * @throws MBeanException on errors.
     */
     public void deleteApplicationVariable(String name) throws MBeanException;
     
     /**
      * Get the Application Variable set for a component.
      *
      * @return  a TabularData which has all the application variables set on the component. 
      */
     public TabularData getApplicationVariables();

    
    /**
      * Get the CompositeType definition for the components application configuration 
      *
      * @return the CompositeType for the components application configuration.
      */
     public CompositeType queryApplicationConfigurationType() throws OpenDataException;
     
     /**
      * Add an application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "name" in the namedConfig
      * @param appConfig - application configuration composite 
      * @throws MBeanException if the application configuration cannot be added.
      */
    public void addApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException;
    
     /**
      * Delete an application configuration. 
      *
      * @param name - identification of the application configuration to be deleted
      * @throws MBeanException if the configuration cannot be deleted.
      */
    public void deleteApplicationConfiguration(String name) throws MBeanException;
     
     /**
      * Update a application configuration. The configuration name is a part of the CompositeData.
      * The itemName for the configuration name is "configurationName" and the type is SimpleType.STRING
      *
      * @param name - configuration name, must match the value of the field "configurationName" in the appConfig
      * @param appConfig - application configuration composite
      * @throws MBeanException if there are errors encountered when updating the configuration.
      */
    public void setApplicationConfiguration(String name, CompositeData appConfig) throws InvalidAttributeValueException, MBeanException;
    
    /**
     * Get a Map of all application configurations for the component.
     *
     * @return a TabularData of all the application configurations for a 
     *         component keyed by the configuration name. 
     */
    public TabularData getApplicationConfigurations(); 
    
    /** Retrieves the application variables map. The map key is the application 
      * variable name, the value is a String[] containing detailed information
      * about the application variable. This method is used to communicate
      * application variable data with in the component and is not intended 
      * for MBean clients
      *
      * @return a Map containing application variable information
      */
    public Map<String, String[]> retrieveApplicationVariablesMap();
    
    /** Updates the application variable map.
      * This method is used to communicate application configuration data within the component, and 
      * not intended for MBean clients
      *
      * @param a Map containing application variable information
      */
    public void updateApplicationVariablesMap(Map<String, String[]> val) throws MBeanException;
    
    
    /** Retrieves the application configuration map. The map key is the  
      * configuration name, the value is a String representing a HTTP URL.
      * This method is used to communicate application configuration data
      * within the component and is not intended for MBean clients.
      *
      * @return a Map containing application configuration information
      */
    public Map<String, Object[]> retrieveApplicationConfigurationsMap();
    
    /** Updates the application configuration map.
      * This method is used to communicate application configuration data within the 
      * component, and not intended for MBean clients
      *
      * @param a Map containing application variable information
      */
    public void updateApplicationConfigurationsMap(Map<String, Object[]> val) throws MBeanException;

}
