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
package com.sun.jbi.filebc;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import java.util.Map;

/**
 * MBean interface for run-time configuration
 * @author aegloff
 */
public interface RuntimeConfigurationMBean {

    public Integer getThreads();

    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
    
    public Integer getIBWorkerThreads();

    public void setIBWorkerThreads(Integer val) throws InvalidAttributeValueException, MBeanException;

//    public TabularData getEnvironmentVariables() throws OpenDataException;
//    public void setEnvironmentVariables(TabularData val) throws InvalidAttributeValueException, OpenDataException, MBeanException;
//    public Map retrieveEnvVariablesMap();
//    public void updateEnvVariablesMap(Map val) throws MBeanException;
    //Optional Configuration Schema
    /*With the requirement to change component configuration from a Web Browser, or from a thick-client GUI, it would be nice 
     *to provide validation of the values being changed, values that are secret like passwords not shown, detailed descriptions 
     *of configuration fields, and descriptive displays of configuration parameter labels. Since this information cannot be 
     *obtained in a detailed fashion from Standard MBeans that may be used to create the Configuration MBeans, it is possible 
     *to provide these details in other ways that can be used by the UI to create appropriate renderers and Validators. 
     *If component developers want to leverage these features, here is what needs to be done.
     *The component developer has to provide a schema corresponding to the attributes that are available in their 
     *Configuration MBean, and xml data that correspond to detailed descriptions of the attributes. Based on this, the UI 
     *can create appropriate renderers and Validators.
     */
    /* The retrieveConfigurationDisplaySchema operation returns the schema (defined in XSD) of the attributes 
     * that the Component Config MBean exposes. The schema can define restrictions and thus allows the developer to 
     * specify Enumerated Strings (which may be displayed as DropDowns in the UI) or restrict the integer fields to 
     * positive integers, specify totalDigits, and minInclusive or maxExclusive � in effect any definition that can 
     * be expressed in XSD can be placed on these attributes.
     */
//    public String retrieveConfigurationDisplaySchema();
    /*The retrieveConfigurationDisplayData operation returns the XML data corresponding to the schema (defined in 
     * XSD) of the attributes that the Component Config MBean exposes. It is here that the component developer can specify 
     * descriptive names for the attributes that can appear in label fields of the UI, descriptions of the fields that can appear in 
     *ToolTips, or whether the field is a secret field (like a password field) or not so the UI can hide the actual data the user be
     *allowed to see from the UI. 
     */
//    public String retrieveConfigurationDisplayData();
    /**
     * This operation adds a new application variable. If a variable with the same name 
     * already exists, the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variable to the 
     *         component. 
     */
    public void addApplicationVariable(String name, CompositeData appVar) throws InvalidAttributeValueException, MBeanException;

    /**
     * This operation sets an application variable. If a variable does not exist with 
     * the same name, its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
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
     * @return  a TabularData which has all the applicationvariables set on the component.
     */
    public TabularData getApplicationVariables();

    /**
     * Get the CompositeType definition for the components application configuration
     *
     * @return the CompositeType for the components application configuration.
     */
    public CompositeType queryApplicationConfigurationType();

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
    public Map retrieveApplicationVariablesMap();

    /** Updates the application variable map.
     * This method is used to communicate application configuration data within the component, and 
     * not intended for MBean clients
     *
     * @param a Map containing application variable information
     */
    public void updateApplicationVariablesMap(Map val) throws MBeanException;

    /** Retrieves the application configuration map. The map key is the
     * configuration name, the value is a ApplicationConfigurationObject representing a file address.
     * This method is used to communicate application configuration data
     * within the component and is not intended for MBean clients.
     *
     * @return a Map containing application configuration information
     */
    public Map retrieveApplicationConfigurationsMap();

    /** Updates the application configuration map.
     * This method is used to communicate application configuration data within the 
     * component, and not intended for MBean clients
     *
     * @param a Map containing application variable information
     */
    public void updateApplicationConfigurationsMap(Map val) throws MBeanException;
}
