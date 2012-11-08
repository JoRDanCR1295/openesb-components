 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component.runtime;

import java.util.Map;
import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;


/**
 * MBean interface for run-time configuration
 */
@SuppressWarnings("unchecked")
public interface RuntimeConfigurationMBean {
    
    
    // Attribute names
    public static final String CONFIG_THREADS = "OutboundThreads";
    
    // Application configurations and application variables
    public static final String CONFIG_APPLICATION_CONFIGURATIONS = "ApplicationConfigurations";

    public Integer getOutboundThreads();

    public void setOutboundThreads(Integer val)
        throws InvalidAttributeValueException, MBeanException;
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
    /** Retrieves the application configuration map. The map key is the  
      * configuration name, the value is a String representing a HTTP URL.
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
    
    /** Retrieves the component specific configuration schema.
      *
      * @return a String containing the configuration schema.
      */
    //public String retrieveConfigurationDisplaySchema();
    
    /** Retrieves the component configuration metadata.
      * The XML data conforms to the component 
      * configuration schema.
      *
      * @return a String containing the configuration metadata.
      */
    //public String retrieveConfigurationDisplayData();

	 /**
     * Retrieves the application variables map. The map key is the application variable name, the
     * value is a String[] containing detailed information about the application variable. This
     * method is used to communicate application variable data with in the component and is not
     * intended for MBean clients
     * 
     * @return a Map containing application variable information
     */
   public Map<String, String[]> retrieveApplicationVariablesMap();
   
   /**
     * Updates the application variable map. This method is used to communicate application
     * configuration data within the component, and not intended for MBean clients
     * 
     * @param a Map containing application variable information
     */
  
  public void updateApplicationVariablesMap(Map<String, String[]> map) throws MBeanException;

  public int countVariables();

   /*---------------------------------------------------------------------------------*\
   *          Operations for Application Variables Management                        *
  \*---------------------------------------------------------------------------------*/
  
  /**
     * This operation adds a new applicationvariable. If a variable already exists with the same
     * name as that specified then the operation fails.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste
     * @throws MBeanException if an error occurs in adding the application variables to the
     *             component.
     */
   public void addApplicationVariable(String name, CompositeData appVar) throws MBeanException;
   
  /**
     * This operation sets an application variable. If a variable does not exist with the same name,
     * its an error.
     * 
     * @param name - name of the application variable
     * @param appVar - this is the application variable compoiste to be updated.
     * @throws MBeanException if one or more application variables cannot be deleted
     */
  public void setApplicationVariable(String name, CompositeData appVar) throws MBeanException; 
   
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
     * @return a TabularData which has all the applicationvariables set on the component.
     */
   public TabularData getApplicationVariables();
   
   /**
    * Retrieves the Configuration Display Schema 
    */   
   public String retrieveConfigurationDisplaySchema();

   /**
    * Retrieves the Configuration Display Data
    */
   public String retrieveConfigurationDisplayData();
    
}
