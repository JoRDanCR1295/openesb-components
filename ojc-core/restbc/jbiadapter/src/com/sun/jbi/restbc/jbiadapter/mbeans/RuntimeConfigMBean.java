package com.sun.jbi.restbc.jbiadapter.mbeans;

import java.util.Map;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;

/**
 * RuntimeConfigIntf.java
 *
 * @author Edward Chou
 */
public interface RuntimeConfigMBean {

    public static final String NMR_THREAD_POOL_SIZE = "NmrThreadPoolSize";
    public static final String NMR_MAX_THREAD_POOL_SIZE = "NmrMaxThreadPoolSize";
    public static final String DEFAULT_HTTP_LISTENER_PORT = "DefaultHttpListenerPort";
    public static final String DEFAULT_HTTP_LISTENER_THREADS = "DefaultHttpListenerThreads";
    public static final String DEFAULT_HTTPS_LISTENER_PORT = "DefaultHttpsListenerPort";
    public static final String DEFAULT_HTTPS_LISTENER_THREADS = "DefaultHttpsListenerThreads";
    public static final String TRUSTSTORE_PASSWORD = "TruststorePassword";
    public static final String KEYSTORE_PASSWORD = "KeystorePassword";
    public static final String ENABLE_HOSTNAME_VERIFIER = "EnableHostnameVerifier";
    
    // Appliation configurations and application variables
    public static final String CONFIG_APPLICATON_VARIABLES = "ApplicationVariables";
    public static final String CONFIG_APPLICATION_CONFIGURATIONS = "ApplicationConfigurations";
    
    
    public Integer getNmrThreadPoolSize();
    public void setNmrThreadPoolSize(Integer nmrThreadPoolSize);

    public Integer getNmrMaxThreadPoolSize();
    public void setNmrMaxThreadPoolSize(Integer nmrMaxThreadPoolSize);
    
    public Integer getDefaultHttpListenerPort();
    public void setDefaultHttpListenerPort(Integer defaultHttpListenerPort);
    
    public Integer getDefaultHttpListenerThreads();
    public void setDefaultHttpListenerThreads(Integer defaultHttpListenerThreads);
    
    public Integer getDefaultHttpsListenerPort();
    public void setDefaultHttpsListenerPort(Integer defaultHttpsListenerPort);
    
    public Integer getDefaultHttpsListenerThreads();
    public void setDefaultHttpsListenerThreads(Integer defaultHttpsListenerThreads);
    
    public String getTruststorePassword();
    public void setTruststorePassword(String truststorePassword);
    
    public Boolean isEnableHostnameVerifier();
    public void setEnableHostnameVerifier(Boolean enableHostnameVerifier);
    
    public String getKeystorePassword();
    public void setKeystorePassword(String keystorePassword);
    
    

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
}
