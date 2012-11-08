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

package com.sun.jbi.httpsoapbc.configuration;

import java.util.Map;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;

/**
 * MBean interface for run-time configuration
 */
public interface RuntimeConfigurationMBean {
    // Attribute names
    public static final String CONFIG_OUTBOUND_THREADS = "OutboundThreads";
    public static final String CONFIG_INBOUND_THREADS = "InboundThreads";
    public static final String CONFIG_INBOUND_REPLY_THREADS = "InboundReplyThreads";
    public static final String CONFIG_HTTP_DEFAULT_PORT = "HttpDefaultPort";
    public static final String CONFIG_HTTPS_DEFAULT_PORT = "HttpsDefaultPort";
    public static final String CONFIG_CLIENT_AUTH_ENABLED = "ClientAuthEnabled";
    
    // Directory containing the Access Manager client configuration props file
    public static final String CONFIG_ACCESS_MANAGER_CONFIG_DIR = "AMConfigDirectory";
    
    public static final String CONFIG_ACCESS_MANAGER_CLASSPATH = "AMClasspath";
    
    // Proxy setting configurations
    public static final String CONFIG_USE_JVM_PROXY_SETTINGS = "UseJVMProxySettings";
    public static final String CONFIG_PROXY_TYPE = "ProxyType";
    public static final String CONFIG_PROXY_HOST = "ProxyHost";
    public static final String CONFIG_PROXY_PORT = "ProxyPort";
    public static final String CONFIG_NON_PROXY_HOSTS = "NonProxyHosts";
    public static final String CONFIG_PROXY_USER_NAME = "ProxyUserName";
    public static final String CONFIG_PROXY_PASSWORD = "ProxyPassword";
    
    // List of valid hostnames
    public static final String CONFIG_WS_PROVIDER_HOSTNAMES = "ValidHostnames";
    
    // Appliation configurations and application variables
    public static final String CONFIG_APPLICATON_VARIABLES = "ApplicationVariables";
    public static final String CONFIG_APPLICATION_CONFIGURATIONS = "ApplicationConfigurations";

    
    
    /**
      * Get the number of outbound threads
      *
      * @return an Integer indicating the number of outbound threads
      */
    public Integer getOutboundThreads();
    
    /** Set the number of outbound threads
      *
      * @param an Integer indicating the number of outbound threads
      * @throws InvalidAttributeValueException if the value is not a valid integer
      * @throws MBeanAttribute for other errors
      */
    public void setOutboundThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
    
    
    /**
     * Get the number of inbound threads
     *
     * @return an Integer indicating the number of inbound threads
     */
   public Integer getInboundThreads();
   
   /** Set the number of inbound threads
     *
     * @param an Integer indicating the number of inbound threads
     * @throws InvalidAttributeValueException if the value is not a valid integer
     * @throws MBeanAttribute for other errors
     */
   public void setInboundThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
   
   
   /**
    * Get the number of inboundReply threads
    *
    * @return an Integer indicating the number of inboundReply threads
    */
  public Integer getInboundReplyThreads();
  
  /** Set the number of inboundReply threads
    *
    * @param an Integer indicating the number of inboundReply threads
    * @throws InvalidAttributeValueException if the value is not a valid integer
    * @throws MBeanAttribute for other errors
    */
  public void setInboundReplyThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
  
    /**
      * Get the default HTTP port number
      *
      * @return an Integer indicating the default HTTP port number
      */
    public Integer getHttpDefaultPort();
    
    /** Set the default HTTP port number
      *
      * @param an Integer indicating the default HTTP port number
      * @throws InvalidAttributeValueException if the value is not a valid integer
      * @throws MBeanAttribute for other errors
      */
    public void setHttpDefaultPort(Integer val) throws InvalidAttributeValueException, MBeanException;
    
     /**
      * Get the default HTTPS port number
      *
      * @return an Integer indicating the default HTTPS port number
      */
    public Integer getHttpsDefaultPort();
    
    /** Set the default HTTPS port number
      *
      * @param an Integer indicating the default HTTPS port number
      * @throws InvalidAttributeValueException if the value is not a valid integer
      * @throws MBeanAttribute for other errors
      */
    public void setHttpsDefaultPort(Integer val) throws InvalidAttributeValueException, MBeanException;
    
    /**
      * Get the flag indicating whether or not client authentication is enabled
      *
      * @return a Boolean indicating whether or not client authentication is enabled
      */
    public Boolean getClientAuthEnabled();
    
    /** Set the flag indicating whether or not client authentication is enabled
      *
      * @param a Boolean indicating whether or not client authentication is enabled
      */
    public void setClientAuthEnabled(Boolean val) throws InvalidAttributeValueException, MBeanException;
    
    /**
      * Get the flag for JVM proxy setting
      *
      * @return a Boolean flag for the JVM proxy setting
      */
    public Boolean getUseJVMProxySettings();
    
    /** Set the flag for JVM proxy setting
      *
      * @param a Boolean for the JVM proxy setting
      * @throws InvalidAttributeValueException if the value is not a valid boolean value
      * @throws MBeanAttribute for other errors
      */
    public void setUseJVMProxySettings(Boolean val) throws InvalidAttributeValueException, MBeanException;
    
    /**
      * Get the proxy type
      *
      * @return the proxy type setting
      */
    public String getProxyType();
    
    /** Set the proxy type
      *
      * @param a String indicating the proxy type
      * @throws MBeanAttribute for errors
      */
    public void setProxyType(String val) throws MBeanException;
    
    /**
      * Get the proxy host 
      *
      * @return the proxy host setting
      */
    public String getProxyHost();
    
    /** Set the proxy host
      *
      * @param a String indicating the proxy host
      * @throws MBeanAttribute for errors
      */
    public void setProxyHost(String val) throws MBeanException;
    
    /**
      * Get the proxy port number
      *
      * @return an Integer indicating the proxy port number
      */
    public Integer getProxyPort();
    
    /** Set the proxy port number
      *
      * @param an Integer indicating the proxy port number
      * @throws InvalidAttributeValueException if the value is not a valid integer
      * @throws MBeanAttribute for other errors
      */
    public void setProxyPort(Integer val) throws InvalidAttributeValueException, MBeanException;
    
    /**
      * Get the names of non-proxy host names
      *
      * @return a String indicating all non-proxy host names
      */
    public String getNonProxyHosts();
    
    /** Set the names of non-proxy host names
      *
      * @param a String indicating all non-proxy host names
      * @throws MBeanAttribute for errors
      */
    public void setNonProxyHosts(String val) throws MBeanException;
    
    /**
      * Get the proxy user name
      *
      * @return a String indicating the proxy user name
      */
    public String getProxyUserName();
    
    /** Set the proxy user name
      *
      * @param a String indicating the proxy user name
      * @throws MBeanAttribute for errors
      */
    public void setProxyUserName(String val) throws MBeanException;
    
    /** Retrieves the proxy password.
      * This method is intended to communicate proxy password within 
      * the component and not intended for MBean clients
      *
      * returns a String for the proxy password;
      */
    public String retrieveProxyPassword(Object obj) throws MBeanException;
    
    /**
      * Get the proxy password. For security purposes, this method
      * always returns ******* if the proxy password is set.
      *
      * @return a String for the masked proxy password
      */
    public String getProxyPassword();
    
    /** Set the proxy password
      *
      * @param a String indicating the proxy password
      * @throws MBeanAttribute for errors
      */
    public void setProxyPassword(String val) throws MBeanException;
    
    /**
     * Retrieves the AM config directory for Access Manager authentication
     *  @return The path to the AM config directory.
     */
     public String getAMConfigDirectory();


    /**
     * Sets the AM config directory for Access Manager authentication
     * @param amConfigDir The path to the AM config directory.
     * @throws InvalidAttributeValueException if the value is invalid
     * @throws MBeanException if the operation failed due to an MBean error
     */
     public void setAMConfigDirectory(String amConfigDir) throws InvalidAttributeValueException, MBeanException ;
    
     
     /**
      * Retrieves the am classpath, set of jar files 
      *  @return path representing am client sdk jar files .
      */
      public String getAMClasspath();


     /**
      * Sets the path representing am client sdk jar files
      * @param amClasspath path representing am client sdk jar files.
      * @throws InvalidAttributeValueException if the value is invalid
      * @throws MBeanException if the operation failed due to an MBean error
      */
      public void setAMClasspath(String amClasspath) throws InvalidAttributeValueException, MBeanException ;
      
      
      /**
      * Retrieves the comma delimited host names
      *  @return a String representing the comma delimited hostnames to be allowed
      */
      public String getValidHostnames();


     /**
      * Sets the comma delimited list of hostnames to be allowed for hostname validation
      * @param hostnames the comma delimited list of hostnames
      * @throws MBeanException if the operation failed due to an MBean error
      */
      public void setValidHostnames(String hostnames) throws MBeanException ;
     
     
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
