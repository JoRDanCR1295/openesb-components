 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi;

import java.util.Properties;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * JBIService Descriptor. Maps the configuratios that cen be read from the configuration WSDLs.
 *
 */
public class JbiServiceDescriptor {	

  public static final String CONSUMER = "consumer";
  public static final String PROVIDER = "provider";

  /**
   * This properties is a map from qualified service interface name to the
   * service name. The service name is used for the corba servant localizatione
   * and for the webservice creation.
   */
  private Properties serviceNames;
  
  //common
  private Properties orbProperties;

  private String localizationType;
  
  private String role;    
  
  private String idlFileNameDirectory;

  private String idlFileName;
  
  private String serviceNameSpace; 
   
  private String serviceName;  
  
  private String corbaServiceName;  
  
  // The portType Name
  private QName portTypeName;  
  
  private String wsdlURL;
  
  

  /**
   * Default constructor.
   */
  public JbiServiceDescriptor(){      
  }
  
  /**
   * @return  The return
   */
  public String getIdlFileName() {
    return idlFileName;
  }

  /**
   *@param idlFileName  The idl file name
   */
  public void setIdlFileName(String idlFileName) {
    this.idlFileName = idlFileName;
  }

  /**
   * @return  The return
   */
  public String getIdlFileNameDirectory() {
    return idlFileNameDirectory;
  }

  /**
   *@param idlFileNameDirectory  The idl file name directory
   */
  public void setIdlFileNameDirectory(String idlFileNameDirectory) {
    this.idlFileNameDirectory = idlFileNameDirectory;
  }

  /**
   * @return  The return
   */
  public String getLocalizationType() {
    return localizationType;
  }

  /**
   *@param localizationType  The localization type
   */
  public void setLocalizationType(String localizationType) {
    this.localizationType = localizationType;
  }

  /**
   * @return  The return
   */
  public Properties getOrbProperties() {
    return orbProperties;
  }

  /**
   *@param orbProperties  The orb properties
   */
  public void setOrbProperties(Properties orbProperties) {
    this.orbProperties = orbProperties;
  }

  /**
   * @return  The return
   */
  public Properties getServiceNames() {
    return serviceNames;
  }

  /**
   *@param serviceNames  The service names
   */
  public void setServiceNames(Properties serviceNames) {
    this.serviceNames = serviceNames;
  }
  /**
   * @param name  The name
   * @return      The return
   */
  public String getServiceName(String name) {
    String serviceName=serviceNames.getProperty(name);    
    return serviceName;
  }

  /**
   * @return  The return
   */
  public String getServiceNameSpace() {
    return serviceNameSpace;
  }

  /**
   *@param serviceNameSpace  The service names space
   */
  public void setServiceNameSpace(String serviceNameSpace) {
    this.serviceNameSpace = serviceNameSpace;
  }

  /**
   * @return  The return
   */
  public String getRole() {
    return role;
  }

  /**
   *@param role  The role
   */
  public void setRole(String role) {
    this.role = role;
  }

  /**
   * @return  The return
   */
  public String getServiceName() {
    return serviceName;
  }

  /**
   *@param serviceName  The service names
   */
  public void setServiceName(String serviceName) {
    this.serviceName = serviceName;
  }
  
  /**
   * @return  The return
   */
  public String toString() {
    return ReflectionToStringBuilder.toString(this);
  }
  
  /**
   * @param obj  The object
   * @return     The return
   */
  public boolean equals(Object obj) {
    return EqualsBuilder.reflectionEquals(this, obj);
  }
  
  /**
   * @return     The return
   */
  
  /* DO NOT IMPLEMENT THIS METHOD 
   * THE IMPLEMENTATION  DO NOT PERMIT THE CORRECT DEPLOY OF CONSUMER
   */
  /****************************************************
  
  public int hashCode() {
      return HashCodeBuilder.reflectionHashCode(this);
	  
  }  
  ******************************************************/

  /**
   * @return  The return
   */
  public String getCorbaServiceName() {
      return corbaServiceName;
  }

  /**
   *@param corbaServiceName  The corba service name
   */
  public void setCorbaServiceName(String corbaServiceName) {
      this.corbaServiceName = corbaServiceName;
  } 
 
  public QName getPortTypeName() {
      return portTypeName;
  }

  public void setPortTypeName(QName portTypeName) {
      this.portTypeName = portTypeName;
  }

  public String getWsdlURL() {
      return wsdlURL;
  }

  public void setWsdlURL(String wsdlURL) {
      this.wsdlURL = wsdlURL;
  }

  

}
