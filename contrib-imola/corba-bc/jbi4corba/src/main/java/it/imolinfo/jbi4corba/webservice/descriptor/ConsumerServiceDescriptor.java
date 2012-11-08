 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.descriptor;

import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.endpoint.ConsumerEndpoint;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.runtime.ConsumerInvocationHandler;
import it.imolinfo.jbi4corba.webservice.runtime.RuntimeInformation;

import java.util.Properties;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.Servant;

/**
 * @author raffaele
 *
 */
public class ConsumerServiceDescriptor {
  //configuration
  private long timeout;
  private String rootPath;
  private String componentRootPath;
  
    //proxied endpoint
  private String targetEndpoint;
  private QName targetService;
  private QName targetInterfaceName;
  
    //corba
  private Properties orbProperties;
  private String serviceName;      
  private String localizationType;
  private String corbaServiceName;

  //runtime
  private ConsumerInvocationHandler consumerInvocationHandler; 
  private String wsdlFileName;
  private ConsumerEndpoint endpoint;  
  private RuntimeInformation runtimeInformation;
  
    //proxied endpoint
  private ServiceEndpoint proxiedService;
  
    //corba
  private ServerCorbaClassesHolder serverCorbaClassesHolder;  
  private ORB orb;
  private Servant poaTie;     
  
  private Definition serviceWSDLDefinition;
  private JbiServiceDescriptor jbiServiceDescriptor;

  /**
   * Default constructor.
   */
  public ConsumerServiceDescriptor(){  
  }
  
 /**
  * @return  The return
  */
  public ServerCorbaClassesHolder getServerCorbaClassesHolder() {
    return serverCorbaClassesHolder;
  }

  /**
   * 
   * @param serverCorbaClassesHolder  The server corba classes holder
   */
  public void setServerCorbaClassesHolder(ServerCorbaClassesHolder serverCorbaClassesHolder) {
    this.serverCorbaClassesHolder = serverCorbaClassesHolder;
  }

  /**
   * 
   * @return  The return
   */
  public Properties getOrbProperties() {
    return orbProperties;
  }

  /**
   * 
   * @param orbProperties  The orb properties
   */
  public void setOrbProperties(Properties orbProperties) {
    this.orbProperties = orbProperties;
  }

  /**
   * 
   * @return  The return
   */
  public ORB getOrb() {
    return orb;
  }

  /**
   * 
   * @param orb  The orb
   */
  public void setOrb(ORB orb) {
    this.orb = orb;
  }

  /**
   * 
   * @return  The return
   */
  public Servant getPoaTie() {
    return poaTie;
  }

  /**
   * 
   * @param poaTie  The servant
   */
  public void setPoaTie(Servant poaTie) {
    this.poaTie = poaTie;
  }

  /**
   * 
   * @return  The return
   */
  public ConsumerInvocationHandler getConsumerInvocationHandler() {
    return consumerInvocationHandler;
  }

  /**
   * 
   * @param consumerInvocationHandler  The consumer invocation handler
   */
  public void setConsumerInvocationHandler(ConsumerInvocationHandler consumerInvocationHandler) {
    this.consumerInvocationHandler = consumerInvocationHandler;
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
   * @return  The return
   */
  public String getServiceName() {
    return serviceName;
  }

  /**
   * 
   * @param serviceName  The service name
   */
  public void setServiceName(String serviceName) {
    this.serviceName = serviceName;
  }

  /**
   * @return  The return
   */
  public ServiceEndpoint getProxiedService() {
    return proxiedService;
  }

  /**
   * 
   * @param proxiedService  The proxied service
   */
  public void setProxiedService(ServiceEndpoint proxiedService) {
    this.proxiedService = proxiedService;
  }

  /**
   * @return  The return
   */
  public String getTargetEndpoint() {
    return targetEndpoint;
  }

  /**
   * 
   * @param targetEndpoint  The target endpoint
   */
  public void setTargetEndpoint(String targetEndpoint) {
    this.targetEndpoint = targetEndpoint;
  }

  /**
   * @return  The return
   */
  public QName getTargetInterfaceName() {
    return targetInterfaceName;
  }

  /**
   * 
   * @param targetInterfaceName  The target interface name
   */
  public void setTargetInterfaceName(QName targetInterfaceName) {
    this.targetInterfaceName = targetInterfaceName;
  }

  /**
   * @return  The return
   */
  public QName getTargetService() {
    return targetService;
  }

  /**
   * 
   * @param targetService  The target service
   */
  public void setTargetService(QName targetService) {
    this.targetService = targetService;
  }

  /**
   * @return  The return
   */
  public String getWsdlFileName() {
    return wsdlFileName;
  }

  /**
   * 
   * @param wsdlFileName  The wsdl file name
   */
  public void setWsdlFileName(String wsdlFileName) {
    this.wsdlFileName = wsdlFileName;
  }

  /**
   * @return  The return
   */
  public ConsumerEndpoint getEndpoint() {
    return endpoint;
  }

  /**
   * 
   * @param endpoint  The end point
   */
  public void setEndpoint(ConsumerEndpoint endpoint) {
    this.endpoint = endpoint;
  }

  /**
   * @return  The return
   */
  public long getTimeout() {
    return timeout;
  }

  /**
   * 
   * @param timeout  The timeout
   */
  public void setTimeout(long timeout) {
    this.timeout = timeout;
  }

  /**
   * @return  The return
   */
  public String getRootPath() {
    return rootPath;
  }

  /**
   * 
   * @param rootPath  The root path
   */
  public void setRootPath(String rootPath) {
    this.rootPath = rootPath;
  }

  /**
   * @return  The return
   */
  public String getLocalizationType() {
    return localizationType;
  }

  /**
   * 
   * @param localizationType  The localization type
   */
  public void setLocalizationType(String localizationType) {
    this.localizationType = localizationType;
  }

  /**
   * @return  The return
   */
  public String getComponentRootPath() {
    return componentRootPath;
  }
  
  /**
   * 
   * @param componentRootPath  The component root path
   */
  public void setComponentRootPath(String componentRootPath) {
    this.componentRootPath = componentRootPath;
  }

  /**
   * @return  The return
   */
  public String getCorbaServiceName() {
      return corbaServiceName;
  }

  /**
   * 
   * @param corbaServiceName  The corba service name
   */
  public void setCorbaServiceName(String corbaServiceName) {
      this.corbaServiceName = corbaServiceName;
  } 
  
  /**
   * @return  The return
   */
  public Definition getServiceWSDLDefinition() {
      return serviceWSDLDefinition;
  }

  /**
   * 
   * @param serviceWSDLDefinition  The service WSDL definition
   */
  public void setServiceWSDLDefinition(Definition serviceWSDLDefinition) {
      this.serviceWSDLDefinition = serviceWSDLDefinition;
  }  
  
  /**
   * @return  The return
   */
  public JbiServiceDescriptor getJbiServiceDescriptor() {
	  return jbiServiceDescriptor;
  }

  /**
   * 
   * @param jbiServiceDescriptor  Maps the configurations read from the WSDL
   */    
  public void setJbiServiceDescriptor(JbiServiceDescriptor jbiServiceDescriptor) {
	  this.jbiServiceDescriptor = jbiServiceDescriptor;
  }  
 
  public RuntimeInformation getRuntimeInformation() {
	  return runtimeInformation;
  }

  public void setRuntimeInformation(RuntimeInformation runtimeInformation) {
	  this.runtimeInformation = runtimeInformation;
  }
}
