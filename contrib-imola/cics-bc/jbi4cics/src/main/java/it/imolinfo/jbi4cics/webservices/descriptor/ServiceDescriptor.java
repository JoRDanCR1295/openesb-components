/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.webservices.descriptor;

import it.imolinfo.jbi4cics.connection.InteractionDescription;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;
import it.imolinfo.jbi4cics.security.Account;

public final class ServiceDescriptor {


  // parametri necessari all'esposizione webservice


  private String serviceName;
  private String operationName;
  private String serviceNameSpace;


  // parametri relativi all'implementazione java del webservice


  private String serviceInterfaceName;
  private String inputBeanClassName;
  private String outputBeanClassName;
  private String serviceInterfacePackageName;
  private Class serviceInterface;


  // parametri relativi alla esecuzione della chiamata del servizio legacy


  private MappingDescriptor inputMappingDescriptor;
  private MappingDescriptor outputMappingDescriptor;
  private ServiceLocation serviceLocation;
  private Account account;
  private InteractionDescription interactionDescription;

  private String codePage;

  /**
   * Does nothing.
   */
  public ServiceDescriptor() {
  }

  /**
   * @return Returns the inputBean.
   */
  public Class getInputBean() {
    return inputMappingDescriptor.getBeanClass();
  }

  /**
   * @param inputBean The inputBean to set.
   */
  public void setInputBean(Class inputBean) {
    inputMappingDescriptor.setBeanClass(inputBean);
  }

  /**
   * @return Returns the operationName.
   */
  public String getOperationName() {
    return operationName;
  }

  /**
   * @param operationName The operationName to set.
   */
  public void setOperationName(String operationName) {
    this.operationName = operationName;
  }

  /**
   * @return Returns the outputBean.
   */
  public Class getOutputBean() {
    return outputMappingDescriptor.getBeanClass();
  }

  /**
   * @param outputBean The outputBean to set.
   */
  public void setOutputBean(Class outputBean) {
    outputMappingDescriptor.setBeanClass(outputBean);
  }

  /**
   * @return Returns the serviceName.
   */
  public String getServiceName() {
    return serviceName;
  }

  /**
   * @param serviceName The serviceName to set.
   */
  public void setServiceName(String serviceName) {
    this.serviceName = serviceName;
  }

  /**
   * @return Returns the account.
   */
  public Account getAccount() {
    return account;
  }

  /**
   * @param account The account to set.
   */
  public void setAccount(Account account) {
    this.account = account;
  }

  /**
   * @return Returns the inputMappingDescriptor.
   */
  public MappingDescriptor getInputMappingDescriptor() {
    return inputMappingDescriptor;
  }

  /**
   * @param inputMappingDescriptor The inputMappingDescriptor to set.
   */
  public void setInputMappingDescriptor(MappingDescriptor inputMappingDescriptor) {
    this.inputMappingDescriptor = inputMappingDescriptor;
  }

  /**
   * @return Returns the interactionDescription.
   */
  public InteractionDescription getInteractionDescription() {
    return interactionDescription;
  }

  /**
   * @param interactionDescription The interactionDescription to set.
   */
  public void setInteractionDescription(InteractionDescription interactionDescription) {
    this.interactionDescription = interactionDescription;
  }

  /**
   * @return Returns the outputMappingDescriptor.
   */
  public MappingDescriptor getOutputMappingDescriptor() {
    return outputMappingDescriptor;
  }

  /**
   * @param outputMappingDescriptor The outputMappingDescriptor to set.
   */
  public void setOutputMappingDescriptor(MappingDescriptor outputMappingDescriptor) {
    this.outputMappingDescriptor = outputMappingDescriptor;
  }

  /**
   * @return Returns the serviceLocation.
   */
  public ServiceLocation getServiceLocation() {
    return serviceLocation;
  }

  /**
   * @param serviceLocation The serviceLocation to set.
   */
  public void setServiceLocation(ServiceLocation serviceLocation) {
    this.serviceLocation = serviceLocation;
  }

  /**
   * @return Returns the serviceInterface.
   */
  public Class getServiceInterface() {
    return serviceInterface;
  }

  /**
   * @param serviceInterface The serviceInterface to set.
   */
  public void setServiceInterface(Class serviceInterface) {
    this.serviceInterface = serviceInterface;
  }

  /**
   * @return Returns the serviceInterfaceName.
   */
  public String getServiceInterfaceName() {
    return serviceInterfaceName;
  }

  /**
   * @param serviceInterfaceName The serviceInterfaceName to set.
   */
  public void setServiceInterfaceName(String serviceInterfaceName) {
    this.serviceInterfaceName = serviceInterfaceName;
  }

  /**
   * @return Returns the serviceInterfacePackageName.
   */
  public String getServiceInterfacePackageName() {
    return serviceInterfacePackageName;
  }

  /**
   * @param serviceInterfacePackageName The serviceInterfacePackageName to set.
   */
  public void setServiceInterfacePackageName(String serviceInterfacePackageName) {
    this.serviceInterfacePackageName = serviceInterfacePackageName;
  }

  /**
   * @return Returns the serviceNameSpace.
   */
  public String getServiceNameSpace() {
    return serviceNameSpace;
  }

  /**
   * @param serviceNameSpace The serviceNameSpace to set.
   */
  public void setServiceNameSpace(String serviceNameSpace) {
    this.serviceNameSpace = serviceNameSpace;
  }

  /**
   * @return Returns the inputBeanClassName.
   */
  public String getInputBeanClassName() {
    return inputBeanClassName;
  }

  /**
   * @param inputBeanClassName The inputBeanClassName to set.
   */
  public void setInputBeanClassName(String inputBeanClassName) {
    this.inputBeanClassName = inputBeanClassName;
  }

  /**
   * @return Returns the outputBeanClassName.
   */
  public String getOutputBeanClassName() {
    return outputBeanClassName;
  }

  /**
   * @param outputBeanClassName The outputBeanClassName to set.
   */
  public void setOutputBeanClassName(String outputBeanClassName) {
    this.outputBeanClassName = outputBeanClassName;
  }

  public void setCodePage(String codePage){
    this.codePage=codePage;
    inputMappingDescriptor.setCodePage(codePage);
    outputMappingDescriptor.setCodePage(codePage);
  }

  public String getCodePage(){
    return codePage;
  }

  /**
   * Sets default values for fields currently <code>null</code>. Some default
   * values are calculated from the actual <i>serviceName</i>, so this method
   * must be called when <code>getServiceName()</code> returns a not
   * <code>null</code> value to avoid <code>NullPointerException</code>.
   *
   * @see #getServiceName()
   * @see #setServiceName(String)
   */
  public void setDefaultValuesIfNecessary() {
    if (getOperationName() == null) {
      setOperationName("execute");
    }
    if (getServiceInterfaceName() == null) {
      setServiceInterfaceName(getServiceName().concat("Interface"));
    }
    if (getInputBeanClassName() == null) {
      setInputBeanClassName(getServiceName().concat("InputBean"));
    }
    if (getOutputBeanClassName() == null) {
      setOutputBeanClassName(getServiceName().concat("OutputBean"));
    }
  }
}
