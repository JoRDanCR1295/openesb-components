/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.service;

import it.imolinfo.jbi4cics.connection.InteractionDescription;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;
import it.imolinfo.jbi4cics.security.Account;

/**
 * @author raffaele
 *
 */
public class ServiceContext {
	private Object inputBean;
	private Object outputBean;
	private String ServiceName;
	private ServiceLocation serviceLocation;
	private Account account;
	private Object inputMessage;
	private Object outputMessage;
  private MappingDescriptor inputMappingDescriptor;
  private MappingDescriptor outputMappingDescriptor;
  private InteractionDescription interactionDescription;
  
  /**
   * void constructor.
   */
    public ServiceContext(){
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
   * @return Returns the inputMappingDescriptor.
   */
  public MappingDescriptor getInputMappingDescriptor() {
    return inputMappingDescriptor;
  }

  /**
   * @param mappingDescriptor The MappingDescriptor to set.
   */
  public void setInputMappingDescriptor(MappingDescriptor mappingDescriptor) {
    this.inputMappingDescriptor = mappingDescriptor;
  }

  /**
	 * @return Returns the inputBean.
	 */
	public Object getInputBean() {
		return inputBean;
	}
	
	/**
	 * @param inputBean The inputBean to set.
	 */
	public void setInputBean(Object inputBean) {
		this.inputBean = inputBean;
	}
	
	/**
	 * @return Returns the outputBean.
	 */
	public Object getOutputBean() {
		return outputBean;
	}
	
	/**
	 * @param outputBean The outputBean to set.
	 */
	public void setOutputBean(Object outputBean) {
		this.outputBean = outputBean;
	}

	/**
	 * @return Returns the serviceName.
	 */
	public String getServiceName() {
		return ServiceName;
	}
	

	/**
	 * @param serviceName The serviceName to set.
	 */
	public void setServiceName(String serviceName) {
		ServiceName = serviceName;
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
	 * @return Returns the inputMessage.
	 */
	public Object getInputMessage() {
		return inputMessage;
	}
	

	/**
	 * @param inputMessage The inputMessage to set.
	 */
	public void setInputMessage(Object inputMessage) {
		this.inputMessage = inputMessage;
	}
	

	/**
	 * @return Returns the outputMessage.
	 */
	public Object getOutputMessage() {
		return outputMessage;
	}
	

	/**
	 * @param outputMessage The outputMessage to set.
	 */
	public void setOutputMessage(Object outputMessage) {
		this.outputMessage = outputMessage;
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
	
	
	
	
	
	

}
