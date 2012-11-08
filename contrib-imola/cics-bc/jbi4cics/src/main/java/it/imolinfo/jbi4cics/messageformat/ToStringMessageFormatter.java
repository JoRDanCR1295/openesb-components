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
package it.imolinfo.jbi4cics.messageformat;

import it.imolinfo.jbi4cics.service.ServiceContext;


/**
 * @author raffaele
 *
 */
public class ToStringMessageFormatter implements MessageFormatter {
	
	/**
	 * void constructor.
	 */
	  public ToStringMessageFormatter(){
	  }


	/* (non-Javadoc)
	 * @see it.imolinfo.jbi4cics.messageformat.MessageFormatter#mapInputBeanToInputMessage(java.lang.Object, it.imolinfo.jbi4cics.service.ServiceContext)
	 */
	public void mapInputBeanToInputMessage(ServiceContext serviceContext) {
    serviceContext.setInputMessage(serviceContext.getInputBean().toString());
	}

	/* (non-Javadoc)
	 * @see it.imolinfo.jbi4cics.messageformat.MessageFormatter#mapOutputMessageToOutputBean(java.lang.Object, it.imolinfo.jbi4cics.service.ServiceContext)
	 */
	public void mapOutputMessageToOutputBean(ServiceContext serviceContext) {
    serviceContext.setOutputBean(serviceContext.getOutputMessage().toString());		
	}



}
