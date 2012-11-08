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
package it.imolinfo.jbi4cics.configuration;


import it.imolinfo.jbi4cics.service.ServiceContext;

/**
 * @author raffaele
 *
 */
public class DummyServiceContextFactory {
	
	/**
	 * void constructor.
	 */
	  public DummyServiceContextFactory(){
		  super();
	  }

	
	public static ServiceContext createServiceContext(String serviceName,Object inputBean){
		ServiceContext serviceContext=new ServiceContext();
		serviceContext.setInputBean(inputBean);
		serviceContext.setServiceName(serviceName);
		return serviceContext;
	}
}
