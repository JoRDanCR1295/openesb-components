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
public class ConfigurationManager {
	
	private static ConfigurationManager theConfigurationManager;
	
	/**
	 * void constructor.
	 */
	  public ConfigurationManager(){
		  super();
	  }
	
	// implementazione vanilla di un singleton
	public static synchronized ConfigurationManager getConfigurationManager(){
		if (theConfigurationManager==null) {
			theConfigurationManager=new ConfigurationManager();
		}
		return theConfigurationManager;
	}
	
	public ServiceContext createServiceContext(String serviceName,Object input){
		return DummyServiceContextFactory.createServiceContext(serviceName,input);
	}

}
