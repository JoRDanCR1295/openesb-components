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

import it.imolinfo.jbi4cics.configuration.ConfigurationManager;
import it.imolinfo.jbi4cics.connection.ConnectionManager;
import it.imolinfo.jbi4cics.connection.ConnectionManagerFactory;
import it.imolinfo.jbi4cics.exception.Jbi4cicsException;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.locator.ServiceLocator;
import it.imolinfo.jbi4cics.locator.ServiceLocatorFactory;
import it.imolinfo.jbi4cics.messageformat.MessageFormatter;
import it.imolinfo.jbi4cics.messageformat.MessageFormatterFactory;
import it.imolinfo.jbi4cics.security.Account;
import it.imolinfo.jbi4cics.security.SecurityManager;
import it.imolinfo.jbi4cics.security.SecurityManagerFactory;

/**
 * @author raffaele
 *
 */
public class ServiceManager {
	
	/**
	 * void constructor.
	 */
	  public ServiceManager(){
	  }
	
	public Object handleService(String name, Object input) throws Jbi4cicsException{
		
		ConfigurationManager configurationManager=ConfigurationManager.getConfigurationManager();
		
		// creo il service context che conterra lo stato del servizio per tutto
		// passando al configuration manager il nome del servizio e il bean di input
		ServiceContext serviceContext=configurationManager.createServiceContext(name,input);
		
		//creo il localizzatore per questo servizio servizio mediante la factory
		ServiceLocator serviceLocator=ServiceLocatorFactory.createLocator(serviceContext);
		
		//localizzo il servizio
		ServiceLocation serviceLocation=serviceLocator.locateService(serviceContext);
		
		//memorizzo la service locaion nel service context
		serviceContext.setServiceLocation(serviceLocation);
		
		//creo il security manager per questo servizio mediante la factory
		SecurityManager securityManager=SecurityManagerFactory.createSecurityManager(serviceContext);
		
		//ottengo un account valido per questo servizio
		Account account=securityManager.getAccount(serviceContext);
		
		//memorizzo l'account nel service context
		serviceContext.setAccount(account);
		
		//creo il message formatter per questo servizio mediante la factory
		MessageFormatter inputMessageFormatter=MessageFormatterFactory.createMessageFormatter(serviceContext,true);
		
		//mappo l'input bean nel messaggio di input alla risorsa legacy
    inputMessageFormatter.mapInputBeanToInputMessage(serviceContext);
		
		//creo il connection manager per questo servizio mediante la factory
		ConnectionManager connectionManager=ConnectionManagerFactory.createConnectionManager(serviceContext);
		
		//eseguo la chiamata ottenendo il messaggio di output
		connectionManager.handleCall(serviceContext);		
    
    //creo il message formatter per questo servizio mediante la factory
    MessageFormatter outputMessageFormatter=MessageFormatterFactory.createMessageFormatter(serviceContext,false);
		
		//mappo il messaggio di output nell output Bean
    outputMessageFormatter.mapOutputMessageToOutputBean(serviceContext);
		
		//restituisco l'output bean
		return serviceContext.getOutputBean();
	}

}
