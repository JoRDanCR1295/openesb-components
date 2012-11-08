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
package it.imolinfo.jbi4cics.connection;

import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.service.ServiceContext;

/**
 * @author raffaele
 *
 */
public class EchoConnectionManager implements ConnectionManager {
	
	/**
	 * void constructor.
	 */
	  public EchoConnectionManager(){
      super();
	  }

	/* (non-Javadoc)
	 * @see it.imolinfo.jbi4cics.connection.ConnectionManager#handleCall(it.imolinfo.jbi4cics.service.ServiceContext)
	 */
	public void handleCall(ServiceContext serviceContext) throws ConnectionException {
    serviceContext.setOutputMessage(serviceContext.getInputMessage());
	}

}
