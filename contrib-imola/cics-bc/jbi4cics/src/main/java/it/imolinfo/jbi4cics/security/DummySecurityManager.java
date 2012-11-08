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
package it.imolinfo.jbi4cics.security;

import it.imolinfo.jbi4cics.service.ServiceContext;

/**
 * @author raffaele
 *
 */
public class DummySecurityManager implements SecurityManager {
	
	/**
	 * void constructor.
	 */
	  public DummySecurityManager(){
		  super();
	  }

	/* (non-Javadoc)
	 * @see it.imolinfo.jbi4cics.security.SecurityManager#getAccount(it.imolinfo.jbi4cics.service.ServiceContext)
	 */
	public Account getAccount(ServiceContext serviceContext) {
		return new Account(){
			public String getUsername(){
				return "Dummy";
			}
			public String getPassword(){
				return "Dummy";
			}
		};
	}

}
