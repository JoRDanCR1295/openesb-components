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
package it.imolinfo.jbi4cics.locator;

import it.imolinfo.jbi4cics.service.ServiceContext;

/**
 * @author raffaele
 *
 */
public class DummyLocator implements ServiceLocator {
	
	/**
	 * void constructor.
	 */
	  public DummyLocator(){
		  super();
	  }
	
	/* (non-Javadoc)
	 * @see it.imolinfo.jbi4cics.locator.ServiceLocator#locateService(it.imolinfo.jbi4cics.service.ServiceContext)
	 */
	public ServiceLocation locateService(ServiceContext serviceContext) {
		return new ServiceLocation(){
			public String getLocationName(){
				return "Dummy";
			}
			public int getConnectionType(){
				return ServiceLocation.DUMMY;
			}
		};
	}
	

}
