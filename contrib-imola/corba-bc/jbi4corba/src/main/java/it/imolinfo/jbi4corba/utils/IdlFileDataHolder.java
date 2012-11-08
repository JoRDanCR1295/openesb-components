 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.utils;



/**
 * Holder for Enpoint Reference information
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public class IdlFileDataHolder {

	/**
	 * List of Interface's Name
	 * 
	 */
	private String interfaceName;
	
	/**
	 * List of Namespace's 
	 **/
	private String interfaceNameSpaces;
	
	
	public String getInterfaceNameSpace() {
		return interfaceNameSpaces;
	}

	public void setInterfaceNameSpaces(String interfaceNameSpace) {
		this.interfaceNameSpaces = interfaceNameSpace;
	}

	public IdlFileDataHolder(){	
		
	}
	
	public IdlFileDataHolder(String name,String nameSpace){	
		this.interfaceName=name;
		this.interfaceNameSpaces=nameSpace;
	}

	public void setInterfaceName(String interfaceName) {
		this.interfaceName = interfaceName;
	}

	public String getInterfaceName() {
		return interfaceName;
	}
	
	
	
}
