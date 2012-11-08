 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;

import java.io.File;

import javax.wsdl.Definition;

import com.sun.jbi.management.descriptor.EndpointIdentifier;


/**
 * Jbi4Corba Binding Component. 
 * This class is a holder for deploy information
 *
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */

public class Jbi4CorbaSUInfo {
	
	/**
         * Endpoint Identifier
         */
        private EndpointIdentifier epIdentifier=null;
	
        /**
         * Endpoint Definition
         */
        private Definition matchedDef=null;
        
        /**
         * Endpoint WSDL File
         */
        private File matchedWSDL=null;
        
        /**
         * Endpoint JBIDesc
         */
        private JbiServiceDescriptor jbiSdesc=null;
    
	
	public Jbi4CorbaSUInfo(EndpointIdentifier epIdentifier,
			Definition matchedDef, File matchedWSDL,
			JbiServiceDescriptor jbiSdesc) {
		
		this.epIdentifier = epIdentifier;
		this.matchedDef = matchedDef;
		this.matchedWSDL = matchedWSDL;
		this.jbiSdesc = jbiSdesc;
	}


        /**
         * get Endpoint Definition
         */
	public EndpointIdentifier getEpIdentifier() {
		return epIdentifier;
	}

        /**
         * set Endpoint Definition
         */
	public void setEpIdentifier(EndpointIdentifier epIdentifier) {
		this.epIdentifier = epIdentifier;
	}

        
	public Definition getMatchedDef() {
		return matchedDef;
	}

	public void setMatchedDef(Definition matchedDef) {
		this.matchedDef = matchedDef;
	}

        /**
         * get Endpoint WSDL File
         */
	public File getMatchedWSDL() {
		return matchedWSDL;
	}

        /**
         *  set Endpoint WSDL File
         */
	public void setMatchedWSDL(File matchedWSDL) {
		this.matchedWSDL = matchedWSDL;
	}

	public JbiServiceDescriptor getJbiSdesc() {
		return jbiSdesc;
	}

	public void setJbiSdesc(JbiServiceDescriptor jbiSdesc) {
		this.jbiSdesc = jbiSdesc;
	}

}
