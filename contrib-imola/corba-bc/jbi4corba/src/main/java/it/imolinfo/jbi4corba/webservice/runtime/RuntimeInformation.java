 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;
import it.imolinfo.jbi4corba.webservice.generator.SearchedType;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;

import java.net.URLClassLoader;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.omg.CORBA.ORB;

/**
 * RuntimeInformation - class enclosing runtime needed information
 * @author lghetu
 *
 */
public class RuntimeInformation {

	private URLClassLoader modifiedClassLoader;
	private URLClassLoader originalClassLoader;
	private Map<String, SearchedType> allCorbaTypes;
	private Jbi4CorbaEndpoint jbi4CorbaEndpoint;
	private ORB orb;
	private Set<Class> allIDLTypes;
	private Map<String, List<String>> allEnumTypes;
    private Map<String,String> idTOClassNameMap;
    private Map<String,TypeDef> typeDefs;
    
    /**
     * Ctor
     * 
     * @param serviceClassLoader
     * @param corbaClassLoader
     * @param allCorbaTypes
     * @param jbi4CorbaEndpoint
     * @param orb
     * @param allIDLTypes
     * @param allEnumTypes
     */
	public RuntimeInformation(URLClassLoader serviceClassLoader,
			URLClassLoader corbaClassLoader,
			Map<String, SearchedType> allCorbaTypes,
			Jbi4CorbaEndpoint jbi4CorbaEndpoint, ORB orb,
			Set<Class> allIDLTypes, Map<String, List<String>> allEnumTypes,Map<String,String> idTOClassMap,Map<String,TypeDef> typeDefs
			) {
		super();
		this.modifiedClassLoader = serviceClassLoader;
		this.originalClassLoader = corbaClassLoader;
		this.allCorbaTypes = allCorbaTypes;
		this.jbi4CorbaEndpoint = jbi4CorbaEndpoint;
		this.orb = orb;
		this.allIDLTypes = allIDLTypes;
		this.allEnumTypes = allEnumTypes;
		this.idTOClassNameMap=idTOClassMap;
		this.typeDefs = typeDefs;
	}

	public URLClassLoader getServiceClassLoader() {
		return modifiedClassLoader;
	}

	public URLClassLoader getCorbaClassLoader() {
		return originalClassLoader;
	}

	public Map<String, SearchedType> getAllCorbaTypes() {
		return allCorbaTypes;
	}

	public Jbi4CorbaEndpoint getJbi4CorbaEndpoint() {
		return jbi4CorbaEndpoint;
	}

	public ORB getOrb() {
		return orb;
	}

	public Set<Class> getAllIDLTypes() {
		return allIDLTypes;
	}

	public Map<String, List<String>> getAllEnumTypes() {
		return allEnumTypes;
	}

	public Map<String, String> getIdTOClassNameMap() {
        return idTOClassNameMap;
    }

	public Map<String, TypeDef> getTypeDefs() {
		return typeDefs;
	}
	
	
    
}
