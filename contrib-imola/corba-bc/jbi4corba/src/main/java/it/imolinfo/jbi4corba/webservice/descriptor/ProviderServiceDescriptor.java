/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.descriptor;

import it.imolinfo.jbi4corba.jbi.endpoint.ProviderEndpoint;
import it.imolinfo.jbi4corba.webservice.generator.ChildFirstClassLoader;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.SearchedType;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;

import java.io.Serializable;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The provider service descriptor.
 */
public class ProviderServiceDescriptor implements Serializable {

	/** A type of localization. */
	public static final String NAMESERVICE = "NameService";

	/** A type of localization. */
	public static final String CORBALOC = "corbaloc";

	/** A type of localization. */
	public static final String CORBANAME = "corbaname";

	/** A type of localization. */
	public static final String IOR = "IOR";

	/** The SerialVersionUID. */
	private static final long serialVersionUID = -6630066280740249302L;

	// has a ...

	/** params used to expose the WebService. */
	private String serviceName;

	private String corbaServiceName;

	private String serviceNameSpace;

	private String wsdlRootDirectory;

	/** params used to define the corba service. */
	private Class serviceInterface;

	private Class corbaHelperClass;

	private Class corbaObjectInterface;

	/* the corba reference used to run the service. */
	private org.omg.CORBA.Object corbaObjectReference;

	/* The properties used to initialize the ORB. */
	private Properties orbProperties;

	/* This property defines how to find the corba service. */
	private String localizationType;

	private URLClassLoader urlClassLoader;

	private ChildFirstClassLoader originalClassLoader;

	private String componentRootPath;

	/* The port type name. */
	private QName portTypeName = null;

	/* Collects method Signature */
	private List<MethodSignature> methodSignatures = new ArrayList<MethodSignature>();

	/* Substituted union types mapping */
	private Map<String, Map<String, String>> substitutedUnionFields = new HashMap<String, Map<String, String>>();

	private Map<String, UnionType> allUniontypes = null;

	/* Substituted Interface types mapping */
	private Map<String, Map<String, String>> substitutedInterfaceFields = new HashMap<String, Map<String, String>>();

	private Map<String, InterfaceType> allInterfacetypes = null;

	private Map<String, TypeDef> typeDefs = null;

	/** The provider end point */
	private ProviderEndpoint endpoint = null;

	/**
	 * This map contains the pairs "valueTypeID, valueTypeFactoryInstance"
	 * registered in the ORB.
	 */
	private Map<String, Object> valueTypeIdAndInstance;

	/** store all types defined in IDL */
	private Set<Class> allIDLTypes = new HashSet<Class>();

	/** store all enums defined in IDL */
	Map<String, List<String>> corbaEnumMap = new HashMap<String, List<String>>();

	/** the ID to ClassName Map */
	private Map<String, String> idTOClassNameMap = new HashMap<String, String>();

	/**
	 * Constructor.
	 */
	public ProviderServiceDescriptor() {
		super();
	}

	/**
	 * @return Returns the serviceName.
	 */
	public String getServiceName() {
		return serviceName;
	}

	public void setIdToClassNameMap(Map<String, String> idToClassMap) {
		this.idTOClassNameMap = idToClassMap;
	}

	public Map<String, String> getIdToClassNameMap() {
		return idTOClassNameMap;
	}

	/**
	 * @param serviceName
	 *            The serviceName to set.
	 */
	public void setServiceName(String serviceName) {
		this.serviceName = serviceName;
	}

	/**
	 * @return Returns the serviceNameSpace.
	 */
	public String getServiceNameSpace() {
		return serviceNameSpace;
	}

	/**
	 * @param serviceNameSpace
	 *            The serviceNameSpace to set.
	 */
	public void setServiceNameSpace(String serviceNameSpace) {
		this.serviceNameSpace = serviceNameSpace;
	}

	/**
	 * 
	 * @return The return
	 */
	public org.omg.CORBA.Object getCorbaObjectReference() {
		return corbaObjectReference;
	}

	/**
	 * 
	 * @param corbaObjectReference
	 *            The corba object reference
	 */
	public void setCorbaObjectReference(
			org.omg.CORBA.Object corbaObjectReference) {
		this.corbaObjectReference = corbaObjectReference;
	}

	/**
	 * 
	 * @return The return
	 */
	public Class getServiceInterface() {
		return serviceInterface;
	}

	/**
	 * 
	 * @param serviceInterface
	 *            The service interface
	 */
	public void setServiceInterface(Class serviceInterface) {
		this.serviceInterface = serviceInterface;
	}

	/**
	 * 
	 * @return The return
	 */
	public Class getCorbaHelperClass() {
		return corbaHelperClass;
	}

	/**
	 * 
	 * @param corbaHelperClass
	 *            The corba helper class
	 */
	public void setCorbaHelperClass(Class corbaHelperClass) {
		this.corbaHelperClass = corbaHelperClass;
	}

	/**
	 * 
	 * @return The return
	 */
	public Class getCorbaObjectInterface() {
		return corbaObjectInterface;
	}

	/**
	 * 
	 * @param corbaObjectInterface
	 *            The corba object interface
	 */
	public void setCorbaObjectInterface(Class corbaObjectInterface) {
		this.corbaObjectInterface = corbaObjectInterface;
	}

	/**
	 * 
	 * @return The return
	 */
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}

	/**
	 * @param obj
	 *            The object
	 * @return The return
	 */
	public boolean equals(Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	/**
	 * 
	 * @return The return
	 */
	public String getLocalizationType() {
		return localizationType;
	}

	/**
	 * 
	 * @param localizationType
	 *            The localization type
	 */
	public void setLocalizationType(String localizationType) {
		this.localizationType = localizationType;
	}

	/**
	 * 
	 * @return The return
	 */
	public Properties getOrbProperties() {
		return orbProperties;
	}

	/**
	 * 
	 * @param orbProperties
	 *            The orb properties
	 */
	public void setOrbProperties(Properties orbProperties) {
		this.orbProperties = orbProperties;
	}

	/**
	 * 
	 * @return The return
	 */
	public String getWsdlRootDirectory() {
		return wsdlRootDirectory;
	}

	/**
	 * 
	 * @param wsdlRootDirectory
	 *            The wsdl root directory
	 */
	public void setWsdlRootDirectory(String wsdlRootDirectory) {
		this.wsdlRootDirectory = wsdlRootDirectory;
	}

	/**
	 * 
	 * @return The return
	 */
	public String getCorbaServiceName() {
		return corbaServiceName;
	}

	/**
	 * 
	 * @param corbaServiceName
	 *            The corba service name
	 */
	public void setCorbaServiceName(String corbaServiceName) {
		this.corbaServiceName = corbaServiceName;
	}

	/**
	 * 
	 * @return The return
	 */
	public URLClassLoader getUrlClassLoader() {
		return urlClassLoader;
	}

	/**
	 * 
	 * @param urlClassLoader
	 *            The url class loader
	 */
	public void setUrlClassLoader(URLClassLoader urlClassLoader) {
		this.urlClassLoader = urlClassLoader;
	}

	/**
	 * 
	 * @return The return
	 */
	public ChildFirstClassLoader getOriginalClassLoader() {
		return originalClassLoader;
	}

	/**
	 * Return the Original Class LoaderURL that refer the original classes
	 * before the bytecode manipulation
	 * 
	 * @param urlClassLoader
	 *            The url class loader
	 */
	public void setOriginalClassLoader(ChildFirstClassLoader originalClassLoader) {
		this.originalClassLoader = originalClassLoader;
	}

	/**
	 * 
	 * @return The return
	 */
	public Map<String, Object> getValueTypeIdAndInstance() {
		return valueTypeIdAndInstance;
	}

	/**
	 * 
	 * @param valueTypeIdAndInstance
	 *            The value type Id and instance
	 */
	public void setValueTypeIdAndInstance(
			Map<String, Object> valueTypeIdAndInstance) {
		this.valueTypeIdAndInstance = valueTypeIdAndInstance;
	}

	/**
	 * 
	 * @return The return
	 */
	public String getComponentRootPath() {
		return componentRootPath;
	}

	/**
	 * 
	 * @param componentRootPath
	 *            The component root path
	 */
	public void setComponentRootPath(String componentRootPath) {
		this.componentRootPath = componentRootPath;
	}

	public QName getPortTypeName() {
		return portTypeName;
	}

	public void setPortTypeName(QName portTypeName) {
		this.portTypeName = portTypeName;
	}

	public List<MethodSignature> getMethodSignatures() {
		return methodSignatures;
	}

	public void setMethodSignatures(List<MethodSignature> methodSignatures) {
		this.methodSignatures = methodSignatures;
	}

	/**
	 * Set End point
	 * 
	 * @param endpoint
	 *            The ProviderEndpoint class
	 */
	public void setEndpoint(ProviderEndpoint endpoint) {
		this.endpoint = endpoint;
	}

	/**
	 * Get End point
	 * 
	 * @return the return
	 */
	public ProviderEndpoint getEndpoint() {
		return endpoint;
	}

	public Map<String, Map<String, String>> getSubstitutedUnionFields() {
		return substitutedUnionFields;
	}

	public void setSubstitutedUnionFields(
			Map<String, Map<String, String>> substitutedUnionFields) {
		this.substitutedUnionFields = substitutedUnionFields;
	}

	public Map<String, UnionType> getAllUniontypes() {
		return allUniontypes;
	}

	public void setAllUniontypes(Map<String, UnionType> allUniontypes) {
		this.allUniontypes = allUniontypes;
	}

	public Map<String, Map<String, String>> getSubstitutedInterfaceFields() {
		return substitutedUnionFields;
	}

	public void setSubstitutedInterfaceFields(
			Map<String, Map<String, String>> substitutedInterfaceFields) {
		this.substitutedInterfaceFields = substitutedInterfaceFields;
	}

	public Map<String, InterfaceType> getAllInterfacetypes() {
		return allInterfacetypes;
	}

	public void setAllInterfacetypes(Map<String, InterfaceType> allInterfaceypes) {
		this.allInterfacetypes = allInterfaceypes;
	}

	public Map<String, SearchedType> getAllCorbaTypes() {
		Map<String, SearchedType> allCorbaTypes = new HashMap<String, SearchedType>();

		allCorbaTypes.putAll(allInterfacetypes);
		allCorbaTypes.putAll(allUniontypes);

		return allCorbaTypes;
	}

	public Set<Class> getAllIDLTypes() {
		return allIDLTypes;
	}

	public void setAllIDLTypes(Set<Class> allIDLTypes) {
		this.allIDLTypes = allIDLTypes;
	}

	public Map<String, List<String>> getCorbaEnumMap() {
		return corbaEnumMap;
	}

	public void setCorbaEnumMap(Map<String, List<String>> corbaEnumMap) {
		this.corbaEnumMap = corbaEnumMap;
	}

	public Map<String, TypeDef> getTypeDefs() {
		return typeDefs;
	}

	public void setTypeDefs(Map<String, TypeDef> typeDefs) {
		this.typeDefs = typeDefs;
	}

}
