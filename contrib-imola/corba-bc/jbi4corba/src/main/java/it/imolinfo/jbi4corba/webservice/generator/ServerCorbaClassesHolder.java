 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;


import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;

import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/** 
 * This class contains all the information collectect during the code generation
 * (WSDL to CORBA) of the component.
 */
public class ServerCorbaClassesHolder {

  private Class webServiceInterface;
  private Class webServiceImpl;
  private Class corbaOperations;
  private Class corbaPOATie;
  private Class corbaHelper;
  private boolean generateClassesFromIDL;

  /**
   * A class that implements a corba interface.
   */
  private Class corbaImplClass;

  private URLClassLoader urlClassLoader;
  
  private URLClassLoader originalClassLoader;
 
  /**
   * This map contains the pairs "valueTypeID, valueTypeFactoryInstance"
   * registered in the ORB.
   */
  private Map<String, Object> valueTypeIdAndInstance;  

  /** Collects method Signature */
  private List<MethodSignature> methodSignatures 
      = new ArrayList<MethodSignature>();
  
  /** The information extracted from the WSDL during the code generation. */
  private WsdlInformation wsdlInformation = null;
  
  /* Substituted union types mapping */
  private Map<String, Map<String,String>> substitutedUnionFields 
      = new HashMap<String, Map<String,String>>();

  private Map<String, UnionType> allUniontypes = null;
  
  /* Substituted Interface types mapping */
  private Map<String, Map<String,String>> substitutedInterfaceFields 
      = new HashMap<String, Map<String,String>>();
  
  private Map<String, InterfaceType> allInterfacetypes = null;
  
  /** store all types defined in IDL */
  private Set<Class> allIDLTypes = new HashSet<Class>();
  
  /** store all enums defined in IDL */
  Map<String, List<String>> corbaEnumMap = new HashMap<String, List<String>>();

  private Map<String,String> idTOClassNameMap = new HashMap<String, String>();
  
  private Map<String, TypeDef> typeDefs = new HashMap<String, TypeDef>();
  // ========================
  //        Getter And Setter
  // ========================

  public WsdlInformation getWsdlInformation() {
    return wsdlInformation;
  }

  public void setWsdlInformation(WsdlInformation wsdlInformation) {
    this.wsdlInformation = wsdlInformation;
  }

  /**
   * Default constructor.
   */
  public ServerCorbaClassesHolder (){
  }

  /**
   * @return  A map where the key is the valuetype ID
   *          and the value is an instance of the factory.
   */
  public Map<String, Object> getValueTypeIdAndInstance() {
    return valueTypeIdAndInstance;
  }

  /**
   * @param valueTypeIdAndInstance  The value type id and instance
   */
  public void setValueTypeIdAndInstance(Map<String, Object> valueTypeIdAndInstance) {
    this.valueTypeIdAndInstance = valueTypeIdAndInstance;
  }

  /**
   * @return  The return
   */
  public Class getCorbaOperations() {
    return corbaOperations;
  }

  /**
   * @param corbaOperations The corba operations
   */
  public void setCorbaOperations(Class corbaOperations) {
    this.corbaOperations = corbaOperations;
  }

  /**
   * @return  The return
   */
  public Class getCorbaPOATie() {
    return corbaPOATie;
  }
  
  /**
   * @param corbaPOATie  The corba POA tie
   */
  public void setCorbaPOATie(Class corbaPOATie) {
    this.corbaPOATie = corbaPOATie;
  }
  
  /**
   * @return  The return
   */
  public Class getWebServiceInterface() {
    return webServiceInterface;
  }
  
  /**
   * @param webServiceInterface  The web service interface
   */
  public void setWebServiceInterface(Class webServiceInterface) {
    this.webServiceInterface = webServiceInterface;
  }

  /**
   * @return  The return
   */
  public String toString() {
    return ReflectionToStringBuilder.toString(this);
  }

  /**
   * @param obj  The object
   * @return     The return
   */
  public boolean equals(Object obj) {
    return EqualsBuilder.reflectionEquals(this, obj);
  }
  
  /**
   * @return  The return
   */
  public Class getCorbaHelper() {
    return corbaHelper;
  }
  
  /**
   * @param corbaHelper  The corba helper
   */
  public void setCorbaHelper(Class corbaHelper) {
    this.corbaHelper = corbaHelper;
  }

  /**
   * @return  The return
   */
  public URLClassLoader getUrlClassLoader() {
    return urlClassLoader;
  }

  /**
   * @param urlClassLoader  The urlClassLoader
   */
  public void setUrlClassLoader(URLClassLoader urlClassLoader) {
    this.urlClassLoader = urlClassLoader;
  }

  public Class getWebServiceImpl() {
      return webServiceImpl;
  }
  public void setWebServiceImpl(Class webServiceImpl) {
      this.webServiceImpl = webServiceImpl;
  }

  /**
   * A getter.
   *
   * @return  A class that implements a corba interface.
   */
  public Class getCorbaImplClass() {
    return corbaImplClass;
  }

  /**
   * A setter.
   *
   * @param  corbaImpl  A class that implements a corba interface.
   */
  public void setCorbaImplClass(Class corbaImpl) {
    this.corbaImplClass = corbaImpl;
  }

  public boolean isGenerateClassesFromIDL() {
	return generateClassesFromIDL;
  }
  public void setGenerateClassesFromIDL(boolean generateClassesFromIDL) {
	this.generateClassesFromIDL = generateClassesFromIDL;
  }

  
  /**
  * 
  * @return  The return
  */
 public URLClassLoader getOriginalClassLoader() {
     return originalClassLoader;
 }

 /**
  * Return the Original Class LoaderURL that refer the original classes before the
  * bytecode manipulation
  * @param urlClassLoader  The url class loader
  */
 public void setOriginalClassLoader(URLClassLoader originalClassLoader) {
     this.originalClassLoader = originalClassLoader;
 }


	public List<MethodSignature> getMethodSignatures() {
		return methodSignatures;
	}

	public void setMethodSignatures(List<MethodSignature> methodSignatures) {
		this.methodSignatures = methodSignatures;
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

    public void setIdToClassNameMap(Map<String, String> idToClassMap) {
        idTOClassNameMap=idToClassMap;
    }
    
    public Map<String,String> getIdToClassNameMap(){
        return idTOClassNameMap;
    }

	public Map<String, TypeDef> getTypeDefs() {
		return typeDefs;
	}

	public void setTypeDefs(Map<String, TypeDef> typeDefs) {
		this.typeDefs = typeDefs;
	}
    
    
}
