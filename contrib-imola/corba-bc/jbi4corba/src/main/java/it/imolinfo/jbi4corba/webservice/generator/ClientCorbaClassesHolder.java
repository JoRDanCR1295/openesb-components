 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * Holds the CORBA service information.
 *
 */
/**
 * @author marco
 *
 */
public class ClientCorbaClassesHolder {
    
  private Class helperClass;

  private Class corbaObjectClass;

  private Class operationsClass;
  
  // Collects method Signature
  private List<MethodSignature> methodSignatures 
      = new ArrayList<MethodSignature>();
  
  // String = ID; Object = FactoryInstance
  private Map<String, Object> valueTypeIdAndInstance;
  
  /** the union types contained by corba operation */
  private Map<String,UnionType> unionTypes = new HashMap<String, UnionType>();
  
  /** store correspondance between substituted fields and the Unions  */
  private Map<String,Map<String,String>> substitutedUnionFields = new HashMap<String, Map<String,String>>();
  
  /** the Interface types contained by corba operation */
  private Map<String,InterfaceType> interfaceTypes = new HashMap<String, InterfaceType>();
  
  /** store correspondance between substituted fields and the Interface */
  private Map<String,Map<String,String>> substitutedInterafceFields = new HashMap<String, Map<String,String>>();
  
   /** the Any types contained by corba operation */
  private Map<String,AnyType> anyTypes = new HashMap<String, AnyType>();
  
  /** the ID to ClassName Map */
  private Map<String,String> idTOClassNameMap = new HashMap<String, String>();
  
  /** store correspondance between substituted fields and the Any */
  private Map<String,Map<String,String>> substitutedAnyFields = new HashMap<String, Map<String,String>>();
  
  /** store all types defined in IDL */
  private Set<Class> allIDLTypes = new HashSet<Class>();
  
  /** store all enums defined in IDL */
  Map<String, List<String>> corbaEnumMap = new HashMap<String, List<String>>();
  
  /** All the typedefs informations */
  private Map<String,TypeDef> typeDefs = new HashMap<String, TypeDef>();


  /**
   * Default constructor.
   */
  public ClientCorbaClassesHolder(){
  }
  
/**
 * @return  The return
 */
  public Class getCorbaObjectClass() {
    return corbaObjectClass;
  }

  /**
   * 
   * @param corbaObjectClass  The corba object class
   */
  public void setCorbaObjectClass(Class corbaObjectClass) {
    this.corbaObjectClass = corbaObjectClass;
  }

  /**
   * @return  The return
   */
  public Class getHelperClass() {
    return helperClass;
  }

  /**
   * @param helperClass  The helper class
   */
  public void setHelperClass(Class helperClass) {
    this.helperClass = helperClass;
  }

  /**
   * @return  The return
   */
  public Class getOperationsClass() {
    return operationsClass;
  }

  /**
   * @param operationsClass  The operations class
   */
  public void setOperationsClass(Class operationsClass) {
    this.operationsClass = operationsClass;
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
  public Map<String, Object> getValueTypeIdAndInstance() {
	  return valueTypeIdAndInstance;
  }

  /**
   * 
   * @param valueTypeIdAndInstance  The value type Id and Instance
   */
  public void setValueTypeIdAndInstance(
                                        Map<String, Object> valueTypeIdAndInstance) {
      this.valueTypeIdAndInstance = valueTypeIdAndInstance;
  }

  public List<MethodSignature> getMethodSignatures() {
      return methodSignatures;
  }

  public void setMethodSignatures(List<MethodSignature> methodSignatures) {
      this.methodSignatures = methodSignatures;
  }
  
  public UnionType getUnionType(String unionTypeName) {
		return unionTypes.get(unionTypeName);
  }
	
  public Collection<UnionType> getAllUnionTypes() {
  	return unionTypes.values();
  }
  
  public Map<String,UnionType> getAllUnionTypesMap() {
	  	return unionTypes;
	  }

  public void addUnionTypes(Map<String,UnionType> unionTypes) {
  	this.unionTypes = unionTypes;
  }

  public Map<String, Map<String,String>> getSubstitutedUnionFields() {
	 return substitutedUnionFields;
  }
	
  public void setSubstitutedUnionFields(Map<String, Map<String,String>> replacedUnionFields) {
	 this.substitutedUnionFields = replacedUnionFields;
  }
  
  public Map<String,InterfaceType> getAllInterafceTypesMap() {
      return interfaceTypes;
  }
  
   public InterfaceType getInterfaceType(String interfaceTypeName) {
      return interfaceTypes.get(interfaceTypeName);
  
  }
	
  public Collection<InterfaceType> getAllInterfaceTypes() {
  	return interfaceTypes.values();
  }

  public void addInterfaceTypes(Map<String,InterfaceType> interfaceTypes) {
  	this.interfaceTypes = interfaceTypes;
  }

  public Map<String, Map<String,String>> getSubstitutedInterfaceFields() {
	 return substitutedInterafceFields;
  }
	
  public void setSubstitutedInterfaceFields(Map<String, Map<String,String>> replacedInterfaceFields) {
	 this.substitutedInterafceFields = replacedInterfaceFields;
  }
  
    public AnyType getAnyType(String anyTypeName) {
		return anyTypes.get(anyTypeName);
  }
	
  public Collection<AnyType> getAllAnyTypes() {
  	return anyTypes.values();
  }

  public void addAnyTypes(Map<String,AnyType> anyTypes) {
  	this.anyTypes = anyTypes;
  }

  public Map<String, Map<String,String>> getSubstitutedAnyFields() {
	 return substitutedAnyFields;
  }
	
  public void setSubstitutedAnyFields(Map<String, Map<String,String>> replacedAnyFields) {
	 this.substitutedAnyFields = replacedAnyFields;
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

public void setIdToClassMap(Map<String, String> idToClassMap) {
       this.idTOClassNameMap= idToClassMap;
}

public Map<String, String> getIdToClassMap() {
       return idTOClassNameMap;
}

public Map<String, TypeDef> getTypeDefs() {
	return typeDefs;
}

public void setTypeDefs(Map<String, TypeDef> typeDefs) {
	this.typeDefs = typeDefs;
}



}
