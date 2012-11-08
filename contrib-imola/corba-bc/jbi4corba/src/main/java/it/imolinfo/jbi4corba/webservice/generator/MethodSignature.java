 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * This class provides the object representation of a method's signature.
 */
public class MethodSignature {

    /**
     * The name of the method.
     */
    private String methodName = null;

    /**
     * The data type used to return the method result.
     * (In general is not a full qualified java name).
     */
    private String returnType = null;

    /**
     * The name of the return element used in the wsdl.
     */
    private String returnName = null;

    /**
     * The list of parameters.
     * The position in the list represent the position of the parameter.
     */
    private List<Param> parameters = new ArrayList<Param>();

    /**
     * The list of Exceptions raised by the method.
     * The list member is the full qualified name of the class of the exception.
     */
    private List<String> exceptionsType = new ArrayList<String>();

    /**
     * true for an asynchronous method (The Oneway annotation will be added).
     */
    private boolean oneway = false;

    /**
     * True if this signature contains a <code>Holder</code>.
     */
    private boolean containsHolder = false;
    
    /** The operations class Name */ 
    private String className = null;
    
    /** The operations class Type */ 
    private Class classType = null;
    
    /** The method object */ 
    private Method method = null;
    
    /** The changed method object (can be null if no holder parames are included)*/ 
    private Method changedMethod = null;
    
    private Set<String> methodUnionTypes =new HashSet<String>();
    
    
    private Set<String> methodInterfaceTypes=new HashSet<String>();
    
    private Set<String> methodAnyTypes=new HashSet<String>(); 

    /**
     * Default constructor.
     */
    public MethodSignature() {
        // NOP
    }

    // Basic

    /**
     * @return  This object as String.
     */
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }

    /**
     * @param   obj   An object.
     * @return  true, if this object is equal to the obj parameter.
     */
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    // Getter And Setter

    /**
     * getter.
     *
     * @return  The current value
     */
    public String getMethodName() {
        return methodName;
    }

    /**
     * setter.
     *
     * @param methodName  the value to set.
     */
    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    /**
     * getter.
     *
     * @return  The current value
     */
    public String getReturnType() {
        return returnType;
    }

    /**
     * setter.
     *
     * @param returnType  the value to set.
     */
    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    /**
     * getter.
     *
     * @return    The current value
     */
    public String getReturnName() {
        return returnName;
    }

    /**
     * setter.
     *
     * @param        returnName    The value to set.
     */
    public void setReturnName(String returnName) {
        this.returnName = returnName;
    }

    /**
     * A getter.
     *
     * @return  The current exceptionsType attribute.
     */
    public List<String> getExceptionsType() {
        return exceptionsType;
    }

    /**
     * A setter.
     * @param   exceptionsType    The new exceptionsType attribute.
     */
    public void setExceptionsType(List<String> exceptionsType) {
        this.exceptionsType = exceptionsType;
    }

    /**
     * getter.
     *
     * @return    The current value of the property.
     */
    public boolean isOneway() {
        return oneway;
    }

    /**
     * setter.
     *
     * @param   oneway  The new value of the property.
     */
    public void setOneway(boolean oneway) {
        this.oneway = oneway;
    }

    public List<Param> getParameters() {
        return parameters;
    }

    public void setParameters(List<Param> parameters) {
        this.parameters = parameters;
    }

    public boolean isContainsHolder() {
        return containsHolder;
    }

    public void setContainsHolder(boolean containsHolder) {
        this.containsHolder = containsHolder;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String classNameStr) {
        this.className = classNameStr;
    }

    public Class getClassType() {
        return classType;
    }

    public void setClassType(Class classType) {
        this.classType = classType;
    }

    public Method getMethod() {
        return method;
    }

    public void setMethod(Method method) {
        this.method = method;
    }

    public Method getChangedMethod() {
        return changedMethod;
    }

    public void setChangedMethod(Method changedMethod) {
        this.changedMethod = changedMethod;
    }

    public Set<String> getMethodUnionTypes() {
		return methodUnionTypes;
    }

    public void setMethodUnionTypes(Set<String> methodUnionTypes) {
		this.methodUnionTypes = methodUnionTypes;
    }    
    
    public Set<String> getMethodInterfaceTypes() {
		return methodInterfaceTypes;
    }
 
    public void setMethodInterfaceTypes(Set<String> methodInterfaceTypes) {
		this.methodInterfaceTypes = methodInterfaceTypes;
    }     
    
     public Set<String> getMethodAnyTypes() {
		return methodInterfaceTypes;
    }
 
    public void setMethodAnyTypes(Set<String> methodInterfaceTypes) {
		this.methodInterfaceTypes = methodInterfaceTypes;
    }     
    
    //It return true if the method contains param or return type that contains a Corba Type
    public boolean hasCorbaTypes()
    {   
        
    	if (!methodInterfaceTypes.isEmpty() || !methodUnionTypes.isEmpty() || !methodAnyTypes.isEmpty())
    		return true;
    	return false;
    }
        
}
