 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * This class provides the object representation of a CORBA param.
 * 
 * If the parameter is an Holder, this object mantains: <br/>
 * <li> The original holder type (for example <code>org.omg.CORBA.StringHolder</code>)
 * <li> The holder value type (for example <code>java.lang.String</code>) 
 * <li> The changed holder type (for example <code>javax.xml.ws.Holder<String></code>) as actual type.
 * 
 */
public class Param {

  /**
   * The name of the method.
   */
  private String name = null;

  /**
   * The (actual) data type name
   */
  private String typeName = null;
  
  /**
   * The (actual) data type 
   */
  private Class type = null;
  
  /**
   * True if it's an holder.
   */
  boolean isHolder = false;
  
  /**
   * The (original) holder type 
   */
  private Class holderType = null;
  
  /**
   * The holder value type
   */
  private Class holderValueType = null;
  
  /**
   * True it the parameter is an array
   */
  private boolean isArray = false;
  
  /**
   * If an array, the array dimension (NOT THE SIZE!!!!).
   */
  private int arrayDimension = 0;
  
  /**
   * True if the parameter is aprimitive type.
   */
  private boolean isPrimitive = false;
  

  /**
   * Default constructor.
   */
  public Param() {}
  
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

  public String getName() {
      return name;
  }

  public void setName(String paramName) {
      this.name = paramName;
  }

  public String getTypeName() {
      return typeName;
  }

  public void setTypeName(String paramType) {
      this.typeName = paramType;
  }

  public boolean isHolder() {
      return isHolder;
  }

  public void setHolder(boolean isHolder) {
      this.isHolder = isHolder;
  }

  public Class getHolderType() {
      return holderType;
  }

  public void setHolderType(Class holderType) {
      this.holderType = holderType;
  }

  public Class getHolderValueType() {
      return holderValueType;
  }

  public void setHolderValueType(Class holderValueType) {
      this.holderValueType = holderValueType;
  }

  public Class getType() {
      return type;
  }

  public void setType(Class type) {
      this.type = type;
  }

  public boolean isArray() {
      return isArray;
  }

  public void setArray(boolean isArray) {
      this.isArray = isArray;
  }

  public int getArrayDimension() {
      return arrayDimension;
  }

  public void setArrayDimension(int arrayDimension) {
      this.arrayDimension = arrayDimension;
  }

  public boolean isPrimitive() {
      return isPrimitive;
  }

  public void setPrimitive(boolean isPrimitive) {
      this.isPrimitive = isPrimitive;
  }  

}
