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
package it.imolinfo.jbi4cics.messageformat;

import it.imolinfo.jbi4cics.exception.FormatException;

import java.util.Map;

/**
 * Questa e' una tag interface per tutti i mapping descriptor.
 * @author raffaele
 *
 */
public interface MappingDescriptor {
	
  /**
   * Add a mapping definition to the mapping descriptor.
   * @param propertyName    The name of the property of the resulting bean or XML
   * @param fieldName    The name of the original field
   * @param fieldDescriptor    The data description of original field
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName,String fieldName, FieldDescriptor fieldDescriptor) throws FormatException;
  
  /**
   * Add a mapping definition to the mapping descriptor.
   * @param propertyName    name of the property of the resulting bean or XML
   * @param fieldIndex    index of original field, useful when original name is not available and order id relevant
   * @param fieldDescriptor    data description of original field
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName,Integer fieldIndex, FieldDescriptor fieldDescriptor) throws FormatException;
  
  /**
   * 
   * @return a map mado of <propertyName/proertyIndex,FieldDescriptor>
   * @throws FormatException    The format exception
   */
  public Map<String, FieldDescriptor> getFieldMap() throws FormatException;
  
  /**
   * Set the java class this mapping descriptor is expected to operate on beanClass.
   * @param beanClass    The Bean Class
   */
  public void setBeanClass(Class beanClass);
  
  /**
   * Get the java class this mapping descriptor is expected to operate on Class.
   * @return Class    The bean class
   */  
  public Class getBeanClass();
  
  /**
   *  @param codePage    The code page
   */
  public void setCodePage(String codePage);
}
