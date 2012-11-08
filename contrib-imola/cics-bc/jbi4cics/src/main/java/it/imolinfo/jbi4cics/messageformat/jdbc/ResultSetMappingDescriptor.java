/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.messageformat.jdbc;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;

import java.util.Map;

import org.apache.commons.collections.map.ListOrderedMap;

public class ResultSetMappingDescriptor implements MappingDescriptor{
  
  /**
   * The responsible to translate localized messages.
   */
private static final Messages MESSAGES
	          = Messages.getMessages(ResultSetMappingDescriptor.class);
	
  private Map<String, String> columnNamePropertyNameMap=new ListOrderedMap();
  private Class beanClass;
  
  public ResultSetMappingDescriptor() {
    super();
    // TODO Auto-generated constructor stub
  }
  
  /**
   * @return Returns the columnNamePropertyNameMap.
   */
  public Map<String, String> getColumnNamePropertyNameMap() {
    return columnNamePropertyNameMap;
  }  

  /**
   * Il field descriptor non e' usato in questo caso.
   * @param propertyName    The property name
   * @param fieldName    The field name
   * @param fieldDescriptor    The field descriptor
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName, String fieldName, FieldDescriptor fieldDescriptor) throws FormatException {
    columnNamePropertyNameMap.put(fieldName,propertyName);    
  }
  
  /**
   * Non implementato.
   * @param propertyName    The property name
   * @param fieldIndex    The field idex
   * @param fieldDescriptor    The field descriptor
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName, Integer fieldIndex, FieldDescriptor fieldDescriptor) throws FormatException {
    throw new FormatException(MESSAGES.getString("CIC001800_Not_implemented"));    
  }

  /**
   * @throws FormatException The format exception
   * @return   A map with string and field descriptor
   */
  public Map<String, FieldDescriptor> getFieldMap() throws FormatException {
//  TODO gestire questo metodo
    throw new FormatException(MESSAGES.getString("CIC001800_Not_implemented"));
  }

  /**
   * @return Returns the beanClass.
   */
  public Class getBeanClass() {
    return beanClass;
  }

  /**
   * @param beanClass The beanClass to set.
   */
  public void setBeanClass(Class beanClass) {
    this.beanClass = beanClass;
  }

  /**
   * @param codePage The code page
   */
  public void setCodePage(String codePage) {
    // TODO Auto-generated method stub
    
  }  

}
