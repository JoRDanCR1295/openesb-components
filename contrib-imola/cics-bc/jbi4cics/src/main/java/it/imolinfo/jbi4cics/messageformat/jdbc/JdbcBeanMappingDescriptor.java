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
package it.imolinfo.jbi4cics.messageformat.jdbc;

import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;

import java.util.Map;

/**
 * @author raffaele
 *
 */
public class JdbcBeanMappingDescriptor implements MappingDescriptor{
  
 /**
  * The responsible to translate localized messages.
  */
  private static final Messages MESSAGES
	          = Messages.getMessages(JdbcBeanMappingDescriptor.class);
  
  private JdbcStatementDescriptor jdbcStatementDescriptor;
  private ResultSetMappingDescriptor resultSetMappingDescriptor;  
  private String resultSetPropertyName;
  private Class beanClass;
  
  /**
   * 
   */
  public JdbcBeanMappingDescriptor() {
  }
  
  /**
   * @return Returns the resultSetPropertyName.
   */
  public String getResultSetPropertyName() {
    return resultSetPropertyName;
  }
  
  /**
   * @param resultSetPropertyName The resultSetPropertyName to set.
   */
  public void setResultSetPropertyName(String resultSetPropertyName) {
    this.resultSetPropertyName = resultSetPropertyName;
  }

   

  /**
   * @return Returns the jdbcStatementDescriptor.
   */
  public JdbcStatementDescriptor getJdbcStatementDescriptor() {
    return jdbcStatementDescriptor;
  }

  /**
   * @param jdbcStatementDescriptor The jdbcStatementDescriptor to set.
   */
  public void setJdbcStatementDescriptor(JdbcStatementDescriptor jdbcStatementDescriptor) {
    this.jdbcStatementDescriptor = jdbcStatementDescriptor;
  }

  /**
   * Non implementato.
   * @param propertyName    The property name
   * @param fieldName    The field name
   * @param fieldDescriptor    The filed descriptor
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName, String fieldName, FieldDescriptor fieldDescriptor) throws FormatException {
    throw new FormatException(MESSAGES.getString("CIC001800_Not_implemented"));    
  }

  /**
   * Non implementato.
   * @param propertyName    The property name
   * @param fieldIndex    The field index
   * @param fieldDescriptor    The filed descriptor
   * @throws FormatException    The format exception
   */
  public void addFieldMapping(String propertyName, Integer fieldIndex, FieldDescriptor fieldDescriptor) throws FormatException {
    throw new FormatException(MESSAGES.getString("CIC001800_Not_implemented"));    
  }

  /**
   * @return Returns the resultSetMappingDescriptor.
   */
  public ResultSetMappingDescriptor getResultSetMappingDescriptor() {
    return resultSetMappingDescriptor;
  }

  /**
   * @param resultSetMappingDescriptor The resultSetMappingDescriptor to set.
   */
  public void setResultSetMappingDescriptor(ResultSetMappingDescriptor resultSetMappingDescriptor) {
    this.resultSetMappingDescriptor = resultSetMappingDescriptor;
  }

  /**
   * @throws FormatException The format exception
   * @return A map with string and field descriptor
   */
  public Map<String, FieldDescriptor> getFieldMap() throws FormatException{
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
   * @param codePage The code Page
   */
  public void setCodePage(String codePage) {
    jdbcStatementDescriptor.setCodePage(codePage);
    resultSetMappingDescriptor.setCodePage(codePage); 
    
  }



}
