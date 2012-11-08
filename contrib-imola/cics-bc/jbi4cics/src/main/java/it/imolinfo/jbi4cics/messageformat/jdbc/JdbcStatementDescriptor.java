/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.messageformat.jdbc;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.MappingDescriptor;

import java.util.HashMap;
import java.util.Map;

public class JdbcStatementDescriptor implements MappingDescriptor{

  public static final int PREPARED_STATEMENT=0;
  public static final int CALLABLE_STATEMENT=1;
  
  public static final int UPDATE_SQL=0;
  public static final int SELECT_SQL=1;
  
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(JdbcStatementDescriptor.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(JdbcStatementDescriptor.class);

  
  private String sql;
  private int statementType;
  private int sqlType;
  private Map<Integer, FieldDescriptor> parameterList=new HashMap<Integer, FieldDescriptor>();
  private Map<String, Integer> propertyNameParameterIndexMap=new HashMap<String, Integer>();
  private Class beanClass;
  
  
  public JdbcStatementDescriptor() {
    super();
    // TODO Auto-generated constructor stub
  }  
  
  public JdbcParameterDescriptor getParameter(int paramterIndex){
    return (JdbcParameterDescriptor)parameterList.get(new Integer(paramterIndex));
  }

  /**
   * @return Returns the sql.
   */
  public String getSql() {
    return sql;
  }

  /**
   * @param sql The sql to set.
   */
  public void setSql(String sql) {
    this.sql = sql;
  }

  /**
   * @return Returns the sqlType.
   */
  public int getSqlType() {
    return sqlType;
  }

  /**
   * @param sqlType The sqlType to set.
   */
  public void setSqlType(int sqlType) {
    this.sqlType = sqlType;
  }

  /**
   * @return Returns the statementType.
   */
  public int getStatementType() {
    return statementType;
  }

  /**
   * @param statementType The statementType to set.
   */
  public void setStatementType(int statementType) {
    this.statementType = statementType;
  }

  public void addFieldMapping(String propertyName, String fieldName, FieldDescriptor fieldDescriptor) throws FormatException {
    // TODO Auto-generated method stub
    
  }
  
  public void addFieldMapping(String propertyName, Integer fieldIndex, FieldDescriptor fieldDescriptor) throws FormatException {
    if (!(fieldDescriptor instanceof JdbcParameterDescriptor)){
      LOG.error("CIC001805_Jdbc_parameter_descriptor_not_found", fieldDescriptor.getClass());
      throw new FormatException(MESSAGES.getString("CIC001805_Jdbc_parameter_descriptor_not_found", fieldDescriptor.getClass()));
    }     
    parameterList.put(fieldIndex,fieldDescriptor); 
    propertyNameParameterIndexMap.put(propertyName,fieldIndex);  
  }  
  
  /**
   * @return Returns the propertyNameParameterIndexMap.
   */
  public Map<String, Integer> getPropertyNameParameterIndexMap() {
    return propertyNameParameterIndexMap;
  }

  public Map<String, FieldDescriptor> getFieldMap() throws FormatException {
    //TODO gestire questo metodo
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

  public void setCodePage(String codePage) {
    // TODO Auto-generated method stub
    
  }  
    
}
