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
package it.imolinfo.jbi4cics.test.mapping.jdbc;

import it.imolinfo.jbi4cics.test.connection.jdbc.util.BaseJdbcTestCase;

/**
 * @author raffaele
 *
 */
public class SimpleJdbcMappingTest extends BaseJdbcTestCase {
  
  /*private static Log log=LogFactory.getLog(SimpleJdbcMappingTest.class);

  *//**
   * @param arg0
   *//*
  public SimpleJdbcMappingTest(String arg0) {
    super(arg0);
    // TODO Auto-generated constructor stub
  }  
  
  public void testQueryPreparedStatement(){
    try {
      //costruisco il parameter descriptor
      JdbcParameterDescriptor tableTypeParameter=new JdbcParameterDescriptor(JdbcParameterDescriptor.IN_PARAMETER);
      
      //costruisco lo statement decriptor
      JdbcStatementDescriptor jdbcStatementDescriptor=new JdbcStatementDescriptor();
      jdbcStatementDescriptor.addFieldMapping("tableType",new Integer(1),tableTypeParameter);
      jdbcStatementDescriptor.setStatementType(JdbcStatementDescriptor.PREPARED_STATEMENT);
      jdbcStatementDescriptor.setSqlType(JdbcStatementDescriptor.SELECT_SQL);
      jdbcStatementDescriptor.setSql("select TABLEID, TABLENAME, TABLETYPE, SCHEMAID, LOCKGRANULARITY from SYS.SYSTABLES where TABLETYPE=?");
      jdbcStatementDescriptor.setBeanClass(SysTableParameterBean.class);
      
      // costrisco il result set mapping descriptor
      ResultSetMappingDescriptor resultSetMappingDescriptor=new ResultSetMappingDescriptor();
      resultSetMappingDescriptor.addFieldMapping("tableId","TABLEID",null);
      resultSetMappingDescriptor.addFieldMapping("tableName","TABLENAME",null);
      resultSetMappingDescriptor.addFieldMapping("tableType","TABLETYPE",null);
      resultSetMappingDescriptor.addFieldMapping("schemaId","SCHEMAID",null);
      resultSetMappingDescriptor.addFieldMapping("lockGranularity","LOCKGRANULARITY",null);
      resultSetMappingDescriptor.setBeanClass(SysTableRowBean.class);
      
      // costruisco il mapping descriptor
      JdbcBeanMappingDescriptor jdbcBeanMappingDescriptor=new JdbcBeanMappingDescriptor();      
      jdbcBeanMappingDescriptor.setJdbcStatementDescriptor(jdbcStatementDescriptor);
      jdbcBeanMappingDescriptor.setResultSetMappingDescriptor(resultSetMappingDescriptor);
      jdbcBeanMappingDescriptor.setResultSetPropertyName("rowBeans");

      // costruisco l'input bean
      SysTableParameterBean inputBean=new SysTableParameterBean();
      inputBean.setTableType("S");
      
      //costruisco il service context
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(jdbcBeanMappingDescriptor);
      serviceContext.setOutputMappingDescriptor(jdbcBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);

      
      // costruisco il jdbc formatter
      JdbcFormatter jdbcFormatter=new JdbcFormatter();
      
      // eseguo la formattazione in andata
      long millis1=System.currentTimeMillis();
      jdbcFormatter.mapInputBeanToInputMessage(serviceContext);
      DisconnectedPreparedStatement inputMessage=(DisconnectedPreparedStatement)serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis();
      log.debug("input message: "+inputMessage);
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      // connetto lo statement ed eseguo la query
      inputMessage.setConnection(conn);
      inputMessage.setConnected(true);
      ResultSet rs=inputMessage.executeQuery();
      log.debug("resultset: "+rs);
      
      //mi disconnetto
      inputMessage.setConnected(false);
      conn.close();
      
      // costruisco il messaggio di output
      JdbcOutputMessage jdbcOutputMessage=new JdbcOutputMessage(inputMessage,rs);
      serviceContext.setOutputMessage(jdbcOutputMessage);
      
      millis1=System.currentTimeMillis();
      jdbcFormatter.mapOutputMessageToOutputBean(serviceContext);
      SysTableParameterBean outputBean=(SysTableParameterBean)serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("output bean: "+outputBean);
      log.debug("conversion time1="+(millis2-millis1)+" millis");
    }
    catch(FormatException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
    catch(SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
    
  }
  
  public static class SysTableParameterBean {
    private String tableType;
    private Collection rowBeans;
    *//**
     * @return Returns the rowBeans.
     *//*
    public Collection getRowBeans() {
      return rowBeans;
    }
    *//**
     * @param rowBeans The rowBeans to set.
     *//*
    public void setRowBeans(Collection rowBeans) {
      this.rowBeans = rowBeans;
    }
    *//**
     * @return Returns the tableType.
     *//*
    public String getTableType() {
      return tableType;
    }
    *//**
     * @param tableType The tableType to set.
     *//*
    public void setTableType(String tableType) {
      this.tableType = tableType;
    }
    
    public String toString() {
      return ReflectionToStringBuilder.toString(this);
    }
    
    public boolean equals(Object obj) {
      return EqualsBuilder.reflectionEquals(this, obj);
    }    
    
  }
  
  public static class SysTableRowBean {
    private String tableId;
    private String tableName;
    private String tableType;
    private String schemaId;
    private String lockGranularity;
    *//**
     * @return Returns the lockGranularity.
     *//*
    public String getLockGranularity() {
      return lockGranularity;
    }
    *//**
     * @param lockGranularity The lockGranularity to set.
     *//*
    public void setLockGranularity(String lockGranularity) {
      this.lockGranularity = lockGranularity;
    }
    *//**
     * @return Returns the schemaId.
     *//*
    public String getSchemaId() {
      return schemaId;
    }
    *//**
     * @param schemaId The schemaId to set.
     *//*
    public void setSchemaId(String schemaId) {
      this.schemaId = schemaId;
    }
    *//**
     * @return Returns the tableId.
     *//*
    public String getTableId() {
      return tableId;
    }
    *//**
     * @param tableId The tableId to set.
     *//*
    public void setTableId(String tableId) {
      this.tableId = tableId;
    }
    *//**
     * @return Returns the tableName.
     *//*
    public String getTableName() {
      return tableName;
    }
    *//**
     * @param tableName The tableName to set.
     *//*
    public void setTableName(String tableName) {
      this.tableName = tableName;
    }
    *//**
     * @return Returns the tableType.
     *//*
    public String getTableType() {
      return tableType;
    }
    *//**
     * @param tableType The tableType to set.
     *//*
    public void setTableType(String tableType) {
      this.tableType = tableType;
    }
    
    public String toString() {
      return ReflectionToStringBuilder.toString(this);
    }
    
    public boolean equals(Object obj) {
      return EqualsBuilder.reflectionEquals(this, obj);
    }    
    
  }
*/
}
