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
package it.imolinfo.jbi4cics.test.connection.jdbc;

import junit.framework.TestCase;

/**
 * @author raffaele
 *
 */
public class SimpleJdbcConnectionManagerTest extends TestCase {
/*  private static Log log=LogFactory.getLog(SimpleJdbcConnectionManagerTest.class);
  *//**
   * 
   *//*
  public SimpleJdbcConnectionManagerTest(String arg0) {
    super(arg0);
    // TODO Auto-generated constructor stu
  }
  
  public void setUp(){
    try {
      String baseDirectory="./main/test/etc";
      if(JNDIUnitTestHelper.notInitialized()){
        JNDIUnitTestHelper.init(baseDirectory+"/"+"jndi_unit_test_helper.properties");
      }
    }
    catch(IOException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
    catch(NamingException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
    catch(ResourceException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }
  
  public void testStoredProcedureOutParameterResultSet(){
    try{
      //costruisco il parameter descriptor
      JdbcParameterDescriptor storedProcedureParameter=new JdbcParameterDescriptor(JdbcParameterDescriptor.IN_OUT_PARAMETER);
      storedProcedureParameter.setSqlType(Types.VARCHAR);
      
      //costruisco lo statement decriptor
      JdbcStatementDescriptor jdbcStatementDescriptor=new JdbcStatementDescriptor();
      jdbcStatementDescriptor.addFieldMapping("storedProcedureParameter",new Integer(1),storedProcedureParameter);
      jdbcStatementDescriptor.setStatementType(JdbcStatementDescriptor.CALLABLE_STATEMENT);
      jdbcStatementDescriptor.setSqlType(JdbcStatementDescriptor.SELECT_SQL);
      jdbcStatementDescriptor.setSql("{call jbi4cics.test_rs (?)}");
      jdbcStatementDescriptor.setBeanClass(StoredProcedureBean.class);
      
      // costrisco il result set mapping descriptor
      ResultSetMappingDescriptor resultSetMappingDescriptor=new ResultSetMappingDescriptor();
      resultSetMappingDescriptor.addFieldMapping("id","ID",null);
      resultSetMappingDescriptor.addFieldMapping("inpuData","input_data",null);
      resultSetMappingDescriptor.addFieldMapping("outputData","output_data",null);
      resultSetMappingDescriptor.setBeanClass(ResultSetBean.class);
      
      // costruisco il mapping descriptor
      JdbcBeanMappingDescriptor jdbcBeanMappingDescriptor=new JdbcBeanMappingDescriptor();      
      jdbcBeanMappingDescriptor.setJdbcStatementDescriptor(jdbcStatementDescriptor);
      jdbcBeanMappingDescriptor.setResultSetMappingDescriptor(resultSetMappingDescriptor);
      jdbcBeanMappingDescriptor.setResultSetPropertyName("resultSet");
      
      // costruisco l'input bean
      StoredProcedureBean inputBean=new StoredProcedureBean();
      inputBean.setStoredProcedureParameter("hello");
      
      // costruisco il locator
      SimpleLocation j2cLocation=new SimpleLocation();
      j2cLocation.setConnectionType(ServiceLocation.JDBC);
      j2cLocation.setLocationName("jdbc/jbi4cics");
      
      //costruisco l'account
      J2CAccount j2cAccount=new J2CAccount();
      j2cAccount.setUsername("jbi4cics");
      j2cAccount.setPassword("jbi4cics");
      
      //costruisco il service context
      //inizializzazione del service context
      ServiceContext serviceContext=new ServiceContext();
      serviceContext.setInputMappingDescriptor(jdbcStatementDescriptor);
      serviceContext.setOutputMappingDescriptor(jdbcBeanMappingDescriptor);
      serviceContext.setInputBean(inputBean);
      serviceContext.setServiceLocation(j2cLocation);
      serviceContext.setAccount(j2cAccount);
      
      
      // costruisco il jdbc formatter
      JdbcFormatter jdbcFormatter=new JdbcFormatter();
      
      // eseguo la formattazione in andata
      long millis1=System.currentTimeMillis();
      jdbcFormatter.mapInputBeanToInputMessage(serviceContext);
      DisconnectedPreparedStatement inputMessage=(DisconnectedPreparedStatement)serviceContext.getInputMessage();
      long millis2=System.currentTimeMillis();
      log.debug("input message: "+inputMessage);
      log.debug("conversion time1="+(millis2-millis1)+" millis");
      
      // costruisco il connection manager
      JdbcConnectionManager jdbcConnectionManager=new JdbcConnectionManager();
            
      // eseguo la chiamata
      millis1=System.currentTimeMillis();
      jdbcConnectionManager.handleCall(serviceContext);
      millis2=System.currentTimeMillis();
      log.debug("execution time="+(millis2-millis1)+" millis");
      
      // eseguo la formattazione all'indietro
      
      millis1=System.currentTimeMillis();
      jdbcFormatter.mapOutputMessageToOutputBean(serviceContext);
      StoredProcedureBean outputBean=(StoredProcedureBean)serviceContext.getOutputBean();
      millis2=System.currentTimeMillis();
      
      log.debug("output bean: "+outputBean);
      log.debug("conversion time1="+(millis2-millis1)+" millis");

      
    }
    catch (Jbi4cicsException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  public static class StoredProcedureBean {
    String storedProcedureParameter;
    List resultSet;

    *//**
     * @return Returns the resultSet.
     *//*
    public List getResultSet() {
      return resultSet;
    }

    *//**
     * @param resultSet The resultSet to set.
     *//*
    public void setResultSet(List resultSet) {
      this.resultSet = resultSet;
    }

    *//**
     * @return Returns the storedProcedureParameter.
     *//*
    public String getStoredProcedureParameter() {
      return storedProcedureParameter;
    }

    *//**
     * @param storedProcedureParameter The storedProcedureParameter to set.
     *//*
    public void setStoredProcedureParameter(String storedProcedureParameter) {
      this.storedProcedureParameter = storedProcedureParameter;
    }
    
  }
  
  public static class ResultSetBean {
    int id;
    String inputData;
    String outputData;
    *//**
     * @return Returns the id.
     *//*
    public int getId() {
      return id;
    }
    *//**
     * @param id The id to set.
     *//*
    public void setId(int id) {
      this.id = id;
    }
    *//**
     * @return Returns the inputData.
     *//*
    public String getInputData() {
      return inputData;
    }
    *//**
     * @param inputData The inputData to set.
     *//*
    public void setInputData(String inputData) {
      this.inputData = inputData;
    }
    *//**
     * @return Returns the outputData.
     *//*
    public String getOutputData() {
      return outputData;
    }
    *//**
     * @param outputData The outputData to set.
     *//*
    public void setOutputData(String outputData) {
      this.outputData = outputData;
    }
    
  }
  */
}
