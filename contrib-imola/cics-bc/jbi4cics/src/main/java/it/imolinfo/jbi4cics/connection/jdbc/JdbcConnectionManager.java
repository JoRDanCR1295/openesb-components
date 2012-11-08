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
package it.imolinfo.jbi4cics.connection.jdbc;

import it.imolinfo.jbi4cics.connection.ConnectionManager;
import it.imolinfo.jbi4cics.connection.jdbc.util.DisconnectedPreparedStatement;
import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.messageformat.jdbc.JdbcOutputMessage;
import it.imolinfo.jbi4cics.messageformat.jdbc.JdbcStatementDescriptor;
import it.imolinfo.jbi4cics.service.ServiceContext;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;

/**
 * @author raffaele
 *
 */
public class JdbcConnectionManager implements ConnectionManager {

  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(JdbcConnectionManager.class);
    
  /**
   * 
   */
  public JdbcConnectionManager() {
    super();
    // TODO Auto-generated constructor stub
  }

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.connection.ConnectionManager#handleCall(it.imolinfo.jbi4cics.service.ServiceContext)
   */
  public void handleCall(ServiceContext serviceContext) throws ConnectionException {
    Connection connection=null;
    DisconnectedPreparedStatement disconnectedStatement=null;
    try{
      //  creo la connessione
      connection=createConnection(serviceContext);
      Object inputMessage=serviceContext.getInputMessage();
      //ottengo l'input message
      if (!(inputMessage instanceof DisconnectedPreparedStatement)){
        LOG.error("CIC000600_Expected_disconnected_statement_input_message", new Object[] {inputMessage.getClass()});
        throw new ConnectionException("CIC000600_Expected_disconnected_statement_input_message", new Object[] {inputMessage.getClass()});
      }
      disconnectedStatement=(DisconnectedPreparedStatement)inputMessage;
      // ottengo lo statement description
      if (!(serviceContext.getInputMappingDescriptor() instanceof JdbcStatementDescriptor)) {
        LOG.error("CIC000601_Expected_jdbc_statement_descriptor_input_message", new Object[] {serviceContext.getInputMappingDescriptor().getClass()});
        throw new ConnectionException("CIC000601_Expected_jdbc_statement_descriptor_input_message", new Object[] {serviceContext.getInputMappingDescriptor().getClass()});
      }
      JdbcStatementDescriptor jdbcStatementDescriptor=(JdbcStatementDescriptor)serviceContext.getInputMappingDescriptor();
      ResultSet resultSet=null;
      disconnectedStatement.setConnection(connection);     
      try {
        disconnectedStatement.setConnected(true);
      }
      catch (SQLException e){
        LOG.error("CIC000602_Error_preparing_statement", new Object[] {e.getMessage()},e);
        throw new ConnectionException("CIC000602_Error_preparing_statement", new Object[] {e.getMessage()},e);
      }
      try {
        switch (jdbcStatementDescriptor.getSqlType()) {
          case JdbcStatementDescriptor.SELECT_SQL : {
            resultSet=disconnectedStatement.executeQuery();
            break;
          }
          case JdbcStatementDescriptor.UPDATE_SQL : {
            boolean result=disconnectedStatement.execute();
            //se ci sono resultset
            if (result){
              // prendo solo il primo, result set multipli non sono supportati
              resultSet=disconnectedStatement.getResultSet();
            }
            break;
          }
          default : {
            LOG.error("CIC000603_Unexpected_sql_type", new Object[] {jdbcStatementDescriptor.getSqlType()});
            throw new ConnectionException("CIC000603_Unexpected_sql_type", new Object[] {jdbcStatementDescriptor.getSqlType()});
          }
        }
      }
      catch (SQLException e){
        //TODO probabilmente qui c'Ã gestire l'eccezione per ora la rilancio
        LOG.error("CIC000604_Error_executing_statement", new Object[] {e.getMessage()}, e);
        throw new ConnectionException("CIC000604_Error_executing_statement", new Object[] {e.getMessage()}, e);
      }   
      JdbcOutputMessage jdbcOutputMessage=new JdbcOutputMessage();
      jdbcOutputMessage.setDisconnectedSatetement(disconnectedStatement);
      jdbcOutputMessage.setResultSet(resultSet);
      serviceContext.setOutputMessage(jdbcOutputMessage);
    }
    finally{
      releaseResources(connection,disconnectedStatement);
    }

  }
  
  private Connection createConnection(ServiceContext serviceContext) throws ConnectionException {
    // per prima cosa ottengo la service location
    ServiceLocation serviceLocation=serviceContext.getServiceLocation();
    if (serviceLocation.getConnectionType()!=ServiceLocation.JDBC){
      throw new ConnectionException("CIC000605_Expected_jdbc_connection_type", new Object[] {serviceLocation.getConnectionType()});
    }
    // ora ottengo il nome jndi
    String jndiConnectionName=serviceLocation.getLocationName();
    DataSource dataSource=null;
    try {
        InitialContext initialContext = new javax.naming.InitialContext();
        dataSource =(DataSource)initialContext.lookup(jndiConnectionName);
    }
    catch (NamingException e) {
      LOG.error("CIC000606_Error_retrieving_data_source", new Object[] {e.getMessage()}, e);
      throw new ConnectionException("CIC000606_Error_retrieving_data_source", new Object[] {e.getMessage()}, e);
    }
    if (dataSource == null) {
      throw new ConnectionException("CIC000607_Lookup_failed", new Object[] {jndiConnectionName});
    }    
    try {
      Connection connection = dataSource.getConnection();
      return connection;
    }
    catch (SQLException e) {
      LOG.error("CIC000608_Error_getting_connection_data_source", new Object[] {e.getMessage()}, e);
      throw new ConnectionException("CIC000608_Error_getting_connection_data_source", new Object[] {e.getMessage()}, e);
    } 
  }
  
  private void releaseResources(Connection connection,DisconnectedPreparedStatement disconnectedPreparedStatement) throws ConnectionException {
    try {
      if (disconnectedPreparedStatement!=null){      
        disconnectedPreparedStatement.setConnected(false);
      }
      if (connection!=null){
        connection.close();
      }
    }
    catch (SQLException e){
      LOG.error("CIC000609_Error_releasing_resources", new Object[] {e.getMessage()}, e);
      throw new ConnectionException("CIC000609_Error_releasing_resources", new Object[] {e.getMessage()}, e);
    }
  }

}
