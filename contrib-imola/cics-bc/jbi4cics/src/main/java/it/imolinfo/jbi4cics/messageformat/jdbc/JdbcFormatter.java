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

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.connection.jdbc.util.DisconnectedCallableStatement;
import it.imolinfo.jbi4cics.connection.jdbc.util.DisconnectedPreparedStatement;
import it.imolinfo.jbi4cics.connection.jdbc.util.DisconnectedStatementFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.messageformat.MessageFormatter;
import it.imolinfo.jbi4cics.service.ServiceContext;

import java.sql.CallableStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.ConvertingWrapDynaBean;
import org.apache.commons.beanutils.DynaBean;
import org.apache.commons.beanutils.ResultSetDynaClass;
import org.apache.commons.beanutils.WrapDynaBean;

/**
 * @author raffaele
 *
 */
public class JdbcFormatter implements MessageFormatter {

  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(JdbcFormatter.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(JdbcFormatter.class);
    
  /**
   * 
   */
  public JdbcFormatter() {
    super();
    // TODO Auto-generated constructor stub
  }

  /**
   * nel caso di Jdbc l'output message è un PreparedStatement o un CallableStatement con i parametri settati. Lo statement usato sarà di tipo disconnected
   * @see it.imolinfo.jbi4cics.messageformat.MessageFormatter#mapInputBeanToInputMessage(java.lang.Object, it.imolinfo.jbi4cics.service.ServiceContext)
   */
  public void mapInputBeanToInputMessage(ServiceContext serviceContext) throws FormatException {
    Object inputBean=serviceContext.getInputBean();
    // se siamo qui il mapping descriptor deve essere di tipo JdbcBeanMappingDescriptor
    if (!(serviceContext.getInputMappingDescriptor() instanceof JdbcStatementDescriptor)){
      LOG.error("CIC001801_Jdbc_statement_descriptor_not_found", 
              serviceContext.getInputMappingDescriptor().getClass());
      throw new FormatException(MESSAGES.getString(
              "CIC001801_Jdbc_statement_descriptor_not_found", 
              serviceContext.getInputMappingDescriptor().getClass()));
    }
    JdbcStatementDescriptor jdbcStatementDescriptor=(
            JdbcStatementDescriptor)serviceContext.getInputMappingDescriptor();
    DisconnectedPreparedStatement statement=null;
    // devo costruire lo statement correttamente
    switch (jdbcStatementDescriptor.getStatementType()) {
      case JdbcStatementDescriptor.PREPARED_STATEMENT : {
        statement=DisconnectedStatementFactory.prepareDisconnectedStatement(
                jdbcStatementDescriptor.getSql());
        break;
      }
      case JdbcStatementDescriptor.CALLABLE_STATEMENT : {
        statement=DisconnectedStatementFactory.prepareDisconnectedCall(
                jdbcStatementDescriptor.getSql());
        break;
      }
      default : throw new FormatException(MESSAGES.getString(
              "CIC001802_Unrecognized_statement_type", 
              jdbcStatementDescriptor.getStatementType())); 
    }
    Map<String, Integer> propertyNameParameterIndexMap=jdbcStatementDescriptor.getPropertyNameParameterIndexMap();
    WrapDynaBean dynaBean=new WrapDynaBean(inputBean);
    for (Iterator<String> i=propertyNameParameterIndexMap.keySet().iterator();i.hasNext();){
      try {
        String propertyName=i.next();
        int parameterIndex=propertyNameParameterIndexMap.get(propertyName).intValue();
        JdbcParameterDescriptor jdbcParameterDescriptor=jdbcStatementDescriptor.getParameter(parameterIndex);
        Object value=dynaBean.get(propertyName);
        if (jdbcStatementDescriptor.getStatementType()==JdbcStatementDescriptor.CALLABLE_STATEMENT) {
          ((CallableStatement)statement).registerOutParameter(parameterIndex,jdbcParameterDescriptor.getSqlType());
        }
        statement.setObject(parameterIndex,value);
      }
      catch (SQLException e){
        //TODO loggare correttamente
        throw new FormatException(e);
      }
    }
    LOG.debug("created statement: "+statement);
    serviceContext.setInputMessage(statement);
  }

  /**
   * nel caso di Jdbc l'output message è la coppia CallableStatement + resultset o valore di ritorno di un update. Ad oggi non è chiaro se serva il valore di ritorno di un update
   * Il callable statement invece serve perchè deve essere possibile estrarre eventuali parametri di ritorno
   * @see it.imolinfo.jbi4cics.messageformat.MessageFormatter#mapOutputMessageToOutputBean(java.lang.Object, it.imolinfo.jbi4cics.service.ServiceContext)
   */
  public void mapOutputMessageToOutputBean(ServiceContext serviceContext) throws FormatException {
    Object outputMessage=serviceContext.getOutputMessage();
    // se siamo qui il mapping descriptor deve essere di tipo JdbcBeanMappingDescriptor
    if (!(serviceContext.getOutputMappingDescriptor() instanceof JdbcBeanMappingDescriptor)){
      LOG.error("CIC001803_Jdbc_bean_mapping_descriptor_not_found", 
              serviceContext.getOutputMappingDescriptor().getClass());
      throw new FormatException(MESSAGES.getString(
              "CIC001803_Jdbc_bean_mapping_descriptor_not_found", 
              serviceContext.getOutputMappingDescriptor().getClass()));
    }
    JdbcBeanMappingDescriptor jdbcBeanMappingDescriptor=(JdbcBeanMappingDescriptor)serviceContext.getOutputMappingDescriptor();
    JdbcStatementDescriptor jdbcStatementDescriptor=jdbcBeanMappingDescriptor.getJdbcStatementDescriptor();
    ResultSetMappingDescriptor resultSetMappingDescriptor=jdbcBeanMappingDescriptor.getResultSetMappingDescriptor();
    // se siamo qui output message deve esseredi tipo JdbcOutputMessage
    if (!(outputMessage instanceof JdbcOutputMessage)){
      LOG.error("CIC001804_Jdbc_output_message_not_found", 
              outputMessage.getClass());
      throw new FormatException(MESSAGES.getString(
              "CIC001804_Jdbc_output_message_not_found", 
              outputMessage.getClass()));
    }   
    JdbcOutputMessage jdbcOutputMessage=(JdbcOutputMessage)outputMessage;
    
    // creo l'output bean
    WrapDynaBean wrappedOutputParameterBean=null;
    try {
      Object beanInstance=jdbcStatementDescriptor.getBeanClass().newInstance();
      wrappedOutputParameterBean=new ConvertingWrapDynaBean(beanInstance);
    }
    catch (IllegalAccessException e){
      LOG.error(e.getMessage(), e);
      throw new FormatException(e.getMessage(), e);
    }
    catch (InstantiationException e){
      LOG.error(e.getMessage(), e);
      throw new FormatException(e.getMessage(), e);
    }
    
    //per prima cosa elaboriamo gli output parameter    
    if (jdbcStatementDescriptor.getStatementType()==JdbcStatementDescriptor.CALLABLE_STATEMENT){
      // in questo caso devo gestire eventuali output parameter
      Map<String, Integer> propertyNameParameterIndexMap=jdbcStatementDescriptor.getPropertyNameParameterIndexMap();
      DisconnectedCallableStatement disconnectedCallableStatement=(DisconnectedCallableStatement)jdbcOutputMessage.getDisconnectedSatetement();
      for (Iterator<String> i=propertyNameParameterIndexMap.keySet().iterator();i.hasNext();){
        try {
          String propertyName=i.next();
          int parameterIndex=propertyNameParameterIndexMap.get(propertyName).intValue();
          JdbcParameterDescriptor jdbcParameterDescriptor=jdbcStatementDescriptor.getParameter(parameterIndex);
          if (jdbcParameterDescriptor.getInOutType()==JdbcParameterDescriptor.IN_OUT_PARAMETER || jdbcParameterDescriptor.getInOutType()==JdbcParameterDescriptor.OUT_PARAMETER){
            Object value=disconnectedCallableStatement.getObject(parameterIndex);
            wrappedOutputParameterBean.set(propertyName,value);
          }
        }
        catch (SQLException e){
          LOG.error(e.getMessage(), e);
          throw new FormatException(e.getMessage(), e);
        }
      }      
    }
    
    // a questo punto devo mappare l'eventuale result set  
    if (jdbcStatementDescriptor.getSqlType()==JdbcStatementDescriptor.SELECT_SQL){
      List<Object> outputResultSetBeanList=new ArrayList<Object>();
      try{
        // costruendo con lowercase a true dovrebbe semplificare (rendere più portabile), l'unico problema si ha se una query ha due colonne 
        // che differiscono solo per il lowercase, ma lo ritengo improbaile
        ResultSetDynaClass resultSetDynaClass=new ResultSetDynaClass(jdbcOutputMessage.getResultSet(),true);
        // ciclo sulle righe del result set
        for (Iterator i=resultSetDynaClass.iterator();i.hasNext();){
          DynaBean rowDynaBean=(DynaBean)i.next();
          // creo l'output bean
          WrapDynaBean wrappedOutputResultSetBean=null;
          try {
            Object beanInstance=resultSetMappingDescriptor.getBeanClass().newInstance();
            wrappedOutputResultSetBean=new ConvertingWrapDynaBean(beanInstance);
          }
          catch (IllegalAccessException e){
            LOG.error(e.getMessage(), e);
            throw new FormatException(e.getMessage(), e);
          }
          catch (InstantiationException e){
            LOG.error(e.getMessage(), e);
            throw new FormatException(e.getMessage(), e);
          }
          // ciclo sulle colonne del result set construendo bean        
          Map<String, String> columnNamePropertyNameMap=resultSetMappingDescriptor.getColumnNamePropertyNameMap();
          for (Iterator<String> j=columnNamePropertyNameMap.keySet().iterator();j.hasNext();){
            String columnName=j.next();
            String propertyName=columnNamePropertyNameMap.get(columnName);            
            // metto il lower case
            Object value=rowDynaBean.get(columnName.toLowerCase());
            wrappedOutputResultSetBean.set(propertyName,value);
          }
          // a questo punto il bean è completo lo aggiungo alla lista
          outputResultSetBeanList.add(wrappedOutputResultSetBean.getInstance());
        }      
      }
      catch (SQLException e){
        LOG.error(e.getMessage(), e);
        throw new FormatException(e.getMessage(), e);
      }
      //imposto il result set nella property corretta del bean contenente
      wrappedOutputParameterBean.set(jdbcBeanMappingDescriptor.getResultSetPropertyName(),outputResultSetBeanList);
    }
    serviceContext.setOutputBean(wrappedOutputParameterBean.getInstance());
  }

}
