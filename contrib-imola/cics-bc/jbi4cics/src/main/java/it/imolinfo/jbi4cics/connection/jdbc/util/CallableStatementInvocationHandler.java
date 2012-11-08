/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/

package it.imolinfo.jbi4cics.connection.jdbc.util;

import it.imolinfo.jbi4cics.jbi.Messages;
import java.lang.reflect.Method;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;


/**
 * @author raffaele
 *
 */
public class CallableStatementInvocationHandler extends PreparedStatementInvocationHandler {
    
  /**
   * The logger for this class and its instances.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(CallableStatementInvocationHandler.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(CallableStatementInvocationHandler.class);

  private List<Object> paramterNameList=new Vector<Object>();
  private List<Object> parameterIndexList=new Vector<Object>();
  private Map<String, Object> nameValueParamterMap=new HashMap<String, Object>();
  private Map<Integer, Object> indexValueParamterMap=new HashMap<Integer, Object>();

  /**
   * @param sql    The SQL
   */
  public CallableStatementInvocationHandler(String sql) {
    super(sql);
    // TODO Auto-generated constructor stub
  }

  /**
   * @param sql    The SQL
   * @param resultSetType    The result set type
   * @param resultSetConcurrency    The result set Councurrency
   */
  public CallableStatementInvocationHandler(String sql, int resultSetType, int resultSetConcurrency) {
    super(sql, resultSetType, resultSetConcurrency);
    // TODO Auto-generated constructor stub
  }

  /**
   * @param sql    The SQL
   * @param resultSetType    The result set type
   * @param resultSetConcurrency    The result set Councurrency
   * @param resultSetHoldability    The result set Holdability
   */
  public CallableStatementInvocationHandler(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) {
    super(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    // TODO Auto-generated constructor stub
  }
  
  /**
   * @return    The create statement
   * @throws SQLException    The SQL exception
   */
  protected Statement createStatement() throws SQLException {
    return getConnection().prepareCall(sql,resultSetType,resultSetConcurrency,resultSetHoldability);
  }
  
  
  /**
   * La gestione e' simile a quella degli altri statement, ma in questo caso bisogna aggiungere 
   * la gestione dei parametri della stored procedur. In particolare e' importante gestire gli outparameter.
   * 
   * @param proxy    The proxy    
   * @param method    The method   
   * @param args    The args  
   * @return object    The invoked object 
   * @throws Throwable    The Throwable    
   */
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    if (method.getName().startsWith("execute")){
      Object object=super.invoke(proxy,method,args);
      handlePostExecute();
      return object;
    }
    // per prima cosa cerchiamo di capire se il metodo è chiamato sull'interfaccia CallableStatement
    Class clazz=method.getDeclaringClass();
    if (!(clazz.equals(CallableStatement.class))){
      // se il metodo non appartiene a CallableStatement lo può gestire la classe madre
      return super.invoke(proxy,method,args);
    }
    if (method.getName().startsWith("set")){
      // anche in questo caso lo può gestire la classe madre
      return super.invoke(proxy,method,args);
    }
    

    /* a questo punto rimangono due tipi di metodi:
     * i register
     * e i get
     * 
     * poi c'e' was null che fa eccezione
     */
    
    if (method.getName().startsWith("register")){
      return handleRegister(proxy, method, args);
    }
    
    if (method.getName().startsWith("get")){
      return handleGetParameter(method, args);
    }
    
    return null;
  }

  /**
   * @param proxy    The proxy
   * @param method    The method
   * @param args    The args
   * @throws Throwable    The throwable
   * @throws SQLException    The SQL Exception
   * @return    The return object
   */
  private Object handleRegister(Object proxy, Method method, Object[] args) throws Throwable, SQLException {
    // se il primo argomento e' di tipo stringa
    LOG.debug("method.getParameterTypes(): "+Arrays.toString(method.getParameterTypes()));
    if (method.getParameterTypes()[0].equals(String.class)){
      //memorizzo che esiste un parametro con questo nome
      paramterNameList.add(args[0]);
      // faccio gestire il parametro dalla classe madre
      return super.invoke(proxy,method,args);
    }
    if (method.getParameterTypes()[0].equals(int.class)){
      //memorizzo che esiste un parametro con questo nome
      parameterIndexList.add(args[0]);
      // faccio gestire il parametro dalla classe madre
      return super.invoke(proxy,method,args);  
    }
    throw new SQLException(MESSAGES.getString("CIC000700_Error_registering_parameter", new Object[] {Arrays.toString(args)}));
  }
  
  private Object handleGetParameter(Method method, Object[] args) throws SQLException {
    if (isConnected()) {
      return invokeMethod(method,args);
    }
    else {
      if (args.length>1) {
        throw new SQLException (MESSAGES.getString("CIC000701_Getting_multiple_parameters_unsupported"));
      }
      if (method.getParameterTypes()[0].equals(String.class)){
        return nameValueParamterMap.get(args[0]);
      }
      if (method.getParameterTypes()[0].equals(int.class)){
        return indexValueParamterMap.get(args[0]);
      }
      throw new SQLException(MESSAGES.getString("CIC000702_Unknown_arguments", new Object[] {Arrays.toString(args)}));
    }
  }
  
  private void handlePostExecute() throws SQLException {
    // dopo l'esecuzione devo leggere e cacheare tutti gli out parameter che sono stati registrati
    for (Iterator<Object> i=paramterNameList.iterator();i.hasNext();){
      String parameterName=(String)i.next();
      Object value=((CallableStatement)getUnderlyingStatement()).getObject(parameterName);
      nameValueParamterMap.put(parameterName,value);
    }
    for (Iterator<Object> i=parameterIndexList.iterator();i.hasNext();){
      int index=((Integer)i.next()).intValue();
      Object value=((CallableStatement)getUnderlyingStatement()).getObject(index);
      indexValueParamterMap.put(new Integer(index),value);
    }
    LOG.debug("name : value keys: "+Arrays.toString(nameValueParamterMap.keySet().toArray()));
    LOG.debug("name : value values: "+Arrays.toString(nameValueParamterMap.values().toArray()));
    LOG.debug("index : value keys: "+Arrays.toString(indexValueParamterMap.keySet().toArray()));
    LOG.debug("index : value values: "+Arrays.toString(indexValueParamterMap.values().toArray()));
  }
}


