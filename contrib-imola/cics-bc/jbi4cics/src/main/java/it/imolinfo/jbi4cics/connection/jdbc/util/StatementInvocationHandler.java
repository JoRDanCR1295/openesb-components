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
package it.imolinfo.jbi4cics.connection.jdbc.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.sql.rowset.CachedRowSet;
import com.sun.rowset.CachedRowSetImpl;
import it.imolinfo.jbi4cics.jbi.Messages;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;



/**
 * memorizza i set* appartenenti a Statement e gli altro metodi che ritornano void.
 * quando si effettua la chiamata setconnected(true) allora vengono chiamati in sequenza i metodi memorizzati
 * 
 * i metodi che non possono essere cacheati devono essere chiamati quando si è in stato connesso
 * @author raffaele
 *
 */
class StatementInvocationHandler implements InvocationHandler {
  
 /**
  * The logger for this class and its instances.
  */
  private static final Logger LOG
	          = LoggerFactory.getLogger(StatementInvocationHandler.class);
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(StatementInvocationHandler.class);
  
  private Connection connection;
  private Statement underlyingStatement;
  private boolean connected;
  
  private List<Command> commands=new ArrayList<Command>();
  
  /* pre imposto il default così si userà solo una firma per la creazione del resultset */
  protected int resultSetType=ResultSet.TYPE_FORWARD_ONLY;
  protected int resultSetConcurrency=ResultSet.CONCUR_READ_ONLY;
  protected int resultSetHoldability=ResultSet.CLOSE_CURSORS_AT_COMMIT;

  /**
   * 
   */
  public StatementInvocationHandler() {
    super();
  }
  
  /**
   * @param resultSetType    The result set type
   * @param resultSetConcurrency    The result set concurrency
   */
  public StatementInvocationHandler(int resultSetType, int resultSetConcurrency) {
    super();
    this.resultSetType=resultSetType;
    this.resultSetConcurrency=resultSetConcurrency;
  }
  
  /**
   * @param resultSetType    The result set type
   * @param resultSetConcurrency    The result set concurrency
   * @param resultSetHoldability    The result set holdability
   */
  public StatementInvocationHandler(int resultSetType, int resultSetConcurrency, int resultSetHoldability) {
    super();
    this.resultSetType=resultSetType;
    this.resultSetConcurrency=resultSetConcurrency;
    this.resultSetHoldability=resultSetHoldability;
  }  

  /* (non-Javadoc)
   * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
   */
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    
    // gestione dei get; non possono essere cacheati
    if (method.getName().startsWith("get")){
      if ("getUnderlyingStatement".equals( method )){
        return getUnderlyingStatement();
      }
      if (method.equals("getConnection")){
        return getConnection();
      }      
      return handleGet(proxy,method,args);
    }
    // gestione dei set; possono essere cacheati
    if (method.getName().startsWith("set")){
      if (method.getName().equals("setConnection")){
        setConnection((Connection)args[0]);
        return null;
      }
      if (method.getName().equals("setConnected")){
        setConnected(((Boolean)args[0]).booleanValue());
        return null;
      }  
      return handleSet(proxy,method,args);
      
    }
    
    // gestione degli execute; non possono essere cacheati
    if (method.getName().startsWith("execute")){
      return handleExecute(proxy,method,args);
    }
    
    if ("toString".equals(method.getName())){
      return toString();
    }
    
    // gli altri metodi ritonano void e possono essere cacheati
    return handleOtherMethods(proxy,method,args);
    
  }
  
  public Object handleGet(Object proxy, Method method, Object[] args) throws SQLException{
    if (isConnected()){
      return invokeMethod(method, args);
    }
    else {
      throw new SQLException("trying to invoke an execute method on a disconnected statement"); 
    }
  }
  
  public Object handleSet(Object proxy, Method method, Object[] args) throws SQLException{
    if (isConnected()){
      return invokeMethod(method, args);
    }
    else {
      commands.add(new Command(method,args));
      return null;
    }
  }  
  
  public Object handleExecute(Object proxy, Method method, Object[] args) throws SQLException{
    if (isConnected()){
      return invokeMethod(method, args);
    }
    else {
      throw new SQLException(MESSAGES.getString("CIC000703_Error_invoking_execute_method")); 
    }
  }  
  
  public Object handleOtherMethods(Object proxy, Method method, Object[] args) throws SQLException{
    if (isConnected()){
      return invokeMethod(method, args);
    }
    else {
      commands.add(new Command(method,args));
      return null;
    }
  }  

  /**
   * @return Returns the connection.
   */
  public Connection getConnection() {
    return connection;
  }

  /**
   * @param conn The connection to set.
   */
  public void setConnection(Connection conn) {
    this.connection = conn;
  }

  /**
   * @return Returns the connected.
   */
  public boolean isConnected() {
    return connected;
  }

  /**
   * @param connected The connected to set.
   * @throws SQLException The SQL exception
   */
  public void setConnected(boolean connected) throws SQLException {
    if (connected){
      if (connection!=null){
        if (underlyingStatement==null){
          //dobbiamo creare lo statement
          underlyingStatement=createStatement();
        }
      }
      else {
        throw new SQLException(MESSAGES.getString("CIC000704_Error_setting_connected_state"));
      }
      // devo eseguire tutti i comandi memorizzati
      for (Iterator<Command> i=commands.iterator();i.hasNext();){
        Command command=i.next();
        Method method=command.getMethod();
        Object[] args=command.getArgs();
        invokeMethod(method, args);        
      }      
    }
    else {
      // in questo modo il disconnected statement è riusabile
      underlyingStatement=null;
      connection=null;
    }
    this.connected = connected;
  }

  /**
   * @return Statement    The created statemant
   * @throws SQLException    The SQL exception
   */
  protected Statement createStatement() throws SQLException {
    return connection.createStatement(resultSetType,resultSetConcurrency,resultSetHoldability);
  }

  /**
   * @param method    The method
   * @param args    The args
   * @throws SQLException    The SQL Exception
   * @return Object    The invoked  method
   */
  protected Object invokeMethod(Method method, Object[] args) throws SQLException {
    try{
      Object result=method.invoke(underlyingStatement,args);
      if (result!=null && result instanceof ResultSet){
        CachedRowSet cachedRowSet=new CachedRowSetImpl();
        cachedRowSet.populate((ResultSet)result);
        return cachedRowSet;        
      }
      else {
        return result; 
      }      
    }
    catch(IllegalAccessException e){
      LOG.error("CIC000705_Illegal_access", new Object[]{method.toGenericString(), Arrays.toString(args), underlyingStatement}, e);
      throw new SQLException(MESSAGES.getString("CIC000705_Illegal_access", new Object[]{method.toGenericString(), Arrays.toString(args), underlyingStatement}));
    }
    catch(InvocationTargetException e){
      LOG.error("CIC000706_Target_exception", new Object[] {e.getTargetException()});
      Throwable targetException=e.getTargetException();
      if (targetException instanceof SQLException){ 
        throw (SQLException)e.getTargetException();
      }
      else {
        throw new SQLException(MESSAGES.getString("CIC000706_Target_exception", new Object[] {targetException.getMessage()}));
      }
    }
  }
  
  /**
   * @return A string
   */
  public String toString(){
    return "connected: "+connected+ ", "+
        (connected?"connection: "+connection+", ":"")+
        (connected?"underlyingStatement: "+underlyingStatement+", ":"")+
        (!connected?"stacked commands: "+Arrays.toString(commands.toArray()):"");
  }

  /**
   * @return Returns the underlyingStatement.
   */
  public Statement getUnderlyingStatement() {
    return underlyingStatement;
  }
 
  /**
   * 
   * @class Command     The commmand
   */
  private static class Command {
    private Method method;
    private Object[] args;
    public Command(Method method, Object[] args) {
      super();
      // TODO Auto-generated constructor stub
      this.method = method;
      this.args = args;
    }
    
    /**
     * @return Returns the args.
     */
    public Object[] getArgs() {
      return args;
    }
    
    /**
     * @param args The args to set.
     */
    public void setArgs(Object[] args) {
      this.args = args;
    }
    /**
     * @return Returns the method.
     */
    public Method getMethod() {
      return method;
    }
    /**
     * @param method The method to set.
     */
    public void setMethod(Method method) {
      this.method = method;
    }
    
    public String toString(){
      return "method: "+method.toGenericString()+" ,"+
             "args: "+Arrays.toString(args);
    }
  }

}
