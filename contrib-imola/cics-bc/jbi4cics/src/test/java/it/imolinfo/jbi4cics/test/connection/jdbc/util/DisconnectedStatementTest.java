/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.connection.jdbc.util;


public class DisconnectedStatementTest extends BaseJdbcTestCase {
  
/*  private static Log log=LogFactory.getLog(DisconnectedStatementTest.class);  
  
  public DisconnectedStatementTest(String arg0) {
    super(arg0);
  }
  

  
  public void testConnection(){
    assertNotNull(conn);
  }
  
  public void testDisconnectedStatement(){
    try{
      DisconnectedStatement statement=DisconnectedStatementFactory.createDisconnectedStatement();
      statement.setConnection(conn);
      statement.setConnected(true);
      ResultSet resultset=statement.executeQuery("select * from SYS.SYSTABLES"); 
      statement.setConnected(false);
      conn.close();
      log.info("resultset: "+resultset.toString());
    }
    catch (SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }
  
  public void testDisconnectedPreparedStatement(){
    try{
      DisconnectedPreparedStatement statement=DisconnectedStatementFactory.prepareDisconnectedStatement("select * from SYS.SYSTABLES where TABLETYPE=?");
      statement.setString(1,"S");
      statement.setConnection(conn);
      statement.setConnected(true);
      ResultSet resultset=statement.executeQuery(); 
      statement.setConnected(false);
      conn.close();
      log.info("resultset: "+resultset.toString());
    }
    catch (SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }  
  
  public void testDisconnectedCallableStatement(){
    try{
      DisconnectedCallableStatement statement=DisconnectedStatementFactory.prepareDisconnectedCall("{call SYSCS_UTIL.SYSCS_SET_DATABASE_PROPERTY (?,?)}");
      statement.setString(1,"prova_name");
      statement.setString(2,"prova_value");
      statement.setConnection(conn);
      statement.setConnected(true);
      statement.execute(); 
      statement.setConnected(false);
      conn.close();      
    }
    catch (SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }    
  
  public void testDisconnectedCallableStatement_resultSet(){
    try{
      DisconnectedCallableStatement statement=DisconnectedStatementFactory.prepareDisconnectedCall("{call jbi4cics.test_rs (?)}");
      statement.registerOutParameter(1,Types.VARCHAR);
      statement.setString(1,"hello");      
      statement.setConnection(conn);
      statement.setConnected(true);
      boolean res=statement.execute();
      log.info("res: "+res);
      ResultSet rs=statement.getResultSet();      
      statement.setConnected(false);
      conn.close();
      String result=statement.getString(1);      
      assertEquals("world",result);
      log.info("returned result set: "+rs);
      assertNotNull(rs);
    }
    catch (SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }   
  
  public void testDisconnectedCallableStatement_registerParameter(){
    try{
      DisconnectedCallableStatement statement=DisconnectedStatementFactory.prepareDisconnectedCall("{call jbi4cics.test_inout (?)}");
      statement.registerOutParameter(1,Types.VARCHAR);
      statement.setString(1,"hello");      
      statement.setConnection(conn);
      statement.setConnected(true);
      statement.execute(); 
      statement.setConnected(false);
      conn.close();
      String result=statement.getString(1);      
      assertEquals("world",result);
    }
    catch (SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }     
  
//  public void testProva(){
//    try{
//      //CallableStatement statement=conn.prepareCall("{call sqlj.install_jar (?, ?, ?)}");
//      CallableStatement statement=conn.prepareCall("{call sqlj.replace_jar (?, ?)}");
//      statement.setString(1,"file:///home/raffaele/java/workspace/jbi4cics/target/jbi4cics-0.1.jar");
//      statement.setString(2,"jbi4cics.tappo");
//      //statement.setInt(3,0);
//      statement.execute(); 
//      conn.close();      
//    }
//    catch (SQLException e){
//      e.printStackTrace();
//      fail(e.getMessage());
//    }
//  } 
*/  
  

}
