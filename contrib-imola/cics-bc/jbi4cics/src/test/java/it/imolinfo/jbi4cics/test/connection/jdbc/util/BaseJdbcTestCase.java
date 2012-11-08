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
package it.imolinfo.jbi4cics.test.connection.jdbc.util;

import junit.framework.TestCase;

/**
 * @author raffaele
 *
 */
public class BaseJdbcTestCase extends TestCase {

/*  protected Connection conn;
  
  public void setUp(){
    try{
      Properties p=System.getProperties();
      p.put("derby.system.home", "./derby");
      DriverManager.registerDriver(new EmbeddedDriver());
      DriverManager.registerDriver(new DB2Driver());
      //conn=DriverManager.getConnection("jdbc:derby:jbi4cics;create=false");      
      conn=DriverManager.getConnection("jdbc:db2://localhost:1527/jbi4cics","jbi4cics","jbi4cics");
    }
    catch(SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  *//**
   * @param arg0
   *//*
  public BaseJdbcTestCase(String arg0) {
    super(arg0);
    // TODO Auto-generated constructor stub
  }
  
  public void tearDown(){
    try{
      if (conn!=null){
        conn.close();
      }
    }
    catch(SQLException e){
      e.printStackTrace();
      fail(e.getMessage());
    }
  }
*/
}
