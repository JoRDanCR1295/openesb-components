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
package it.imolinfo.jbi4cics.test.tappo.storedprocedure;


/**
 * @author raffaele
 *
 */
public class TappoStoredProcedure {

  /**
   * 
   *//*
  public TappoStoredProcedure() {
    super();
    // TODO Auto-generated constructor stub
  }

  *//**
   * @param args
   *//*
  public static void test_inout(String[] args) throws SQLException {
    try {
      Connection conn = DriverManager.getConnection("jdbc:default:connection");
      String paramIn=args[0];
      System.out.println("esecuzione TappoSP.test_inout, input data: "+paramIn);
      PreparedStatement prep=conn.prepareStatement("select id,output_data from jbi4cics.tappo where input_data=?");
      prep.setString(1,paramIn);
      ResultSet rs=prep.executeQuery();
      rs.next();
      String paramOut=rs.getString("output_data");
      args[0]=paramOut;
      System.out.println("esecuzione TappoSP.test_inout, ouput data: "+paramOut);
      rs.close();
      prep.close();
      conn.close();
    }
    catch (SQLException e){
      e.printStackTrace();
      throw e;
    }
  }
  
  *//**
   * @param args
   *//*
  public static void test_rs(String[] args,ResultSet[] argRs) throws SQLException {
    try {
      Connection conn = DriverManager.getConnection("jdbc:default:connection");
      String paramIn=args[0];
      System.out.println("esecuzione TappoSP.test_rs, input data: "+paramIn);
      PreparedStatement prep=conn.prepareStatement("select id,output_data from jbi4cics.tappo where input_data=?");
      prep.setString(1,paramIn);
      ResultSet rs=prep.executeQuery();
      rs.next();
      String paramOut=rs.getString("output_data");
      args[0]=paramOut;
      System.out.println("esecuzione TappoSP.test_rs, ouput data: "+paramOut);
      rs.close();
      prep.close();
      PreparedStatement prep1=conn.prepareStatement("select id,input_data,output_data from jbi4cics.tappo");      
      ResultSet rs1=prep1.executeQuery();      
      argRs[0]=rs1;
      System.out.println("esecuzione TappoSP.test_rs, ouput rs: "+rs1);;      
      conn.close();
    }
    catch (SQLException e){
      e.printStackTrace();
      throw e;
    }
  }  */

}
