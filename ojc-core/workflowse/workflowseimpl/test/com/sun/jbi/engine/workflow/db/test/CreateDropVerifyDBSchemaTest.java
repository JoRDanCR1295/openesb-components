/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)CreateDropVerifyDBSchemaTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Properties;

import javax.sql.DataSource;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.jbi.engine.workflow.db.connection.ConnectionManager;
import com.sun.jbi.engine.workflow.db.connection.ConnectionProperties;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.schema.DBSchemaCreator;

/**
 *
 * @author jwaldorf
 */
public class CreateDropVerifyDBSchemaTest extends TestCase {
    
    private static String DBNAME="workflowDB;create=true";
    private static int NETWORKSERVER_PORT=1527;   
    private static final String DERBY_CLIENT_DS = "org.apache.derby.jdbc.ClientDataSource";
    private static final String DERBY_CLIENT_URL= "jdbc:derby://localhost:"+ NETWORKSERVER_PORT+"/"+DBNAME+";create=true";
    
    private String jdbcDataSource = DERBY_CLIENT_DS;    


    public CreateDropVerifyDBSchemaTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CreateDropVerifyDBSchemaTest.class);

        return suite;
    }
    
    /**
     * Creates a client data source and sets all the necessary properties in order to
     * connect to Derby Network Server
     * The server is assumed to be running on 1527 and on the localhost
     * @param   database    database name; can include Derby URL attributes
     * @param   user        database user
     * @param   password
     * @return  returns DataSource
     * @throws Exception if there is any error
     */
    public javax.sql.DataSource getClientDataSource(String database, String user, String
                                      password) throws SQLException, ClassNotFoundException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException
    {
        Class nsDataSource = Class.forName(jdbcDataSource);
        DataSource ds = (DataSource) nsDataSource.newInstance();

        // can also include Derby URL attributes along with the database name
        Class[] methodParams = new Class[] {String.class};
        Method dbname = nsDataSource.getMethod("setDatabaseName", methodParams);
        Object[] args = new Object[] {database};
        dbname.invoke(ds, args);

        if (user != null) {
            Method setuser = nsDataSource.getMethod("setUser", methodParams);
            args = new Object[] {user};
            setuser.invoke(ds, args);
        }
        if (password != null) {
            Method setpw = nsDataSource.getMethod("setPassword", methodParams);
            args = new Object[] {password};
            setpw.invoke(ds, args);
        }
        // host on which network server is running
        Method servername = nsDataSource.getMethod("setServerName", methodParams);
        args = new Object[] {"localhost"};
        servername.invoke(ds, args);

        // port on which Network Server is listening
        methodParams = new Class[] {int.class};
        Method portnumber = nsDataSource.getMethod("setPortNumber", methodParams);
        args = new Object[] {new Integer(1527)};
        portnumber.invoke(ds, args);

        return ds;

    }
    

    /**
     * Get a database connection from DataSource
     * @pre Derby Network Server is started
     * @param   ds  data source
     * @return  returns database connection
     * @throws Exception if there is any error
     */
    public Connection getClientDataSourceConn(javax.sql.DataSource ds)
        throws Exception
    {
        Connection conn = ds.getConnection();
        System.out.print("connection from datasource; getDriverName = ");
        System.out.println(conn.getMetaData().getDriverName());
        return conn;
    }
    
    

    public void testGetConnection() {
          try {
//              Connection dbConnection = getClientDataSourceConn(getClientDataSource(DBNAME, "workflow", "workflow"));
//              assertNotNull(dbConnection);
             Properties props = new Properties ();
             props.setProperty(ConnectionProperties.DB_TYPE, DAO.DERBY);
             props.setProperty(ConnectionProperties.DB_DATABASE_NAME, "workflowDB");
             props.setProperty(ConnectionProperties.DB_SERVER_NAME, "localhost");
             props.setProperty(ConnectionProperties.DB_PORT, "1527");
             props.setProperty(ConnectionProperties.DB_USERNAME, "workflow");
             props.setProperty(ConnectionProperties.DB_PASSWORD, "workflow");
             
             ConnectionManager.init(props);
             
             ConnectionManager connectionManager = ConnectionManager.getInstance();
             com.sun.jbi.engine.workflow.db.connection.Connection conn = connectionManager.getConnection();
             Connection con = conn.getConnection();
             System.out.print("connection from datasource; getDriverName = ");
             System.out.println(con.getMetaData().getDriverName());     
             connectionManager.freeConnection(conn);

        } catch (Exception ex) {
            fail(ex.getMessage());
        }
    }
    
    public void testCheckSchema () {
        try {
//            Connection dbConnection = getClientDataSourceConn(getClientDataSource(DBNAME, "workflow", "workflow"));
//            assertNotNull(dbConnection);
           Properties props = new Properties ();
           props.setProperty(ConnectionProperties.DB_TYPE, DAO.DERBY);
           props.setProperty(ConnectionProperties.DB_DATABASE_NAME, "WORKFLOWDB");
           props.setProperty(ConnectionProperties.DB_SERVER_NAME, "localhost");
           props.setProperty(ConnectionProperties.DB_PORT, "1527");
           props.setProperty(ConnectionProperties.DB_USERNAME, "WORKFLOW");
           props.setProperty(ConnectionProperties.DB_PASSWORD, "WORKFLOW");
           
           DataSource dataSource = DAOFactory.getDataSource(props, DAOFactory.getDBType(DAO.DERBY));
           
           DBSchemaCreator dbSchemaCreator = new DBSchemaCreator (props.getProperty(ConnectionProperties.DB_TYPE), dataSource, "WORKFLOW");
           boolean result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
           dbSchemaCreator.executeScript("workflow_drop.sql");

           result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
           assertFalse("schema should not exists", result);
           System.out.println("Schema exist : " + result);
           
           
           dbSchemaCreator.executeScript("workflow_schema.sql");
           result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
           assertTrue("schema should exists", result);
           System.out.println("Schema exist : " + result);
            
           System.out.println("Executed script successfully");
      } catch (Exception ex) {
          fail(ex.getMessage());
      }
    }
      
      public void testDropSchema () {
          try {
//              Connection dbConnection = getClientDataSourceConn(getClientDataSource(DBNAME, "workflow", "workflow"));
//              assertNotNull(dbConnection);
             Properties props = new Properties ();
             props.setProperty(ConnectionProperties.DB_TYPE, DAO.DERBY);
             props.setProperty(ConnectionProperties.DB_DATABASE_NAME, "WORKFLOWDB");
             props.setProperty(ConnectionProperties.DB_SERVER_NAME, "localhost");
             props.setProperty(ConnectionProperties.DB_PORT, "1527");
             props.setProperty(ConnectionProperties.DB_USERNAME, "WORKFLOW");
             props.setProperty(ConnectionProperties.DB_PASSWORD, "WORKFLOW");
             
             DataSource dataSource = DAOFactory.getDataSource(props, DAOFactory.getDBType(DAO.DERBY));
             
             DBSchemaCreator dbSchemaCreator = new DBSchemaCreator (props.getProperty(ConnectionProperties.DB_TYPE), dataSource, "WORKFLOW");
             try {
                 dbSchemaCreator.executeScript("workflow_drop.sql");
             } catch (Exception e) {
                 e.printStackTrace();
             }
 
             boolean result = dbSchemaCreator.checkSchemaAndTablesIntegrity();
             assertFalse("schema should not exists", result);
             System.out.println("Schema exist : " + result);
              
             System.out.println("Executed script successfully");
        } catch (Exception ex) {
            fail(ex.getMessage());
        }      
      
  }    
    

    /**
     * Runs the test suite using the textual runner.
     */
    public static void main(String[] args) {
        junit.textui.TestRunner.run(suite());
    }
}
