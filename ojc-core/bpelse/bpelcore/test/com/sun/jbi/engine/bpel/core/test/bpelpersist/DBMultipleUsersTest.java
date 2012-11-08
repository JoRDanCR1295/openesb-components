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
 * @(#)DBMultipleUsersTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.bpelpersist;

import java.util.Properties;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.core.test.common.TestContext;


/**
 * @author Sun Microsystems
 */
public class DBMultipleUsersTest extends AbstractTestCase {
    public static final String PROVIDER_URL = "file:.";

    public DBMultipleUsersTest(String testName) {
        super(testName);
    }

    public void testDerbyMultipleUsers() throws Exception {
/*        Utility.logEnter(getClass().getSimpleName(), "testDerbyMultipleUsers");
        String createTestTable = "CREATE TABLE TestTable (TestTableColumn1 varchar(128))";

        DBSchemaCreation tablesCreator = DBSchemaCreation.getInstance();
        
        DBConnectionFactory dbConnFac = initTestUser("TestUser1");
        Connection conn1 = dbConnFac.createNonTxConnection();
        Statement stmt1 = conn1.createStatement();
        stmt1.execute(createTestTable);
        stmt1.execute("Insert into TestTable values ('TestUser1')");
        ResultSet rs1 = stmt1.executeQuery("Select * from TestTable");
        assertTrue(rs1.next());        
        rs1.close();
        stmt1.close();
        conn1.close();
        
        //Change the connection
        dbConnFac = initTestUser("TestUser2");
        Connection conn2 = dbConnFac.createNonTxConnection();
        Statement stmt2 = conn2.createStatement();
        ResultSet rs2;
        
        //Make sure that the seclect fails because TestTable has not been created for
        //TestUser2 yet
        boolean selectSucceeded = true;
        try {
            rs2 = stmt2.executeQuery("Select * from TestTable");
        } catch (SQLException ex) {
            selectSucceeded = false;
        }        
        assert(!selectSucceeded);
        
        stmt2.execute(createTestTable);
        
        //Make sure that the select returns nothing since the TestTable for TestUser2 
        //does not have anything yet
        rs2 = stmt2.executeQuery("Select * from TestTable");
        assertTrue(!rs2.next());
        stmt2.execute("Insert into TestTable values ('TestUser2')");

        //Make sure that the inserted value is same as that was inserted by this
        //TestUser2
        rs2 = stmt2.executeQuery("Select * from TestTable");
        assertTrue(rs2.next());
        String s2 = rs2.getString("TestTableColumn1");
        assertTrue(s2.equals("TestUser2"));
        stmt2.execute("Drop table TestTable");
        rs2.close();
        stmt2.close();
        conn2.close();
        
        dbConnFac = initTestUser("TestUser1");
        Connection conn3 = dbConnFac.createNonTxConnection();
        Statement stmt3 = conn3.createStatement();
        stmt3.execute("Drop table TestTable");
        stmt3.close();
        conn3.close();
        
        Utility.logExit(getClass().getSimpleName(), "testDerbyMultipleUsers");*/
    }
    
    public void testOracleMultipleUsersTest() throws Exception {
        //TODO
    }    
    
    private DBConnectionFactory initTestUser(String userpass) throws Exception {
        Properties props = new Properties();
        props.setProperty(ConnectionProperties.DB_TYPE, ConnectionProperties.DERBY_DB.toString());
        props.setProperty(ConnectionProperties.DatabaseNonXAJNDIName, "jdbc/DerbyXA");
        props.setProperty(ConnectionProperties.DatabaseXAJNDIName, "jdbc/DerbyNonXA");
        props.setProperty(ConnectionProperties.DB_URL, "jdbc:derby://localhost:1527/testUserDB;create=true");        
        props.setProperty(ConnectionProperties.DB_USERNAME, userpass);
        props.setProperty(ConnectionProperties.DB_PASSWORD, userpass);

        DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(props);
        DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
            new DummyNonXATxManagerAndDataSource(props);
        BPELSERegistry registry = BPELSERegistry.getInstance();
        registry.register(TransactionManager.class.getName(), dummyTMAndDS);
        InitialContext ctx = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, props);
            
        return new DBConnectionFactory(props, ctx, null);
    }
}
